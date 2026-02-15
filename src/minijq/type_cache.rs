use crate::minijq::types::Type;
use std::collections::BTreeMap;
use std::fmt::Write;
use std::fs;
use std::io;
use std::path::Path;
use std::path::PathBuf;

const CACHE_FORMAT_VERSION: u32 = 2;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct CachedReconstruction {
    pub final_input_type: Type,
    pub output_type: Type,
    pub subset_types: Vec<Type>,
    pub converged: bool,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct CacheLookupResult {
    pub analysis: CachedReconstruction,
    pub hit: bool,
}

#[derive(Clone, Debug)]
struct TypeCacheEntry {
    source: String,
    analysis: CachedReconstruction,
}

#[derive(Clone, Debug)]
pub struct TypeCache {
    path: PathBuf,
    entries: BTreeMap<String, TypeCacheEntry>,
    dirty: bool,
}

impl TypeCache {
    pub fn new(path: impl Into<PathBuf>) -> TypeCache {
        TypeCache {
            path: path.into(),
            entries: BTreeMap::new(),
            dirty: false,
        }
    }

    pub fn load_or_default(path: impl Into<PathBuf>) -> io::Result<TypeCache> {
        let path = path.into();
        if !path.exists() {
            return Ok(TypeCache::new(path));
        }

        let body = fs::read_to_string(&path)?;
        let entries = parse_cache(&body, &path)?;
        Ok(TypeCache {
            path,
            entries,
            dirty: false,
        })
    }

    pub fn len(&self) -> usize {
        self.entries.len()
    }

    pub fn lookup(&self, name: &str, source: &str) -> Option<CachedReconstruction> {
        self.entries
            .get(name)
            .filter(|entry| entry.source == source)
            .map(|entry| entry.analysis.clone())
    }

    pub fn resolve_or_insert<F>(
        &mut self,
        name: &str,
        source: &str,
        compute: F,
    ) -> CacheLookupResult
    where
        F: FnOnce() -> CachedReconstruction,
    {
        if let Some(analysis) = self.lookup(name, source) {
            return CacheLookupResult {
                analysis,
                hit: true,
            };
        }

        let analysis = compute();
        self.insert(name, source, analysis.clone());
        CacheLookupResult {
            analysis,
            hit: false,
        }
    }

    pub fn insert(&mut self, name: &str, source: &str, analysis: CachedReconstruction) {
        let next = TypeCacheEntry {
            source: source.to_string(),
            analysis,
        };
        let unchanged = self
            .entries
            .get(name)
            .map(|current| current.source == next.source && current.analysis == next.analysis)
            .unwrap_or(false);
        if unchanged {
            return;
        }

        self.entries.insert(name.to_string(), next);
        self.dirty = true;
    }

    pub fn save_if_dirty(&mut self) -> io::Result<bool> {
        if !self.dirty {
            return Ok(false);
        }

        self.save()?;
        Ok(true)
    }

    pub fn save(&mut self) -> io::Result<()> {
        if let Some(parent) = self.path.parent() {
            fs::create_dir_all(parent)?;
        }

        let rendered = render_cache(&self.entries)?;
        fs::write(&self.path, rendered)?;
        self.dirty = false;
        Ok(())
    }
}

fn render_cache(entries: &BTreeMap<String, TypeCacheEntry>) -> io::Result<String> {
    let mut out = String::new();
    let _ = writeln!(out, "# minijq type interface cache");
    let _ = writeln!(out, "# format-version: {CACHE_FORMAT_VERSION}");

    for (name, entry) in entries {
        let source_json = serde_json::to_string(&entry.source).map_err(|err| {
            io::Error::new(
                io::ErrorKind::InvalidData,
                format!("failed to encode source for `{name}`: {err}"),
            )
        })?;
        let final_input_json =
            serde_json::to_string(&entry.analysis.final_input_type).map_err(|err| {
                io::Error::new(
                    io::ErrorKind::InvalidData,
                    format!("failed to encode final input for `{name}`: {err}"),
                )
            })?;
        let output_json = serde_json::to_string(&entry.analysis.output_type).map_err(|err| {
            io::Error::new(
                io::ErrorKind::InvalidData,
                format!("failed to encode output for `{name}`: {err}"),
            )
        })?;
        let subset_types_json =
            serde_json::to_string(&entry.analysis.subset_types).map_err(|err| {
                io::Error::new(
                    io::ErrorKind::InvalidData,
                    format!("failed to encode subset markers for `{name}`: {err}"),
                )
            })?;

        let _ = writeln!(out);
        let _ = writeln!(
            out,
            "let {name} : {} -> {}",
            entry.analysis.final_input_type, entry.analysis.output_type
        );
        let _ = writeln!(out, "  -- source-json: {source_json}");
        let _ = writeln!(out, "  -- final-input-json: {final_input_json}");
        let _ = writeln!(out, "  -- output-json: {output_json}");
        let _ = writeln!(out, "  -- subset-types-json: {subset_types_json}");
        let _ = writeln!(out, "  -- converged: {}", entry.analysis.converged);
    }

    Ok(out)
}

fn parse_cache(body: &str, path: &Path) -> io::Result<BTreeMap<String, TypeCacheEntry>> {
    #[derive(Debug, Default)]
    struct PendingEntry {
        name: String,
        source: Option<String>,
        final_input_type: Option<Type>,
        output_type: Option<Type>,
        subset_types: Option<Vec<Type>>,
        converged: Option<bool>,
    }

    impl PendingEntry {
        fn into_entry(self, path: &Path) -> io::Result<(String, TypeCacheEntry)> {
            let source = self.source.ok_or_else(|| {
                io::Error::new(
                    io::ErrorKind::InvalidData,
                    format!(
                        "missing source metadata for `{}` in `{}`",
                        self.name,
                        path.display()
                    ),
                )
            })?;
            let final_input_type = self.final_input_type.ok_or_else(|| {
                io::Error::new(
                    io::ErrorKind::InvalidData,
                    format!(
                        "missing final-input metadata for `{}` in `{}`",
                        self.name,
                        path.display()
                    ),
                )
            })?;
            let output_type = self.output_type.ok_or_else(|| {
                io::Error::new(
                    io::ErrorKind::InvalidData,
                    format!(
                        "missing output metadata for `{}` in `{}`",
                        self.name,
                        path.display()
                    ),
                )
            })?;
            let subset_types = self.subset_types.unwrap_or_default();
            let converged = self.converged.unwrap_or(true);

            Ok((
                self.name,
                TypeCacheEntry {
                    source,
                    analysis: CachedReconstruction {
                        final_input_type,
                        output_type,
                        subset_types,
                        converged,
                    },
                },
            ))
        }
    }

    let mut entries: BTreeMap<String, TypeCacheEntry> = BTreeMap::new();
    let mut pending: Option<PendingEntry> = None;
    let mut seen_version = false;

    for (idx, raw_line) in body.lines().enumerate() {
        let line_no = idx + 1;
        let trimmed = raw_line.trim();
        if trimmed.is_empty() {
            continue;
        }

        if let Some(rest) = trimmed.strip_prefix("# format-version:") {
            let version = rest.trim().parse::<u32>().map_err(|err| {
                io::Error::new(
                    io::ErrorKind::InvalidData,
                    format!(
                        "invalid cache format version at {}:{}: {err}",
                        path.display(),
                        line_no
                    ),
                )
            })?;
            if version != CACHE_FORMAT_VERSION {
                return Err(io::Error::new(
                    io::ErrorKind::InvalidData,
                    format!(
                        "unsupported cache format {version} in `{}` (expected {CACHE_FORMAT_VERSION})",
                        path.display()
                    ),
                ));
            }
            seen_version = true;
            continue;
        }

        if trimmed.starts_with('#') {
            continue;
        }

        if let Some(rest) = trimmed.strip_prefix("let ") {
            if let Some(current) = pending.take() {
                let (name, entry) = current.into_entry(path)?;
                entries.insert(name, entry);
            }

            let (name_part, _) = rest.split_once(" : ").ok_or_else(|| {
                io::Error::new(
                    io::ErrorKind::InvalidData,
                    format!("invalid let declaration at {}:{}", path.display(), line_no),
                )
            })?;
            pending = Some(PendingEntry {
                name: name_part.trim().to_string(),
                ..PendingEntry::default()
            });
            continue;
        }

        let Some(current) = pending.as_mut() else {
            return Err(io::Error::new(
                io::ErrorKind::InvalidData,
                format!(
                    "entry metadata without declaration at {}:{}",
                    path.display(),
                    line_no
                ),
            ));
        };

        let metadata = trimmed.trim_start_matches('-').trim();
        if let Some(rest) = metadata.strip_prefix("source-json:") {
            let source = serde_json::from_str::<String>(rest.trim()).map_err(|err| {
                io::Error::new(
                    io::ErrorKind::InvalidData,
                    format!(
                        "invalid source-json for `{}` at {}:{}: {err}",
                        current.name,
                        path.display(),
                        line_no
                    ),
                )
            })?;
            current.source = Some(source);
            continue;
        }

        if let Some(rest) = metadata.strip_prefix("source:") {
            current.source = Some(rest.trim().to_string());
            continue;
        }

        if let Some(rest) = metadata.strip_prefix("final-input-json:") {
            let final_input = serde_json::from_str::<Type>(rest.trim()).map_err(|err| {
                io::Error::new(
                    io::ErrorKind::InvalidData,
                    format!(
                        "invalid final-input-json for `{}` at {}:{}: {err}",
                        current.name,
                        path.display(),
                        line_no
                    ),
                )
            })?;
            current.final_input_type = Some(final_input);
            continue;
        }

        if let Some(rest) = metadata.strip_prefix("output-json:") {
            let output = serde_json::from_str::<Type>(rest.trim()).map_err(|err| {
                io::Error::new(
                    io::ErrorKind::InvalidData,
                    format!(
                        "invalid output-json for `{}` at {}:{}: {err}",
                        current.name,
                        path.display(),
                        line_no
                    ),
                )
            })?;
            current.output_type = Some(output);
            continue;
        }

        if let Some(rest) = metadata.strip_prefix("subset-types-json:") {
            let subset_types = serde_json::from_str::<Vec<Type>>(rest.trim()).map_err(|err| {
                io::Error::new(
                    io::ErrorKind::InvalidData,
                    format!(
                        "invalid subset-types-json for `{}` at {}:{}: {err}",
                        current.name,
                        path.display(),
                        line_no
                    ),
                )
            })?;
            current.subset_types = Some(subset_types);
            continue;
        }

        if let Some(rest) = metadata.strip_prefix("converged:") {
            let converged = rest.trim().parse::<bool>().map_err(|err| {
                io::Error::new(
                    io::ErrorKind::InvalidData,
                    format!(
                        "invalid converged value for `{}` at {}:{}: {err}",
                        current.name,
                        path.display(),
                        line_no
                    ),
                )
            })?;
            current.converged = Some(converged);
            continue;
        }

        return Err(io::Error::new(
            io::ErrorKind::InvalidData,
            format!(
                "unrecognized cache line at {}:{}: `{}`",
                path.display(),
                line_no,
                raw_line
            ),
        ));
    }

    if let Some(current) = pending.take() {
        let (name, entry) = current.into_entry(path)?;
        entries.insert(name, entry);
    }

    if !seen_version {
        return Err(io::Error::new(
            io::ErrorKind::InvalidData,
            format!("missing cache format header in `{}`", path.display()),
        ));
    }

    Ok(entries)
}

#[cfg(test)]
mod tests {
    use super::{CachedReconstruction, TypeCache};
    use crate::minijq::types::Type;
    use std::fs;
    use std::path::PathBuf;
    use std::time::{SystemTime, UNIX_EPOCH};

    fn temp_cache_path(label: &str) -> PathBuf {
        let ts = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("clock should be after epoch")
            .as_nanos();
        std::env::temp_dir().join(format!(
            "minijq_type_cache_{label}_{}_{}.mjqi",
            std::process::id(),
            ts
        ))
    }

    fn cleanup(path: &PathBuf) {
        let _ = fs::remove_file(path);
    }

    #[test]
    fn cache_round_trip_hits_after_persist() {
        let path = temp_cache_path("round_trip");
        cleanup(&path);

        let mut cache = TypeCache::load_or_default(&path).expect("cache should load");
        let cached = CachedReconstruction {
            final_input_type: Type::String,
            output_type: Type::String,
            subset_types: vec![Type::String],
            converged: true,
        };
        cache.insert("identity", ".", cached.clone());
        assert!(cache.save_if_dirty().expect("cache should save"));

        let reloaded = TypeCache::load_or_default(&path).expect("cache should reload");
        assert_eq!(reloaded.len(), 1);
        let hit = reloaded
            .lookup("identity", ".")
            .expect("cache should hit after reload");
        assert_eq!(hit, cached);

        let iface_body = fs::read_to_string(path.clone()).expect("cache file should exist");
        assert!(iface_body.contains("let identity : String -> String"));
        assert!(iface_body.contains("-- final-input-json:"));
        assert!(iface_body.contains("-- subset-types-json:"));

        cleanup(&path);
    }

    #[test]
    fn source_change_invalidates_cached_entry() {
        let path = temp_cache_path("source_change");
        cleanup(&path);

        let mut cache = TypeCache::load_or_default(&path).expect("cache should load");
        cache.insert(
            "f",
            ".",
            CachedReconstruction {
                final_input_type: Type::Number,
                output_type: Type::Number,
                subset_types: vec![],
                converged: true,
            },
        );
        assert!(cache.save_if_dirty().expect("cache should save"));

        let replacement = CachedReconstruction {
            final_input_type: Type::String,
            output_type: Type::String,
            subset_types: vec![],
            converged: true,
        };
        assert_eq!(
            cache.lookup("f", "."),
            Some(CachedReconstruction {
                final_input_type: Type::Number,
                output_type: Type::Number,
                subset_types: vec![],
                converged: true,
            })
        );

        assert_eq!(cache.lookup("f", ". + ."), None);
        cache.insert("f", ". + .", replacement.clone());
        assert_eq!(cache.lookup("f", ". + ."), Some(replacement));

        cleanup(&path);
    }
}
