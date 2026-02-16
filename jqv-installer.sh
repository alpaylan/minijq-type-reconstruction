#!/bin/sh
set -eu

REPO="${REPO:-alpaylan/minijq-type-reconstruction}"
BINARY_NAME="${BINARY_NAME:-jqv}"
INSTALL_DIR="${INSTALL_DIR:-$HOME/.local/bin}"

usage() {
  cat <<'EOF'
Install the latest jqv release binary.

Usage:
  sh jqv-installer.sh [--install-dir DIR] [--repo OWNER/REPO]

Environment variables:
  REPO         GitHub repo to install from (default: alpaylan/minijq-type-reconstruction)
  INSTALL_DIR  Destination directory (default: $HOME/.local/bin)
EOF
}

while [ "$#" -gt 0 ]; do
  case "$1" in
    --install-dir)
      INSTALL_DIR="$2"
      shift 2
      ;;
    --repo)
      REPO="$2"
      shift 2
      ;;
    -h|--help)
      usage
      exit 0
      ;;
    *)
      echo "unknown argument: $1" >&2
      usage >&2
      exit 2
      ;;
  esac
done

OS="$(uname -s)"
ARCH="$(uname -m)"

case "$OS" in
  Linux)
    case "$ARCH" in
      x86_64|amd64)
        TARGET="x86_64-unknown-linux-gnu"
        ;;
      *)
        echo "unsupported architecture on Linux: $ARCH" >&2
        exit 1
        ;;
    esac
    ;;
  Darwin)
    case "$ARCH" in
      arm64|aarch64)
        TARGET="aarch64-apple-darwin"
        ;;
      x86_64|amd64)
        TARGET="x86_64-apple-darwin"
        ;;
      *)
        echo "unsupported architecture on macOS: $ARCH" >&2
        exit 1
        ;;
    esac
    ;;
  *)
    echo "unsupported OS: $OS" >&2
    exit 1
    ;;
esac

ARCHIVE="${BINARY_NAME}-${TARGET}.tar.gz"
URL="https://github.com/${REPO}/releases/latest/download/${ARCHIVE}"

TMP_DIR="$(mktemp -d)"
cleanup() {
  rm -rf "$TMP_DIR"
}
trap cleanup EXIT

echo "Downloading ${ARCHIVE} from ${URL}"
curl -fsSL "$URL" -o "${TMP_DIR}/${ARCHIVE}"
tar -xzf "${TMP_DIR}/${ARCHIVE}" -C "$TMP_DIR"

BINARY_PATH="${TMP_DIR}/${BINARY_NAME}-${TARGET}/${BINARY_NAME}"
if [ ! -f "$BINARY_PATH" ]; then
  echo "unexpected archive layout: missing ${BINARY_PATH}" >&2
  exit 1
fi

if [ ! -d "$INSTALL_DIR" ]; then
  PARENT_DIR="$(dirname "$INSTALL_DIR")"
  if [ -w "$PARENT_DIR" ]; then
    mkdir -p "$INSTALL_DIR"
  elif command -v sudo >/dev/null 2>&1; then
    sudo mkdir -p "$INSTALL_DIR"
  else
    echo "cannot create install dir and sudo is unavailable: $INSTALL_DIR" >&2
    exit 1
  fi
fi

if [ -w "$INSTALL_DIR" ]; then
  install -m 0755 "$BINARY_PATH" "${INSTALL_DIR}/${BINARY_NAME}"
else
  if command -v sudo >/dev/null 2>&1; then
    sudo install -m 0755 "$BINARY_PATH" "${INSTALL_DIR}/${BINARY_NAME}"
  else
    echo "install dir is not writable and sudo is unavailable: $INSTALL_DIR" >&2
    exit 1
  fi
fi

echo "Installed ${BINARY_NAME} to ${INSTALL_DIR}/${BINARY_NAME}"
echo "Run '${BINARY_NAME} --jqv-help' to verify."
