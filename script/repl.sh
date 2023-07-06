#!/usr/bin/env bash
set -o errexit -o nounset -o pipefail
cd "$(dirname "$0")/.."

clj $@ -J--add-opens=java.base/java.io=ALL-UNNAMED -M -m user