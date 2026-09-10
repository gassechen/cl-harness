#!/usr/bin/env bash
# Lanza cl-harness con SBCL + Quicklisp (arranque limpio: sin banner, sin
# warnings de ASDF/UIOP; el proyecto se resuelve vía ~/quicklisp/local-projects/).
set -euo pipefail
cd "$(dirname "$0")"
exec sbcl --noinform --non-interactive \
     --eval '(ql:quickload :cl-harness)' \
     --eval '(cl-harness:start-harness)'