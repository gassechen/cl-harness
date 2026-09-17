#!/usr/bin/env bash
# Build a standalone cl-harness executable into bin/cl-harness.
#
# The binary boots with a FRESH engine (no baked session), so it can be copied
# anywhere: at runtime it resolves config.json / dumps/ / sessions/ / metrics/
# against the current working directory (or CL_HARNESS_DIR / CL_HARNESS_CONFIG).
#
# Usage:
#   ./build.sh                             # build bin/cl-harness
#   bin/cl-harness                         # interactive TUI REPL
#   CL_HARNESS_PROMPT="mi mensaje" bin/cl-harness   # one-shot run (preferred:
#                                          keeps the prompt out of argv so the
#                                          agent cannot pkill itself)
#   bin/cl-harness "mi mensaje"            # one-shot run (argv fallback)
set -euo pipefail
cd "$(dirname "$0")"

exec sbcl --noinform --non-interactive \
  --eval '(ql:quickload :cl-harness :silent t)' \
  --eval '(sb-ext:save-lisp-and-die
             (merge-pathnames #P"bin/cl-harness" *default-pathname-defaults*)
             :executable t
             :toplevel (function cl-harness:standalone-toplevel))'