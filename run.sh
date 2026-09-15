#!/usr/bin/env bash
# cl-harness launcher — SBCL + Quicklisp (arranque limpio: sin banner, sin
# warnings de ASDF/UIOP; el proyecto se resuelve via ~/quicklisp/local-projects/).
#
# Uso:
#   ./run.sh                 Modo TUI interactivo (como `opencode`)
#   ./run.sh run "mensaje"   Modo no interactivo: un turno y sale (como `opencode run "..."`)
#
# Variables de entorno:
#   CL_HARNESS_CONFIG  ruta a un config.json alternativo (p. ej. offline)
set -euo pipefail
cd "$(dirname "$0")"

CONFIG_OVERRIDE='(let ((p (uiop:getenv "CL_HARNESS_CONFIG")))
                   (when (and p (plusp (length p)))
                     (setf cl-harness:*config-file* (uiop:parse-native-namestring p))))'

case "${1:-}" in
  run|r)
    shift
    msg="$*"
    if [ -z "$msg" ]; then
      echo "usage: ./run.sh run \"mensaje\"" >&2
      exit 2
    fi
    export CL_HARNESS_RUN="$msg"
    exec sbcl --noinform --non-interactive \
         --eval '(ql:quickload :cl-harness)' \
         --eval "$CONFIG_OVERRIDE" \
         --eval '(cl-harness:run-one-shot (uiop:getenv "CL_HARNESS_RUN"))'
    ;;
  '')
    if [ -t 0 ]; then
      exec rlwrap sbcl --noinform --non-interactive \
           --eval '(ql:quickload :cl-harness)' \
           --eval "$CONFIG_OVERRIDE" \
           --eval '(cl-harness:start-harness)'
    else
      exec sbcl --noinform --non-interactive \
           --eval '(ql:quickload :cl-harness)' \
           --eval "$CONFIG_OVERRIDE" \
           --eval '(cl-harness:start-harness)'
    fi
    ;;
  *)
    echo "usage: ./run.sh [run \"mensaje\"]" >&2
    exit 2
    ;;
esac