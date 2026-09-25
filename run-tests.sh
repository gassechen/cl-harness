#!/usr/bin/env bash
# cl-harness test runner — offline, sin llamadas al proveedor.
#
#   ./run-tests.sh              ejecuta la suite completa
#   ./run-tests.sh metrics      ejecuta sólo las pruebas cuyo nombre coincide
#
# Sale con código 0 si todo pasa, 1 si falla alguna, 2 si el sistema no carga.
# El sistema de pruebas vive en cl-harness.asd ("cl-harness/tests") porque
# Quicklisp sólo registra los .asd del primer nivel de local-projects/.
set -uo pipefail
cd "$(dirname "$0")"

FILTER="${1:-}"
TMPDIR_RUN="$(mktemp -d)"
trap 'rm -rf "$TMPDIR_RUN"' EXIT
LOADER="$TMPDIR_RUN/load.lisp"
RUNNER="$TMPDIR_RUN/run.lisp"

# Etapa 1: cargar el sistema. Se lee antes de que exista el paquete de las
# pruebas, así que aquí sólo se usan símbolos de CL y UIOP. No se llama a
# sb-ext:exit al terminar bien: mataría el proceso antes de leer la etapa 2.
cat > "$LOADER" <<'EOF'
(require :asdf)
(load "~/quicklisp/setup.lisp")
(handler-case
    (ql:quickload :cl-harness/tests :silent t)
  (error (e)
    (format t "~&ERROR CARGANDO LAS PRUEBAS: ~A~%" e)
    (sb-ext:exit :code 2)))
EOF

# Etapa 2: ejecutar. Se lee cuando el paquete CL-HARNESS/TESTS ya existe.
cat > "$RUNNER" <<EOF
(uiop:symbol-call :cl-harness/tests :main "$FILTER")
EOF

out=$(sbcl --noinform --non-interactive --load "$LOADER" --load "$RUNNER" 2>&1)
status=$?
printf '%s\n' "$out" | grep -vE "WARNING: redefining (QL-SETUP|QUICKLISP)|iiscv/tests"
exit "$status"
