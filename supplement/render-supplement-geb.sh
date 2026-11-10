#!/usr/bin/env bash
# Render the Supporting Information to HTML and print it to PDF.
# Run from anywhere: bash supplement/render-supplement-geb.sh
set -euo pipefail
cd "$(dirname "$0")"

CHROME="${CHROME:-$(command -v google-chrome || command -v chromium || command -v chromium-browser)}"

quarto render SI-geb.qmd
"${CHROME}" --headless=new --disable-gpu --no-sandbox --no-pdf-header-footer \
  --print-to-pdf=SI-geb.pdf "file://${PWD}/SI-geb.html" 2>/dev/null
echo "-> supplement/SI-geb.pdf"
