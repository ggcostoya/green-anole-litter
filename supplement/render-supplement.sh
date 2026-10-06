#!/usr/bin/env bash
# Render the Supporting Information appendix to HTML and print it to PDF.
# Run from anywhere: bash supplement/render-supplement.sh
set -euo pipefail
cd "$(dirname "$0")"

CHROME="${CHROME:-$(command -v google-chrome || command -v chromium || command -v chromium-browser)}"

quarto render SI-appendix.qmd
"${CHROME}" --headless=new --disable-gpu --no-sandbox --no-pdf-header-footer \
  --print-to-pdf=SI-appendix.pdf "file://${PWD}/SI-appendix.html" 2>/dev/null
echo "-> supplement/SI-appendix.pdf"
