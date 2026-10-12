#!/usr/bin/env bash
# Render the Nature Ecology & Evolution Supplementary Information to HTML and print it to PDF.
# Run from anywhere: bash supplement/render-supplement-nee.sh
set -euo pipefail
cd "$(dirname "$0")"

CHROME="${CHROME:-$(command -v google-chrome || command -v chromium || command -v chromium-browser)}"

quarto render SI-nee.qmd
"${CHROME}" --headless=new --disable-gpu --no-sandbox --no-pdf-header-footer \
  --print-to-pdf=SI-nee.pdf "file://${PWD}/SI-nee.html" 2>/dev/null
echo "-> supplement/SI-nee.pdf"
