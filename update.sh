#!/usr/bin/env bash
# Monthly update (Task Scheduler "Tariflohntracker"): refresh the data with script.R,
# build and deploy the Framework site, then commit and push to GitHub.
# Output is appended to update.log.
set -euo pipefail

cd /k/Github/tariflohnmonitor
exec > >(tee -a update.log) 2>&1

echo "=== $(date +"%Y-%m-%d %H:%M:%S") start"

git pull

"/c/Program Files/R/R-4.3.1/bin/Rscript.exe" scripts/script.R

npm run deploy

git add .
if git diff --cached --quiet; then
  echo "no changes to commit"
else
  git commit -m "auto-commit $(date +"at %H:%M:%S on %d/%m/%Y")"
  git push
fi

echo "=== $(date +"%Y-%m-%d %H:%M:%S") done"
