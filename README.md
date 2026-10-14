# Tariflohn-Tracker

Interactive WIFO tool comparing collectively agreed wages (Tariflohnindex), consumer prices (VPI) and labour productivity in Austria, including the Benya formula and real wage development.

**Live:** https://data-science.wifo.ac.at/tariflohntracker/ (English: [`/en/`](https://data-science.wifo.ac.at/tariflohntracker/en/))

The site is an [Observable Framework](https://observablehq.com/framework/) app. An R script refreshes the data once a month; a scheduled task then rebuilds and deploys the site and pushes the new data to this repo.

## Repository layout

| Path | Purpose |
|---|---|
| `scripts/script.R` | Downloads and chains the source series, writes `src/data/*.csv` |
| `scripts/names.xlsx`, `scripts/KV_names.xlsx` | Series IDs and mappings between the 2006 and 2016 classifications |
| `src/index.md`, `src/en/index.md` | German and English page (text and layout) |
| `src/components/tracker.js` | Data pipeline, charts, inputs and xlsx download |
| `src/style.css` | Page and control styling |
| `src/data/` | `TLI.csv`, `VPI.csv`, `PROD.csv` written by `script.R` |
| `update.sh` | Monthly job: `script.R` → build + deploy → commit + push |
| `deploy.js` | Uploads `dist/` to the web server and removes files no longer in the build |
| `poll-watch.js` | Makes `npm run dev` work on network drives (see below) |

## Data

`script.R` combines Statistik Austria open data (via `STATcubeR`) and the WIFO database (via `wifo.data`):

| File | Series | Base |
|---|---|---|
| `TLI.csv` | Tariflohnindex: total, WKO classification, collective agreements, groups | 2006 = 100 (2006 and 2016 series chained) |
| `VPI.csv` | Consumer price index, total and main groups | 2020 = 100 (2010, 2015, 2020 chained) |
| `PROD.csv` | Productivity per hour worked / per employee, production sectors and total economy | 2010 = 100 (2010, 2015, 2021 chained) |

The VGR-based total-economy rows in `PROD.csv` (`Gesamt (VGR-Daten; Alle Sektoren)`) are GDP per employee/hour levels, not an index. The app rebases every series to the selected base period, so this does not affect the charts.

## Monthly update

The Windows Task Scheduler task **Tariflohntracker** runs `update.sh` with Git Bash on the 20th of each month at 07:00. It:

1. pulls the repo,
2. runs `scripts/script.R` (R 4.3.1),
3. runs `npm run deploy` (build + upload),
4. commits and pushes the updated data.

The script stops at the first failing step, so a failed R run never deploys. Output is appended to `update.log` (not committed).

To run it by hand, start the task in Task Scheduler or run `bash update.sh`.

## Local development

Requires Node.js 18+.

```sh
npm install
npm run dev      # preview at http://127.0.0.1:3000 with live reload
npm run build    # static site in dist/
npm run deploy   # build and upload to data-science.wifo.ac.at
```

- **Network drives:** Node's file watching fails on network shares such as `K:`, which crashes Framework's preview server. `npm run dev` therefore preloads `poll-watch.js`, which replaces it with polling.
- **Deploy:** needs SSH key access to `lschmoigl@data-science.wifo.ac.at`. On Windows, `deploy.js` uses Git's `ssh`/`scp`, because Windows' built-in OpenSSH rejects the key due to its file permissions. Use `node deploy.js --dry-run` to list what would be deleted on the server without changing anything.

## Notes

- The calculations and charts are a port of the original Observable notebooks [`@wifo-viz/tarifloehne`](https://observablehq.com/@wifo-viz/tarifloehne) and `d/662b7dbdbc98b5f9` (Metaller scenario). Library versions are pinned to match them (Arquero 5.1.0, Vega 5.23, Vega-Lite 5.6).
- The "Metaller-Verhandlungen" scenario starts in October 2021. The figures quoted in its text (16.1 %, 9.6 %, –8.0 %) date from 2023 and are not updated automatically.
