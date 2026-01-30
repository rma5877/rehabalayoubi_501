<<<<<<< HEAD
# Web as Data

**Theme:** The Web as Data: Scraping + HTML

## Goals
- Scrape pages responsibly (requests, headers, pacing).
- Parse HTML and handle pagination.
- Save raw sources + document provenance.

## Materials
- demo/ — in-class demo code (if present)
- Use /data/raw, /data/intermediate, /data/processed for datasets and outputs
- Environment/setup notes live in /environment

## Readings
- Brown et al. (2025) Big Data and Society
- Mahdavi, P. (2017) PSRM

## Notes
- Add links to slides / notebooks / datasets here as you publish them.

## Web-scraped data can be systematically unrepresentative, since platform users are not equivalent to the broader population, which limits the generalizability of findings. In a reproducible workflow, I would document all scraping filters and time windows in code and benchmark the sample against external demographics to make this bias transparent. A second major risk is privacy and ethical harm, because even public posts can contain sensitive information or be re-identified. To mitigate this, I would minimize the data collected, remove or hash identifiers at the point of ingestion, and log compliance with platform terms of service and robots.txt as part of the pipeline.
=======
# SODA 501 — Big Social Data (Spring 2026)

Public course materials for SODA 501-002. :contentReference[oaicite:0]{index=0}

## Course info
- **Thursdays 12:00–3:00 pm** (124 Pond) :contentReference[oaicite:1]{index=1}  
- **Instructor:** Jared Edgerton (jfe4@psu.edu) :contentReference[oaicite:2]{index=2}  
- **Canvas:** videos, announcements, and submissions :contentReference[oaicite:3]{index=3}  

## What’s in this repo
- `weeks/` — weekly folders (slides, demos, and lab materials)
- `demo/` — in-class demo code (when posted)
- `problemPset/` in-class problem set

## Weekly class format
1) Research talk (60 min)  
2) Coding lab (80 min)  
3) Paper discussion (40 min) :contentReference[oaicite:4]{index=4}  

## Student work (your own repo/project)
For assignments and the final project, you should build your own reproducible project structure (e.g., separate raw vs processed data, scripts, and a short README explaining how to run your pipeline). :contentReference[oaicite:5]{index=5}

## Tools
You’ll need Python or R, Git/GitHub, and Canvas access. :contentReference[oaicite:6]{index=6}
>>>>>>> a36e6612ecf89cd6d28593efc2a9954a81e2cc92
