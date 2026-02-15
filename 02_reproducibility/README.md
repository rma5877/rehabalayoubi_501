# Reproducibility

**Theme:** Reproducibility: Git, Containers, Research Artifacts

## Goals
- Turn code into a shareable artifact someone else can run.
- Capture environment + document run steps.
- Use Git effectively for collaborative workflows.

## Materials
- demo/ — in-class demo code (if present)
- Use /data/raw, /data/intermediate, /data/processed for datasets and outputs
- Environment/setup notes live in /environment

## Readings
- Heath, Davidson, et al. (2023) The Journal of Finance
- Holzmeister,et al. (2025) Nature Human Behaviour


## Notes
- Add links to slides / notebooks / datasets here as you publish them.

## Why renv is needed for reproducibility


The renv.lock file stores a snapshot of the project environment, including the version of R, the exact versions of all packages used, and where those packages were installed from (e.g., CRAN or GitHub). This file acts as a blueprint for rebuilding the same environment elsewhere. When another user runs renv::restore(), renv reads renv.lock and installs those exact package versions into a project-specific library. This recreates the original computational environment and prevents version mismatches.


