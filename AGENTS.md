# Repository Guidelines

## Project Structure & Module Organization

This repository contains an R pipeline for BUZUFBA GTFS generation, route animation, and UFBA accessibility scenario comparison. Executable scripts are in `scripts/` (`gtfs.R`, `animacao_buzufba.R`, and `analise_comparacao_cenarios_ufba.R`). Quarto documentation and reports live in `analysis/`. Input geospatial datasets are under `data/raw/`; scenario figures are organized under `img/comparacao_cenarios/`. Run scripts from the repository root because their data paths are relative. Generated outputs and large raw datasets should not be added casually; check `.gitignore` and discuss large or derived data changes in the pull request.

## Build, Test, and Development Commands

There is no package build or automated test suite configured. Install the R packages required by the script you are working on (see `README.md`), then run from the root, for example:

```powershell
Rscript scripts/gtfs.R
Rscript scripts/animacao_buzufba.R
Rscript scripts/analise_comparacao_cenarios_ufba.R
quarto render analysis/relatorio_estudo_buzufba.qmd
```

GTFS generation expects a local Valhalla routing service at `http://localhost:8002`. The analysis script also expects its configured R5 network and building GeoPackages. Confirm paths and required inputs before running; analyses may create large outputs or take substantial time.

## Coding Style & Naming Conventions

Follow the Tidyverse Style Guide for consistent R code formatting; Use snake_case for file names, variables, functions, and column names; Use descriptive, meaningful names that clearly communicate purpose and intent; Name functions with verbs that describe their actions (e.g., validate_gtfs(), calculate_accessibility()); Use lowercase file names with underscores and the .R extension; Prefix scripts with sequential numbers when execution order matters (e.g., 01_prepare_data.R); Keep scripts focused on a single responsibility and extract reusable logic into functions; Use package::function() when explicit namespace qualification improves clarity or avoids ambiguity; Prefer file.path() or here::here() for project-relative paths; avoid hardcoded absolute paths and unnecessary setwd() calls; Add comments to explain non-obvious logic, assumptions, and methodological decisions rather than restating the code; Use consistent section headers to organize scripts into logical blocks; Avoid duplicated code, unnecessary global variables, and overwriting built-in function names; Use styler and lintr to maintain consistent formatting and detect style issues.

## Testing Guidelines

No test framework or coverage threshold is currently defined. For changes, run the affected script with its required inputs and dependencies, and inspect generated GTFS, maps, figures, or rendered Quarto output. Document any checks that could not be run because external routing services or local datasets were unavailable.

## Commit & Pull Request Guidelines

Recent history uses short, often conventional-style subjects such as `chore(repo): ...`, `feat(analises): ...`, and `docs(relatorio): ...`. Prefer `type(scope): imperative summary`, in Portuguese or English as appropriate. A pull request should explain the purpose, list affected scripts/data and manual checks, link a related issue when available, and include before/after images for visual output changes. Do not commit credentials, local environment files, or generated artifacts unless they are intentional project deliverables.
