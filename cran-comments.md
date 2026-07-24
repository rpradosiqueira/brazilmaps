## R CMD check results

Tested on:

* Windows 11 x64, R 4.6.0, `R CMD check --as-cran`
* Win-builder Windows Server 2022 x64, R-devel
  (2026-07-23 r90295)

Both checks included the PDF manual.

Result:

* 0 errors
* 0 warnings
* 1 expected note

The expected incoming-feasibility note identifies this as a new submission
because `brazilmaps` was archived on 2020-05-06 after earlier check problems
were not corrected. The same note flags `IBGE` as a possibly misspelled word;
this is the official acronym for the Brazilian Institute of Geography and
Statistics, whose name is written out in full in `DESCRIPTION`.

Version 1.0.0 replaces the legacy spatial stack with `sf`, removes obsolete
`ggplot2` fortification paths, adds offline tests and vignettes, and passes the
current checks.

## Package size

The source tarball is 9,713,873 bytes (approximately 9.71 MB). This release
ships six official IBGE municipal mesh milestones (2000, 2001, 2007, 2010,
2023 and 2025) and the current maps for the other supported territorial levels,
so users never download spatial data at runtime. Within each consecutive run
with the same municipality count, only the latest municipal edition is
bundled. All spatial files are topology encoded and gzip compressed.

## Network access

Package code, examples, tests and vignettes do not access the network. Official
IBGE downloads occur only in excluded maintainer scripts under `data-raw/`.

## Reverse dependencies

No reverse dependencies are currently known.
