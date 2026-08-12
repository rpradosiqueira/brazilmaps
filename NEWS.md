# brazilmaps (development version)

* Preserved the complete public API while making unmatched DTB queries return
  a schema-stable zero-row data frame.
* Documented runtime and maintenance cache behavior, source-provenance gaps and
  the exact fields in the municipal edition inventory.
* Made maintenance downloads validate temporary ZIP or JSON files before
  replacing cached artifacts, allowed explicit cache/work directories and
  prevented stale extraction files from entering rebuilds.
* Split CI into a fast Ubuntu release check for pushes and pull requests and a
  comprehensive cross-platform check available on demand.
* Added a separate scheduled/manual, rate-limited smoke test for the live IBGE
  source schema and latest municipal archive.

# brazilmaps 1.0.0

## Spatial data

* Added six municipal mesh milestones: 2000, 2001, 2007, 2010, 2023 and
  2025. Within each consecutive run with the same municipality count, the
  latest official edition is bundled.
* Municipal files are bundled as topology-preserving compressed TopoJSON.
  They retain 5% of weighted Visvalingam vertices. Runtime map access is
  fully local and never downloads data.
* Converted current non-municipal maps to topology-preserving compressed
  TopoJSON without additional simplification, substantially reducing the
  source package while keeping every spatial object installed locally.
* Updated the current hierarchy to 5,571 municipal units, 510 immediate
  regions and 133 intermediate regions, including Boa Esperança do Norte and
  the Escada–Ribeirão correction.
* Recreated all CRS metadata as SIRGAS 2000 (EPSG:4674) and repaired invalid
  geometries.
* Added a reproducible `data-raw/` pipeline with pinned mapshaper, exact
  official source URLs, checksums and post-serialization validation.

## API

* Standardized public functions, arguments, level values and returned spatial
  columns in lower snake case.
* `get_brmap()` now uses `level`, `filters` and `output`; geographic levels
  include `"municipality"`, `"state"`, `"immediate_region"` and related
  explicit names.
* Added `brmap_editions()`, `get_dtb()`, `get_dtb_levels()`,
  `join_brmap()` and `theme_brmap()`.
* Renamed ambiguous fields to explicit forms such as `municipality_code`,
  `state_code`, `immediate_region_code` and `state_abbreviation`.
* Filters use intersection (AND), eliminating duplicated rows from multiple
  conditions.
* `output = "data.frame"` returns attributes without geometry and no longer
  depends on removed ggplot2 spatial fortify methods.
* Previous function names, argument names, PascalCase levels and returned
  columns remain available through deprecated compatibility paths.
* Reimplemented `plot_brmap()` with `geom_sf()` and made its `theme` argument
  effective. Its join and fill arguments are now `data`, `by` and `fill_by`.
* Fixed type preservation in `join_brmap()` and DTB queries.

## Quality

* Added offline tests, cross-platform R CMD check CI and offline vignettes.
* Documented all exported functions and installed datasets.
* Decoded literal Unicode escapes in `deaths`, `gini2015` and `pop2017`.

# brazilmaps 0.2.0

* Added support for `sf`.

# brazilmaps 0.1.0

* Initial release.
