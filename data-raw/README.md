# Spatial data maintenance

The files in `inst/maps/` are generated artifacts and are installed with the
package. Package users do not download spatial data at runtime.

Install the pinned maintenance tool with `npm install --prefix data-raw`, then
run `municipal-meshes.R` from the package root to rebuild the selected
historical municipal editions from the official IBGE GeoFTP files. The
selection is recorded in `municipal-edition-selection.csv`: within each
consecutive run with the same municipality count, only the latest official
edition is bundled. The pipeline:

1. caches the official ZIP archives outside the built package;
2. standardizes identifiers and CRS to SIRGAS 2000 (EPSG:4674);
3. writes explicit lower-snake-case fields such as `municipality_code`,
   `state_code` and `region_code`;
4. simplifies every selected edition with the same 5% weighted Visvalingam
   rule;
5. rebuilds each municipal ring, preferring topology cleaning and
   automatically falling back to feature-preserving mode if cleaning changes
   the feature count or validity;
6. writes topology-preserving compressed TopoJSON;
7. rebuilds only residual spherical-invalid rings with `s2`, when needed;
8. reads every generated file again and rejects duplicated codes, invalid
   geometries, feature-count changes or an unexpected CRS;
9. records source URLs, file size and MD5 checksum in the installed index.

Maintenance dependencies (`sf`, Node.js and the pinned `mapshaper` CLI) are
deliberately not runtime dependencies.

The legacy 2000, 2001 and 2005 archives do not contain `.prj` files. Their
official geographic-coordinate products use SAD69 and are assigned EPSG:4618
before transformation. The explicitly SIRGAS 2000 product is used from 2007
onwards.

On systems where R's internal ZIP reader cannot decode an auxiliary file name,
the extraction step falls back to the system `tar` executable. Only shapefile
components are read by the pipeline.

Run `current-maps.R` after the municipal build. It uses the current municipal
TopoJSON plus the official IBGE Localities API to rebuild states, regions,
immediate regions, intermediate regions and the installed DTB hierarchy.
It also standardizes all installed map and DTB columns in lower snake case
and writes every current map as quantized, topology-preserving compressed
TopoJSON without additional geometry simplification.
Microregions and mesoregions are retained as discontinued historical divisions;
the curated state cartograms are also preserved.
