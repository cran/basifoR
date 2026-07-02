\name{nfiMetrics_spatial}
\alias{nfiMetrics_spatial}
\title{Tree-level metrics carrying a spatial sidecar}
\description{Compute tree-level Spanish NFI metrics while preserving plot geometry

This function mirrors the public arguments and calculations of
\code{\link{nfiMetrics}} but adds an optional \code{spatial} sidecar.  The metric
computation itself is delegated to the original \code{nfiMetrics()}, so the
returned diameter, height, basal-area, trees-per-hectare, dominant-height,
dominant-diameter, unit, and \code{design} metadata remain consistent with the
non-spatial workflow.

Spatial information is prepared before the metric call by invoking
\code{readNFI_spatial()} with \code{spatial = "attribute"} and copied back to the
metric output after \code{nfiMetrics()} finishes.  Thus the geometry column
never travels through the tree-metric calculations.  If \code{spatial =
"sf"}, the function reconstructs an \code{sf} point object only at the end.}
\usage{nfiMetrics_spatial(nfi, 
    var = c("d", "h", 
        "ba", "n", "Hd", 
        "Dd"), levels = c("esta", 
        "espe"), design = snfi_design(), 
    domheight_method = "Hd", 
    domheight_registry = dominant_height_method_registry(), 
    ..., spatial = c("attribute", 
        "sf", "none", 
        "inherit"), geometry.dt.nm = NULL, 
    schema = NULL, coords = NULL, 
    x.col = NULL, y.col = NULL, 
    pr.col = NULL, plot.col = NULL, 
    crs = NULL, coord.units = "m", 
    geometry.source = c("auto", 
        "snfi", "external", 
        "none"), validate = TRUE, 
    coord.nm = NULL, 
    coord.factor = NULL, 
    huso.method = c("auto", 
        "candidate", 
        "province", "none"), 
    target.crs = NULL, 
    boundary = TRUE, 
    boundary.source = c("auto", 
        "gisco", "gadm", 
        "user", "none"), 
    boundary.object = NULL, 
    boundary.level = 2, 
    boundary.path = tools::R_user_dir("basifoR", 
        "cache"), boundary.ext = "json", 
    boundary.version = "4.1", 
    boundary.crs = NULL, 
    infer.huso = NULL, 
    na.action = c("keep", 
        "drop", "error"))}
\arguments{
  \item{nfi}{\code{character(1)}, \code{"readNFI"}, \code{"readNFI_spatial"},
or \code{"sf"} object.  Province codes/names and file paths are
first read through \code{readNFI_spatial()} when \code{spatial} output is
requested.  Objects that already carry
\code{attr(x, "nfi_geometry_registry")} reuse that sidecar.}
  \item{var}{Same as \code{nfiMetrics()}. Metrics
to compute: diameter \code{'d'}, height
\code{'h'}, basal area \code{'ba'},
trees per hectare \code{'n'}, dominant
height \code{'Hd'}, and dominant
diameter \code{'Dd'}.}
  \item{levels}{Same as \code{nfiMetrics()}. Column-name
patterns used to keep grouping variables
in the tree-level output.  Spatial
reconstruction to \code{sf} requires a plot
identifier to remain in the output, so the
default should usually be kept while
developing the \code{spatial} chain.}
  \item{design}{Same as \code{nfiMetrics()}. Sampling \code{design}
used to compute expansion factors for
\code{'n'}, dominant height, and dominant
diameter.}
  \item{domheight_method}{Dominant-height method code resolved
through \code{domheight_registry} when
\code{'Hd'} is requested.  The default
matches \code{nfiMetrics()}.}
  \item{domheight_registry}{Named
dominant-height registry created with
\code{dominant_height_method_registry()}.}
  \item{\dots}{Additional arguments passed to \code{readNFI_spatial()} and then
to \code{nfiMetrics()}.  Typical examples are \code{nfi.nr},
\code{dt.nm}, \code{dir}, \code{file_ext}, and \code{timeOut}.}
  \item{spatial}{Spatial output
mode.  \code{"attribute"} keeps the ordinary tabular
\code{nfiMetrics()} output and stores the plot geometry registry
in attributes.  \code{"sf"} reconstructs point geometry at the
end.  \code{"none"} runs the original non-spatial workflow.
\code{"inherit"} returns \code{"sf"} for sf input,
\code{"attribute"} for objects already carrying a sidecar, and
\code{"none"} otherwise.}
  \item{geometry.dt.nm}{Optional table name used to build the geometry
registry.  When \code{NULL}, the default of
\code{readNFI_spatial()} is used, normally the
same table requested by \code{dt.nm}.}
  \item{schema}{Optional external \code{schema} forwarded to
\code{readNFI_spatial()}.  This lets compatible
non-standard inventories define \code{spatial} metadata in
\code{schema$defaults$spatial} while keeping the metric
calculation unchanged.}
  \item{coords}{Optional coordinate table forwarded to
\code{readNFI_spatial()} when coordinates are stored
outside the main table.}
  \item{x.col}{Optional X/easting/longitude coordinate column for
external coordinate tables.}
  \item{y.col}{Optional Y/northing/latitude coordinate column for
external coordinate tables.}
  \item{pr.col}{Optional province, region, or stratum column used in
\code{spatial} joins.}
  \item{plot.col}{Optional plot identifier column used in \code{spatial} joins.}
  \item{crs}{Optional CRS for external coordinates, for example an
EPSG code such as \code{25830}.}
  \item{coord.units}{Coordinate units recorded in the \code{spatial} sidecar
when external coordinates are supplied.}
  \item{geometry.source}{Forwarded
to \code{readNFI_spatial()}.  Use \code{"snfi"}
to force Spanish NFI coordinate discovery and
\code{"external"} when coordinates come from the
main table or a user-supplied coordinate table.}
  \item{validate}{Warn about missing \code{spatial} helpers, failed sidecar
creation, or failed sf reconstruction.  Metric
calculations still follow \code{nfiMetrics()}.}
  \item{coord.nm}{Optional SNFI coordinate table name forwarded to
\code{readNFI_spatial()}.}
  \item{coord.factor}{Optional coordinate multiplier for SNFI coordinate
tables. By default IFN2 kilometre coordinates are
detected and converted to metres.}
  \item{huso.method}{UTM-zone
assignment method forwarded to
\code{readNFI_spatial()}. \code{"auto"} uses
direct \code{Huso}, then historical
\code{huso1}/\code{huso2}/\code{huso3} +
\code{CoorX}, then province fallback.}
  \item{target.crs}{Optional common CRS for SNFI geometries. When
\code{NULL}, mixed ED50 zones are transformed to
EPSG:23030.}
  \item{boundary}{\code{logical}. If \code{TRUE} (default), attach an optional
administrative \code{boundary} sidecar for plot maps.}
  \item{boundary.source}{Boundary
source passed to \code{readNFI_spatial()}.
\code{"auto"} tries GISCO/NUTS before GADM for
Spanish province-like inputs.}
  \item{boundary.object}{Optional user-provided \code{sf} polygon layer.}
  \item{boundary.level}{GADM administrative level, used when
\code{boundary.source} resolves to \code{"gadm"}.}
  \item{boundary.path}{Cache directory
for optional \code{boundary} downloads.}
  \item{boundary.ext}{GADM extension used by \code{gadm_spatial()}.}
  \item{boundary.version}{GADM version used by \code{gadm_spatial()}.}
  \item{boundary.crs}{CRS assigned to \code{boundary.object} when missing.}
  \item{infer.huso}{Backward-compatible fallback argument forwarded to
\code{readNFI_spatial()}.}
  \item{na.action}{Used only when
\code{spatial = "sf"}.  It controls rows that do
not match the plot geometry registry during final
\code{sf} reconstruction.}
}
\details{The default \code{spatial = "attribute"} returns a normal

\code{data.frame} with the \code{nfiMetrics} metadata intact

and an extra \code{attr(x, "nfi_geometry_registry")} sidecar.

This is the safest mode for developing

\code{metrics2Vol_spatial()} and \code{dendroMetrics_spatial()}

because no geometry column is passed through the metric

internals.  Use \code{spatial = "sf"} only when direct \code{spatial}

output is needed at this stage.}
\value{A \code{nfiMetrics} object.  With
\code{spatial = "attribute"}, the result remains tabular and carries a
plot-level geometry registry in \code{attr(x, "nfi_geometry_registry")}
for later \code{*_spatial()} functions.  With \code{spatial = "sf"},
the function attempts to return an \code{sf} object by joining plot
geometries back to the tree-level metric rows.  If the metric output no
longer contains a plot identifier, the function warns and returns the
tabular output with the sidecar instead.}

\author{Wilson Lara [aut, cre] (ORCID: <https://orcid.org/0000-0003-3527-1380>),
Cristobal Ordonez [aut] (ORCID:
<https://orcid.org/0000-0001-5354-3760>),
Aitor Vázquez-Veloso [aut] (ORCID:
<https://orcid.org/0000-0003-0227-506X>),
Felipe Bravo [aut] (ORCID: <https://orcid.org/0000-0001-7348-6695>)}




\examples{
	toy <- data.frame(
	    nfi.nr = 4,
	    pr = 28,
	    esta = c("P1", "P1", "P2"),
	    espe = c(21, 21, 25),
	    Dn = c(120, 185, 260),
	    altura = c(7.1, 9.4, 13.2),
	    x = c(-3.70, -3.70, -3.69),
	    y = c(40.40, 40.40, 40.41)
	)
	attr(toy, "nfi.nr") <- 4

	x <- nfiMetrics_spatial(
	    toy,
	    var = c("d", "h", "ba", "n"),
	    levels = c("esta", "espe"),
	    spatial = "attribute",
	    geometry.source = "external",
	    plot.col = "esta",
	    x.col = "x",
	    y.col = "y",
	    crs = 4326,
	    boundary = FALSE
	)

	inherits(x, "nfiMetrics")
	hasNFIgeometry_spatial(x)

	## Spanish NFI: compute metrics and carry geometry as an attribute sidecar.
	## x <- nfiMetrics_spatial(28, nfi.nr = 3, dir = tempdir())
	## inherits(x, "nfiMetrics")
	## hasNFIgeometry_spatial(x)
## reg <- getNFIgeometry_spatial(x)
## reg$geometry
## attr(x, "dominant_height_meta")

## Direct sf output for visual checks.
## xsf <- nfiMetrics_spatial(28, nfi.nr = 3, dir = tempdir(),
##                           spatial = "sf")
## inherits(xsf, "sf")

## Disable spatial behavior and recover the ordinary nfiMetrics workflow.
	## y <- nfiMetrics_spatial(28, nfi.nr = 3, dir = tempdir(),
	##                         spatial = "none")
}
