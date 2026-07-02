\name{inventoryMetrics_spatial}
\alias{inventoryMetrics_spatial}
\title{Complete inventory metrics with optional sf reconstruction
Complete inventory workflow carrying a spatial sidecar}
\description{Run the complete inventory workflow while preserving optional plot geometry

This function is a conservative \code{spatial} companion to
\code{\link{inventoryMetrics}}.  It keeps the original calculation
engines for Spanish NFI and external inventories, but prepares or reuses a
plot-level geometry registry and carries that registry in attributes.  The
geometry column is reconstructed only at the end when \code{spatial =
"sf"}.  The original \code{inventoryMetrics()} function is not replaced.}
\usage{inventoryMetrics_spatial(nfi, 
    backend = c("auto", 
        "snfi", "external"), 
    summ.vr = "Estadillo", 
    cut.dt = "d == d", 
    report = FALSE, mc.cores = getOption("mc.cores", 
        1L), design = NULL, 
    schema = NULL, method_registry = NULL, 
    domheight_method = "Hd_strict", 
    domheight_registry = dominant_height_method_registry(), 
    parameter_table = NULL, 
    ..., spatial = c("attribute", 
        "sf", "none", 
        "inherit"), spatial.summary = c("auto", 
        "centroid", "none"), 
    keep.geometry.meta = TRUE, 
    geometry.role.name = NULL, 
    geometry.dt.nm = NULL, 
    coord.nm = NULL, 
    coords = NULL, x.col = NULL, 
    y.col = NULL, pr.col = NULL, 
    plot.col = NULL, 
    crs = NULL, coord.units = "m", 
    geometry.source = c("auto", 
        "snfi", "external", 
        "none"), file_ext = NULL, 
    file_name = NULL, 
    coord.factor = NULL, 
    huso.method = c("auto", 
        "candidate", 
        "xgap", "province", 
        "none"), huso.candidates = NULL, 
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
    validate = TRUE, 
    na.action = c("keep", 
        "drop", "error"))}
\arguments{
  \item{nfi}{Inventory source accepted by the selected \code{backend}.  For Spanish
NFI this can be a province code/name, URL, ZIP file, decompressed
files, a \code{readNFI} object, a \code{readNFI_spatial}
object, or an \code{sf} object.  For external inventories it can
be the same input accepted by \code{external_dendroMetrics()}.}
  \item{backend}{Backend selector.  The
default infers \code{"external"} when \code{schema} or
\code{parameter_table} is supplied, otherwise uses
\code{"snfi"}.}
  \item{summ.vr}{Grouping variable passed to the selected
\code{backend}. When omitted, Spanish NFI uses
\code{"Estadillo"} and external inventories use
the \code{schema} level or detected plot identifier.
Supply a vector such as plot and species for
finer groups, or \code{NULL} for tree-level
output.}
  \item{cut.dt}{Logical expression used by the \code{backend} to subset
the metric output.}
  \item{report}{Request \code{backend} reports when supported.}
  \item{mc.cores}{Number of cores passed to the
selected \code{backend}.}
  \item{design}{Optional sampling \code{design} passed to the \code{backend}.}
  \item{schema}{Optional external \code{schema}.  If it contains
\code{defaults$spatial}, it can also define external
coordinates for the \code{spatial} sidecar.}
  \item{method_registry}{Optional volume method registry passed to
the selected \code{backend}.}
  \item{domheight_method}{Dominant-height method passed to
\code{dendroMetrics()} for SNFI inputs.
For external inputs, this method is resolved
to a function and passed as
\code{domheight_fun}.}
  \item{domheight_registry}{Named
dominant-height registry created with
\code{dominant_height_method_registry()}.}
  \item{parameter_table}{Optional parameter table for external
inventories.}
  \item{\dots}{Additional arguments forwarded to the selected \code{backend}.  A safe
subset such as \code{nfi.nr}, \code{dt.nm}, \code{dir}, and
\code{timeOut} is also used by \code{readNFI_spatial()} when a
sidecar must be prepared from a source.}
  \item{spatial}{Spatial policy.
\code{"attribute"} returns a tabular result with
\code{attr(x, "nfi_geometry_registry")}.  \code{"sf"}
returns an \code{sf} object when geometry can be restored.
\code{"none"} runs the original tabular workflow.
\code{"inherit"} returns \code{sf} for \code{sf} input,
\code{"attribute"} for sidecar input, and tabular output
otherwise.}
  \item{spatial.summary}{Geometry rule after
summarization.  \code{"auto"} preserves observed plot points
when plot identity remains and uses centroids for multi-plot
summaries.  \code{"centroid"} forces centroid construction for
multi-plot summaries.  \code{"none"} leaves the result tabular
while preserving the sidecar attribute.}
  \item{keep.geometry.meta}{Store geometry role and spatial-summary
metadata as attributes.}
  \item{geometry.role.name}{Optional visible column containing the
geometry role, for example
\code{"plot_point"} or
\code{"summary_centroid"}.}
  \item{geometry.dt.nm}{Optional table name used by
\code{readNFI_spatial()} to build the sidecar.}
  \item{coord.nm}{Optional coordinate table name for Spanish NFI.}
  \item{coords}{Optional external coordinate table.}
  \item{x.col}{External X/easting/longitude coordinate column.}
  \item{y.col}{External Y/northing/latitude coordinate column.}
  \item{pr.col}{Optional province/region key for coordinate joins.}
  \item{plot.col}{Optional plot identifier key for coordinate joins.}
  \item{crs}{CRS for external coordinates, for example EPSG 25830.}
  \item{coord.units}{Coordinate units recorded in the sidecar.}
  \item{geometry.source}{Geometry
source passed to \code{readNFI_spatial()}.}
  \item{file_ext}{Optional file extension passed to
\code{readNFI_spatial()}.}
  \item{file_name}{Optional file name filter passed to
\code{readNFI_spatial()}.}
  \item{coord.factor}{Optional coordinate multiplier for Spanish NFI
tables.  IFN2 kilometre coordinates are converted
to metres by default in \code{readNFI_spatial()}.}
  \item{huso.method}{UTM-zone
assignment method passed to
\code{readNFI_spatial()}.}
  \item{huso.candidates}{Optional candidate UTM zones passed to
\code{readNFI_spatial()}, for example
\code{c(29, 30)} for IFN2 provinces that
span Huso 29 and 30 when no Huso columns are
available in the coordinate table.}
  \item{target.crs}{Optional common CRS for Spanish NFI geometries.}
  \item{boundary}{\code{logical}. If \code{TRUE} (default), attach an optional
administrative \code{boundary} sidecar for plot maps.}
  \item{boundary.source}{Boundary
source passed to \code{readNFI_spatial()}. For external
inventories, \code{"auto"} uses only a supplied
\code{boundary.object}; it does not infer a Spanish
built-in \code{boundary}. For Spanish inputs, \code{"auto"}
tries GISCO/NUTS before GADM.}
  \item{boundary.object}{Optional user-provided \code{sf} polygon or
multipolygon layer. This is the supported \code{boundary}
route for external inventories.}
  \item{boundary.level}{GADM administrative level, used when
\code{boundary.source} resolves to \code{"gadm"}.}
  \item{boundary.path}{Cache directory
for optional \code{boundary} downloads.}
  \item{boundary.ext}{GADM extension used by \code{gadm_spatial()}.}
  \item{boundary.version}{GADM version used by \code{gadm_spatial()}.}
  \item{boundary.crs}{CRS assigned to \code{boundary.object} when
missing. It is required when the supplied object has
no CRS metadata.}
  \item{infer.huso}{Backward-compatible Huso inference argument passed
to \code{readNFI_spatial()}.}
  \item{validate}{Warn about sidecar or geometry-restoration problems.}
  \item{na.action}{Handling of metric rows that
do not match the plot geometry registry when
\code{spatial = "sf"}.}
}
\details{External inventories keep their original \code{backend} path.  When

external coordinates are supplied through an \code{sf} input,

\code{coords}, or \code{schema$defaults$spatial}, the geometry

registry is carried in attributes and may be restored if the

output keeps compatible grouping keys.  Without external

coordinate information, external outputs remain tabular.

When \code{summ.vr} is omitted for an external inventory, the

function uses \code{schema$levels} when available and otherwise

detects the plot identifier from the prepared input.

External boundaries are never inferred from province-like

columns. Supply an \code{sf} polygon or multipolygon through

\code{boundary.object}; its CRS must be present or supplied

through \code{boundary.crs}.}
\value{A result equivalent to the selected \code{backend} output, augmented

with class \code{"inventoryMetrics_spatial"}.  With

\code{spatial = "attribute"}, the output is tabular and carries

the plot geometry in \code{attr(x, "nfi_geometry_registry")}.

With \code{spatial = "sf"}, the result is an \code{sf} object

when geometry restoration succeeds.
Preserve explicit \code{summ.vr} = NULL; NULL selects tree-level output.
Preserve explicit \code{summ.vr} = NULL; NULL selects tree-level output.}

\author{Wilson Lara [aut, cre] (ORCID: <https://orcid.org/0000-0003-3527-1380>),
Cristobal Ordonez [aut] (ORCID:
<https://orcid.org/0000-0001-5354-3760>),
Aitor Vázquez-Veloso [aut] (ORCID:
<https://orcid.org/0000-0003-0227-506X>),
Felipe Bravo [aut] (ORCID: <https://orcid.org/0000-0001-7348-6695>)}




\examples{
	ext <- data.frame(
	    plot = c("P1", "P1", "P2"),
	    species = c("sp1", "sp1", "sp2"),
	    diameter_mm = c(120, 185, 260),
	    height_m = c(7.1, 9.4, 13.2),
	    x = c(-3.70, -3.70, -3.69),
	    y = c(40.40, 40.40, 40.41)
	)

	sch <- new_external_schema(
	    colmap = list(
	        plot = "plot",
	        species = "species",
	        d = "diameter_mm",
	        h = "height_m"
	    ),
	    units = list(d = "mm", h = "m"),
	    levels = "plot",
	    keep_cols = c("plot", "species"),
	    defaults = list(
	        spatial = list(plot = "plot", x = "x", y = "y", crs = 4326)
	    )
	)

	dsg <- new_inventory_design(
	    sample_area_m2 = 1000,
	    min_dbh_cm = 7.5,
	    name = "Square 0.1-ha plot"
	)

	x <- inventoryMetrics_spatial(
	    ext,
	    backend = "external",
	    schema = sch,
	    design = dsg,
	    summ.vr = "plot",
	    var = c("d", "h", "ba", "n"),
	    spatial = "attribute",
	    geometry.source = "external",
	    boundary = FALSE
	)

	inherits(x, "inventoryMetrics")
	hasNFIgeometry_spatial(x)

	## Tabular sidecar output for Spanish NFI:
	## x <- inventoryMetrics_spatial(28, nfi.nr = 2, dir = tempdir())
	## hasNFIgeometry_spatial(x)

## Direct sf output:
## xsf <- inventoryMetrics_spatial(28, nfi.nr = 2, dir = tempdir(), spatial = "sf")
## inherits(xsf, "sf")
## attr(xsf, "geometry_role")

## Dominant-height calculations use the same inspectable registry exposed
## by nfiMetrics(). Metadata is preserved in attr(x, "dominant_height_meta")
## when Hd is computed.
##
	## External inventories keep the original external backend. If the schema
	## contains defaults$spatial or the input is sf, the sidecar can be carried.
}
