\name{metrics2Vol_spatial}
\alias{metrics2Vol_spatial}
\title{Tree-level volumes carrying a spatial sidecar}
\description{Compute tree-level SNFI volume variables while preserving plot geometry

This function mirrors the public arguments and calculations of
\code{\link{metrics2Vol}} but adds an optional \code{spatial} sidecar.  The volume
computation itself is delegated to the original \code{metrics2Vol()}, so the
equation registry, fallback logic, provenance, units, and volume metadata
remain consistent with the non-spatial workflow.

Spatial information is prepared before the volume call by invoking
\code{nfiMetrics_spatial()} with \code{spatial = "attribute"} when the input is not
already an \code{"nfiMetrics"} object, and is copied back to the volume
output after \code{metrics2Vol()} finishes.  Thus the geometry column never
travels through the volume-equation calculations.  If \code{spatial =
"sf"}, the function reconstructs an \code{sf} point object only at the end.}
\usage{metrics2Vol_spatial(nfi, 
    cub.met = "freq", 
    parametro = c("VCC"), 
    keep.var = TRUE, 
    keep.legacy = FALSE, 
    method_registry = snfi_volume_method_registry(), 
    track_provenance = FALSE, 
    ..., spatial = c("attribute", 
        "sf", "none", 
        "inherit"), var = c("d", 
        "h", "ba", "n", 
        "Hd", "Dd"), 
    levels = c("esta", 
        "espe"), design = snfi_design(), 
    domheight_method = "Hd", 
    domheight_registry = dominant_height_method_registry(), 
    geometry.dt.nm = NULL, 
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
  \item{nfi}{Input accepted by \code{\link{metrics2Vol}}.  This can be a
province code/name, a file path, a \code{"readNFI"} object, a
\code{"readNFI_spatial"} object, a \code{"nfiMetrics"} object,
or an \code{"sf"} object carrying plot-level SNFI records.}
  \item{cub.met}{Same as \code{metrics2Vol()}. Cubication selector
used when several coefficient rows match.}
  \item{parametro}{Same as \code{metrics2Vol()}. One or more
volume outputs to compute.}
  \item{keep.var}{Same as \code{metrics2Vol()}. Keep auxiliary
coefficient columns when available.}
  \item{keep.legacy}{Same as \code{metrics2Vol()}. Also return the
legacy volume estimate for backward compatibility.}
  \item{method_registry}{Same as
\code{metrics2Vol()}. Registry that maps each
requested output to its equation function,
output column name, units, and fallback rule.}
  \item{track_provenance}{Same as \code{metrics2Vol()}. Add per-row
provenance columns and audit metadata.}
  \item{\dots}{Additional arguments passed to \code{nfiMetrics_spatial()} when
metrics must be computed before volume calculation.  Typical
examples are \code{nfi.nr}, \code{dt.nm}, \code{dir},
\code{file_ext}, and \code{timeOut}.}
  \item{spatial}{Spatial output
mode.  \code{"attribute"} keeps the ordinary tabular
\code{metrics2Vol()} output and stores the plot geometry
registry in attributes.  \code{"sf"} reconstructs point geometry
at the end.  \code{"none"} runs the original non-spatial
workflow. \code{"inherit"} returns \code{"sf"} for sf input,
\code{"attribute"} for objects already carrying a sidecar, and
\code{"none"} otherwise.}
  \item{var}{Forwarded to
\code{nfiMetrics_spatial()} if the
input is not already an
\code{"nfiMetrics"} object.}
  \item{levels}{Forwarded to \code{nfiMetrics_spatial()}.
Keeping the plot identifier is important
for final \code{sf} reconstruction.}
  \item{design}{Forwarded to \code{nfiMetrics_spatial()}.}
  \item{domheight_method}{Dominant-height method forwarded to
\code{nfiMetrics_spatial()} or,
when metrics are computed inside
\code{metrics2Vol()}, to
\code{nfiMetrics()}.}
  \item{domheight_registry}{Named
dominant-height registry created with
\code{dominant_height_method_registry()}.}
  \item{geometry.dt.nm}{Optional table name used to build the geometry
registry.  When \code{NULL}, the default of
\code{readNFI_spatial()} is used, normally the
same table requested by \code{dt.nm}.}
  \item{schema}{Optional external \code{schema} forwarded to
\code{nfiMetrics_spatial()} and \code{readNFI_spatial()}.}
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
creation, or failed sf reconstruction.  Volume
calculations still follow \code{metrics2Vol()}.}
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

\code{data.frame} with \code{metrics2vol} metadata intact and

an extra \code{attr(x, "nfi_geometry_registry")} sidecar.

This is the safest mode for developing

\code{dendroMetrics_spatial()} and \code{inventoryMetrics_spatial()}

because no geometry column is passed through the volume

internals.  Use \code{spatial = "sf"} only when direct \code{spatial}

output is needed at this stage.}
\value{A \code{metrics2vol} object.  With
\code{spatial = "attribute"}, the result remains tabular and carries a
plot-level geometry registry in \code{attr(x, "nfi_geometry_registry")}
for later \code{*_spatial()} functions.  With \code{spatial = "sf"},
the function attempts to return an \code{sf} object by joining plot
geometries back to the tree-level volume rows.  If the volume output no
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
	    esta = c("P1", "P2"),
	    especie = c(21, 21),
	    Dn = c(180, 260),
	    altura = c(9.5, 14.2),
	    x = c(-3.70, -3.69),
	    y = c(40.40, 40.41)
	)
	attr(toy, "nfi.nr") <- 4

	demo_registry <- snfi_volume_method_registry(list(
	    VCC = list(
	        output = "vcc_demo",
	        fun = function(dbh_mm, h_m, pars) {
	            pars$k[1] * dbh_mm^2 * h_m
	        },
	        raw_unit = "m3 tree-1",
	        unit = "m3 tree-1",
	        scale_to_m3 = 1,
	        build_args = function(ctx, pars, resolved) {
	            list(dbh_mm = ctx$d_mm, h_m = ctx$h_m, pars = pars)
	        },
	        fallback = function(ctx, pars, resolved) NA_real_,
	        pars = data.frame(
	            nfi.nr = 4,
	            pr = 28,
	            especie = 21,
	            k = 1e-7
	        )
	    )
	))

	x <- metrics2Vol_spatial(
	    toy,
	    parametro = "VCC",
	    method_registry = demo_registry,
	    var = c("d", "h", "ba", "n"),
	    levels = c("esta", "especie"),
	    spatial = "attribute",
	    geometry.source = "external",
	    plot.col = "esta",
	    x.col = "x",
	    y.col = "y",
	    crs = 4326,
	    boundary = FALSE
	)

	inherits(x, "metrics2vol")
	hasNFIgeometry_spatial(x)

	## Spanish NFI: compute tree metrics, volumes, and carry geometry as an
	## attribute sidecar.
	## x <- metrics2Vol_spatial(28, nfi.nr = 3, dir = tempdir(),
	##                          parametro = c("VCC", "VSC"))
	## inherits(x, "metrics2vol")
## hasNFIgeometry_spatial(x)
## attr(x, "units")
## attr(x, "volume_meta")
## attr(x, "dominant_height_meta")

## Direct sf output for visual checks.
## xsf <- metrics2Vol_spatial(28, nfi.nr = 3, dir = tempdir(),
##                            parametro = c("VCC", "VSC"),
##                            spatial = "sf")
## inherits(xsf, "sf")

## Continue from an existing spatial nfiMetrics object.
## nm <- nfiMetrics_spatial(28, nfi.nr = 3, dir = tempdir())
## vv <- metrics2Vol_spatial(nm, parametro = "VCC")
## hasNFIgeometry_spatial(vv)

## Disable spatial behavior and recover the ordinary metrics2Vol workflow.
## y <- metrics2Vol_spatial(28, nfi.nr = 3, dir = tempdir(),
##                          spatial = "none")
}
