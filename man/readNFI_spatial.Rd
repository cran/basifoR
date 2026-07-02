\name{readNFI_spatial}
\alias{readNFI_spatial}
\title{readNFI spatial}
\description{Read SNFI data and store plot geometry as an attribute sidecar

This function is a conservative \code{spatial} companion to \code{readNFI()}.
It keeps the ordinary tabular output of \code{readNFI()} and stores a
plot-level geometry registry in \code{attr(x, "nfi_geometry_registry")}.
By default, the returned object remains a data frame and carries geometry
as an attribute sidecar. If \code{spatial = "sf"}, the function returns an
\code{sf} object by joining the sidecar geometry back to each row. The
attribute-sidecar default lets downstream metric functions carry \code{spatial}
information without pushing a geometry column through tabular routines.}
\usage{readNFI_spatial(nfi, 
    nfi.nr = 4, dt.nm = "PCMayores", 
    file_ext = NULL, 
    file_name = NULL, 
    ..., spatial = c("attribute", 
        "sf", "none"), 
    geometry.dt.nm = dt.nm, 
    coord.nm = NULL, 
    schema = NULL, coords = NULL, 
    x.col = NULL, y.col = NULL, 
    pr.col = NULL, plot.col = NULL, 
    crs = NULL, coord.units = "m", 
    coord.accuracy.note = NULL, 
    geometry.source = c("auto", 
        "snfi", "external", 
        "none"), validate = TRUE, 
    coord.factor = NULL, 
    huso.method = c("auto", 
        "candidate", 
        "xgap", "province", 
        "none"), huso.candidates = NULL, 
    repair.table.huso = TRUE, 
    target.crs = NULL, 
    boundary = FALSE, 
    boundary.source = c("auto", 
        "gisco", "gadm", 
        "user", "none"), 
    boundary.object = NULL, 
    boundary.level = 2, 
    boundary.path = tools::R_user_dir("basifoR", 
        "cache"), boundary.ext = "json", 
    boundary.version = "4.1", 
    boundary.crs = NULL, 
    infer.huso = NULL)}
\arguments{
  \item{nfi}{Inventory source passed to \code{readNFI()}: province code or
name, local/remote zip archive, decompressed files, or a data frame.}
  \item{nfi.nr}{Spanish NFI stage. Use \code{2}, \code{3}, or \code{4}.}
  \item{dt.nm}{Main table to read and return as tabular data.}
  \item{file_ext}{Optional file extension passed to \code{readNFI()} and
\code{readNFIsf()} when the input must be fetched or
filtered.}
  \item{file_name}{Optional file-name filter passed to \code{readNFI()}.
The \code{spatial} registry reads coordinates independently.}
  \item{\dots}{Additional arguments passed to \code{readNFI()} and
\code{readNFIsf()}, such as \code{dir} or \code{timeOut}.}
  \item{spatial}{\code{"attribute"} stores a
plot-level \code{spatial} registry in an
attribute and keeps a tabular output.
\code{"sf"} returns an \code{sf}
object by joining the sidecar geometry
to each row. \code{"none"} returns
the ordinary \code{readNFI()} output.}
  \item{geometry.dt.nm}{Table used to build the geometry registry. In
most SNFI workflows this can match \code{dt.nm}.}
  \item{coord.nm}{Optional SNFI coordinate table name. If \code{NULL},
the function detects \code{DATEST} for IFN2 and
\code{PCDatosMap}/\code{Listado definitivo} for IFN3/IFN4.}
  \item{schema}{Optional \code{external_schema}. For external
inventories, \code{spatial} metadata can be stored in
\code{schema$defaults$spatial}, for example
a named \code{list} containing plot, x, y, and \code{crs} entries.
Explicit coordinate arguments below override \code{schema} values.}
  \item{coords}{Optional external coordinate table. Use this for non-
Spanish inventories when coordinates live in a separate
table. It can be a data frame or an \code{sf} object.}
  \item{x.col}{Optional X/easting/longitude column for external
coordinate tables. If \code{NULL}, common names such as
\code{x}, \code{coord_x}, \code{easting}, and
\code{longitude} are detected.}
  \item{y.col}{Optional Y/northing/latitude column for external
coordinate tables. If \code{NULL}, common names such as
\code{y}, \code{coord_y}, \code{northing}, and
\code{latitude} are detected.}
  \item{pr.col}{Optional province, region, or stratum key for external
coordinate matching. It is not required for one-region
external inventories.}
  \item{plot.col}{Optional plot identifier column for external
coordinate matching. If \code{NULL}, common plot names
such as \code{plot_id}, \code{sample_id}, and
\code{site_id} are detected.}
  \item{crs}{Optional CRS for external coordinates, passed to
\code{sf::st_as_sf()}. Use an EPSG code such as
\code{25830} or \code{4326}.}
  \item{coord.units}{Coordinate units recorded in the external \code{spatial}
sidecar metadata.}
  \item{coord.accuracy.note}{Optional note describing the accuracy or
provenance of user-supplied coordinates. This is
most useful when replacing public inventory
coordinates with field-validated plot-centre
coordinates. Spanish NFI public coordinates
receive an automatic accuracy note.}
  \item{geometry.source}{Source used
to build the sidecar. \code{"auto"} first uses
explicit external coordinates when available, then
tries SNFI geometry only for province-like sources.}
  \item{validate}{Warn when duplicate plot geometries are found or when
geometry cannot be attached.}
  \item{coord.factor}{Optional coordinate multiplier for SNFI coordinate
tables. By default IFN2 kilometre coordinates are
detected and converted to metres.}
  \item{huso.method}{Method for
assigning SNFI UTM zones when the coordinate table
does not contain a direct \code{Huso} column.
\code{"auto"} uses table Huso, then the historical
\code{huso1}/\code{huso2}/\code{huso3} +
\code{CoorX} rule, then province fallback.}
  \item{huso.candidates}{Optional candidate UTM zones for SNFI Huso inference.
Use, for example, \code{c(29, 30)} for provinces
that span zones 29 and 30 when no Huso columns are
available.}
  \item{repair.table.huso}{\code{logical}. If \code{TRUE}, direct
table-supplied Huso values are checked against the
candidate-zone \code{CoorX} threshold rule before
building geometry. This repairs boundary-zone cases
such as plots stored in the neighbouring UTM zone.}
  \item{target.crs}{Optional common CRS for projected SNFI geometries.
When \code{NULL}, a single source EPSG is kept, or
mixed ED50 zones are transformed to EPSG:23030.}
  \item{boundary}{\code{logical}. If \code{TRUE}, attach an optional
\code{boundary} sidecar in \code{attr(x, "nfi_boundary")}.
Spanish NFI province-like inputs can use built-in
GISCO/GADM download helpers; external inventories must
provide \code{boundary.object}.}
  \item{boundary.source}{Boundary
source. \code{"auto"} uses a user object when supplied
and otherwise tries GISCO/NUTS before GADM for Spanish
province-like inputs.}
  \item{boundary.object}{Optional user-provided \code{sf} polygon or
multipolygon layer. External inventories must use this
route; automatic built-in \code{boundary} selection is limited
to Spanish NFI inputs.}
  \item{boundary.level}{GADM administrative level used when
\code{boundary.source = "gadm"}. GISCO uses NUTS-3.}
  \item{boundary.path}{Cache
directory used for optional \code{boundary} downloads.}
  \item{boundary.ext}{GADM extension used by \code{gadm_spatial()}.}
  \item{boundary.version}{GADM version used by \code{gadm_spatial()}.}
  \item{boundary.crs}{CRS assigned to \code{boundary.object} when it
lacks CRS metadata. It is required when the supplied
object has no CRS.}
  \item{infer.huso}{Backward-compatible argument forwarded to the
fallback \code{readNFIsf()} route. The new SNFI table
route prefers \code{huso.method}.}
}
\details{For Spanish NFI sources, the coordinate note is stage-specific
and is stored in
\code{attr(x, "nfi_geometry_registry")$coord_accuracy_note}.
IFN2 coordinates are treated as official public locations
derived from the kilometre UTM sampling grid. IFN3 coordinates
are treated as official cartographic plot-location coordinates
in UTM metres; when their \code{Huso} is not explicit, CRS
metadata may be reconstructed from candidate Huso fields,
coordinate rules, or fallback methods. IFN4 \code{CoorX}/
\code{CoorY} are treated as official public coordinates
available before fieldwork for the plot centre. These stage
notes document coordinate interpretation; they do not claim
field-validated exact plot-centre precision.

External inventories or user-supplied coordinate tables do not
receive an automatic coordinate-accuracy note because basifoR
cannot know their measurement protocol or precision. If better
coordinates become available, for example field GPS or
institutional plot-centre coordinates, pass them through
\code{coords}, \code{x.col}, \code{y.col}, \code{plot.col},
and optionally \code{pr.col}, with \code{geometry.source =
"external"} and provide \code{coord.accuracy.note} only when
the coordinate source is known. This replaces the \code{spatial}
sidecar while keeping the ordinary \code{readNFI()} table and
downstream metric calculations unchanged.}
\value{A data frame with the same tabular content returned by
\code{readNFI()}. When \code{spatial = "attribute"}, the object also
carries a plot-level geometry sidecar in
\code{attr(x, "nfi_geometry_registry")}. The sidecar stores one geometry
per inventory plot, CRS metadata, coordinate provenance, and normalized
join keys for later \code{*_spatial()} metric functions.}

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

	x <- readNFI_spatial(
	    toy,
	    spatial = "attribute",
	    geometry.source = "external",
	    plot.col = "esta",
	    x.col = "x",
	    y.col = "y",
	    crs = 4326,
	    boundary = FALSE
	)

	hasNFIgeometry_spatial(x)
	names(getNFIgeometry_spatial(x))

	## Keep the ordinary readNFI() output but carry plot geometry in attributes.
	## cache <- tools::R_user_dir("basifoR", "cache")
	## x <- readNFI_spatial(28, nfi.nr = 3, dt.nm = "PCMayores", dir = cache)
## is.data.frame(x)
## hasNFIgeometry_spatial(x)
## reg <- getNFIgeometry_spatial(x)
## reg$geometry

## Return a true sf object when direct spatial output is wanted.
## xsf <- readNFI_spatial(28, nfi.nr = 3, dt.nm = "PCMayores",
##                        dir = cache, spatial = "sf")
## inherits(xsf, "sf")

## External inventory: keep tabular data but store geometry sidecar.
## ext <- data.frame(plot_id = 1:2, x = c(440000, 441000),
##                   y = c(4488000, 4489000), d = c(20, 30))
## y <- readNFI_spatial(ext, spatial = "attribute",
##                      geometry.source = "external",
##                      plot.col = "plot_id", x.col = "x", y.col = "y",
##                      crs = 25830)
## hasNFIgeometry_spatial(y)
##
## External inventory with schema-stored spatial metadata:
## sch <- new_external_schema(
##     colmap = list(plot = "plot_id", d = "d"),
##     units = list(d = "cm"),
##     levels = "plot_id",
##     defaults = list(spatial = list(plot = "plot_id", x = "x", y = "y", crs = 25830))
## )
## y2 <- readNFI_spatial(ext, spatial = "attribute", schema = sch)
## hasNFIgeometry_spatial(y2)
##
## Replacing public SNFI coordinates with improved coordinates uses the
## same external sidecar mechanism.  The improved table must contain the
## inventory plot keys and coordinate columns.
## improved <- data.frame(pr = 28, Estadillo = c(1, 2),
##                        x_gps = c(440000, 441000),
##                        y_gps = c(4488000, 4489000))
	## z <- readNFI_spatial(28, nfi.nr = 4, spatial = "attribute",
	##                      geometry.source = "external", coords = improved,
	##                      pr.col = "pr", plot.col = "Estadillo",
	##                      x.col = "x_gps", y.col = "y_gps", crs = 25830,
	##                      coord.accuracy.note = "Field-validated plot-centre coordinates.")
}
