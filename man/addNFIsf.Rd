\name{addNFIsf}
\alias{addNFIsf}
\title{addNFIsf}
\description{addNFIsf: convert an NFI table and coordinate table to sf points.

Join an already loaded inventory table to an already loaded coordinate
table and return an \code{sf} point data frame. The inventory table is
preserved as the attribute table, while coordinates are used to build a
geometry column instead of being returned as ordinary x/y columns.}
\usage{addNFIsf(nfi, coords, 
    nfi.nr = attr(nfi, 
        "nfi.nr"), x.col = NULL, 
    y.col = NULL, huso.col = NULL, 
    crs = NULL, keep.coord.meta = FALSE, 
    mixed.crs = c("na", 
        "error"), na.action = c("keep", 
        "drop", "error"), 
    overwrite = FALSE, 
    keep.raw.coords = FALSE, 
    coord.factor = NULL, 
    validate = TRUE, 
    infer.huso = NULL)}
\arguments{
  \item{nfi}{\code{data.frame}. Inventory table, commonly returned by
\code{\link{readNFI}}.}
  \item{coords}{\code{data.frame} or one-table \code{list}. Coordinate table.}
  \item{nfi.nr}{\code{integer}. Inventory stage: 2,
3, or 4.}
  \item{x.col}{Optional source X-coordinate column in \code{coords}.}
  \item{y.col}{Optional source Y-coordinate column in \code{coords}.}
  \item{huso.col}{Optional source UTM-zone column in \code{coords}.}
  \item{crs}{Optional CRS passed to \code{sf::st_as_sf}. When NULL,
the function uses the unique EPSG derived from coordinate
metadata, if one is available.}
  \item{keep.coord.meta}{Keep Huso/datum/EPSG metadata columns in
the sf attribute table. When FALSE, these are
stored only in attributes.}
  \item{mixed.crs}{What to do when rows imply more than
one EPSG code. The default creates sf
with CRS NA and warns.}
  \item{na.action}{Handling of rows that do not
match a coordinate record.}
  \item{overwrite}{Allow overwriting coordinate metadata columns
when \code{keep.coord.meta = TRUE}.}
  \item{keep.raw.coords}{Passed to \code{addNFIcoords}; mainly useful
for debugging and ignored in the final sf
output unless \code{keep.coord.meta = TRUE}.}
  \item{coord.factor}{Numeric multiplier applied to raw coordinates.
Defaults to 1000 for IFN2 and 1 for IFN3/IFN4.}
  \item{validate}{\code{logical}. Warn about duplicate coordinate
keys and unmatched plots.}
  \item{infer.huso}{Fill missing UTM zones from province code. By
default, TRUE for IFN2 and FALSE for IFN3/IFN4.}
}
\details{This helper is the spatial analogue of \code{addNFIcoords()}.
It first creates temporary coordinate and CRS metadata columns, then
converts the result to an \code{sf} point object with
\code{sf::st_as_sf()}. Temporary coordinate columns are removed from
the final object unless \code{keep.coord.meta = TRUE}.

An \code{sf} geometry column can store only one active CRS.
If rows imply more than one EPSG code, \code{mixed.crs = "na"} creates
the object with an undefined CRS and stores the detected metadata in
\code{attr(x, "coord_reference")}; \code{mixed.crs = "error"}
stops instead.

Rows without matching coordinates can be kept as missing
point geometries, dropped, or rejected with \code{na.action}. This is
useful when external CSV files contain partial plot coverage.}
\value{An \code{sf} data frame with point geometries. The geometry
coordinates are UTM metres. CRS is assigned only when a unique EPSG is
available or when \code{crs} is supplied explicitly.}

\author{Wilson Lara [aut, cre] (ORCID: <https://orcid.org/0000-0003-3527-1380>),
Cristobal Ordonez [aut] (ORCID:
<https://orcid.org/0000-0001-5354-3760>),
Aitor Vázquez-Veloso [aut] (ORCID:
<https://orcid.org/0000-0003-0227-506X>),
Felipe Bravo [aut] (ORCID: <https://orcid.org/0000-0001-7348-6695>)}




\examples{
trees <- data.frame(
    nfi.nr = 2,
    pr = 45,
    ESTADILLO = c(1, 1, 2),
    ESPECIE = c(21, 21, 25)
)
attr(trees, "nfi.nr") <- 2

coords <- data.frame(
    PROVINCIA = 45,
    ESTADILLO = c(1, 2),
    COORDEX = c(412, 413),
    COORDEY = c(4411, 4412)
)

if (requireNamespace("sf", quietly = TRUE)) {
    x <- addNFIsf(trees, coords, infer.huso = TRUE)
    sf::st_crs(x)
    sf::st_geometry(x)
}
}
