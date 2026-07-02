\name{readNFIsf}
\alias{readNFIsf}
\title{readNFIsf}
\description{readNFIsf: read a Spanish NFI table as an sf point data frame.

Read one Spanish National Forest Inventory table and return an
\code{sf} object with plot geometries. The table requested by
\code{dt.nm} becomes the attribute table. Coordinates come from the
appropriate plot-coordinate table detected from \code{listNFI_tables()}.}
\usage{readNFIsf(nfi, nfi.nr = 4, 
    dt.nm = "PCMayores", 
    coord.nm = NULL, 
    file_ext = NULL, 
    file_name = NULL, 
    validate = TRUE, 
    ..., crs = NULL, 
    keep.coord.meta = FALSE, 
    mixed.crs = c("na", 
        "error"), na.action = c("keep", 
        "drop", "error"), 
    infer.huso = NULL)}
\arguments{
  \item{nfi}{Input accepted by \code{\link{listNFI_tables}}: province
identifier, zip archive, URL, or decompressed files.}
  \item{nfi.nr}{\code{integer}. Inventory stage: 2, 3, or 4.}
  \item{dt.nm}{Table to import and preserve as the sf
attribute table.}
  \item{coord.nm}{Optional coordinate table name. If \code{NULL},
the function detects DATEST for IFN2 and
PCDatosMap/Listado definitivo for IFN3/IFN4.}
  \item{file_ext}{Optional file extension passed to
\code{listNFI_tables()}.}
  \item{file_name}{Optional file name filter for the main-table read.
Coordinate discovery ignores this argument.}
  \item{validate}{\code{logical}. Validate coordinate-table discovery
and warn about unmatched plot keys.}
  \item{\dots}{Additional arguments passed to \code{listNFI_tables()}, such as
\code{dir} or \code{timeOut}.}
  \item{crs}{Optional CRS passed to \code{sf::st_as_sf}. When NULL,
the function uses the unique EPSG derived from metadata.}
  \item{keep.coord.meta}{Keep Huso/datum/EPSG metadata columns in
the sf attribute table. Otherwise they are
stored in \code{attr(x, "coord_reference")}.}
  \item{mixed.crs}{What to do when rows imply more than
one EPSG code.}
  \item{na.action}{Handling of rows that do not
match a coordinate record.}
  \item{infer.huso}{Fill missing UTM zones from province code. By
default, TRUE for IFN2 and FALSE for IFN3/IFN4.}
}
\details{This is the high-level spatial reader. It mirrors the usual
\code{readNFI()} workflow for the selected \code{dt.nm} table, but
returns an \code{sf} point data frame. It should complement rather than
replace \code{readNFI()}, because the return type changes and a
coordinate table must be available.

The function calls \code{listNFI_tables()} once, reuses the
local files in \code{dir}, reads the requested table as attributes,
reads the coordinate table separately, and delegates the geometry
construction to \code{addNFIsf()}.

By default, \code{infer.huso} is \code{TRUE} for IFN2 and
\code{FALSE} for IFN3/IFN4. This prevents silently assigning uncertain
UTM zones for inventories where a province-level fallback may be less
precise than the original map-sheet reference.}
\value{An \code{sf} data frame. The imported table named by
\code{dt.nm} remains the attribute table; plot coordinates are stored
in the geometry column.}

\author{Wilson Lara [aut, cre] (ORCID: <https://orcid.org/0000-0003-3527-1380>),
Cristobal Ordonez [aut] (ORCID:
<https://orcid.org/0000-0001-5354-3760>),
Aitor Vázquez-Veloso [aut] (ORCID:
<https://orcid.org/0000-0003-0227-506X>),
Felipe Bravo [aut] (ORCID: <https://orcid.org/0000-0001-7348-6695>)}




\examples{
if (interactive()) {
    ## Real use with a persistent cache directory. This may download data
    ## the first time and reuse the local files later.
    cache <- tools::R_user_dir("basifoR", "cache")
    x <- readNFIsf(45, nfi.nr = 4, dt.nm = "PCMayores", dir = cache)
    sf::st_crs(x)
    attr(x, "coord_reference")
}

## Visual check against a province boundary, if optional packages exist.
## if (interactive() &&
##     requireNamespace("geodata", quietly = TRUE) &&
##     requireNamespace("terra", quietly = TRUE)) {
##     gadm <- geodata::gadm("ESP", level = 2, path = tempdir())
##     gadm_sf <- sf::st_as_sf(gadm)
##     plot(sf::st_geometry(gadm_sf))
##     plot(sf::st_geometry(sf::st_transform(x, sf::st_crs(gadm_sf))),
##          add = TRUE, pch = 16, cex = 0.3)
## }
}
