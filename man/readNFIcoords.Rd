\name{readNFIcoords}
\alias{readNFIcoords}
\title{readNFIcoords}
\description{readNFIcoords: read an NFI table and append coordinate columns.

Read one Spanish National Forest Inventory table, locate the matching
plot-coordinate table, and append UTM coordinates and CRS metadata as
ordinary columns. The wrapper discovers files with
\code{listNFI_tables()} once and then reuses the local files present in
\code{dir}; it does not intentionally download the same source twice.}
\usage{readNFIcoords(nfi, nfi.nr = 4, 
    dt.nm = "PCMayores", 
    coord.nm = NULL, 
    file_ext = NULL, 
    file_name = NULL, 
    validate = TRUE, 
    ..., x.name = "x", 
    y.name = "y", huso.name = "huso", 
    huso.source.name = "huso_source", 
    datum.name = "datum", 
    epsg.name = "epsg", 
    crs.name = "crs", 
    overwrite = FALSE, 
    keep.raw.coords = FALSE, 
    infer.huso = FALSE)}
\arguments{
  \item{nfi}{Input accepted by \code{\link{listNFI_tables}} and
\code{\link{readNFI}}: province identifier, zip archive,
decompressed files, or a previously loaded data frame.}
  \item{nfi.nr}{\code{integer}. Inventory stage: 2, 3, or 4.}
  \item{dt.nm}{Table to import and preserve as the main output.}
  \item{coord.nm}{Optional coordinate table name. If \code{NULL},
the function validates \code{DATEST*} in
\code{listNFI_tables()} but passes the generic
\code{DATEST} stem for IFN2, and uses
\code{PCDatosMap}/\code{Listado definitivo} for
IFN3/IFN4 when available.}
  \item{file_ext}{Optional file extension passed to
\code{listNFI_tables()}.}
  \item{file_name}{Optional file name filter for the main-table
read. Coordinate discovery ignores this argument.}
  \item{validate}{\code{logical}. Validate coordinate-table discovery
and warn about unmatched plot keys.}
  \item{\dots}{Additional arguments passed to \code{listNFI_tables()}, such
as \code{dir} or \code{timeOut}. The resulting local paths are
then passed to \code{readNFI()}.}
  \item{x.name}{Output X-coordinate column name, in metres.}
  \item{y.name}{Output Y-coordinate column name, in metres.}
  \item{huso.name}{Output UTM-zone column name when available.}
  \item{huso.source.name}{Output column describing whether
Huso comes from the coordinate table
or was inferred from province code.}
  \item{datum.name}{Output geodetic datum column name.}
  \item{epsg.name}{Output EPSG code column name when known.}
  \item{crs.name}{Output CRS label column name when known.}
  \item{overwrite}{Allow overwriting existing coordinate columns.}
  \item{keep.raw.coords}{Keep raw source coordinates before conversion.}
  \item{infer.huso}{Fill missing UTM zones from province code only when
requested. A Huso column in the coordinate table has
precedence. Province-filled values are marked in
huso_source.}
}
\details{The wrapper first calls \code{listNFI_tables()} to discover
and, when necessary, fetch the available files. After that discovery
step, the main table and the coordinate table are read from the local
paths returned by \code{listNFI_tables()}, respecting the package
cache philosophy and avoiding a second intentional download.

The argument \code{dt.nm} controls the main table. The
coordinate table only supplies plot positions. For IFN2 the coordinate
table is detected as the generic \code{DATEST} stem after validating
available \code{DATESTXX} files. For IFN3 and IFN4 the function looks
for \code{PCDatosMap} or \code{Listado definitivo}, unless
\code{coord.nm} is supplied explicitly.

The output remains a regular \code{data.frame} enriched with
coordinate and CRS metadata columns. Use \code{readNFIsf()} when an
\code{sf} geometry column is preferred.}
\value{A \code{readNFI} data frame enriched with plot-level spatial
coordinates. The imported table named by \code{dt.nm} remains the main
output; coordinate fields are joined by province and plot identifier.}

\author{Wilson Lara [aut, cre] (ORCID: <https://orcid.org/0000-0003-3527-1380>),
Cristobal Ordonez [aut] (ORCID:
<https://orcid.org/0000-0001-5354-3760>),
Aitor Vázquez-Veloso [aut] (ORCID:
<https://orcid.org/0000-0003-0227-506X>),
Felipe Bravo [aut] (ORCID: <https://orcid.org/0000-0001-7348-6695>)}




\examples{
## Synthetic example for the joining helper.
trees <- data.frame(
    nfi.nr = 3,
    pr = 28,
    Estadillo = c(10, 10, 11),
    Especie = c(21, 21, 25)
)
attr(trees, "nfi.nr") <- 3

coords <- data.frame(
    Provincia = 28,
    Estadillo = c(10, 11),
    CoorX = c(440000, 441000),
    CoorY = c(4488000, 4489000),
    Huso = 30
)

addNFIcoords(trees, coords)

## Real use with a persistent cache directory. This may download data
## the first time and reuse the local files later.
## cache <- tools::R_user_dir("basifoR", "cache")
## x <- readNFIcoords(45, nfi.nr = 4, dt.nm = "PCMayores", dir = cache)
## unique(x[, c("huso", "huso_source", "datum", "epsg")])
}
