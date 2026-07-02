\name{addNFIcoords}
\alias{addNFIcoords}
\title{addNFIcoords}
\description{addNFIcoords: attach NFI plot coordinates as ordinary columns.

Join a processed Spanish National Forest Inventory table with a
plot-level coordinate table and append UTM coordinate metadata. This
function is intentionally independent from \code{readNFI()}, so it can
enrich already loaded tables without creating circular calls.}
\usage{addNFIcoords(nfi, coords, 
    nfi.nr = attr(nfi, 
        "nfi.nr"), x.col = NULL, 
    y.col = NULL, huso.col = NULL, 
    x.name = "x", y.name = "y", 
    huso.name = "huso", 
    huso.source.name = "huso_source", 
    datum.name = "datum", 
    epsg.name = "epsg", 
    crs.name = "crs", 
    overwrite = FALSE, 
    keep.raw.coords = FALSE, 
    coord.factor = NULL, 
    validate = TRUE, 
    infer.huso = FALSE)}
\arguments{
  \item{nfi}{\code{data.frame}. Inventory table, commonly an object returned
by \code{\link{readNFI}}.}
  \item{coords}{\code{data.frame} or one-table \code{list}. Coordinate table.
For IFN2 this is usually \code{DATESTXX.DBF} or the parcel
layer table with \code{CX}/\code{CY}. For IFN3/IFN4 this is
usually \code{PCDatosMap} or \code{Listado definitivo}.}
  \item{nfi.nr}{\code{integer}. Inventory stage: 2,
3, or 4. IFN2 coordinates are converted
from kilometres to metres by default.}
  \item{x.col}{Optional source X-coordinate column in \code{coords}.}
  \item{y.col}{Optional source Y-coordinate column in \code{coords}.}
  \item{huso.col}{Optional source UTM-zone column in \code{coords}.}
  \item{x.name}{Output X-coordinate column name, in metres.}
  \item{y.name}{Output Y-coordinate column name, in metres.}
  \item{huso.name}{Output UTM-zone column name when available.}
  \item{huso.source.name}{Output column describing whether
Huso comes from the coordinate table
or was inferred from province code.}
  \item{datum.name}{Output geodetic datum column name.}
  \item{epsg.name}{Output EPSG code column name when known.}
  \item{crs.name}{Output CRS label column name when known.}
  \item{overwrite}{\code{logical}. Allow overwriting existing output
coordinate columns.}
  \item{keep.raw.coords}{\code{logical}. Keep raw source coordinate
values before unit conversion.}
  \item{coord.factor}{Numeric multiplier applied to raw coordinates.
Defaults to 1000 for IFN2 and 1 for IFN3/IFN4.}
  \item{validate}{\code{logical}. Warn about duplicate coordinate keys
and unmatched plots.}
  \item{infer.huso}{\code{logical}. Fill missing UTM zones from the
province code. A Huso column in \code{coords} has
precedence when present. Province-filled values are
marked in \code{huso_source}.}
}
\details{The function uses province and plot identifiers to match
rows in \code{nfi} with records in \code{coords}. It searches common
IFN column names such as \code{pr}, \code{Provincia},
\code{ESTADILLO}, \code{Estadillo}, and \code{NUMPAR}. The main
table keeps its original row order and number of rows; tree-level
tables therefore receive repeated plot coordinates.

Coordinate values are treated as UTM coordinates. IFN2
coordinate columns, normally \code{COORDEX}/\code{COORDEY} or
\code{CX}/\code{CY}, are interpreted as kilometres and multiplied by
1000 by default. IFN3 and IFN4 coordinate columns, normally
\code{CoorX}/\code{CoorY} or \code{CoorXC}/\code{CoorYC}, are
interpreted as metres.

A \code{Huso} column in the coordinate table always has
priority. When \code{infer.huso = TRUE}, missing UTM zones are filled
from the province code and marked as \code{province_inferred} in
\code{huso_source}; otherwise the corresponding CRS fields remain
\code{NA}.}
\value{A data frame with the same rows and row order as \code{nfi},
plus coordinate columns. Output \code{x} and \code{y} are always in
metres. Attributes record the source coordinate columns, source units,
and multiplier used during conversion.}

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

x <- addNFIcoords(trees, coords, infer.huso = TRUE)
x[, c("pr", "ESTADILLO", "x", "y", "huso", "datum", "epsg")]

## Keep raw IFN2 kilometre coordinates for checking.
addNFIcoords(trees, coords, keep.raw.coords = TRUE)
}
