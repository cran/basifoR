\name{readNFI}
\alias{readNFI}
\title{Read NFI data}
\description{This function can retrieve data sets of the Spanish National
Forest Inventory (SNFI). It can process either \code{URLs} to data
stored in the SNFI web page (\code{"http://www.miteco.gob.es"}) or
paths to files locally stored.}
\usage{readNFI(nfi, dt.nm = "PCMayores", 
    ...)}
\arguments{
  \item{nfi}{\code{character} or \code{data.frame}.  \code{URL/path}
to a compressed file of the SNFI (\code{.zip}) having data
of either .dbf or .mdb file extensions.}
  \item{dt.nm}{\code{character}. Name of a data set
stored in the imported NFI data. Default
reads \code{'PCMayores'} (3rd NFI) or
\code{'PIESMA'} (2nd NFI).}
  \item{\dots}{Additional arguments in \code{\link{urlToTemp}}.}
}
\details{Compressed data having file
extensions other than \code{.dbf} or
\code{.mdb} are not supported. Most data bases
in \code{2nd} and \code{3rd} stages of the
SNFI can be imported directly from
\code{http://www.miteco.gob.es} using
appropriate \code{URLs}. Data sets from 2nd
SNFI are imported using
\code{\link{read.dbf}}. Data from latter
stages are imported using either
\code{\link{RODBC}} (Windows) or
\code{\link{mdb.get}} (unix-alike
systems). Data from 4th SNFI must be read from
local paths.  On Windows, a driver for Office
2010 can be installed via the installer
\code{'AccessDatabaseEngine.exe'} available
from Microsoft, and the package must be
implemented using a 32-bit R version. In the
case of unix-alike systems, the linux
dependence \code{mdbtools} must be installed.}
\value{\code{data.frame}. A data base  of the NFI.}

\author{Wilson Lara <wilarhen@gmail.com>, Cristobal Ordonez <a_cristo@pvs.uva.es>, Felipe Bravo <fbravo@pvs.uva.es>}




\examples{
madridNFI <- system.file("ifn3p28_tcm30-293962.zip", package="basifoR")
rmad <- readNFI(madridNFI)[1:100,]
head(rmad)

## Retrieval of a data base from the second stage of the second SNFI:

\donttest{
path <- '/es/biodiversidad/servicios/banco-datos-naturaleza/090471228013cbbd_tcm30-278511.zip'
url2 <- httr::modify_url("https://www.miteco.gob.es", path = path)
rnfi <- readNFI(url2)
head(rnfi,3)
}


}
