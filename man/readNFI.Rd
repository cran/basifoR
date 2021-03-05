\name{readNFI}
\alias{readNFI}
\title{Read NFI data}
\description{This function can retrieve data sets of the Spanish National
Forest Inventory (SNFI). It can process either \code{URLs} to data
stored in the SNFI web page (\code{"http://www.mapama.gob.es"}) or
paths to files locally stored.}
\usage{readNFI(nfi, dt.nm = "PCMayores")}
\arguments{
  \item{nfi}{\code{character} or \code{data.frame}.  URL/path to
a compressed file of the SNFI (.zip) having data of
either .dbf or .mdb file extensions.}
  \item{dt.nm}{\code{character}. Name of a data set
stored in the imported NFI data. Default
reads \code{'PCMayores'} (3rd NFI) or
\code{'PIESMA'} (2nd NFI).}
}
\details{Compressed data having file
extensions other than \code{.dbf} or
\code{.mdb} are not supported. Most data bases
in \code{2nd} and \code{3rd} stages of the
SNFI can be imported directly from
\code{http://mapama.gob.es} using appropriate
links to the files. Data in the 4th NFI must
be read from local paths. Data bases from the
second stage of the SNFI are imported using
\code{\link{read.dbf}}. Data bases from latter
stages are imported using either
\code{\link{RODBC}} (Windows) or
\code{\link{mdb.get}} (unix-alike systems). On
Windows, a driver for Office 2010 can be
installed via the installer
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
url2 <- httr::modify_url("http://www.mapama.gob.es", path = path)
rnfi <- readNFI(url2)
head(rnfi,3)
}


}
