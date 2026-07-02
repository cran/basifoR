\name{gadm_spatial}
\alias{gadm_spatial}
\title{gadm spatial}
\description{Download and read GADM boundaries without depending on geodata.}
\usage{gadm_spatial(country = "ESP", 
    level = 2, path = tools::R_user_dir("basifoR", 
        "cache"), ext = c("json", 
        "gpkg"), version = "4.1", 
    quiet = TRUE)}
\arguments{
  \item{country}{ISO3 \code{country} code. The default downloads Spain.}
  \item{level}{Administrative \code{level} to download. For Spain, \code{level} 2 is
commonly useful as a province boundary layer; higher levels
can be used for municipal or local cartographic context.}
  \item{path}{Cache directory used
to store downloaded GADM files.}
  \item{ext}{GADM file format. The default
\code{"json"} downloads the level-specific zipped GeoJSON.}
  \item{version}{GADM \code{version} string used to build the download URL.}
  \item{quiet}{Passed to \code{sf::st_read()}.}
}
\details{This helper downloads GADM directly from the public GADM
distribution endpoint, caches the file in \code{path}, and
reads it with \pkg{sf}. It is intended as an optional
cartographic support layer for plotting and spatial quality
checks. GADM boundaries are not Spanish NFI products and should
not be interpreted as official inventory geometries.}
\value{An \code{sf} object containing the requested GADM administrative
boundary layer. Attributes record \code{country}, \code{level}, \code{version}, and URL.}

\author{Wilson Lara [aut, cre] (ORCID: <https://orcid.org/0000-0003-3527-1380>),
Cristobal Ordonez [aut] (ORCID:
<https://orcid.org/0000-0001-5354-3760>),
Aitor Vázquez-Veloso [aut] (ORCID:
<https://orcid.org/0000-0003-0227-506X>),
Felipe Bravo [aut] (ORCID: <https://orcid.org/0000-0001-7348-6695>)}



\seealso{readNFI_spatial, getNFIboundary_spatial}
\examples{
if (interactive()) {
    adm <- gadm_spatial("ESP", level = 2, path = tempdir())
    plot(sf::st_geometry(adm))
}
}
