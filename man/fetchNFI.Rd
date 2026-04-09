\name{fetchNFI}
\alias{fetchNFI}
\title{Fetch SNF Data }
\description{This function can download and decompress data sets from the Spanish
National Forest Inventory (SNF) stored in \code{.zip} files,
whether they are local or remote (URL-based).}
\usage{fetchNFI(url., dir = tempdir(), 
    file_ext = c("mdb", 
        "DBF", "accdb"), 
    file_name = NULL, 
    timeOut = timeout(60))}
\arguments{
  \item{url.}{\code{character}. Specifies the URL/path to a
compressed SNF (.zip).}
  \item{dir}{\code{character}. Directory where the fetched
file will be stored.}
  \item{file_ext}{\code{character}. Supported
file extensions.}
  \item{file_name}{\code{character}. Optional file names inside
the decompressed content to keep. It accepts
either full file names or bare stems without
extensions.}
  \item{timeOut}{\code{request}. Maximum request time,
see \code{\link[httr]{timeout}}. Default is
\code{timeout(60)}.}
}
\details{The data should be in files with
extensions specified in the \code{file_ext}
argument. Use \code{file_name} to restrict the
returned files to specific names inside the
decompressed content. It accepts either full
file names or bare stems without extensions.}
\value{\code{character}. Returns the path to the fetched and decompressed
NFI data (.mdb, .DBF, .accdb, or other extensions requested in
\code{file_ext}) stored in a temporary file.}

\author{Wilson Lara <wilarhen@gmail.com> [aut, cre] (ORCID: <https://orcid.org/0000-0003-3527-1380>), Cristobal Ordonez <angelcristobal.ordonez@uva.es> [aut] (ORCID: <https://orcid.org/0000-0001-5354-3760>), Aitor Vázquez-Veloso <aitor.vazquez.veloso@uva.es> [aut] (ORCID: <https://orcid.org/0000-0003-0227-506X>), Felipe Bravo <felipe.bravo@uva.es> [aut] (ORCID: <https://orcid.org/0000-0001-7348-6695>)}




\examples{
## Process SNF data for Toledo stored locally
# Path to Toledo data file in 'basifoR' package
ifn4p45 <- system.file("Ifn4_Toledo.zip", package="basifoR")

# Decompress SNF data from the specified file path or URL
fetch_ifn4p45 <- fetchNFI(ifn4p45)
print(fetch_ifn4p45)

## French NFI tree table read from the official web resource
## f <- "https://inventaire-forestier.ign.fr/dataifn/data/export_dataifn_2024_en.zip"
## fnfi <- fetchNFI(f, file_ext = "csv", file_name = "ARBRE")
## print(fnfi)
    
}
