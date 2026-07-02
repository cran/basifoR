\name{listNFI_tables}
\alias{listNFI_tables}
\title{List available raw SNFI tables}
\description{Discover the raw tables or
files available in Spanish National Forest
Inventory (SNFI) downloads, local archives,
or already decompressed files. The function
helps users inspect available table names
before reading data with \code{readNFI()}.}
\usage{listNFI_tables(nfi, nfi.nr = 4, 
    dir = tempdir(), 
    file_ext = NULL, 
    file_name = NULL, 
    ...)}
\arguments{
  \item{nfi}{\code{character} or \code{numeric}. Inventory source to
inspect. Accepted values are: (i) a province name or
province code to be resolved to an official SNFI download
URL; (ii) a local or remote \code{.zip} archive; or (iii)
one or more direct paths to decompressed \code{.dbf},
\code{.mdb}, \code{.accdb}, or \code{.csv} files.}
  \item{nfi.nr}{\code{integer}. SNFI stage used when \code{nfi}
is given as a province identifier. Use \code{2},
\code{3}, or \code{4}. The value selects the internal
URL resolver \code{nfi2()}, \code{nfi3()}, or
\code{nfi4()}.}
  \item{dir}{\code{character}. Directory used by
\code{fetchNFI()} to store downloaded archives
and extracted files. Use the same \code{dir} in
later calls to \code{readNFI()} to reuse cached
files and avoid unnecessary downloads.}
  \item{file_ext}{\code{character} or \code{NULL}. Optional file
extension or extensions forwarded to
\code{\link{fetchNFI}}. Leave \code{NULL} to use
the default extensions defined by \code{fetchNFI()}.}
  \item{file_name}{\code{character} or \code{NULL}. Optional file
name or bare stem forwarded to
\code{\link{fetchNFI}} to keep only specific files
inside a compressed archive.}
  \item{\dots}{Additional arguments passed to \code{\link{fetchNFI}}, such
as \code{timeOut = httr::timeout(120)}.}
}
\details{The input \code{nfi} can be a
province name or province code, a local or
remote \code{.zip} archive, or one or more
already decompressed \code{.dbf}, \code{.mdb},
\code{.accdb}, or \code{.csv} files.

When \code{nfi} is a province identifier, the
function resolves it with the internal URL
resolver selected by \code{nfi.nr}: \code{nfi2()},
\code{nfi3()}, or \code{nfi4()}. It then calls
\code{fetchNFI()} to download or reuse the
corresponding local files.

For second-stage SNFI data and other DBF-based
sources, the function lists one row per DBF file
and uses the DBF file stem as \code{dt.nm}. For
CSV sources, it lists one row per CSV file and
also uses the file stem as \code{dt.nm}. For
Access sources, it lists one row per table found
inside each \code{.mdb} or \code{.accdb} file.

The function does not read full data tables. It
only discovers table or file names. However, it
must inspect local files. Therefore, when \code{nfi}
is remote or province-based, at least one download
or extraction step can be necessary unless the
files already exist in the same \code{dir} cache.

The arguments \code{file_ext} and \code{file_name}
are forwarded to \code{fetchNFI()} only when they
are not \code{NULL}. This keeps \code{fetchNFI()}
as the single authority for default downloadable
extensions and matches the forwarding style used
by \code{readNFI()}.

Access table listing is platform dependent. On
Windows it requires \pkg{RODBC} and a Microsoft
Access ODBC driver. Package \pkg{odbc} is optional
and is used only to check whether such a driver is
visible. On Unix-like systems, table listing
requires the external \code{mdbtools} command
\code{mdb-tables}.}
\value{A \code{data.frame} with one row per discovered raw table
or file. The columns are \code{source}, which identifies the
backend as \code{"DBF"}, \code{"CSV"}, or \code{"Access"};
\code{dt.nm}, the table or file stem that users can pass to
\code{readNFI()} when appropriate; \code{file}, the local file
basename; and \code{path}, the normalized local file path. The
function returns an empty data frame with these columns when no
matching files or tables are found.}

\author{Wilson Lara [aut, cre] (ORCID: <https://orcid.org/0000-0003-3527-1380>),
Cristobal Ordonez [aut] (ORCID:
<https://orcid.org/0000-0001-5354-3760>),
Aitor Vázquez-Veloso [aut] (ORCID:
<https://orcid.org/0000-0003-0227-506X>),
Felipe Bravo [aut] (ORCID: <https://orcid.org/0000-0001-7348-6695>)}




\examples{
## Self-contained example using a temporary DBF file.
tmp <- tempfile(fileext = ".dbf")
foreign::write.dbf(
    data.frame(
        plot = 1:2,
        tree = 1:2,
        dbh = c(15.2, 31.8)
    ),
    file = tmp
)

tabs <- listNFI_tables(tmp)
tabs[, c("source", "dt.nm", "file")]

unlink(tmp)

## Typical SNFI use with a persistent cache directory.
## This can download data and may require mdbtools or an Access driver,
## so it is intentionally left as commented example code.
## cache <- tools::R_user_dir("basifoR", "cache")
## tabs4 <- listNFI_tables(28, nfi.nr = 4, dir = cache)
## head(tabs4)
## x4 <- readNFI(28, nfi.nr = 4, dt.nm = "PCMayores", dir = cache)
}
