\name{readNFI}
\alias{readNFI}
\title{Read raw SNFI tables from archives, URLs, or local files}
\description{Read raw tables from the Spanish National Forest Inventory (SNFI)
Import raw inventory tables from
the Spanish National Forest Inventory (SNFI)
or compatible tabular exports. Use
\code{readNFI()} when you need the original
table structure before computing metrics with
higher-level workflows.
and compatible inventory exports. The function accepts province
Import raw inventory tables from
the Spanish National Forest Inventory (SNFI)
or compatible tabular exports. Use
\code{readNFI()} when you need the original
table structure before computing metrics with
higher-level workflows.
identifiers that are resolved to official SNFI download URLs, local
Import raw inventory tables from
the Spanish National Forest Inventory (SNFI)
or compatible tabular exports. Use
\code{readNFI()} when you need the original
table structure before computing metrics with
higher-level workflows.
or remote \code{.zip} archives, and direct paths to decompressed
Import raw inventory tables from
the Spanish National Forest Inventory (SNFI)
or compatible tabular exports. Use
\code{readNFI()} when you need the original
table structure before computing metrics with
higher-level workflows.
\code{.csv}, \code{.dbf}, \code{.mdb}, or \code{.accdb} files.
Import raw inventory tables from
the Spanish National Forest Inventory (SNFI)
or compatible tabular exports. Use
\code{readNFI()} when you need the original
table structure before computing metrics with
higher-level workflows.}
\usage{readNFI(nfi, nfi.nr = 4, 
    dt.nm = "PCMayores", 
    file_ext = NULL, 
    file_name = NULL, 
    ...)}
\arguments{
  \item{nfi}{\code{character}. Inventory source to read. Accepted
values are: (i) a province name or province code to be
resolved to an official SNFI download URL; (ii) a local or
remote \code{.zip} archive; or (iii) one or more direct
paths to decompressed \code{.csv}, \code{.dbf},
\code{.mdb}, or \code{.accdb} files.}
  \item{nfi.nr}{\code{integer}. SNFI stage used when \code{nfi}
is given as a province identifier or when the
inventory stage cannot be inferred from file names.
Use \code{2}, \code{3}, or \code{4}.}
  \item{dt.nm}{\code{character}. Table name or names to
import from the selected inventory source.
For second-stage DBF inputs,
\code{"PCMayores"} is internally remapped to
\code{"PIESMA"}. For many third- and
fourth-stage tree workflows,
\code{"PCMayores"} is the main tree table.}
  \item{file_ext}{\code{character}. Optional file extension or
extensions forwarded to \code{\link{fetchNFI}}
when \code{nfi} is a province identifier or a
\code{.zip} archive. Leave \code{NULL} to use
the default Access/DBF extensions handled by
\code{fetchNFI()}. Use \code{"csv"} for zipped
CSV exports.}
  \item{file_name}{\code{character}. Optional file name or stem
passed to \code{\link{fetchNFI}} to keep only
specific files inside a \code{.zip} archive.}
  \item{\dots}{Additional arguments passed to \code{\link{fetchNFI}},
such as \code{dir} or \code{timeOut}.}
}
\details{The input \code{nfi} can be supplied
in three main forms. First, it can be a
province name or code; in that case,
\code{readNFI()} resolves the identifier to
an official SNFI download URL according to
\code{nfi.nr}. Second, it can be a local or
remote \code{.zip} archive; the function then
delegates extraction to \code{\link{fetchNFI}}.
Third, it can be one or more already
decompressed file paths.

When the selected files are \code{.csv}, the
function detects the field separator
automatically and returns either one data
frame or a named list of data frames. When the
selected files are Access or DBF tables from
the SNFI, the function reads the requested
table, converts numeric-looking factors back to
numeric values, preserves character columns,
and adds province and inventory-stage metadata.

Access backends are platform dependent. On
Windows, reading \code{.mdb} or \code{.accdb}
files requires package \pkg{RODBC} and an
installed Microsoft Access driver. On
Unix-like systems, it requires package
\pkg{Hmisc} together with the external
\code{mdbtools} utilities. Use
\code{file_ext = "csv"} to bypass those
dependencies when you work with zipped CSV
exports.}
\value{Returns one of three object types, depending on the input.
First, when \code{nfi} resolves to \code{.csv} file paths, the
function returns a single \code{data.frame} for one file or a named
\code{list} of \code{data.frame}s for several files. Second, when
it reads Access or DBF tables from the SNFI, it returns a
\code{data.frame} of class \code{c("readNFI", "data.frame")}.
This object includes leading columns \code{nfi.nr} and \code{pr},
stores the province vector in \code{attr(x, "pr.")}, and stores the
inferred inventory stage in \code{attr(x, "nfi.nr")}. Third, the
function returns \code{NULL} when fetching, extraction, or import
fails, or when no requested file can be read.}

\author{Wilson Lara <wilarhen@gmail.com> [aut, cre] (ORCID: <https://orcid.org/0000-0003-3527-1380>), Cristobal Ordonez <angelcristobal.ordonez@uva.es> [aut] (ORCID: <https://orcid.org/0000-0001-5354-3760>), Aitor Vázquez-Veloso <aitor.vazquez.veloso@uva.es> [aut] (ORCID: <https://orcid.org/0000-0003-0227-506X>), Felipe Bravo <felipe.bravo@uva.es> [aut] (ORCID: <https://orcid.org/0000-0001-7348-6695>)}




\examples{
## Minimal example using a local CSV file created on the fly
tmp <- tempfile(fileext = ".csv")
utils::write.table(
    data.frame(
        plot = 1:2,
        species = c("sp1", "sp2"),
        dbh_cm = c(12.5, 18.0)
    ),
    file = tmp,
    sep = ";",
    row.names = FALSE,
    quote = FALSE
)

x <- readNFI(tmp)
str(x)

unlink(tmp)
}
