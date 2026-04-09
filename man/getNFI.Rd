\name{getNFI}
\alias{getNFI}
\title{Read raw SNFI tables (deprecated; use \code{readNFI()})}
\description{Deprecated wrapper around \code{\link{readNFI}}. \code{getNFI()} is kept for backward compatibility, but it now delegates internally to \code{readNFI()} and returns imported tables instead of acting as a file-fetching helper.}
\usage{getNFI(provincia, ...)}
\arguments{
  \item{provincia}{Input accepted by \code{\link{readNFI}}. This can be a province identifier, a local or remote \code{.zip} archive, one or more decompressed inventory file paths, or a data frame already loaded in memory.}
  \item{\dots}{Additional arguments passed to \code{\link{readNFI}}.}
}
\details{\strong{Deprecated.} Use \code{\link{readNFI}} in new code. This compatibility wrapper preserves the historical function name while redirecting all supported inputs to \code{readNFI()}, including province identifiers, local or remote \code{.zip} archives, direct paths to decompressed inventory files, and data frames already loaded in memory.

Because \code{getNFI()} now calls \code{readNFI()} internally, its behavior follows \code{readNFI()} semantics. In particular, the function returns imported data rather than extracted file paths. Users who still need archive extraction without import should call \code{\link{fetchNFI}} directly.

The argument names are kept for compatibility with older code,

but users should migrate to \code{\link{readNFI}}.

A deprecation warning is emitted on each call to make that transition explicit.}
\value{The same return value as \verb{readNFI}. Depending on the input and selected files, this is typically a data frame or a named list of data frames containing imported SNFI tables.}

\author{Wilson Lara <wilarhen@gmail.com> [aut, cre] (ORCID: <https://orcid.org/0000-0003-3527-1380>), Cristobal Ordonez <angelcristobal.ordonez@uva.es> [aut] (ORCID: <https://orcid.org/0000-0001-5354-3760>), Aitor Vázquez-Veloso <aitor.vazquez.veloso@uva.es> [aut] (ORCID: <https://orcid.org/0000-0003-0227-506X>), Felipe Bravo <felipe.bravo@uva.es> [aut] (ORCID: <https://orcid.org/0000-0001-7348-6695>)}




\examples{
## Minimal self-contained example using a temporary CSV file
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

x <- suppressWarnings(getNFI(tmp))
str(x)

unlink(tmp)
}
