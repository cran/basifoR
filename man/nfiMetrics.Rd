\name{nfiMetrics}
\alias{nfiMetrics}
\title{Tree-level metrics for Spanish NFI inputs}
\description{Compute tree-level diameter, height, basal area, trees per
hectare, and optional dominant height from Spanish National Forest
Inventory inputs. Supply either an object returned by
\code{\link{readNFI}} or a path/URL that \code{\link{readNFI}}
can import. The function returns the requested metrics together
with the matched grouping columns and attaches unit metadata to the
result.}
\usage{nfiMetrics(nfi, var = c("d", 
    "h", "ba", "n", "Hd"), 
    levels = c("esta", 
        "espe"), design = snfi_design(), 
    ...)}
\arguments{
  \item{nfi}{\code{character(1)} or a \code{"readNFI"} object.
<< Supply either a path/URL that \code{\link{readNFI}} can
<< import or an already imported object returned by
<< \code{\link{readNFI}}. The input should contain the SNFI
<< diameter and height fields, usually aliases such as
<< \code{Dn} and \code{altura}.}
  \item{var}{\code{character}. Metrics to
<< compute. Supported values are
<< \code{'d'} (diameter in
<< \code{mm}), \code{'h'}
<< (height in \code{dm}),
<< \code{'ba'} (basal area per
<< tree in \code{m^2}),
<< \code{'n'} (trees per hectare),
<< and \code{'Hd'} (dominant
<< height in \code{dm}).
<< Request \code{'h'},
<< \code{'d'}, and \code{'n'}
<< together when you request
<< \code{'Hd'}.}
  \item{levels}{\code{character}. Column-name
<< patterns used to keep grouping
<< variables in the output. Matching
<< ignores case and accepts partial
<< matches. The default usually keeps
<< plot and species identifiers when
<< those fields are present.}
  \item{design}{Sampling \code{design} used to derive
<< \code{'n'} and any dependent
<< \code{'Hd'} calculation. Supply the
<< default \code{\link{snfi_design}()},
<< another \code{"concentric_design"},
<< or any \code{"inventory_design"}
<< supported by \code{\link{trees_per_ha}}.
<< The returned object stores a summary of
<< the \code{design} in \code{attr(x,
<< "design_meta")} when relevant.}
  \item{\dots}{Additional arguments passed to \code{\link{readNFI}} when
<< \code{nfi} is not already a \code{"readNFI"} object.}
}
\details{If you request \code{'Hd'}, the function computes

dominant height within the groups selected by

\code{levels} by using \code{'h'}, \code{'d'}, and

\code{'n'}.}
\value{\code{data.frame} with the matched identifier and grouping
columns plus the metrics requested in \code{var}. The output
inherits from \code{'nfiMetrics'} and \code{'data.frame'} and
stores \code{attr(x, 'nfi.nr')} when available. Inspect
\code{attr(x, 'units')} for the returned metric units and
\code{attr(x, 'design_meta')} for the sampling \code{design} summary
attached when \code{'n'} or \code{'Hd'} is requested.}

\author{Wilson Lara <wilarhen@gmail.com> [aut, cre] (ORCID: <https://orcid.org/0000-0003-3527-1380>), Cristobal Ordonez <angelcristobal.ordonez@uva.es> [aut] (ORCID: <https://orcid.org/0000-0001-5354-3760>), Aitor Vázquez-Veloso <aitor.vazquez.veloso@uva.es> [aut] (ORCID: <https://orcid.org/0000-0003-0227-506X>), Felipe Bravo <felipe.bravo@uva.es> [aut] (ORCID: <https://orcid.org/0000-0001-7348-6695>)}



\seealso{dendroMetrics, dbhMetric, readNFI, snfi_design, trees_per_ha}
\examples{
## Minimal reproducible example with a small object that mimics
## readNFI() output
toy_ifn <- structure(
    data.frame(
        esta = c("plot1", "plot1", "plot2"),
        espe = c("sp1", "sp2", "sp1"),
        Dn = c(120, 185, 260),
        altura = c(7.1, 9.4, 13.2),
        stringsAsFactors = FALSE
    ),
    class = c("readNFI", "data.frame"),
    nfi.nr = 4
)

x <- nfiMetrics(
    toy_ifn,
    var = c("d", "h", "ba", "n"),
    levels = c("esta", "espe")
)

x
attr(x, "units")
}
