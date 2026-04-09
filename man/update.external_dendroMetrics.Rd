\name{update.external_dendroMetrics}
\alias{update.external_dendroMetrics}
\title{Update a stored external_dendroMetrics call}
\description{Rebuild the call stored in a previous \code{"external_dendroMetrics"} result, optionally replace named arguments, and either \code{evaluate} the updated call or return it unevaluated.}
\usage{\method{update}{external_dendroMetrics}(object, 
    ..., evaluate = TRUE)}
\arguments{
  \item{object}{Object returned by \code{external_dendroMetrics()}.}
  \item{\dots}{Named arguments used to replace entries in the stored call.}
  \item{evaluate}{If \code{TRUE}, \code{evaluate} the updated call; otherwise return the call.}
}
\details{The method checks that \code{object} inherits from \code{"external_dendroMetrics"} and that the original matched call is stored in \verb{attr(\code{object}, "call")}. It then replaces any named arguments supplied in \code{...} inside that stored call.

Use \code{evaluate = FALSE} to inspect the reconstructed call before execution. This is useful when debugging filters, grouping variables, schemas, or volume-method options.

The method checks that \code{object} inherits from
\code{"external_dendroMetrics"} and that it stores the original
matched call in \verb{attr(\code{object}, "call")}. It then replaces any
named arguments supplied in \code{...} inside that stored call.

Use \code{evaluate = FALSE} to inspect the reconstructed call
before execution, which is useful when debugging filters,
grouping variables, schemas, or volume-method options.

The method only changes arguments supplied explicitly in
\code{...}; all other arguments remain as stored in the original
call.}
\value{A new \code{"external_dendroMetrics"} \code{object} when \code{evaluate = TRUE}; otherwise the updated call.}

\author{Wilson Lara <wilarhen@gmail.com> [aut, cre] (ORCID: <https://orcid.org/0000-0003-3527-1380>), Cristobal Ordonez <angelcristobal.ordonez@uva.es> [aut] (ORCID: <https://orcid.org/0000-0001-5354-3760>), Aitor Vázquez-Veloso <aitor.vazquez.veloso@uva.es> [aut] (ORCID: <https://orcid.org/0000-0003-0227-506X>), Felipe Bravo <felipe.bravo@uva.es> [aut] (ORCID: <https://orcid.org/0000-0001-7348-6695>)}




\examples{
x <- structure(
    data.frame(plot = "P1", d = 12, h = 8),
    class = c("external_dendroMetrics", "dendroMetrics", "data.frame")
)

attr(x, "call") <- quote(
    external_dendroMetrics(
        x = data.frame(plot = "P1", d = 12, h = 8),
        var = c("d", "h"),
        summ.vr = NULL
    )
)

update(x, var = c("d", "h", "ba"), evaluate = FALSE)
}
