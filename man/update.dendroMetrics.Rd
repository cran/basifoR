\name{update.dendroMetrics}
\alias{update.dendroMetrics}
\title{Update a stored dendroMetrics call
Update a dendroMetrics result}
\description{Re-run \code{\link{dendroMetrics}} from the call stored
in a previous \code{"dendroMetrics"} result while replacing one or
more named arguments in \code{...}. This method supports reproducible
re-evaluation of the original workflow and can either return the
updated result or the reconstructed call.}
\usage{\method{update}{dendroMetrics}(object, 
    ..., evaluate = TRUE)}
\arguments{
  \item{object}{A \code{"dendroMetrics"} \code{object} created by the
updatable \code{\link{dendroMetrics}} definition.}
  \item{\dots}{Named arguments used to replace entries in the stored
call before evaluation.}
  \item{evaluate}{\code{logical}. If \code{TRUE}, \code{evaluate} the
updated call and return the resulting \code{object}.
If \code{FALSE}, return the reconstructed call
without evaluation.}
}

\value{A new \code{"dendroMetrics"} \code{object} when
\code{evaluate = TRUE}; otherwise the updated call.}

\author{Wilson Lara <wilarhen@gmail.com> [aut, cre] (ORCID: <https://orcid.org/0000-0003-3527-1380>), Cristobal Ordonez <angelcristobal.ordonez@uva.es> [aut] (ORCID: <https://orcid.org/0000-0001-5354-3760>), Aitor Vázquez-Veloso <aitor.vazquez.veloso@uva.es> [aut] (ORCID: <https://orcid.org/0000-0003-0227-506X>), Felipe Bravo <felipe.bravo@uva.es> [aut] (ORCID: <https://orcid.org/0000-0001-7348-6695>)}





