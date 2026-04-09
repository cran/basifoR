\name{update.list}
\alias{update.list}
\title{Guard raw list inputs in \code{update}
Guard against raw dendroMetrics inputs}
\description{Catch attempts to call \code{update}
on raw input lists intended for \code{\link{dendroMetrics}} and
return a workflow-specific error message. When the input does not
look like a \code{dendroMetrics} workflow, this method falls back to
\code{update.default}.}
\usage{\method{update}{list}(object, ..., 
    evaluate = TRUE)}
\arguments{
  \item{object}{A \code{list} passed to \code{update()}.}
  \item{\dots}{Additional arguments passed to \code{update()}.}
  \item{evaluate}{\code{logical}. Passed through to
\code{update.default} when no
\code{dendroMetrics}-specific guard is triggered.}
}

\value{Either an error with a \code{dendroMetrics}-specific
message or the result of \code{update.default()}.}

\author{Wilson Lara <wilarhen@gmail.com> [aut, cre] (ORCID: <https://orcid.org/0000-0003-3527-1380>), Cristobal Ordonez <angelcristobal.ordonez@uva.es> [aut] (ORCID: <https://orcid.org/0000-0001-5354-3760>), Aitor Vázquez-Veloso <aitor.vazquez.veloso@uva.es> [aut] (ORCID: <https://orcid.org/0000-0003-0227-506X>), Felipe Bravo <felipe.bravo@uva.es> [aut] (ORCID: <https://orcid.org/0000-0001-7348-6695>)}





