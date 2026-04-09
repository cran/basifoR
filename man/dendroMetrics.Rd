\name{dendroMetrics}
\alias{dendroMetrics}
\title{Summarize dendrometrics}
\description{This function summarizes dendrometric data from the Spanish
National Forest Inventory (SNF). It primarily accepts a province
name or number, a local compressed SNF file, or a URL to a
compressed SNF file hosted by \code{www.miteco.gob.es}. It can
also process data frames previously returned by
\code{\link{readNFI}}, \code{\link{nfiMetrics}}, or
\code{\link{metrics2Vol}}. Dendrometric variables in the output
are transformed into stand units, see Details section.}
\usage{dendroMetrics(nfi, summ.vr = "Estadillo", 
    metric_levels = NULL, 
    cut.dt = "d == d", 
    report = FALSE, mc.cores = getOption("mc.cores", 
        1L), ...)}
\arguments{
  \item{nfi}{\code{character}, \code{data.frame}, or \code{list}. A
province name or province number used to locate SNF data;
a local path or URL to a compressed SNF file
(\code{.zip}), including ZIP files hosted by
\code{www.miteco.gob.es}; a data frame such as that
returned by \code{\link{readNFI}},
\code{\link{nfiMetrics}}, or
\code{\link{metrics2Vol}}; or a list of such objects.}
  \item{summ.vr}{\code{character} or \code{NULL}. Name
of a Categorical variables in the SNF
data used to summarize the outputs. If
\code{NULL} then output from
\code{\link{metrics2Vol}} is
returned. Default \code{'Estadillo'}
processes sample plots.}
  \item{metric_levels}{\code{character} or \code{NULL}. Grouping
variables used to compute tree-level
metrics such as \code{Hd} before final
summarization. When \code{NULL}, metric
computation follows \code{summ.vr}.}
  \item{cut.dt}{\code{character}. Logical condition used
to subset the output. Default \code{'d == d'}
avoids subsetting.}
  \item{report}{\code{logical}. Print a \code{report} of the output
in the current working directory.}
  \item{mc.cores}{\code{integer}. Number
of cores used when several
inputs are processed.}
  \item{\dots}{Additional arguments passed to \code{\link{readNFI}},
\code{\link{nfiMetrics}}, or \code{\link{metrics2Vol}},
including \code{nfi.nr} when required.}
}
\details{Dendrometric variables are
summarized according to the levels of
argument \code{summ.vr}. Summary outputs
include the categorical columns defined
by \code{summ.vr} together with the
quantitative variables available after
processing with \code{\link{nfiMetrics}}
and \code{\link{metrics2Vol}}.

These variables may include tree basal
area \code{ba} (\code{'m2 ha-1'}), mean
diameter at breast height \code{d}
(\code{'cm'}), quadratic mean diameter
\code{dg} (\code{'cm'}), mean tree
height \code{h} (\code{'m'}), number of
trees per hectare \code{n}
(\code{'dimensionless'}), and over-bark
volume \code{v} (\code{'m3 ha-1'}).

When \code{summ.vr = NULL}, the function
returns tree-level outputs from
\code{\link{metrics2Vol}} after applying
the filter defined in \code{cut.dt}.

When \code{summ.vr} is not \code{NULL},
the function converts supported
variables to stand units, splits the data
by the requested grouping variable, and
computes summaries by group. Extensive
variables are multiplied by \code{n}
before summation. Variables \code{d},
\code{h}, and \code{Hd}, when present,
are returned as averages weighted by
\code{n}.

If both \code{ba} and \code{n} are
available, the function also derives the
quadratic mean diameter \code{dg}.

Output subsets are extracted using the
logical expression supplied in
\code{cut.dt}, see syntax in
\code{\link{Logic}}.

The function accepts one input object or
several inputs. When several inputs are
supplied, each one is processed
independently and the results are merged
into a single output data frame.
Parallel processing is controlled by
\code{mc.cores}. Values greater than 1
process several inputs in parallel.}
\value{\code{data.frame}. Depending on \code{summ.vr = NULL}, an output from
\code{\link{metrics2Vol}}, or a summary of the variables, see
Details section.}

\author{Wilson Lara <wilarhen@gmail.com> [aut, cre] (ORCID: <https://orcid.org/0000-0003-3527-1380>), Cristobal Ordonez <angelcristobal.ordonez@uva.es> [aut] (ORCID: <https://orcid.org/0000-0001-5354-3760>), Aitor Vázquez-Veloso <aitor.vazquez.veloso@uva.es> [aut] (ORCID: <https://orcid.org/0000-0003-0227-506X>), Felipe Bravo <felipe.bravo@uva.es> [aut] (ORCID: <https://orcid.org/0000-0001-7348-6695>)}




\examples{

## Minimal precomputed metrics object with units
toy_metrics <- structure(
    data.frame(
        Estadillo = c("plot1", "plot1", "plot2"),
        Especie   = c("sp1", "sp2", "sp1"),
        d         = c(120, 185, 260),          # mm
        h         = c(71, 94, 132),            # dm
        ba        = c(0.0113, 0.0269, 0.0531), # m2 tree-1
        n         = c(127.32, 31.83, 14.15),
        stringsAsFactors = FALSE
    ),
    class = c("nfiMetrics", "data.frame"),
    units = c(
        d  = "mm",
        h  = "dm",
        ba = "m2 tree-1",
        n  = "ha-1"
    ),
    nfi.nr = 4
)

## Summarize by plot
dendromet_toy <- dendroMetrics(toy_metrics, summ.vr = "Estadillo")

## Display output structure
str(dendromet_toy)

## Check returned units
attr(dendromet_toy, "units")

## Return tree-level processed data
tree_toy <- dendroMetrics(toy_metrics, summ.vr = NULL, cut.dt = "h > 8")
head(tree_toy)

## Alternatively, download data from 'www.miteco.gob.es'
## Specify province name/number and nfi number to compute dendrometrics.
    
}
