\name{inventoryMetrics}
\alias{inventoryMetrics}
\title{Dispatch complete workflows for SNFI and external inventories
Unified dispatcher for inventory workflows}
\description{Run a complete inventory workflow by dispatching to either
\code{\link{dendroMetrics}} for Spanish National Forest Inventory
inputs or to \code{external_dendroMetrics()} for external
inventories. The function is intentionally thin: it decides which
\code{backend} to use, forwards the relevant arguments, and stores the
original call so the result can be updated later with
\code{\link[basifoR:update.inventoryMetrics]{update}}.}
\usage{inventoryMetrics(nfi, 
    backend = c("auto", 
        "snfi", "external"), 
    summ.vr = "Estadillo", 
    cut.dt = "d == d", 
    report = FALSE, mc.cores = getOption("mc.cores", 
        1L), design = NULL, 
    schema = NULL, method_registry = NULL, 
    parameter_table = NULL, 
    ...)}
\arguments{
  \item{nfi}{\code{character}, \code{data.frame}, or \code{list}. One
<< inventory input or several inputs accepted by the selected
<< \code{backend}.}
  \item{backend}{\code{character(1)}.
<< Backend selector.
<< Use \code{"auto"}
<< to infer the \code{backend}
<< from the supplied
<< arguments.}
  \item{summ.vr}{\code{character} or \code{NULL}. Grouping
<< variable passed to the selected \code{backend}.
<< When this argument is not supplied,
<< \code{inventoryMetrics()} uses a backend-
<< aware default: \code{"Estadillo"} for
<< \code{backend = "snfi"} and the \code{backend}
<< default for \code{backend = "external"}.}
  \item{cut.dt}{\code{character}. Logical expression used
<< to subset the \code{backend} output.}
  \item{report}{\code{logical}. If \code{TRUE}, request a
<< CSV \code{report} from the selected \code{backend}.}
  \item{mc.cores}{\code{integer}. Number
<< of cores used when the
<< \code{backend} supports several
<< inputs.}
  \item{design}{Optional sampling \code{design}. Passed to the
<< selected \code{backend} when relevant.}
  \item{schema}{Optional external \code{schema} object. Used only by
<< \code{backend = "external"}.}
  \item{method_registry}{Optional method registry passed to the
<< selected \code{backend}. This may define
<< custom volume or summarization methods.}
  \item{parameter_table}{Optional table of parameters required
<< by the external \code{backend}.}
  \item{\dots}{Additional named arguments forwarded to the selected
<< \code{backend}.}
}
\details{The dispatcher keeps computation inside the \code{backend}

functions. It only validates the \code{backend} choice,

forwards arguments, stores the reconstructed call in

\code{attr(x, "call")}, and records the chosen \code{backend} in

\code{attr(x, "backend")}. That makes the wrapper easier

to maintain while preserving a single public entry point.}
\value{A \code{backend} result augmented with class

\code{"inventoryMetrics"}, a stored call in

\code{attr(x, "call")}, and the selected \code{backend} in

\code{attr(x, "backend")}. The returned object otherwise

preserves the class and attributes produced by the \code{backend}.}

\author{Wilson Lara <wilarhen@gmail.com> [aut, cre] (ORCID: <https://orcid.org/0000-0003-3527-1380>), Cristobal Ordonez <angelcristobal.ordonez@uva.es> [aut] (ORCID: <https://orcid.org/0000-0001-5354-3760>), Aitor Vázquez-Veloso <aitor.vazquez.veloso@uva.es> [aut] (ORCID: <https://orcid.org/0000-0003-0227-506X>), Felipe Bravo <felipe.bravo@uva.es> [aut] (ORCID: <https://orcid.org/0000-0001-7348-6695>)}
\note{Keep backend-specific computation in the backend functions,

not in this dispatcher.}


\seealso{dendroMetrics, nfiMetrics, metrics2Vol, update.inventoryMetrics}
\examples{
    
    ## External workflows require an external backend function and the
    ## corresponding schema, design, and parameter objects.

ext <- data.frame(
  plot = c("P1", "P1", "P2"),
  species = c("sp1", "sp1", "sp2"),
  diameter_mm = c(120, 185, 260),
  height_m = c(7.1, 9.4, 13.2),
  stringsAsFactors = FALSE
)

sch <- new_external_schema(
  colmap = list(
    plot = "plot",
    species = "species",
    d = "diameter_mm",
    h = "height_m"
  ),
  units = list(d = "mm", h = "m"),
  levels = "plot",
  keep_cols = c("plot", "species")
)

dsg <- new_inventory_design(
  sample_area_m2 = 1000,
  min_dbh_cm = 7.5,
  name = "Square 0.1-ha plot",
  metadata = list(shape = "square", side_m = sqrt(1000))
)

pars <- data.frame(
  species = c("sp1", "sp2"),
  a = c(0.00002, 0.00003),
  b = c(2.30, 2.10),
  stringsAsFactors = FALSE
)

reg <- external_volume_method_registry(list(
  V = new_volume_method(
    output = "v",
    fun = function(dbh_mm, h_m, pars) {
      dbh_cm <- dbh_mm / 10
      pars$a + pars$b * (dbh_cm^2) * h_m
    },
    raw_unit = "cm3",
    unit = "m3",
    scale_to_m3 = 1 / 1e6,
    build_args = function(ctx, pars, resolved) {
      list(dbh_mm = ctx$d_mm, h_m = ctx$h_m, pars = pars)
    },
    fallback = function(ctx, pars, resolved) NA_real_,
    match_by = "species",
    required_inputs = c("d", "h")
  )
))

out <- inventoryMetrics(
  ext,
  backend = "external",
  schema = sch,
  design = dsg,
  parameter_table = pars,
  method_registry = reg,
  summ.vr = "plot",
  var = c("d", "h", "ba", "n", "v"),
  parametro = "V"
)

out
attr(out, "backend")
attr(out, "units")


}
