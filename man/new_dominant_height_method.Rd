\name{new_dominant_height_method}
\alias{new_dominant_height_method}
\title{Define one dominant-height computation method}
\description{Create a compact method specification for registry-based dominant-height calculations.}
\usage{new_dominant_height_method(output = "Hd", 
    fun = NULL, fun_name = NULL, 
    diameter_fun = NULL, 
    diameter_fun_name = NULL, 
    diameter_unit = "mm", 
    diameter_equation = NULL, 
    unit = "m", threshold = 100, 
    equation = NULL, 
    selection_rule = NULL, 
    fallback = NULL, 
    variables = c(h = "tree height in metres", 
        d = "diameter at breast height in millimetres", 
        n = "tree expansion factor in trees per hectare"), 
    required_inputs = c("h", 
        "d", "n"), reference = NULL, 
    description = NULL)}
\arguments{
  \item{output}{\code{character(1)}. Name of the \code{output} column produced by the method.}
  \item{fun}{Optional \code{function}. Direct function used to compute dominant height.}
  \item{fun_name}{Optional \code{character(1)}. Name of a function to resolve at run time when \code{fun} is not supplied.}
  \item{diameter_fun}{Optional paired function used to compute dominant diameter from \code{d} and \code{n}.}
  \item{diameter_fun_name}{Optional run-time name of the paired dominant-diameter function.}
  \item{diameter_unit}{\code{character(1)}. Unit reported for dominant diameter.}
  \item{diameter_equation}{\code{character(1)}. Human-readable dominant-diameter \code{equation}.}
  \item{unit}{\code{character(1)}. Unit reported for the returned dominant-height value.}
  \item{threshold}{\code{numeric(1)}. Reference number of trees per hectare used to define the dominant-height subset.}
  \item{equation}{\code{character(1)}. Human-readable \code{equation} used by the method.}
  \item{selection_rule}{\code{character(1)}. Human-readable rule used to select trees before applying the \code{equation}.}
  \item{fallback}{\code{character(1)}. Human-readable \code{description} of what the method returns when the \code{threshold} cannot be reached.}
  \item{variables}{Named \code{character}. Meaning and expected units of the \code{variables} used by \code{equation}.}
  \item{required_inputs}{\code{character}. Standardized inputs required by the method.}
  \item{reference}{Optional \code{character(1)}. Source or implementation \code{reference}.}
  \item{description}{Optional \code{character(1)}. Short \code{description} of the method.}
}
\details{The object does not evaluate dominant height by itself. It only

records the \code{output} name, function or function name, \code{unit},

dominant-tree \code{threshold}, selection rule, \code{equation}, \code{fallback} rule,

variable definitions, and optional \code{reference}.}
\value{A named \code{list} with class \code{"dominant_height_method"}.}

\author{Wilson Lara [aut, cre] (ORCID: <https://orcid.org/0000-0003-3527-1380>),
Cristobal Ordonez [aut] (ORCID:
<https://orcid.org/0000-0001-5354-3760>),
Aitor Vázquez-Veloso [aut] (ORCID:
<https://orcid.org/0000-0003-0227-506X>),
Felipe Bravo [aut] (ORCID: <https://orcid.org/0000-0001-7348-6695>)}




\examples{
m <- new_dominant_height_method(
    fun_name = "domheight",
    equation = "Hd = sum(h_i * n_i) / sum(n_i)",
    selection_rule = "Sort trees by decreasing diameter and select dominant trees.",
    fallback = "Use the weighted mean height of all valid trees."
)
m$equation
}
