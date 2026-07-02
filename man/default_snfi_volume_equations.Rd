\name{default_snfi_volume_equations}
\alias{default_snfi_volume_equations}
\title{Default SNFI volume-equation methods}
\description{Return the default SNFI volume-equation registry. Creates the
default set of method definitions used by \code{metrics2Vol()} to
compute tree-level volume variables. Each registry entry includes
descriptive metadata together with the target column name,
equation function, unit conversion, argument builder, and fallback
rule.}
\usage{default_snfi_volume_equations()}

\value{A named list with the default methods \code{V}, \code{VCC}, and \code{VSC}.

Each entry includes \code{label}, \code{equation}, \code{reference}, and

\code{description} fields for documentation, in addition to the

computational fields used by \code{metrics2Vol()}.}

\author{Wilson Lara [aut, cre] (ORCID: <https://orcid.org/0000-0003-3527-1380>),
Cristobal Ordonez [aut] (ORCID:
<https://orcid.org/0000-0001-5354-3760>),
Aitor Vázquez-Veloso [aut] (ORCID:
<https://orcid.org/0000-0003-0227-506X>),
Felipe Bravo [aut] (ORCID: <https://orcid.org/0000-0001-7348-6695>)}




\examples{
methods <- default_snfi_volume_equations()
names(methods)
methods$VCC$output
methods$VCC$unit
}
