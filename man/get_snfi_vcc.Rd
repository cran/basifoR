\name{get_snfi_vcc}
\alias{get_snfi_vcc}
\title{SNFI merchantable volume over bark}
\description{Compute SNFI VCC from tree diameter, height, and one coefficient row.}
\usage{get_snfi_vcc(dbh_mm, 
    h_t, pars)}
\arguments{
  \item{dbh_mm}{Tree diameter at breast height in millimetres.}
  \item{h_t}{Tree height in metres.}
  \item{pars}{One-row \code{data.frame} of SNFI coefficients. The function
<< assumes exactly one row with a \code{Modelo} field and the
<< coefficients required by that model.}
}
\details{This function evaluates the SNFI equation for merchantable

volume over bark (VCC) and returns the result in dm3.

It currently supports models 1 and 11 from the SNFI

coefficient table.}
\value{Numeric VCC in dm3. Returns \code{NA_real_} when \code{pars}

is empty or when the model is not recognised.
print("Model not recognized for VCC.")}

\author{Wilson Lara <wilarhen@gmail.com> [aut, cre] (ORCID: <https://orcid.org/0000-0003-3527-1380>), Cristobal Ordonez <angelcristobal.ordonez@uva.es> [aut] (ORCID: <https://orcid.org/0000-0001-5354-3760>), Aitor Vázquez-Veloso <aitor.vazquez.veloso@uva.es> [aut] (ORCID: <https://orcid.org/0000-0003-0227-506X>), Felipe Bravo <felipe.bravo@uva.es> [aut] (ORCID: <https://orcid.org/0000-0001-7348-6695>)}





