\name{get_snfi_iavc}
\alias{get_snfi_iavc}
\title{SNFI annual increment of volume over bark}
\description{Compute SNFI IAVC using the variables required by the selected model.}
\usage{get_snfi_iavc(dbh_mm = NULL, 
    dnm_mm = NULL, h_t = NULL, 
    vcc = NULL, pars)}
\arguments{
  \item{dbh_mm}{Tree diameter at breast height in millimetres, when required by the model.}
  \item{dnm_mm}{Mean plot diameter in millimetres, when required by the model.}
  \item{h_t}{Tree height in metres, when required by the model.}
  \item{vcc}{Merchantable volume over bark in dm3, when required by the model.}
  \item{pars}{One-row \code{data.frame} of SNFI coefficients. The function
<< assumes exactly one row with a \code{Modelo} field and the
<< coefficients required by that model.}
}
\details{This function evaluates the SNFI equation for annual increment

of volume over bark (IAVC) and returns the result in dm3.

It supports models 8, 13, 14, 16, 17, 18, 19, 20, 21, and 25.

The required inputs depend on the selected model.}
\value{Numeric IAVC in dm3. Returns \code{NA_real_} when \code{pars}

is empty, when required inputs are missing, or when the model

is not recognised. Missing required inputs also trigger a

printed message.
print("Model not recognized for IAVC.")}

\author{Wilson Lara <wilarhen@gmail.com> [aut, cre] (ORCID: <https://orcid.org/0000-0003-3527-1380>), Cristobal Ordonez <angelcristobal.ordonez@uva.es> [aut] (ORCID: <https://orcid.org/0000-0001-5354-3760>), Aitor Vázquez-Veloso <aitor.vazquez.veloso@uva.es> [aut] (ORCID: <https://orcid.org/0000-0003-0227-506X>), Felipe Bravo <felipe.bravo@uva.es> [aut] (ORCID: <https://orcid.org/0000-0001-7348-6695>)}





