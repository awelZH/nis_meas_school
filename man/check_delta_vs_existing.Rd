% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check_existing_data.R
\name{check_delta_vs_existing}
\alias{check_delta_vs_existing}
\title{function to check the delta between OGD Ressources online and measurement data in the folder (fails informatively)}
\usage{
check_delta_vs_existing(existing_messorte, delta_langzeit, delta_einzel)
}
\arguments{
\item{existing_messorte}{list with existing ogd - produced by \code{\link[=retrieve_existing_data]{retrieve_existing_data()}})}

\item{delta_langzeit}{list with delta Langzeit (new Langzeitmessungen data in folder)}

\item{delta_einzel}{list with delta einzel (new Einzelmessungen data in folder)}
}
\value{
print statements in console about status of delta
}
\description{
function to check the delta between OGD Ressources online and measurement data in the folder (fails informatively)
}
