% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/attributes.R
\name{create_attribute}
\alias{create_attribute}
\title{Create attribute from one or more variable in microdata}
\usage{
create_attribute(df, condition, attrib_name, attrib_vars)
}
\arguments{
\item{df}{Data frame. Household-level microdata that contains the attribute to extract.}

\item{condition}{Logical expression}

\item{attrib_name}{Character. Name to give the attribute in the issues data file}

\item{attrib_vars}{Character. Regular expression that identifies the variable in \code{var}.}
}
\value{
Data frame of attributes with the following columns:
\itemize{
\item \code{interview__id}
\item \code{interview__key}
\item \code{attrib_name}
\item \code{attrib_val}
\item \code{attrib_vars}
}
}
\description{
Create attribute from one or more variable in microdata
}
