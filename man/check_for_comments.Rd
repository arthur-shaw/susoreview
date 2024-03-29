% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/workflow.R
\name{check_for_comments}
\alias{check_for_comments}
\title{Determine whether interviews contain comments relevant to the rejection decision.}
\usage{
check_for_comments(df_comments, df_issues, df_cases_to_review)
}
\arguments{
\item{df_comments}{Data frame. Corresponds to interview__comments export file.}

\item{df_issues}{Data frame. File of issues.}

\item{df_cases_to_review}{Data frame. File of cases that are subject to review.}
}
\value{
Data frame. One observation per interview with a comment.
}
\description{
Determine whether interviews contain comments relevant to the rejection decision.
}
