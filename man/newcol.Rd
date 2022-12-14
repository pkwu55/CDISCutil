% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/newcol.R
\name{newcol}
\alias{newcol}
\title{Add a column with column name from the name_from column
and values from name_to column
Note that the value of the names_from column must be unique}
\usage{
newcol(dst, name_from, name_to)
}
\arguments{
\item{dst}{data set name}

\item{name_from}{column to get the new column name, values must be unique}

\item{name_to}{column to have the new name}
}
\value{
data with new column added
}
\description{
Add a column with column name from the name_from column
and values from name_to column
Note that the value of the names_from column must be unique
}
