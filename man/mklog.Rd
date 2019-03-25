\name{mklog}
\alias{mklog}
\title{Making Formatted Log Files}
\usage{
mklog(x, path, sep = '\t')
}
\description{
See arguments.
}
\arguments{

\item{x}{Content of a log record.}

\item{path}{The path of the log file, if '', the log record is sent to standard output.}

\item{sep}{The seperator in a record.}
}
\value{
None (invisible NULL)
}
\examples{
mklog('test log', '')
}
