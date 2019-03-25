\name{ecycle}
\alias{ecycle}
\title{Frame Work for Handling Errors in a Batch Process}
\usage{
ecycle(code, ehandle, max_try, thandle, ecorrect, cond = TRUE)
}
\description{
See arguments.
}
\arguments{

\item{code}{The code block to run}

\item{ehandle}{The code block to run when running \code{code} fails}

\item{max_try}{The number of attempts to run \code{code} before claiming that \code{code} fails}

\item{thandle}{(Optional) The code block to run when running \code{code} succeeds}

\item{ecorrect}{(Optional) The code block to run after running \code{code} doesn't succeed and before running \code{code} again}

\item{cond}{(Optional) Additional conditions for deciding that running \code{code} succeeds.}
}
\value{
If running \code{code} fails, the result of \code{ehandle}.

If running \code{code} succeeds and \code{thandle} is missing, the result of \code{code}.

If running \code{code} succeeds and \code{thandle} isn't missing, the result of \code{thandle}.
}
\examples{
ecycle({a <-1; print(a+b); b <- 2}, print('failed'), 3, print('succeeded'), b <- 3)
print(b)
}
