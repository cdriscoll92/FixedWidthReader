\name{fwf_read}
\alias{fwf_read}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{fwf_read
%%  ~~function to do ... ~~
}
\description{
Efficiently reads in a fixed-width file (.txt, .dat, etc.) as a DataFrame. Optimal for large data sources (>100MB).
}
\usage{
fwf_read(file, widths, header = F, sep = '\t',
          varnames = NULL, rownames = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{Filepath to be opened. Supports all text files in machine-readable
  format.}
  \item{widths}{Width of columns}
  \item{header}{\code{TRUE} if first row of data contains header data. Overrides \code{varnames}.}
  \item{sep}{(optional) Separator of header row. Required if \code{header = TRUE.}}
  \item{varnames}{(optional) Names of columns (variable names) in data.}
  \item{rownames}{(optional) Names of rows in data.}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{DataFrame
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Colleen Driscoll,
Department of Government,
Harvard University
\email{cdriscoll@g.harvard.edu}
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{IO}% __ONLY ONE__ keyword per line
\keyword{data}% use one of  RShowDoc("KEYWORDS")
\keyword{internal}% __ONLY ONE__ keyword per line
\keyword{utilities}% __ONLY ONE__ keyword per line
