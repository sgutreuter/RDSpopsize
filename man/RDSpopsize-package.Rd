\name{RDSpopsize-package}
\alias{RDSpopsize-package}
\alias{RDSpopsize}
\docType{package}
\title{
\packageTitle{RDSpopsize}
}
\description{
\packageDescription{RDSpopsize}
}
\details{

The DESCRIPTION file:
\packageDESCRIPTION{RDSpopsize}
\packageIndices{RDSpopsize}

}
\author{
\packageAuthor{RDSpopsize}

Maintainer: \packageMaintainer{RDSpopsize}
}
\references{
Berchenko Y, Frost SDW (2011) Capture-recapture methods and
respondent-drive sampling; their potential and limitations. Sexually
Transmitted Infections 87(4):267-268.

}

\keyword{ package }
\seealso{
~~ Optional links to other man pages, e.g. ~~
~~ \code{\link[<pkg>:<pkg>-package]{<pkg>}} ~~
}
\examples{

data(FSW)
## Estimate the number of female sex workers
result <- RDS_2S_PSE(1000, data = FSW, list2 = "I.object", weight = "Weight")
print(result)

}
