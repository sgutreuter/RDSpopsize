\name{RDS_2S_PSE}
\alias{RDS_2S_PSE}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Function to produce estimates of population size from
  respondent-driven sampling.
}
\description{
  Estimate the size of some population from two-source
  (mark-and-recapture) data.  The first list is comprised of the
  individuals who received some mark or identifying object or,
  equivalently, the roster of individuals who received some service or
  attended some event.  The second list (recapture data) is comprised of
  the individuals who were recruited by respondent-driven sampling, and
  the respondent-driven sampling weights must be available for each individual on that list.

  This function can also be used for "multipier-method" sampling, in
  which the multiplier is the fraction of the individuals included in
  the second list who are listed on some roster or service list.
}
\usage{
RDS_2S_PSE(n.list1 = NULL, data = NULL, list2 = NULL, weight = NULL, bootreps = 2000, conf.level = 0.95, include.naive = FALSE)
}
\arguments{
  \item{n.list1}{The total number of individuals who received some mark or
  identifying object, or the total number of individuals listed on some roster.
}
  \item{data}{A data frame containg at least two columns containing data on
  individuals who were recruited to a respondent-driven sampling survey.  Each row represents
  one unique respondent.  One column is a binary (0,1) indicator
  variable for receipt of the mark (unique object).
  The other required column contains the individual respondent-driven sampling weights.  All
  other columns are ignored.
}
  \item{list2}{A character string which identifies the column name in the data for the binary (0,1)
  indicator for whether each respondent-driven survey respondent received the mark (unique
  object). A value of 0 in that data frame column indicates that the respondent did not
  receive the mark (unique object) or, equivalently, was not listed on a
  service roster, and a value of 1 indicates receipt of the mark or
  inclusion in the roster.

}
  \item{weight}{A charater string which identifies the column name in data for the
  numeric respondent-driven survey weights.
}
  \item{bootreps}{A numeric value for the number of bootstrap replications required for
  estimation of confidence intervals.
}
  \item{conf.level}{A numeric value in the open interval (0, 1) for the desired confidence level.
}
  \item{include.naive}{A logical value indicating whether a naive (unweighted) estimate of
  populations size is computed.  Unweighted estimates are provided for
  comparison only, and should not be used for estimation from respondent-driven sampling.
}
  \item{seed}{RNG seed for bootstrap replication. Defaults to system
  time. Set to a particular value to generate repeatable replications.
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}

\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
A data frame containing 7 columns.  The columns are:
\item{Type}{RDS-weighted or naive estimate}
\item{Estimate}{Point estimate of population size}
\item{conf.level}{Confidence level}
\item{lower}{Lower confidence limit}
\item{uower}{Upper confidence limit}
\item{CI_type}{Confidence interval type}
\item{reps}{Number of bootstrap replicates}

}
\references{
  %% ~put references to the literature/web site here ~
  Berchenko Y, Frost SDW. (2011) Capture-recapture methods and
  respondent-driven sampling; their potential and limitations. Sexually
  Transmitted Infections 87(4):267-268.
}
\author{
  %%  ~~who you are~~
  Steve Gutreuter
}
%%\note{
%%  ~~further notes~~
%%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%%}
\examples{
require(boot)

help(FSW)

## Inspect the first 10 rows of survey responses
FSW[1:10, ]

## Estimate the number of female sex workers
RDS_2S_PSE(1000, data = FSW, list2 = "I.object", weight = "Weight")

## The same as above, but controlling the RNG seed to produce repeatable
## bootstrap replications
RDS_2S_PSE(1000, data = FSW, list2 = "I.object", weight = "Weight",
           seed = 12345)

## The same but with binary (0,1) responses to the respondent-driven
## sampling survey
FSW01[1:10, ]
RDS_2S_PSE(1000, data = FSW01, list2 = "I.object", weight = "Weight",
           seed = 12345)

## Unweighted estimates are not appropriate for data from respondent-
## driven sampling, but suppose you want to compare to see the effect
## of weighting
RDS_2S_PSE(1000, data = FSW, list2 = "I.object", weight = "Weight",
           include.naive = TRUE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
