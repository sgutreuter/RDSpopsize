\name{RDS_2S_PSE}
\alias{RDS_2S_PSE}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
RDS_2S_PSE(n.list1 = NULL, data = NULL, list2 = NULL, weight = NULL, bootreps = 2000, conf.level = 0.95, include.naive = FALSE, seed = Sys.time())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n.list1}{
%%     ~~Describe \code{n.list1} here~~
}
  \item{data}{
%%     ~~Describe \code{data} here~~
}
  \item{list2}{
%%     ~~Describe \code{list2} here~~
}
  \item{weight}{
%%     ~~Describe \code{weight} here~~
}
  \item{bootreps}{
%%     ~~Describe \code{bootreps} here~~
}
  \item{conf.level}{
%%     ~~Describe \code{conf.level} here~~
}
  \item{include.naive}{
%%     ~~Describe \code{include.naive} here~~
}
  \item{seed}{
%%     ~~Describe \code{seed} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (n.list1 = NULL, data = NULL, list2 = NULL, weight = NULL, 
    bootreps = 2000, conf.level = 0.95, include.naive = FALSE, 
    seed = Sys.time()) 
{
    if (!"data.frame" \%in\% class(data)) 
        stop("data not a data.frame object")
    if (include.naive) 
        warning("Unweighted estimates are not trustworthy!", 
            call. = FALSE)
    mat <- cbind(data[[list2]], data[[weight]])
    cc <- complete.cases(mat)
    if (!all(cc)) {
        data <- data[cc, ]
        warning("Missing values were present and have been removed.", 
            call. = FALSE)
    }
    if (!all(data[[list2]] \%in\% c(0, 1)) & !all(is.logical(data[[list2]]))) {
        stop("data$list2 is not a logical (TRUE/FALSE) or binary (0/1) vector")
    }
    data[[list2]] <- as.integer(data[[list2]])
    if (any(data[[weight]] <= 0)) 
        stop("All weights must be non-negative")
    n_1. <- n.list1
    I_11 <- data[[list2]]
    n_.1 <- length(I_11)
    n_11 <- sum(I_11)
    I_01 <- rep(1L, n_.1) - I_11
    w <- data[[weight]]
    n_10 <- n_1. - n_11
    counts <- data.frame(n_1. = n_1., n_.1 = n_.1, n_11 = n_11)
    rdsdat <- data.frame(I_01 = I_01, I_11 = I_11, w = w)
    N.naive <- function(data) {
        Nhat <- data$n_1. * data$n_.1/data$n_11
        V <- data$n_1. * data$n_.1 * (data$n_1. - data$n_11) * 
            (data$n_.1 - data$n_11)/data$n_11^3
        Z <- 1 - (1 - conf.level)/2
        xx <- Z * V^0.5
        result <- data.frame(Type = "Unweighted", Estimate = round(Nhat), 
            conf.level = conf.level, lower = round(Nhat - xx), 
            upper = round(Nhat + xx), CI_type = "Asymptotic", 
            reps = NA)
        result
    }
    N.rds <- function(data, indices, n_10 = n_10) {
        d <- data[indices, ]
        sum.01 <- sum(d$I_01/d$w)
        sum.11 <- sum(d$I_11/d$w)
        n_11 <- sum(d$I_11)
        result <- (n_10 + n_11) * (sum.01 + sum.11)/sum.11
        result
    }
    set.seed(seed)
    boot.out <- boot(data = rdsdat, statistic = N.rds, R = bootreps, 
        n_10 = n_10)
    rds <- boot.ci(boot.out, type = "perc", conf = conf.level)
    result <- data.frame(Type = "RDS weighted", Estimate = round(rds$t0), 
        conf.level = conf.level, lower = round(rds$percent[, 
            4]), upper = round(rds$percent[, 5]), CI_type = "Bootstrap percentile", 
        reps = bootreps)
    if (include.naive) 
        result <- rbind(N.naive(counts), result)
    result
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
