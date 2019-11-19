#' Estimate population size from two lists where the second list was created
#' during respondent-driven sampling
#'
#' Estimate the size of some population from two-source
#' (capture-and-recapture) data.  The first list is comprised of the
#' individuals who received some mark or identifying object or,
#' equivalently, the roster of individuals who received some service or
#' attended some event.  The second list (recapture data) is comprised of
#' the individuals who were recruited by respondent-driven sampling, and
#' the respondent-driven sampling weights must be available for each individual
#' on that list.
#'
#'  This function can also be used for "multipier-method" sampling, in
#'  which the multiplier is the fraction of the individuals included in
#'  the second list who are listed on some roster or service list.
#'
#' @param n.list1 the total number of individuals who received some mark or
#' identifying object, or the total number of individuals listed on some roster.
#' @param data a data frame containg at least two columns containing data on
#'  individuals who were recruited to a respondent-driven sampling survey.  Each
#' row represents one unique respondent.  One column is a logical (TRUE/FALSE) or
#' binary(1/0) indicator variable for receipt of the mark (unique object). The
#' other required column contains the individual respondent-driven sampling
#' weights.  All other columns are ignored.
#' @param list2 a character string which identifies the column name in the data
#' for the logical (TRUE/FALSE) or binary (1/0) indicator for whether each
#' respondent-driven survey respondent received the mark (unique object). A value
#' of FALSE or 0 in that data frame column indicates that the respondent did not
#'  receive the mark (unique object) or, equivalently, was not listed on a
#'  service roster, and a value of TRUE or 1 indicates receipt of the mark or
#'  inclusion in the roster.
#' @param weight a charater string which identifies the column name in data for
#' the numeric respondent-driven survey weights.
#' @param bootreps a numeric value for the number of bootstrap replications
#' required for estimation of confidence intervals.
#' @param conf.level a numeric value in the open interval (0, 1) for the desired
#' confidence level.
#' @param include.naive a logical value indicating whether a naive (unweighted)
#' estimate of populations size is computed.  Unweighted estimates are provided
#' for comparison only, and should not be used for estimation from
#' respondent-driven sampling.
#' @param seed a numeric seed for the pseudo-random number generator used for
#' bootstrap replication. Defaults to system time. Set to a particular value to
#' generate repeatable replications.
#'
#' @return A data frame containing seven columns:
#' \describe{
#' \item{\code{Type}}{RDS-weighted or naive estimate}
#' \item{\code{Estimate}}{Point estimate of population size}
#' \item{\code{conf.level}}{Confidence level}
#' \item{\code{lower}}{Lower confidence limit}
#' \item{\code{upper}}{Upper confidence limit}
#' \item{\code{CI_type}}{Confidence interval type}
#' \item{\code{reps}}{Number of bootstrap replicates}
#' }
#'
#' @references Berchenko Y, Frost SDW. Capture-recapture methods and
#'  respondent-driven sampling; their potential and limitations. Sexually
#'  Transmitted Infections 2011; 87(4):267-268.
#'
#' @examples
#' data(FSW)
#' help(FSW)
#' ## Estimate the number of female sex workers
#' RDS_2S_PSE(1000, data = FSW, list2 = "I.object", weight = "Weight")
#' ## Unweighted estimates are not appropriate for data from respondent-
#' ## driven sampling, but suppose you want to compare to see the effect
#' ## of weighting
#' RDS_2S_PSE(1000, data = FSW, list2 = "I.object", weight = "Weight",
#'            include.naive = TRUE)
#'
#' @import boot
#' @importFrom boot boot
#' @importFrom boot boot.ci
#' @importFrom stats complete.cases
#' @importFrom utils make.packages.html
#' @export
RDS_2S_PSE <-
function(n.list1 = NULL, data = NULL, list2 = NULL, weight = NULL,
             bootreps = 2000, conf.level = 0.95, include.naive = FALSE,
             seed = Sys.time()){
    if(!"data.frame" %in% class(data)) stop("data not a data.frame object")
    if(include.naive) warning("Unweighted estimates are not trustworthy!",
                              call. = FALSE)
    mat <- cbind(data[[list2]], data[[weight]])
    cc <- stats::complete.cases(mat)
    if(!all(cc)){
        data <- data[cc, ]
        warning("Missing values were present and have been removed.",
                call. = FALSE)
        }
    if(!all(data[[list2]] %in% c(0,1)) & !all(is.logical(data[[list2]]))){
        stop("data$list2 is not a logical (TRUE/FALSE) or binary (0/1) vector")
        }
    data[[list2]] <- as.integer(data[[list2]])
    if(any(data[[weight]] <= 0)) stop("All weights must be non-negative")
    n_1. <- n.list1
    I_11 <- data[[list2]]
    n_.1 <- length(I_11)
    n_11 <- sum(I_11)
    I_01 <- rep(1L, n_.1) - I_11
    w <- data[[weight]]
    n_10 <- n_1. - n_11
    counts <- data.frame(n_1. = n_1., n_.1 = n_.1, n_11 = n_11)
    rdsdat <- data.frame(I_01 = I_01, I_11 = I_11, w = w)
    N.naive <- function(data){
        Nhat <- data$n_1. * data$n_.1 / data$n_11
        V <- data$n_1. * data$n_.1 * (data$n_1. - data$n_11) *
            (data$n_.1 - data$n_11) / data$n_11^3
        Z <- 1 - (1 - conf.level) / 2
        xx <- Z * V^0.5
        result <- data.frame(Type = "Unweighted", Estimate = round(Nhat),
                             conf.level = conf.level,
                             lower = round(Nhat - xx),
                             upper = round(Nhat + xx),
                             CI_type = "Asymptotic",
                             reps = NA)
        result
    }
    N.rds <- function(data, indices, n_10 = n_10){
        d <- data[indices, ]
        sum.01 <- sum(d$I_01 / d$w)
        sum.11 <- sum(d$I_11 / d$w)
        n_11 <- sum(d$I_11)
        result <- (n_10 + n_11) * (sum.01 + sum.11) / sum.11
        result
    }
    set.seed(seed)
    boot.out <- boot::boot(data = rdsdat, statistic = N.rds, R = bootreps, n_10 = n_10)
    rds <- boot::boot.ci(boot.out,type = "perc", conf = conf.level)
    result <- data.frame(Type = "RDS weighted", Estimate = round(rds$t0),
                                      conf.level = conf.level,
                                      lower = round(rds$percent[, 4]),
                                      upper = round(rds$percent[, 5]),
                         CI_type = "Bootstrap percentile",
                         reps = bootreps)
    if(include.naive) result <- rbind(N.naive(counts), result)
    result
}
