RDS_2S_PSE <-
function(n.list1 = NULL, data = NULL, list2 = NULL, weight = NULL,
             bootreps = 2000, conf.level = 0.95, include.naive = FALSE,
             seed = Sys.time()){
    if(!"data.frame" %in% class(data)) stop("data not a data.frame object")
    if(include.naive) warning("Unweighted estimates are not trustworthy!",
                              call. = FALSE)
    mat <- cbind(data[[list2]], data[[weight]])
    cc <- complete.cases(mat)
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
    boot.out <- boot(data = rdsdat, statistic = N.rds, R = bootreps, n_10 = n_10)
    rds <- boot.ci(boot.out,type = "perc", conf = conf.level)
    result <- data.frame(Type = "RDS weighted", Estimate = round(rds$t0),
                                      conf.level = conf.level,
                                      lower = round(rds$percent[, 4]),
                                      upper = round(rds$percent[, 5]),
                         CI_type = "Bootstrap percentile",
                         reps = bootreps)
    if(include.naive) result <- rbind(N.naive(counts), result)
    result
}
