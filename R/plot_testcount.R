#' Plot average daily number of test
#'
#' @description \code{plot_testcount} returns an plot representing the average daily number of test performed
#' over the simulations at different scales and for different populations. The daily number of test is represented
#' by a barplot up to 30 time steps, then they are represented by dots and lines.
#'
#' @usage plot_testcount(trajmwss, scale = 0, pop = NULL, iter = FALSE, ward = FALSE, daysint = 1)
#'
#' @param trajmwss List of data.table. Epidemiological trajectories simulated by the function \code{mwss::multisim}
#' @param scale Integer. Indicated the scale for statistics: 0/ whole facility, 1/ wards.
#' @param pop String. Define a subpopulation for statistics ("P": Test performed on patients, "H": Test performed on professionals ; default is FALSE: considering all tests).
#' @param iter Integer. Define a specific simulation. Default is FALSE, providing average values over all simulations.
#' @param ward String. Define a specific ward. Default is FALSE, considering all wards. At the facility scale '0' no ward can be specified.
#' @param daysint Integer. Indicated time step use to calculate average daily number of test (default is 1 day).
#'
#' @importFrom data.table copy
#' @importFrom data.table ':='
#'
#' @return Test count plot
#'
#' @export

plot_testcount <- function(trajmwss,
                      scale = 0,
                      pop = NULL,
                      iter = FALSE,
                      ward = FALSE,
                      daysint = 1) {

  if (!isFALSE(ward) & scale == 0) {
    scale = 1
  }

  ntest <- keyoutput(trajmwss,
                   scale = scale,
                   focus = "test")$ndtest

  if (scale == 1 & ward %in% trajmwss[[1]]$node)
    ntest %<>% .[node == ward]

  if (is.numeric(iter))
    ntest %<>% .[iteration == iter]

  if (is.null(pop))
    ntest[, ndtest := n_dtestP + n_dtestH]
  else
    if (pop == "P")
      setnames(ntest, "n_dtestP", "ndtest")
  else
    if (pop == "H")
      setnames(ntest, "n_dtestH", "ndtest")


  if (scale == 1)
    ntest %<>% .[, c("iteration", "node", "time", "ndtest")]
  else
    ntest %<>% .[, c("iteration", "time", "ndtest")]

  ntest[, dint := cut(
    time,
    c(seq(
      min(ntest$time),
      max(ntest$time),
      daysint
    ), max(ntest$time)) %>% unique,
    include.lowest = T,
    right = F,
    ordered_result = T
  )]

  levels(ntest$dint) <- ntest$dint %>% nlevels %>% seq

  if (length(unique(ntest$dint)) >= 30)
    if (scale == 1)
      ntest[, avntest := mean(ndtest), by = c("dint", "node")]
  else
    ntest[, avntest := mean(ndtest), by = c("dint")]

  if (is.null(pop))
    ylabpop <- "both patients and professionals"
  else
    if (pop == "P")
      ylabpop <- "patients"
  else
    if (pop == "H")
      ylabpop <- "professionals"

  if (daysint > 1)
    ylabel <-
    paste0("Daily number of tests of ",
           ylabpop,
           " over ",
           daysint,
           "-days periods")
  else
    ylabel <- paste0("Daily number of tests of ", ylabpop)

  if (scale == 1) {
    if (daysint > 1 & length(unique(ntest$dint)) < 30)
      p <-
        ggplot(ntest, aes(
          x = dint,
          y = ndtest,
          fill = node %>% as.factor
        )) +
        geom_boxplot(
          outlier.colour = "red",
          outlier.shape = 8,
          outlier.size = 2,
          notch = FALSE
        ) +
        facet_grid(node %>% as.factor ~ .) +
        xlab(paste0("Time steps (", daysint, " days)")) +
        ylab(ylabel) +
        theme(legend.position = "none")

    if (daysint > 1 & length(unique(ntest$dint)) >= 30) {
      p <-
        ggplot(
          ntest,
          aes(
            x = dint  %>% as.numeric,
            y = avntest,
            group = node %>% as.factor,
            colour = node %>% as.factor
          )
        ) +
        geom_line() + geom_point() +
        facet_grid(node %>% as.factor ~ .) +
        xlab(paste0("Time steps (", daysint, " days)")) +
        ylab(ylabel) +
        theme(legend.position = "none")
    }

    #### do not aggregate periods

    if (daysint == 1 & length(unique(ntest$dint)) < 30)
      p <-
        ggplot(ntest, aes(
          x = dint,
          y = ndtest,
          fill = node %>% as.factor
        )) +
        geom_boxplot(
          outlier.colour = "red",
          outlier.shape = 8,
          outlier.size = 2,
          notch = FALSE
        ) +
        facet_grid(node %>% as.factor ~ .) +
        xlab("Time (day)") +
        ylab(ylabel) +
        theme(legend.position = "none")

    #### use geomline when exceeding 30 time steps

    if (daysint == 1 & length(unique(ntest$dint)) >= 30) {
      p <-
        ggplot(
          ntest,
          aes(
            x = dint %>% as.numeric,
            y = avntest,
            group = node %>% as.factor,
            colour = node %>% as.factor
          )
        ) +
        geom_line() + geom_point() +
        facet_grid(node %>% as.factor ~ .) +
        xlab("Time (day)") +
        ylab(ylabel) +
        theme(legend.position = "none")
    }

  } else {
    if (daysint > 1 & length(unique(ntest$dint)) < 30)
      p <-  ggplot(ntest, aes(x = dint, y = ndtest)) +
        geom_boxplot(
          outlier.colour = "red",
          outlier.shape = 8,
          outlier.size = 2,
          notch = FALSE
        ) +
        xlab(paste0("Time steps (", daysint, " days)")) +
        ylab(ylabel) +
        theme(legend.position = "none")

    if (daysint > 1 & length(unique(ntest$dint)) >= 30) {
      p <-  ggplot(ntest, aes(x = dint %>% as.numeric, y = avntest)) +
        geom_line() + geom_point() +
        xlab(paste0("Time steps (", daysint, " days)")) +
        ylab(ylabel) +
        theme(legend.position = "none")
    }

    #### do not aggregate periods

    if (daysint == 1 & length(unique(ntest$dint)) < 30)
      p <-  ggplot(ntest, aes(x = dint, y = ndtest)) +
        geom_boxplot(
          outlier.colour = "red",
          outlier.shape = 8,
          outlier.size = 2,
          notch = FALSE
        ) +
        xlab("Time (day)") +
        ylab(ylabel) +
        theme(legend.position = "none")

    #### use geomline when exceeding 30 time steps

    if (daysint == 1 & length(unique(ntest$dint)) >= 30) {
      p <-  ggplot(ntest, aes(x = dint %>% as.numeric, y = avntest)) +
        geom_line() + geom_point() +
        xlab("Time (day)") +
        ylab(ylabel) +
        theme(legend.position = "none")
    }
  }

  print(p)
  return(p)

}
