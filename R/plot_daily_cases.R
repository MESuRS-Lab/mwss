#' Plot the incidence
#'
#' @description The function \code{plot_daily_cases} returns FIX ME
#'
#' @param trajmwss List of data.table. Epidemiological trajectories simulated by the function \code{mwss::multisim}
#'
#' @import ggplot2
#' @importFrom data.table melt
#'
#' @return Incidence plot.
#'
#' @examples
#'
#' data("toydata")
#' list2env(toydata,envir=.GlobalEnv)
#' gdata <- build_gdata()
#'
#' model <- mwss(ward_names, pop_size_P, pop_size_H, nVisits, LS, gdata, tSim = 30)
#' results <- multisim(model, 5, ward_names)
#'
#' plot_daily_cases(trajmwss = results)
#'
#' @export

plot_daily_cases <- function(trajmwss) {
  # checks FIX Me

  n_it <- seq(length(trajmwss))

  # add iteration
  trajmwss <- lapply(n_it, function(sim) {
    trajmwss[[sim]][, `:=`(iteration = sim)]
    trajmwss[[sim]]
  })

  # group into unique data.table
  trajmwss %<>% do.call(rbind, .)

  #  cumulative daily incidence per node per iteration
  trajmwss[, `:=`(incP = (sum(incPA, incPM,  incPS)),
                  incH = (sum(incHA, incHM, incHS))),
           by = c("iteration", "time", "node")]

  #  daily incidence per node per iteration
  trajmwss[, `:=`(d_incP = diff(c(0,incP)),
                  d_incH = diff(c(0,incH))),
           by = c("node", "iteration")]
  trajmwss[, `:=`(casImpP = sum(admE, admEA, admES, admIA, admIM, admIS),
                  casImpH = infHout),
           by = c("iteration", "node","time")]

  my_incidence <- trajmwss

  my_incidence %<>% .[, c("time","node", "iteration", "casImpP", "casImpH", "infP", "infH"), with=FALSE]

  my_incidence[, `:=`(casImpP = c(0,diff(casImpP)),
                      casImpH = c(0,diff(casImpH)),
                      infP = c(0,diff(infP)),
                      infH = c(0,diff(infH))),
               by = c("node", "iteration")]

  my_incidence %<>% melt(., id.vars = c("time" , "node" , "iteration"))

  my_incidence[, `:=`(value = sum(value)),
               by = c("time", "iteration", "variable")]

  my_incidence %<>% .[, c("time", "variable", "value"), with=FALSE]

  my_incidence[, `:=`(mean = mean(value),
                      sd = sd(value)),
               by = c("time", "variable")]


  incidence_plot <- ggplot(my_incidence, aes(x=time, y=value, group=variable, color = variable, fill = variable)) +
    geom_smooth(span = 0.5) +
    geom_point(data = my_incidence[mean != 0], aes(x=time, y=mean, group=variable, color = variable)) +
    xlab("Time (day)") +
    ylab("Avearge daily number of cases") +
    scale_fill_manual(values =  c("#f6ec23", "#f68323", "#6495ED", "#CCCCFF"),
                      name = "",
                      limits = c("casImpP", "infP","casImpH", "infH"),
                      labels = c("Imported (patients)",
                                 "Nosocomial (patients)",
                                 "Imported (healthcare workers)",
                                 "Nosocomial (healthcare workers)"
                      )) +
    scale_colour_manual(values =  c("#f6ec23", "#f68323", "#6495ED", "#CCCCFF"),
                        name = "",
                        limits = c("casImpP", "infP","casImpH", "infH"),
                        labels = c("Imported (patients)",
                                   "Nosocomial (patients)",
                                   "Imported (healthcare workers)",
                                   "Nosocomial (healthcare workers)"
                        )
    )

  return(incidence_plot)
}
