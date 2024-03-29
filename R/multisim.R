#' Simulate multiple trajectories
#'
#' @description The function \code{multisim} run the model multiple times, save the results in a list of class 'mwss'.
#' Results are trajectories simulated by SimInf package. Epidemiological and immunity states of each ward are save at each time steps.
#' When run in shiny, a progress bar is also displayed during simulations.
#'
#' @usage multisim(model, nSim, ward_names)
#'
#' @param model SimInf model generated by the function \code{mwss}
#' @param nSim Number of simulations
#' @param ward_names String vector with ward names/id in the same order than the one used in the function \code{mwss} to build the model
#'
#' @return List of data.table of class 'mwss'. Additional ISO, SA,ScreenP, ScreenH, TestSympP and TestSympH classes are added depending on the prevention, surveillance and control measures implemented in the model.
#'
#' @importFrom SimInf run
#' @importFrom SimInf trajectory
#' @importFrom data.table as.data.table
#' @importFrom data.table ':='
#'
#' @examples
#' data("toydata")
#' list2env(toydata,envir=.GlobalEnv)
#' gdata <- build_gdata()
#' model <- mwss(ward_names, pop_size_P, pop_size_H, nVisits, LS, gdata, tSim = 30)
#' results <- multisim(model, 5, ward_names)
#'
#' @export

multisim <- function(model, nSim, ward_names){

        trajmwss <- lapply(1:nSim, function(x){
          result <- trajectory(run(model))
          result %<>% as.data.table
          result[, node := ward_names[node]]
        })


  class(trajmwss) <- c("mwss")
  if(model@gdata[["pISO"]] > 0) class(trajmwss) %<>% c(., "ISO")
  if(TRUE %in% (model@u0 %>% rownames %>% grepl("PSA",.))) class(trajmwss) %<>% c(., "SA")
  if(sum(model@gdata[c("ptestPWNI","ptestPWLI","ptestPWHI")]) > 0) class(trajmwss) %<>% c(., "ScreenP")
  if(sum(model@gdata[c("ptestHNI","ptestHLI","ptestHHI")]) > 0) class(trajmwss) %<>% c(., "ScreenH")
  if(model@gdata[c("ptestPWsymp")] > 0) class(trajmwss) %<>% c(., "TestSympP")
  if(model@gdata[c("ptestHsymp")] > 0) class(trajmwss) %<>% c(., "TestSympH")

  return(trajmwss)
  }
