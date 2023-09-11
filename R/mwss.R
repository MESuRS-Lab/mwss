#' Build model
#'
#' @description The function \code{mwss} build SimInf model usable either by \code{mwss::multisim} or \code{SimInf::run}.
#' Vectors ward_names, pop_size_P, pop_size_H, nVisits, LS must be in the same order
#'
#' @usage mwss(ward_names, pop_size_P, pop_size_H, nVisits, LS, gdata,
#'        matContact = NULL, IMMstate = NULL, EPIstate = NULL,
#'        SA = FALSE, nH_SA = NULL, tSim =  365, verbose = TRUE)
#'
#' @param ward_names String vector. Contains ward ids.
#' @param pop_size_P Numerical vector. Contains the number of beds per ward (ordered as ward_names).
#' @param pop_size_H Numerical vector. Contains the number of professionals affiliated to each ward (ordered as ward_names).
#' @param nVisits Numerical vector. Contains the average daily numbers of visits per ward (ordered as ward_names).
#' @param LS Numerical vector. Contains the average length of stay per ward (in days, ordered as ward_names).
#' @param gdata Named numeric vector or one-row data.frame describing global parameters. (see ? SimInf::mparse) Should be generated using mwss::build_gdata.
#' @param matContact Matrix of contact - for more details, see ? mwss::startvec.
#' @param IMMstate Data.frame describing the initial immunity levels of patients and healthcare workers - for more details, see ? mwss::startvec. Default is NULL (all individuals are considered non immunized).
#' @param EPIstate Data.frame describing the initial epidemiological states of patients and healthcare workers - for more details, see ? mwss::startvec.  Default is NULL (all individuals are considered susceptible).
#' @param SA Logical. Contact are restricted at the admission. Default is FALSE.
#' @param nH_SA Vector of number of healthcare workers in charge of admissions in the screening area (clinical examination/test).
#' @param tSim Integer. Number of simulated days (default is 365 days).
#' @param verbose Logical used to display or shut down warning messages (default is TRUE).
#'
#' @importFrom SimInf mparse
#' @importFrom magrittr subtract
#'
#' @return a SimInf_model object
#'
#' @examples
#' data("toydata")
#' list2env(toydata,envir=.GlobalEnv)
#' gdata <- build_gdata()
#'
#' ## Use the mwss function to create a 'SimInf_model' object that
#' ## expresses an complex compartmental model developed for structured healthcare systems
#'
#' model <- mwss(ward_names, pop_size_P, pop_size_H, nVisits, LS, gdata, tSim = 30)
#'
#' @export

mwss <- function(ward_names,
                 pop_size_P,
                 pop_size_H,
                 nVisits,
                 LS,
                 gdata,
                 matContact = NULL,
                 IMMstate = NULL,
                 EPIstate = NULL,
                 SA = FALSE,
                 nH_SA = NULL,
                 tSim =  365,
                 verbose = TRUE){

  # run startvec
  xstart <- startvec(ward_names = ward_names,
                     pop_size_P = pop_size_P,
                     pop_size_H = pop_size_H,
                     nVisits = nVisits,
                     LS = LS,
                     matContact = matContact,
                     IMMstate = IMMstate,
                     EPIstate = EPIstate,
                     SA = SA,
                     nH_SA = nH_SA,
                     verbose = verbose)

  ### Build u0
  u0 <- xstart$u0

  ### Build ldata
  # A numeric matrix with local data specific to each node. The column ldata[, j]
  # contains the local data vector for node j. The local data vector is passed as an argument
  # to the transition rate functions and the post time step function.

  ldata <- xstart$ldata

  v0 <- build_v0(u0, SA)

  pts_fun <- build_pts_fun(u0, SA, v0, gdata)

  transitions <- build_transitions(SA, gdata)

  compartments <- colnames(u0)

  E <- build_E(compartments, SA)

model <- mparse(
  transitions = transitions,
  compartments = compartments,
  ldata = ldata,
  gdata = gdata,
  u0 = u0,
  v0 = v0,
  tspan = seq(tSim),
  events = NULL,
  E = E,
  N = NULL,
  pts_fun = pts_fun
)

return(model)
}
