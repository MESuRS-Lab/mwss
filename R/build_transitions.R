#' Write transitions
#'
#' @description Internal function used by \code{mwss} function to build the string vector describing transitions and required by SimInf package
#'
#' @usage build_transitions(SA)
#'
#' @return String vector describing transitions
#'
#' @keywords internal
#' @noRd


build_transitions <- function(SA, gdata){

  ### Check

  if(!is.logical(SA)) stop("SA must be logical - see build transition function")

  ### Write transitions

  ## Number of individuals in subpopulations
  # PSA: patients in the screening area
  # PW: patients in the ward
  # H: healthcare workers/professionals
  # The prefix inf only count infectious individuals

  nPSA <-
    paste0("(",
           paste(
             c(sapply(
               paste("PSA", c("S", "E", "EA", "ES", "IA", "IM", "IS"), sep = "_"), function(pref)
                 paste(pref, c("NI", "LI", "HI"), sep = "_")
             )),
             collapse = " + "),
           ")")

  infPSA <-
    paste0("(",
           paste(paste(c(
             sapply(paste("PSA", c(
               "EA", "ES", "IA", "IM", "IS"
             ), sep = "_"), function(pref)
               paste(pref, c("NI", "LI", "HI"), sep = "_"))
           ), paste(
             "r", c(sapply(c("EA", "ES", "IA", "IM", "IS"), function(epi)
               rep(epi, 3))),
             sep = ""
           ),
           sep = " * "),
           collapse = " + "),
           ")")
  nPW <-
    paste0("(",
           paste(
             c(sapply(
               paste("PW", c("S", "E", "EA", "ES", "IA", "IM", "IS"), sep = "_"), function(pref)
                 paste(pref, c("NI", "LI", "HI"), sep = "_")
             )),
             collapse = " + ")," + ",
           paste(
             c(sapply(
               c(sapply(
                 paste("PW", c("S", "E", "EA", "ES", "IA", "IM", "IS"), sep = "_"), function(pref)
                   paste(pref, c("NI", "LI", "HI"), sep = "_")
               )), function(pref)
                 paste(pref, "T", sep = "_")
             )),
             collapse = " + "),
           ")")

  nPWctc <-
    paste0("((",
           paste(
             c(sapply(
               paste("PW", c("S", "E", "EA", "ES", "IA", "IM", "IS"), sep = "_"), function(pref)
                 paste(pref, c("NI", "LI", "HI"), sep = "_")
             )),
             collapse = " + "),
           ") + (1 - pISO) * (",
           paste(
             c(sapply(
               c(sapply(
                 paste("PW", c("S", "E", "EA", "ES", "IA", "IM", "IS"), sep = "_"), function(pref)
                   paste(pref, c("NI", "LI", "HI"), sep = "_")
               )), function(pref)
                 paste(pref, "T", sep = "_")
             )),
             collapse = " + "),
           "))")

  infPW <-
    paste0("((",
           paste(paste(c(
             sapply(paste("PW", c(
               "EA", "ES", "IA", "IM", "IS"
             ), sep = "_"), function(pref)
               paste(pref, c("NI", "LI", "HI"), sep = "_"))
           ), paste(
             "r", c(sapply(c("EA", "ES", "IA", "IM", "IS"), function(epi)
               rep(epi, 3))),
             sep = ""
           ),
           sep = " * "),
           collapse = " + "),
           ") + (1 - pISO) * (",
           paste(paste(c(
             sapply(c(sapply(paste("PW", c("EA", "ES", "IA", "IM", "IS"), sep = "_"), function(pref)
               paste(pref, c("NI", "LI", "HI"), sep = "_"))), function(pref)
                 paste(pref, "T", sep = "_"))
           ), paste(
             "r", c(sapply(c("EA", "ES", "IA", "IM", "IS"), function(epi)
               rep(epi, 3))),
             sep = ""
           ),
           sep = " * "),
           collapse = " + "),
           "))")


  nH <-
    "(H_S_NI + H_S_NI_T + H_S_LI + H_S_LI_T + H_S_HI + H_S_HI_T + H_E_NI + H_E_NI_T + H_E_LI + H_E_LI_T + H_E_HI + H_E_HI_T + H_EA_NI + H_EA_NI_T + H_EA_LI + H_EA_LI_T + H_EA_HI + H_EA_HI_T + H_ES_NI + H_ES_NI_T + H_ES_LI + H_ES_LI_T + H_ES_HI + H_ES_HI_T + H_IA_NI + H_IA_NI_T + H_IA_LI + H_IA_LI_T + H_IA_HI + H_IA_HI_T + H_IM_NI + H_IM_NI_T + H_IM_LI + H_IM_LI_T + H_IM_HI + H_IM_HI_T + H_IS_NI + H_IS_NI_T + H_IS_LI + H_IS_LI_T + H_IS_HI + H_IS_HI_T)"

  infH <-
    paste0("(",
           paste(paste(c(
             sapply(paste("H", c(
               "EA", "ES", "IA", "IM", "IS"
             ), sep = "_"), function(pref)
               paste(pref, c("NI", "LI", "HI"), sep = "_"))
           ), paste(
             "r", c(sapply(c("EA", "ES", "IA", "IM", "IS"), function(epi)
               rep(epi, 3))),
             sep = ""
           ),
           sep = " * "),
           collapse = " + "),
           " + ",
           paste(paste(c(
             sapply(c(sapply(paste("H", c("EA", "ES", "IA", "IM", "IS"), sep = "_"), function(pref)
               paste(pref, c("NI", "LI", "HI"), sep = "_"))), function(pref)
                 paste(pref, "T", sep = "_"))
           ), paste(
             "r", c(sapply(c("EA", "ES", "IA", "IM", "IS"), function(epi)
               rep(epi, 3))),
             sep = ""
           ),
           sep = " * "),
           collapse = " + "),
           ")")

  ## Conditions to activate transitions
  # Is there any available bed?
  if(SA)
    SPACE <- paste0("nP - (", nPSA," + ", nPW," + IC) > 0") else
      SPACE <- paste0("nP - (", nPW," + IC) > 0")

  # Is the subpopulations empty?
  PSAEMPTY <- paste0(nPSA," < 1")
  PWEMPTY <- paste0(nPW," < 1")
  HEMPTY <- paste0(nH," < 1")
  PEMPTY <- paste0("(", nPSA," + ", nPW, ") < 1")

  ## Forces of infection

  # Continuous weighted proportions of infected individuals in subpopulations from v0 updated by pts_fun:
  # wpropInfHorig: proportion of infected professionals among professionals of all wards weighted by the time spent in a given ward
  # wpropInfHdest: proportion of infected professionals among professionals of all wards weighted by the time spent by professionals of a given ward in each ward
  # wpropInfPSAdest: proportion of infected patients among screening areas of all wards weighted by the time spent by professionals of a given ward in each ward
  # wpropInfPWdest: proportion of infected patients among wards of all wards weighted by the time spent by professionals of a given ward in each ward

  # https://doi.org/10.1093/cid/ciaa682: Laura Temime, Marie-Paule Gustin, Audrey Duval, Niccolò Buetti, Pascal Crépey, Didier Guillemot, Rodolphe Thiébaut, Philippe Vanhems, Jean-Ralph Zahar, David R M Smith, Lulla Opatowski, A Conceptual Discussion About the Basic Reproduction Number of Severe Acute Respiratory Syndrome Coronavirus 2 in Healthcare Settings, Clinical Infectious Diseases, Volume 72, Issue 1, 1 January 2021, Pages 141–143, https://doi.org/10.1093/cid/ciaa682

  # Infection of patients in the screening area
  lambdaPSA <-
    paste("pconta * (", # probability of transmission for one day of contact *
          "(1 - epsPPSA) * ctcPPSA * (", infPSA, "/", nPSA, #  1-infection control ratio * proportion of one day in contacts with patients * proportion of infected patients in the screening area of a given ward
          ") + (1 - epsHPSA) * ctcHPSA * wpropInfHorig)") # 1-infection control ratio * proportion of one day in contacts with professionals * proportion of infected professionals working in the screening area of a given ward

  # Infection of patients in the ward
  lambdaPW <-
    paste("pconta * (", # probability of transmission for one day of contact *
          "(1 - epsPPW) * ctcPPW * (", infPW, "/", nPWctc,#  1-infection control ratio * proportion of one day in contacts with patients * proportion of infected patients in the given ward
          ") + (1 - epsHPW) * ctcHPW * wpropInfHorig +",  #  1-infection control ratio * proportion of one day in contacts with professionals *  proportion of infected professionals working in the given ward
          "(1 - epsVPW) * ctcV * nV * (prev * (1-rtE)))") #  1-infection control ratio * average daily duration of contact with visitors * daily number of visitor per patients * proportion of infectious in the community (prevalence minus non infectious exposed)

  transitions <-
    ## Patients
    c(####
      #### Ward
      ####
      ### Tests (screening and diagnostic testing)
      ## No Immunity
      # tested with negative test result
      "PW_S_NI -> PW_S_NI * ptestPWNI * ( 1 / (tbtwtestP + ttestPW)) * (speW) -> PW_S_NI + nTestP",
      "PW_E_NI -> PW_E_NI * ptestPWNI * ( 1 /  (tbtwtestP + ttestPW)) * (1 - senW) -> PW_E_NI + nTestP",
      "PW_EA_NI -> PW_EA_NI * ptestPWNI * ( 1 /  (tbtwtestP + ttestPW)) * (1 - senW) -> PW_EA_NI + nTestP",
      "PW_ES_NI -> PW_ES_NI * ptestPWNI * ( 1 /  (tbtwtestP + ttestPW)) * (1 - senW) -> PW_ES_NI + nTestP",
      "PW_IA_NI -> PW_IA_NI * ptestPWNI * ( 1 /  (tbtwtestP + ttestPW)) * (1 - senW) -> PW_IA_NI + nTestP",
      "PW_IM_NI -> PW_IM_NI * ptestPWsymp * ( 1 / (tbeftestPsymp + ttestsymp)) * (1 - sensymp) -> PW_IM_NI + nTestP",
      "PW_IS_NI -> PW_IS_NI * ptestPWsymp * ( 1 / (tbeftestPsymp + ttestsymp)) * (1 - sensymp) -> PW_IS_NI + nTestP",
      # tested with positive test result
      "PW_S_NI -> PW_S_NI * ptestPWNI * ( 1 / (tbtwtestP + ttestPW)) * (1 - speW) -> PW_S_NI_T + nTestP",
      "PW_E_NI -> PW_E_NI * ptestPWNI * ( 1 /  (tbtwtestP + ttestPW)) * senW -> PW_E_NI_T + nTestP",
      "PW_EA_NI -> PW_EA_NI * ptestPWNI * ( 1 /  (tbtwtestP + ttestPW)) * senW -> PW_EA_NI_T + nTestP",
      "PW_ES_NI -> PW_ES_NI * ptestPWNI * ( 1 /  (tbtwtestP + ttestPW)) * senW -> PW_ES_NI_T + nTestP",
      "PW_IA_NI -> PW_IA_NI * ptestPWNI * ( 1 /  (tbtwtestP + ttestPW)) * senW -> PW_IA_NI_T + nTestP",
      "PW_IM_NI -> PW_IM_NI * ptestPWsymp * ( 1 / (tbeftestPsymp + ttestsymp)) * sensymp -> PW_IM_NI_T + nTestP",
      "PW_IS_NI -> PW_IS_NI * ptestPWsymp * ( 1 / (tbeftestPsymp + ttestsymp)) * sensymp -> PW_IS_NI_T + nTestP",
      ## Low immunity
      # tested with negative test result
      "PW_S_LI -> PW_S_LI * ptestPWLI * ( 1 / (tbtwtestP + ttestPW)) * (speW) -> PW_S_LI + nTestP",
      "PW_E_LI -> PW_E_LI * ptestPWLI * ( 1 /  (tbtwtestP + ttestPW)) * (1 - senW) -> PW_E_LI + nTestP",
      "PW_EA_LI -> PW_EA_LI * ptestPWLI * ( 1 /  (tbtwtestP + ttestPW)) * (1 - senW) -> PW_EA_LI + nTestP",
      "PW_ES_LI -> PW_ES_LI * ptestPWLI * ( 1 /  (tbtwtestP + ttestPW)) * (1 - senW) -> PW_ES_LI + nTestP",
      "PW_IA_LI -> PW_IA_LI * ptestPWLI * ( 1 /  (tbtwtestP + ttestPW)) * (1 - senW) -> PW_IA_LI + nTestP",
      "PW_IM_LI -> PW_IM_LI * ptestPWsymp * ( 1 / (tbeftestPsymp + ttestsymp)) * (1 - sensymp) -> PW_IM_LI + nTestP",
      "PW_IS_LI -> PW_IS_LI * ptestPWsymp * ( 1 / (tbeftestPsymp + ttestsymp)) * (1 - sensymp) -> PW_IS_LI + nTestP",
      # tested with positive test result
      "PW_S_LI -> PW_S_LI * ptestPWLI * ( 1 / (tbtwtestP + ttestPW)) * (1 - speW) -> PW_S_LI_T + nTestP",
      "PW_E_LI -> PW_E_LI * ptestPWLI * ( 1 /  (tbtwtestP + ttestPW)) * senW -> PW_E_LI_T + nTestP",
      "PW_EA_LI -> PW_EA_LI * ptestPWLI * ( 1 /  (tbtwtestP + ttestPW)) * senW -> PW_EA_LI_T + nTestP",
      "PW_ES_LI -> PW_ES_LI * ptestPWLI * ( 1 /  (tbtwtestP + ttestPW)) * senW -> PW_ES_LI_T + nTestP",
      "PW_IA_LI -> PW_IA_LI * ptestPWLI * ( 1 /  (tbtwtestP + ttestPW)) * senW -> PW_IA_LI_T + nTestP",
      "PW_IM_LI -> PW_IM_LI * ptestPWsymp * ( 1 / (tbeftestPsymp + ttestsymp)) * sensymp -> PW_IM_LI_T + nTestP",
      "PW_IS_LI -> PW_IS_LI * ptestPWsymp * ( 1 / (tbeftestPsymp + ttestsymp)) * sensymp -> PW_IS_LI_T + nTestP",
      ## High immunity
      # tested with negative test result
      "PW_S_HI -> PW_S_HI * ptestPWHI * ( 1 / (tbtwtestP + ttestPW)) * (speW) -> PW_S_HI + nTestP",
      "PW_E_HI -> PW_E_HI * ptestPWHI * ( 1 /  (tbtwtestP + ttestPW)) * (1 - senW) -> PW_E_HI + nTestP",
      "PW_EA_HI -> PW_EA_HI * ptestPWHI * ( 1 /  (tbtwtestP + ttestPW)) * (1 - senW) -> PW_EA_HI + nTestP",
      "PW_ES_HI -> PW_ES_HI * ptestPWHI * ( 1 /  (tbtwtestP + ttestPW)) * (1 - senW) -> PW_ES_HI + nTestP",
      "PW_IA_HI -> PW_IA_HI * ptestPWHI * ( 1 /  (tbtwtestP + ttestPW)) * (1 - senW) -> PW_IA_HI + nTestP",
      "PW_IM_HI -> PW_IM_HI * ptestPWsymp * ( 1 / (tbeftestPsymp + ttestsymp)) * (1 - sensymp) -> PW_IM_HI + nTestP",
      "PW_IS_HI -> PW_IS_HI * ptestPWsymp * ( 1 / (tbeftestPsymp + ttestsymp)) * (1 - sensymp) -> PW_IS_HI + nTestP",
      # tested with positive test result
      "PW_S_HI -> PW_S_HI * ptestPWHI * ( 1 / (tbtwtestP + ttestPW)) * (1 - speW) -> PW_S_HI_T + nTestP",
      "PW_E_HI -> PW_E_HI * ptestPWHI * ( 1 /  (tbtwtestP + ttestPW)) * senW -> PW_E_HI_T + nTestP",
      "PW_EA_HI -> PW_EA_HI * ptestPWHI * ( 1 /  (tbtwtestP + ttestPW)) * senW -> PW_EA_HI_T + nTestP",
      "PW_ES_HI -> PW_ES_HI * ptestPWHI * ( 1 /  (tbtwtestP + ttestPW)) * senW -> PW_ES_HI_T + nTestP",
      "PW_IA_HI -> PW_IA_HI * ptestPWHI * ( 1 /  (tbtwtestP + ttestPW)) * senW -> PW_IA_HI_T + nTestP",
      "PW_IM_HI -> PW_IM_HI * ptestPWsymp * ( 1 / (tbeftestPsymp + ttestsymp)) * sensymp -> PW_IM_HI_T + nTestP",
      "PW_IS_HI -> PW_IS_HI * ptestPWsymp * ( 1 / (tbeftestPsymp + ttestsymp)) * sensymp -> PW_IS_HI_T + nTestP",
      ### susceptible with false positive test return to untested
      "PW_S_NI_T -> PW_S_NI_T * 1 / td -> PW_S_NI",
      "PW_S_LI_T -> PW_S_LI_T * 1 / td -> PW_S_LI",
      "PW_S_HI_T -> PW_S_HI_T * 1 / td -> PW_S_HI",
      ### Infection
      # pinfWp
      paste("PW_S_NI -> ", PWEMPTY,"? 0 : PW_S_NI * ", lambdaPW ," -> PW_E_NI + infP"),
      paste("PW_S_NI_T -> ", PWEMPTY,"? 0 : PW_S_NI_T * (1 - pISO) *", lambdaPW ," -> PW_E_NI_T + infP"),
      paste("PW_S_LI -> ", PWEMPTY,"? 0 : PW_S_LI * ", lambdaPW ," * rinfLI -> PW_E_LI + infP"),
      paste("PW_S_LI_T -> ", PWEMPTY,"? 0 : PW_S_LI_T * (1 - pISO) *", lambdaPW ," * rinfLI -> PW_E_LI_T + infP"),
      paste("PW_S_HI -> ", PWEMPTY,"? 0 : PW_S_HI * ", lambdaPW ," * rinfHI -> PW_E_HI + infP"),
      paste("PW_S_HI_T -> ", PWEMPTY,"? 0 : PW_S_HI_T * (1 - pISO) *", lambdaPW ," * rinfHI -> PW_E_HI_T + infP"),
      ### Disease cycle
      # NI
      if(gdata["psympNI"] < 1) "PW_E_NI -> PW_E_NI * ( 1 / tE) * (1 - psympNI) -> PW_EA_NI",
      if(gdata["psympNI"] < 1) "PW_EA_NI -> PW_EA_NI * ( 1 / tEA) -> PW_IA_NI + incPA",
      if(gdata["psympNI"] < 1) "PW_IA_NI -> PW_IA_NI * ( 1 / tIA) -> PW_S_HI",
      "PW_E_NI -> PW_E_NI * ( 1 / tE) * psympNI -> PW_ES_NI",
      "PW_ES_NI -> PW_ES_NI * ( 1 / tES) * (1 - psevPNI) -> PW_IM_NI + incPM",
      "PW_ES_NI -> PW_ES_NI * ( 1 / tES) * psevPNI -> PW_IS_NI + incPS",
      "PW_IM_NI -> PW_IM_NI * ( 1 / tIM) -> PW_S_HI",
      "PW_IS_NI -> PW_IS_NI * (1 - pIC) * ( 1 / tIS) -> PW_S_HI",
      "PW_IS_NI -> PW_IS_NI * pIC * ( 1 / tIS) -> IC",
      if(gdata["psympNI"] < 1) "PW_E_NI_T -> PW_E_NI_T * ( 1 / tE) * (1 - psympNI) -> PW_EA_NI_T",
      if(gdata["psympNI"] < 1) "PW_EA_NI_T -> PW_EA_NI_T * ( 1 / tEA) -> PW_IA_NI_T + incPA",
      if(gdata["psympNI"] < 1) "PW_IA_NI_T -> PW_IA_NI_T * ( 1 / tIA) -> PW_S_HI",
      "PW_E_NI_T -> PW_E_NI_T * ( 1 / tE) * psympNI -> PW_ES_NI_T",
      "PW_ES_NI_T -> PW_ES_NI_T * ( 1 / tES) * (1 - psevPNI) -> PW_IM_NI_T + incPM",
      "PW_ES_NI_T -> PW_ES_NI_T * ( 1 / tES) * psevPNI -> PW_IS_NI_T + incPS",
      "PW_IM_NI_T -> PW_IM_NI_T * ( 1 / tIM) -> PW_S_HI",
      "PW_IS_NI_T -> PW_IS_NI_T * (1 - pIC) * ( 1 / tIS) -> PW_S_HI",
      "PW_IS_NI_T -> PW_IS_NI_T * pIC * ( 1 / tIS) -> IC",
      # LI
      if(gdata["psympLI"] < 1) "PW_E_LI -> PW_E_LI * ( 1 / tE) * (1 - psympLI) -> PW_EA_LI",
      if(gdata["psympLI"] < 1) "PW_EA_LI -> PW_EA_LI * ( 1 / tEA) -> PW_IA_LI + incPA",
      if(gdata["psympLI"] < 1) "PW_IA_LI -> PW_IA_LI * ( 1 / tIA) -> PW_S_HI",
      "PW_E_LI -> PW_E_LI * ( 1 / tE) * psympLI -> PW_ES_LI",
      "PW_ES_LI -> PW_ES_LI * ( 1 / tES) * (1 - psevPLI) -> PW_IM_LI + incPM",
      "PW_ES_LI -> PW_ES_LI * ( 1 / tES) * psevPLI -> PW_IS_LI + incPS",
      "PW_IM_LI -> PW_IM_LI * ( 1 / tIM) -> PW_S_HI",
      "PW_IS_LI -> PW_IS_LI * (1 - pIC) * ( 1 / tIS) -> PW_S_HI",
      "PW_IS_LI -> PW_IS_LI * pIC * ( 1 / tIS) -> IC",
      if(gdata["psympLI"] < 1) "PW_E_LI_T -> PW_E_LI_T * ( 1 / tE) * (1 - psympLI) -> PW_EA_LI_T",
      if(gdata["psympLI"] < 1) "PW_EA_LI_T -> PW_EA_LI_T * ( 1 / tEA) -> PW_IA_LI_T + incPA",
      if(gdata["psympLI"] < 1) "PW_IA_LI_T -> PW_IA_LI_T * ( 1 / tIA) -> PW_S_HI",
      "PW_E_LI_T -> PW_E_LI_T * ( 1 / tE) * psympLI -> PW_ES_LI_T",
      "PW_ES_LI_T -> PW_ES_LI_T * ( 1 / tES) * (1 - psevPLI) -> PW_IM_LI_T + incPM",
      "PW_ES_LI_T -> PW_ES_LI_T * ( 1 / tES) * psevPLI -> PW_IS_LI_T + incPS",
      "PW_IM_LI_T -> PW_IM_LI_T * ( 1 / tIM) -> PW_S_HI",
      "PW_IS_LI_T -> PW_IS_LI_T * (1 - pIC) * ( 1 / tIS) -> PW_S_HI",
      "PW_IS_LI_T -> PW_IS_LI_T * pIC * ( 1 / tIS) -> IC",
      # HI
      if(gdata["psympHI"] < 1) "PW_E_HI -> PW_E_HI * ( 1 / tE) * (1 - psympHI) -> PW_EA_HI",
      if(gdata["psympHI"] < 1) "PW_EA_HI -> PW_EA_HI * ( 1 / tEA) -> PW_IA_HI + incPA",
      if(gdata["psympHI"] < 1) "PW_IA_HI -> PW_IA_HI * ( 1 / tIA) -> PW_S_HI",
      "PW_E_HI -> PW_E_HI * ( 1 / tE) * psympHI -> PW_ES_HI",
      "PW_ES_HI -> PW_ES_HI * ( 1 / tES) * (1 - psevPHI) -> PW_IM_HI + incPM",
      "PW_ES_HI -> PW_ES_HI * ( 1 / tES) * psevPHI -> PW_IS_HI + incPS",
      "PW_IM_HI -> PW_IM_HI * ( 1 / tIM) -> PW_S_HI",
      "PW_IS_HI -> PW_IS_HI * (1 - pIC) * ( 1 / tIS) -> PW_S_HI",
      "PW_IS_HI -> PW_IS_HI * pIC * ( 1 / tIS) -> IC",
      if(gdata["psympHI"] < 1) "PW_E_HI_T -> PW_E_HI_T * ( 1 / tE) * (1 - psympHI) -> PW_EA_HI_T",
      if(gdata["psympHI"] < 1) "PW_EA_HI_T -> PW_EA_HI_T * ( 1 / tEA) -> PW_IA_HI_T + incPA",
      if(gdata["psympHI"] < 1) "PW_IA_HI_T -> PW_IA_HI_T * ( 1 / tIA) -> PW_S_HI",
      "PW_E_HI_T -> PW_E_HI_T * ( 1 / tE) * psympHI -> PW_ES_HI_T",
      "PW_ES_HI_T -> PW_ES_HI_T * ( 1 / tES) * (1 - psevPHI) -> PW_IM_HI_T + incPM",
      "PW_ES_HI_T -> PW_ES_HI_T * ( 1 / tES) * psevPHI -> PW_IS_HI_T + incPS",
      "PW_IM_HI_T -> PW_IM_HI_T * ( 1 / tIM) -> PW_S_HI",
      "PW_IS_HI_T -> PW_IS_HI_T * (1 - pIC) * ( 1 / tIS) -> PW_S_HI",
      "PW_IS_HI_T -> PW_IS_HI_T * pIC * ( 1 / tIS) -> IC",
      ### Immunity waning
      "PW_S_HI -> PW_S_HI * (1 / tHI) -> PW_S_LI",
      "PW_S_HI_T -> PW_S_HI_T * (1 / tHI) -> PW_S_LI_T",
      "PW_S_LI -> PW_S_LI * (1 / tLI) -> PW_S_NI",
      "PW_S_LI_T -> PW_S_LI_T * (1 / tLI) -> PW_S_NI_T",
      ### Immunity gain
      "PW_S_NI -> PW_S_NI * hNI2LI -> PW_S_LI",
      "PW_S_LI -> PW_S_LI * hLI2HI -> PW_S_HI",
      "PW_S_LI_T -> PW_S_LI_T * hLI2HI -> PW_S_HI_T",
      ### Exit
      ## No immunity
      "PW_S_NI -> PW_S_NI * ( 1 / tLS) -> @",
      "PW_E_NI -> PW_E_NI * ( 1 / tLS) -> @",
      "PW_EA_NI -> PW_EA_NI * ( 1 / tLS) -> @",
      "PW_ES_NI -> PW_ES_NI * ( 1 / tLS) -> @",
      "PW_IA_NI -> PW_IA_NI * ( 1 / tLS) -> @",
      "PW_IM_NI -> PW_IM_NI * ( 1 / tLS) -> @",
      "PW_IS_NI -> PW_IS_NI * ( 1 / tLS) -> @",
      #
      "PW_S_NI_T -> PW_S_NI_T * ( 1 / tLS) -> @",
      "PW_E_NI_T -> PW_E_NI_T * ( 1 / tLS) -> @",
      "PW_EA_NI_T -> PW_EA_NI_T * ( 1 / tLS) -> @",
      "PW_ES_NI_T -> PW_ES_NI_T * ( 1 / tLS) -> @",
      "PW_IA_NI_T -> PW_IA_NI_T * ( 1 / tLS) -> @",
      "PW_IM_NI_T -> PW_IM_NI_T * ( 1 / tLS) -> @",
      "PW_IS_NI_T -> PW_IS_NI_T * ( 1 / tLS) -> @",
      ## Low immunity
      "PW_S_LI -> PW_S_LI * ( 1 / tLS) -> @",
      "PW_E_LI -> PW_E_LI * ( 1 / tLS) -> @",
      "PW_EA_LI -> PW_EA_LI * ( 1 / tLS) -> @",
      "PW_ES_LI -> PW_ES_LI * ( 1 / tLS) -> @",
      "PW_IA_LI -> PW_IA_LI * ( 1 / tLS) -> @",
      "PW_IM_LI -> PW_IM_LI * ( 1 / tLS) -> @",
      "PW_IS_LI -> PW_IS_LI * ( 1 / tLS) -> @",
      #
      "PW_S_LI_T -> PW_S_LI_T * ( 1 / tLS) -> @",
      "PW_E_LI_T -> PW_E_LI_T * ( 1 / tLS) -> @",
      "PW_EA_LI_T -> PW_EA_LI_T * ( 1 / tLS) -> @",
      "PW_ES_LI_T -> PW_ES_LI_T * ( 1 / tLS) -> @",
      "PW_IA_LI_T -> PW_IA_LI_T * ( 1 / tLS) -> @",
      "PW_IM_LI_T -> PW_IM_LI_T * ( 1 / tLS) -> @",
      "PW_IS_LI_T -> PW_IS_LI_T * ( 1 / tLS) -> @",
      ## High immunity
      "PW_S_HI -> PW_S_HI * ( 1 / tLS) -> @",
      "PW_E_HI -> PW_E_HI * ( 1 / tLS) -> @",
      "PW_EA_HI -> PW_EA_HI * ( 1 / tLS) -> @",
      "PW_ES_HI -> PW_ES_HI * ( 1 / tLS) -> @",
      "PW_IA_HI -> PW_IA_HI * ( 1 / tLS) -> @",
      "PW_IM_HI -> PW_IM_HI * ( 1 / tLS) -> @",
      "PW_IS_HI -> PW_IS_HI * ( 1 / tLS) -> @",
      #
      "PW_S_HI_T -> PW_S_HI_T * ( 1 / tLS) -> @",
      "PW_E_HI_T -> PW_E_HI_T * ( 1 / tLS) -> @",
      "PW_EA_HI_T -> PW_EA_HI_T * ( 1 / tLS) -> @",
      "PW_ES_HI_T -> PW_ES_HI_T * ( 1 / tLS) -> @",
      "PW_IA_HI_T -> PW_IA_HI_T * ( 1 / tLS) -> @",
      "PW_IM_HI_T -> PW_IM_HI_T * ( 1 / tLS) -> @",
      "PW_IS_HI_T -> PW_IS_HI_T * ( 1 / tLS) -> @",
      ####
      #### IC
      ####
      "IC -> IC * ( 1 / tLS) -> @",
      "IC -> IC * pdieIC -> @",
      "IC -> IC * (1 - pdieIC) * ( 1 / tIC) -> PW_S_HI",
      ######
      ###### Healthcare professionals
      ######
      ### Immunity waning
      "H_S_HI -> H_S_HI * (1 / tHI) -> H_S_LI",
      "H_S_HI_T -> H_S_HI_T * (1 / tHI) -> H_S_LI_T",
      "H_S_LI -> H_S_LI * (1 / tLI) -> H_S_NI",
      "H_S_LI_T -> H_S_LI_T * (1 / tLI) -> H_S_NI_T",
      ### Immunity gain
      "H_S_NI -> H_S_NI * hNI2LI -> H_S_LI",
      "H_S_LI -> H_S_LI * hLI2HI -> H_S_HI",
      ### susceptible with false positive test return to untested
      "H_S_NI_T -> H_S_NI_T * 1 / td -> H_S_NI",
      "H_S_LI_T -> H_S_LI_T * 1 / td -> H_S_LI",
      "H_S_HI_T -> H_S_HI_T * 1 / td -> H_S_HI",
      #  infection in community
      paste("H_S_NI -> H_S_NI * hinc * ptow -> H_E_NI + infHout"),
      paste("H_S_NI_T -> H_S_NI_T * hinc * ptow -> H_E_NI_T + infHout"),
      paste("H_S_LI -> H_S_LI * hinc * rinfLI * ptow -> H_E_LI + infHout"),
      paste("H_S_LI_T -> H_S_LI_T * hinc * rinfLI * ptow -> H_E_LI_T + infHout"),
      paste("H_S_HI -> H_S_HI * hinc * rinfHI * ptow -> H_E_HI + infHout"),
      paste("H_S_HI_T -> H_S_HI_T * hinc * rinfHI * ptow -> H_E_HI_T + infHout"),
      ###
      ### Disease cycle
      ###
      # NI
      if(gdata["psympPNI"] < 1) "H_E_NI -> H_E_NI * ( 1 / tE) * (1 - psympPNI) -> H_EA_NI",
      if(gdata["psympPNI"] < 1) "H_EA_NI -> H_EA_NI * ( 1 / tEA) -> H_IA_NI + incHA",
      if(gdata["psympPNI"] < 1) "H_IA_NI -> H_IA_NI * ( 1 / tIA) -> H_S_HI",
      "H_E_NI -> H_E_NI * ( 1 / tE) * psympPNI -> H_ES_NI",
      "H_ES_NI -> H_ES_NI * ( 1 / tES) * (1 - psevNI) -> H_IM_NI + incHM",
      "H_ES_NI -> H_ES_NI * ( 1 / tES) * psevNI -> H_IS_NI + incHS",
      "H_IM_NI -> H_IM_NI * (1 - pSL) * ( 1 / tIM) -> H_S_HI",
      "H_IM_NI -> H_IM_NI * pSL * ( 1 / tIM) -> SL",
      "H_IS_NI -> H_IS_NI * (1 - pESL) * ( 1 / tIS) -> H_S_HI",
      "H_IS_NI -> H_IS_NI * pESL * ( 1 / tIS) -> ESL",
      #
      if(gdata["psympPNI"] < 1) "H_E_NI_T -> H_E_NI_T * ( 1 / tE) * (1 - psympPNI) -> H_EA_NI_T",
      if(gdata["psympPNI"] < 1) "H_EA_NI_T -> H_EA_NI_T * ( 1 / tEA) -> H_IA_NI_T + incHA",
      if(gdata["psympPNI"] < 1) "H_IA_NI_T -> H_IA_NI_T * ( 1 / tIA) -> H_S_HI",
      "H_E_NI_T -> H_E_NI_T * ( 1 / tE) * psympPNI -> H_ES_NI_T",
      "H_ES_NI_T -> H_ES_NI_T * ( 1 / tES) * (1 - psevNI) -> H_IM_NI_T + incHM",
      "H_ES_NI_T -> H_ES_NI_T * ( 1 / tES) * psevNI -> H_IS_NI_T + incHS",
      "H_IM_NI_T -> H_IM_NI_T * (1 - pSL) * ( 1 / tIM) -> H_S_HI_T",
      "H_IM_NI_T -> H_IM_NI_T * pSL * ( 1 / tIM) -> SL",
      "H_IS_NI_T -> H_IS_NI_T * (1 - pESL) * ( 1 / tIS) -> H_S_HI_T",
      "H_IS_NI_T -> H_IS_NI_T * pESL * ( 1 / tIS) -> ESL",
      # LI
      if(gdata["psympPLI"] < 1) "H_E_LI -> H_E_LI * ( 1 / tE) * (1 - psympPLI) -> H_EA_LI",
      if(gdata["psympPLI"] < 1) "H_EA_LI -> H_EA_LI * ( 1 / tEA) -> H_IA_LI + incHA",
      if(gdata["psympPLI"] < 1) "H_IA_LI -> H_IA_LI * ( 1 / tIA) -> H_S_HI",
      "H_E_LI -> H_E_LI * ( 1 / tE) * psympPLI -> H_ES_LI",
      "H_ES_LI -> H_ES_LI * ( 1 / tES) * (1 - psevLI) -> H_IM_LI + incHM",
      "H_ES_LI -> H_ES_LI * ( 1 / tES) * psevLI -> H_IS_LI + incHS",
      "H_IM_LI -> H_IM_LI * (1 - pSL) * ( 1 / tIM) -> H_S_HI",
      "H_IM_LI -> H_IM_LI * pSL * ( 1 / tIM) -> SL",
      "H_IS_LI -> H_IS_LI * (1 - pESL) * ( 1 / tIS) -> H_S_HI",
      "H_IS_LI -> H_IS_LI * pESL * ( 1 / tIS) -> ESL",
      #
      if(gdata["psympPLI"] < 1) "H_E_LI_T -> H_E_LI_T * ( 1 / tE) * (1 - psympPLI) -> H_EA_LI_T",
      if(gdata["psympPLI"] < 1) "H_EA_LI_T -> H_EA_LI_T * ( 1 / tEA) -> H_IA_LI_T + incHA",
      if(gdata["psympPLI"] < 1) "H_IA_LI_T -> H_IA_LI_T * ( 1 / tIA) -> H_S_HI",
      "H_E_LI_T -> H_E_LI_T * ( 1 / tE) * psympPLI -> H_ES_LI_T",
      "H_ES_LI_T -> H_ES_LI_T * ( 1 / tES) * (1 - psevLI) -> H_IM_LI_T + incHM",
      "H_ES_LI_T -> H_ES_LI_T * ( 1 / tES) * psevLI -> H_IS_LI_T + incHS",
      "H_IM_LI_T -> H_IM_LI_T * (1 - pSL) * ( 1 / tIM) -> H_S_HI_T",
      "H_IM_LI_T -> H_IM_LI_T * pSL * ( 1 / tIM) -> SL",
      "H_IS_LI_T -> H_IS_LI_T * (1 - pESL) * ( 1 / tIS) -> H_S_HI_T",
      "H_IS_LI_T -> H_IS_LI_T * pESL * ( 1 / tIS) -> ESL",
      # HI
      if(gdata["psympPHI"] < 1) "H_E_HI -> H_E_HI * ( 1 / tE) * (1 - psympPHI) -> H_EA_HI",
      if(gdata["psympPHI"] < 1) "H_EA_HI -> H_EA_HI * ( 1 / tEA) -> H_IA_HI + incHA",
      if(gdata["psympPHI"] < 1) "H_IA_HI -> H_IA_HI * ( 1 / tIA) -> H_S_HI",
      "H_E_HI -> H_E_HI * ( 1 / tE) * psympPHI -> H_ES_HI",
      "H_ES_HI -> H_ES_HI * ( 1 / tES) * (1 - psevHI) -> H_IM_HI + incHM",
      "H_ES_HI -> H_ES_HI * ( 1 / tES) * psevHI -> H_IS_HI + incHS",
      "H_IM_HI -> H_IM_HI * (1 - pSL) * ( 1 / tIM) -> H_S_HI",
      "H_IM_HI -> H_IM_HI * pSL * ( 1 / tIM) -> SL",
      "H_IS_HI -> H_IS_HI * (1 - pESL) * ( 1 / tIS) -> H_S_HI",
      "H_IS_HI -> H_IS_HI * pESL * ( 1 / tIS) -> ESL",
      #
      if(gdata["psympPHI"] < 1) "H_E_HI_T -> H_E_HI_T * ( 1 / tE) * (1 - psympPHI) -> H_EA_HI_T",
      if(gdata["psympPHI"] < 1) "H_EA_HI_T -> H_EA_HI_T * ( 1 / tEA) -> H_IA_HI_T + incHA",
      if(gdata["psympPHI"] < 1) "H_IA_HI_T -> H_IA_HI_T * ( 1 / tIA) -> H_S_HI",
      "H_E_HI_T -> H_E_HI_T * ( 1 / tE) * psympPHI -> H_ES_HI_T",
      "H_ES_HI_T -> H_ES_HI_T * ( 1 / tES) * (1 - psevHI) -> H_IM_HI_T + incHM",
      "H_ES_HI_T -> H_ES_HI_T * ( 1 / tES) * psevHI -> H_IS_HI_T + incHS",
      "H_IM_HI_T -> H_IM_HI_T * (1 - pSL) * ( 1 / tIM) -> H_S_HI_T",
      "H_IM_HI_T -> H_IM_HI_T * pSL * ( 1 / tIM) -> SL",
      "H_IS_HI_T -> H_IS_HI_T * (1 - pESL) * ( 1 / tIS) -> H_S_HI_T",
      "H_IS_HI_T -> H_IS_HI_T * pESL * ( 1 / tIS) -> ESL",
      ###
      ### Return from SL and ESL
      ###
      ## SL
      "SL -> SL *  ( 1 / tSL) -> H_S_HI",
      ## ESL
      "ESL -> ESL *  ( 1 / tESL) -> H_S_HI",
      ###
      ### Tests
      ###
      ## NI
      # untested to tested
      "H_S_NI -> H_S_NI * ptestHNI * (1 / tbtwtestH) -> H_S_NI_T",
      "H_E_NI -> H_E_NI * ptestHNI * (1 / tbtwtestH) -> H_E_NI_T",
      "H_EA_NI -> H_EA_NI * ptestHNI * (1 / tbtwtestH) -> H_EA_NI_T",
      "H_ES_NI -> H_ES_NI * ptestHNI * (1 / tbtwtestH) -> H_ES_NI_T",
      "H_IA_NI -> H_IA_NI * ptestHNI * (1 / tbtwtestH) -> H_IA_NI_T",
      "H_IM_NI -> H_IM_NI * ptestHsymp * (1 / tbeftestHsymp) -> H_IM_NI_T",
      "H_IS_NI -> H_IS_NI * ptestHsymp * (1 / tbeftestHsymp) -> H_IS_NI_T",
      # tested to untested (negative test and positive but stay)
      "H_S_NI_T -> H_S_NI_T * speH * ( 1 / ttestHW) -> H_S_NI + nTestH",
      "H_E_NI_T -> H_E_NI_T * (1 - senH) * ( 1 / ttestHW) -> H_E_NI + nTestH",
      "H_EA_NI_T -> H_EA_NI_T * (1 - senH) * ( 1 / ttestHW) -> H_EA_NI + nTestH",
      "H_ES_NI_T -> H_ES_NI_T * (1 - senH) * ( 1 / ttestHW) -> H_ES_NI + nTestH",
      "H_IA_NI_T -> H_IA_NI_T * (1 - senH) * ( 1 / ttestHW) -> H_IA_NI + nTestH",
      "H_IM_NI_T -> H_IM_NI_T * (1 - sensymp) * ( 1 / ttestsymp) -> H_IM_NI + nTestH",
      "H_IS_NI_T -> H_IS_NI_T * (1 - sensymp) * ( 1 / ttestsymp) -> H_IS_NI + nTestH",
      # tested to sick leave (positive test)
      "H_S_NI_T -> H_S_NI_T * (1 - speH) * pSLT * ( 1 / ttestHW) -> SL + nTestH",
      "H_E_NI_T -> H_E_NI_T * senH * pSLT * ( 1 / ttestHW) -> SL + nTestH",
      "H_EA_NI_T -> H_EA_NI_T * senH * pSLT * ( 1 / ttestHW) -> SL + nTestH",
      "H_ES_NI_T -> H_ES_NI_T * senH * pSLT * ( 1 / ttestHW) -> SL + nTestH",
      "H_IA_NI_T -> H_IA_NI_T * senH * pSLT * ( 1 / ttestHW) -> SL + nTestH",
      "H_IM_NI_T -> H_IM_NI_T * sensymp * pSLT * ( 1 / ttestsymp) -> SL + nTestH",
      "H_IS_NI_T -> H_IS_NI_T * sensymp * pSLT * ( 1 / ttestsymp) -> ESL + nTestH",
      ## LI
      # untested to tested
      "H_S_LI -> H_S_LI * ptestHLI * (1 / tbtwtestH) -> H_S_LI_T",
      "H_E_LI -> H_E_LI * ptestHLI * (1 / tbtwtestH) -> H_E_LI_T",
      "H_EA_LI -> H_EA_LI * ptestHLI * (1 / tbtwtestH) -> H_EA_LI_T",
      "H_ES_LI -> H_ES_LI * ptestHLI * (1 / tbtwtestH) -> H_ES_LI_T",
      "H_IA_LI -> H_IA_LI * ptestHLI * (1 / tbtwtestH) -> H_IA_LI_T",
      "H_IM_LI -> H_IM_LI * ptestHsymp * (1 / tbeftestHsymp) -> H_IM_LI_T",
      "H_IS_LI -> H_IS_LI * ptestHsymp * (1 / tbeftestHsymp) -> H_IS_LI_T",
      # tested to untested (negative test and positive but stay)
      "H_S_LI_T -> H_S_LI_T * speH * ( 1 / ttestHW) -> H_S_LI + nTestH",
      "H_E_LI_T -> H_E_LI_T * (1 - senH) * ( 1 / ttestHW) -> H_E_LI + nTestH",
      "H_EA_LI_T -> H_EA_LI_T * (1 - senH) * ( 1 / ttestHW) -> H_EA_LI + nTestH",
      "H_ES_LI_T -> H_ES_LI_T * (1 - senH) * ( 1 / ttestHW) -> H_ES_LI + nTestH",
      "H_IA_LI_T -> H_IA_LI_T * (1 - senH) * ( 1 / ttestHW) -> H_IA_LI + nTestH",
      "H_IM_LI_T -> H_IM_LI_T * (1 - sensymp) * ( 1 / ttestsymp) -> H_IM_LI + nTestH",
      "H_IS_LI_T -> H_IS_LI_T * (1 - sensymp) * ( 1 / ttestsymp) -> H_IS_LI + nTestH",
      # tested to sick leave (positive test)
      "H_S_LI_T -> H_S_LI_T * (1 - speH) * pSLT * ( 1 / ttestHW) -> SL + nTestH",
      "H_E_LI_T -> H_E_LI_T * senH * pSLT * ( 1 / ttestHW) -> SL + nTestH",
      "H_EA_LI_T -> H_EA_LI_T * senH * pSLT * ( 1 / ttestHW) -> SL + nTestH",
      "H_ES_LI_T -> H_ES_LI_T * senH * pSLT * ( 1 / ttestHW) -> SL + nTestH",
      "H_IA_LI_T -> H_IA_LI_T * senH * pSLT * ( 1 / ttestHW) -> SL + nTestH",
      "H_IM_LI_T -> H_IM_LI_T * sensymp * pSLT * ( 1 / ttestsymp) -> SL + nTestH",
      "H_IS_LI_T -> H_IS_LI_T * sensymp * pSLT * ( 1 / ttestsymp) -> ESL + nTestH",
      ## HI
      # untested to tested
      "H_S_HI -> H_S_HI * ptestHHI * (1 / tbtwtestH) -> H_S_HI_T",
      "H_E_HI -> H_E_HI * ptestHHI * (1 / tbtwtestH) -> H_E_HI_T",
      "H_EA_HI -> H_EA_HI * ptestHHI * (1 / tbtwtestH) -> H_EA_HI_T",
      "H_ES_HI -> H_ES_HI * ptestHHI * (1 / tbtwtestH) -> H_ES_HI_T",
      "H_IA_HI -> H_IA_HI * ptestHHI * (1 / tbtwtestH) -> H_IA_HI_T",
      "H_IM_HI -> H_IM_HI * ptestHsymp * (1 / tbeftestHsymp) -> H_IM_HI_T",
      "H_IS_HI -> H_IS_HI * ptestHsymp * (1 / tbeftestHsymp) -> H_IS_HI_T",
      # tested to untested (negative test and positive but stay)
      "H_S_HI_T -> H_S_HI_T * speH * ( 1 / ttestHW) -> H_S_HI + nTestH",
      "H_E_HI_T -> H_E_HI_T * (1 - senH) * ( 1 / ttestHW) -> H_E_HI + nTestH",
      "H_EA_HI_T -> H_EA_HI_T * (1 - senH) * ( 1 / ttestHW) -> H_EA_HI + nTestH",
      "H_ES_HI_T -> H_ES_HI_T * (1 - senH) * ( 1 / ttestHW) -> H_ES_HI + nTestH",
      "H_IA_HI_T -> H_IA_HI_T * (1 - senH) * ( 1 / ttestHW) -> H_IA_HI + nTestH",
      "H_IM_HI_T -> H_IM_HI_T * (1 - sensymp) * ( 1 / ttestsymp) -> H_IM_HI + nTestH",
      "H_IS_HI_T -> H_IS_HI_T * (1 - sensymp) * ( 1 / ttestsymp) -> H_IS_HI + nTestH",
      # tested to sick leave (positive test)
      "H_S_HI_T -> H_S_HI_T * (1 - speH) * pSLT * ( 1 / ttestHW) -> SL + nTestH",
      "H_E_HI_T -> H_E_HI_T * senH * pSLT * ( 1 / ttestHW) -> SL + nTestH",
      "H_EA_HI_T -> H_EA_HI_T * senH * pSLT * ( 1 / ttestHW) -> SL + nTestH",
      "H_ES_HI_T -> H_ES_HI_T * senH * pSLT * ( 1 / ttestHW) -> SL + nTestH",
      "H_IA_HI_T -> H_IA_HI_T * senH * pSLT * ( 1 / ttestHW) -> SL + nTestH",
      "H_IM_HI_T -> H_IM_HI_T * sensymp * pSLT * ( 1 / ttestsymp) -> SL + nTestH",
      "H_IS_HI_T -> H_IS_HI_T * sensymp * pSLT * ( 1 / ttestsymp) -> ESL + nTestH"
    )

  # Add admission - SA epidemiological events - HCWS infections
  if(isTRUE(SA))
    transitions %<>% c(.,
                       c(
                         ####
                         #### screening area
                         ####
                         ### New patient (if there is an available bed)
                         ## Non Immune
                         # Susceptible
                         paste("@ ->", SPACE ,"? (1 - pLI - pHI) * (1 - prev) : 0 -> PSA_S_NI + adm"),
                         # Exposed
                         paste("@ ->", SPACE ,"? (1 - pLI - pHI) *  prev * rtE : 0 -> PSA_E_NI + adm + admE"),
                         # Exposed infectious future asymptomatic
                         paste("@ ->", SPACE ,"? (1 - pLI - pHI) *  prev * rtEA : 0 -> PSA_EA_NI + adm + admEA"),
                         # Exposed infectious future symptomatic
                         paste("@ ->", SPACE ,"? (1 - pLI - pHI) *  prev * rtES : 0 -> PSA_ES_NI + adm + admES"),
                         # Infectious asymptomatic
                         paste("@ ->", SPACE ,"? (1 - pLI - pHI) *  prev * rtIA : 0 -> PSA_IA_NI + adm + admIA"),
                         # Infectious with mild symptoms
                         paste("@ ->", SPACE ,"? (1 - pLI - pHI) *  prev * rtIM : 0 -> PSA_IM_NI + adm + admIM"),
                         # Infectious with severe symptoms
                         paste("@ ->", SPACE ,"? (1 - pLI - pHI) *  prev * rtIS : 0 -> PSA_IS_NI + adm + admIS"),
                         ## Partially Immune
                         # Susceptible
                         paste("@ ->", SPACE ,"? pLI * (1 - prev) : 0 -> PSA_S_LI + adm"),
                         # Exposed
                         paste("@ ->", SPACE ,"? pLI *  prev * rtE : 0 -> PSA_E_LI + adm + admE"),
                         # Exposed infectious future asymptomatic
                         paste("@ ->", SPACE ,"? pLI *  prev * rtEA : 0 -> PSA_EA_LI + adm + admEA"),
                         # Exposed infectious future symptomatic
                         paste("@ ->", SPACE ,"? pLI *  prev * rtES : 0 -> PSA_ES_LI + adm + admES"),
                         # Infectious asymptomatic
                         paste("@ ->", SPACE ,"? pLI *  prev * rtIA : 0 -> PSA_IA_LI + adm + admIA"),
                         # Infectious with mild symptoms
                         paste("@ ->", SPACE ,"? pLI *  prev * rtIM : 0 -> PSA_IM_LI + adm + admIM"),
                         # Infectious with severe symptoms
                         paste("@ ->", SPACE ,"? pLI *  prev * rtIS : 0 -> PSA_IS_LI + adm + admIS"),
                         ## Fully Immune
                         # Susceptible
                         paste("@ ->", SPACE ,"? pHI * (1 - prev) : 0 -> PSA_S_HI + adm"),
                         # Exposed
                         paste("@ ->", SPACE ,"? pHI *  prev * rtE : 0 -> PSA_E_HI + adm + admE"),
                         # Exposed infectious future asymptomatic
                         paste("@ ->", SPACE ,"? pHI *  prev * rtEA : 0 -> PSA_EA_HI + adm + admEA"),
                         # Exposed infectious future symptomatic
                         paste("@ ->", SPACE ,"? pHI *  prev * rtES : 0 -> PSA_ES_HI + adm + admES"),
                         # Infectious asymptomatic
                         paste("@ ->", SPACE ,"? pHI *  prev * rtIA : 0 -> PSA_IA_HI + adm + admIA"),
                         # Infectious with mild symptoms
                         paste("@ ->", SPACE ,"? pHI *  prev * rtIM : 0 -> PSA_IM_HI + adm + admIM"),
                         # Infectious with severe symptoms
                         paste("@ ->", SPACE ,"? pHI *  prev * rtIS : 0 -> PSA_IS_HI + adm + admIS"),
                         # Admission
                         ####
                         ### Test
                         ## No immunity - negative
                         "PSA_S_NI -> PSA_S_NI * ptestPSANI * ( 1 / (tSA + ttestSA)) * speSA -> PW_S_NI + nTestP",
                         "PSA_E_NI -> PSA_E_NI * ptestPSANI * ( 1 / (tSA + ttestSA)) * (1 - senSA) -> PW_E_NI + nTestP",
                         "PSA_EA_NI -> PSA_EA_NI * ptestPSANI * ( 1 / (tSA + ttestSA)) * (1 - senSA) -> PW_EA_NI + nTestP",
                         "PSA_ES_NI -> PSA_ES_NI * ptestPSANI * ( 1 / (tSA + ttestSA)) * (1 - senSA) -> PW_ES_NI + nTestP",
                         "PSA_IA_NI -> PSA_IA_NI * ptestPSANI * ( 1 / (tSA + ttestSA)) * (1 - senSA) -> PW_IA_NI + nTestP",
                         "PSA_IM_NI -> PSA_IM_NI * ptestPSAsymp * ( 1 / (tSA + ttestsymp)) * (1 - sensymp) -> PW_IM_NI + nTestP",
                         "PSA_IS_NI -> PSA_IS_NI * ptestPSAsymp * ( 1 / (tSA + ttestsymp)) * (1 - sensymp) -> PW_IS_NI + nTestP",
                         ## No immunity - positive
                         "PSA_S_NI -> PSA_S_NI * ptestPSANI * ( 1 / (tSA + ttestSA)) * (1 - speSA) -> PW_S_NI_T + nTestP",
                         "PSA_E_NI -> PSA_E_NI * ptestPSANI * ( 1 / (tSA + ttestSA)) * senSA -> PW_E_NI_T + nTestP",
                         "PSA_EA_NI -> PSA_EA_NI * ptestPSANI * ( 1 / (tSA + ttestSA)) * senSA -> PW_EA_NI_T + nTestP",
                         "PSA_ES_NI -> PSA_ES_NI * ptestPSANI * ( 1 / (tSA + ttestSA)) * senSA -> PW_ES_NI_T + nTestP",
                         "PSA_IA_NI -> PSA_IA_NI * ptestPSANI * ( 1 / (tSA + ttestSA)) * senSA -> PW_IA_NI_T + nTestP",
                         "PSA_IM_NI -> PSA_IM_NI * ptestPSAsymp * ( 1 / (tSA + ttestsymp)) * sensymp -> PW_IM_NI_T + nTestP",
                         "PSA_IS_NI -> PSA_IS_NI * ptestPSAsymp * ( 1 / (tSA + ttestsymp)) * sensymp -> PW_IS_NI_T + nTestP",
                         ## Low immunity - negative
                         "PSA_S_LI -> PSA_S_LI * ptestPSALI * ( 1 / (tSA + ttestSA)) * speSA -> PW_S_LI + nTestP",
                         "PSA_E_LI -> PSA_E_LI * ptestPSALI * ( 1 / (tSA + ttestSA)) * (1 - senSA) -> PW_E_LI + nTestP",
                         "PSA_EA_LI -> PSA_EA_LI * ptestPSALI * ( 1 / (tSA + ttestSA)) * (1 - senSA) -> PW_EA_LI + nTestP",
                         "PSA_ES_LI -> PSA_ES_LI * ptestPSALI * ( 1 / (tSA + ttestSA)) * (1 - senSA) -> PW_ES_LI + nTestP",
                         "PSA_IA_LI -> PSA_IA_LI * ptestPSALI * ( 1 / (tSA + ttestSA)) * (1 - senSA) -> PW_IA_LI + nTestP",
                         "PSA_IM_LI -> PSA_IM_LI * ptestPSAsymp * ( 1 / (tSA + ttestsymp)) * (1 - sensymp) -> PW_IM_LI + nTestP",
                         "PSA_IS_LI -> PSA_IS_LI * ptestPSAsymp * ( 1 / (tSA + ttestsymp)) * (1 - sensymp) -> PW_IS_LI + nTestP",
                         ## Low immunity - positive
                         "PSA_S_LI -> PSA_S_LI * ptestPSALI * ( 1 / (tSA + ttestSA)) * (1 - speSA) -> PW_S_LI_T + nTestP",
                         "PSA_E_LI -> PSA_E_LI * ptestPSALI * ( 1 / (tSA + ttestSA)) * senSA -> PW_E_LI_T + nTestP",
                         "PSA_EA_LI -> PSA_EA_LI * ptestPSALI * ( 1 / (tSA + ttestSA)) * senSA -> PW_EA_LI_T + nTestP",
                         "PSA_ES_LI -> PSA_ES_LI * ptestPSALI * ( 1 / (tSA + ttestSA)) * senSA -> PW_ES_LI_T + nTestP",
                         "PSA_IA_LI -> PSA_IA_LI * ptestPSALI * ( 1 / (tSA + ttestSA)) * senSA -> PW_IA_LI_T + nTestP",
                         "PSA_IM_LI -> PSA_IM_LI * ptestPSAsymp * ( 1 / (tSA + ttestsymp)) * sensymp -> PW_IM_LI_T + nTestP",
                         "PSA_IS_LI -> PSA_IS_LI * ptestPSAsymp * ( 1 / (tSA + ttestsymp)) * sensymp -> PW_IS_LI_T + nTestP",
                         ## High immunity - negative
                         "PSA_S_HI -> PSA_S_HI * ptestPSAHI * ( 1 / (tSA + ttestSA)) * speSA -> PW_S_HI + nTestP",
                         "PSA_E_HI -> PSA_E_HI * ptestPSAHI * ( 1 / (tSA + ttestSA)) * (1 - senSA) -> PW_E_HI + nTestP",
                         "PSA_EA_HI -> PSA_EA_HI * ptestPSAHI * ( 1 / (tSA + ttestSA)) * (1 - senSA) -> PW_EA_HI + nTestP",
                         "PSA_ES_HI -> PSA_ES_HI * ptestPSAHI * ( 1 / (tSA + ttestSA)) * (1 - senSA) -> PW_ES_HI + nTestP",
                         "PSA_IA_HI -> PSA_IA_HI * ptestPSAHI * ( 1 / (tSA + ttestSA)) * (1 - senSA) -> PW_IA_HI + nTestP",
                         "PSA_IM_HI -> PSA_IM_HI * ptestPSAsymp * ( 1 / (tSA + ttestsymp)) * (1 - sensymp) -> PW_IM_HI + nTestP",
                         "PSA_IS_HI -> PSA_IS_HI * ptestPSAsymp * ( 1 / (tSA + ttestsymp)) * (1 - sensymp) -> PW_IS_HI + nTestP",
                         ## High immunity - positive
                         "PSA_S_HI -> PSA_S_HI * ptestPSAHI * ( 1 / (tSA + ttestSA)) * (1 - speSA) -> PW_S_HI_T + nTestP",
                         "PSA_E_HI -> PSA_E_HI * ptestPSAHI * ( 1 / (tSA + ttestSA)) * senSA -> PW_E_HI_T + nTestP",
                         "PSA_EA_HI -> PSA_EA_HI * ptestPSAHI * ( 1 / (tSA + ttestSA)) * senSA -> PW_EA_HI_T + nTestP",
                         "PSA_ES_HI -> PSA_ES_HI * ptestPSAHI * ( 1 / (tSA + ttestSA)) * senSA -> PW_ES_HI_T + nTestP",
                         "PSA_IA_HI -> PSA_IA_HI * ptestPSAHI * ( 1 / (tSA + ttestSA)) * senSA -> PW_IA_HI_T + nTestP",
                         "PSA_IM_HI -> PSA_IM_HI * ptestPSAsymp * ( 1 / (tSA + ttestsymp)) * sensymp -> PW_IM_HI_T + nTestP",
                         "PSA_IS_HI -> PSA_IS_HI * ptestPSAsymp * ( 1 / (tSA + ttestsymp)) * sensymp -> PW_IS_HI_T + nTestP",
                         ### Admission
                         # non tested to ward
                         ## No immunity
                         "PSA_S_NI -> PSA_S_NI * (1 - ptestPSANI) * ( 1 / tSA) -> PW_S_NI",
                         "PSA_E_NI -> PSA_E_NI * (1 - ptestPSANI) * ( 1 / tSA) -> PW_E_NI",
                         "PSA_EA_NI -> PSA_EA_NI * (1 - ptestPSANI) * ( 1 / tSA) -> PW_EA_NI",
                         "PSA_ES_NI -> PSA_ES_NI * (1 - ptestPSANI) * ( 1 / tSA) -> PW_ES_NI",
                         "PSA_IA_NI -> PSA_IA_NI * (1 - ptestPSANI) * ( 1 / tSA) -> PW_IA_NI",
                         "PSA_IM_NI -> PSA_IM_NI * (1 - ptestPSAsymp) * ( 1 / tSA) -> PW_IM_NI",
                         "PSA_IS_NI -> PSA_IS_NI * (1 - ptestPSAsymp) * ( 1 / tSA) -> PW_IS_NI",
                         ## Low immunity
                         "PSA_S_LI -> PSA_S_LI * (1 - ptestPSALI) * ( 1 / tSA) -> PW_S_LI",
                         "PSA_E_LI -> PSA_E_LI * (1 - ptestPSALI) * ( 1 / tSA) -> PW_E_LI",
                         "PSA_EA_LI -> PSA_EA_LI * (1 - ptestPSALI) * ( 1 / tSA) -> PW_EA_LI",
                         "PSA_ES_LI -> PSA_ES_LI * (1 - ptestPSALI) * ( 1 / tSA) -> PW_ES_LI",
                         "PSA_IA_LI -> PSA_IA_LI * (1 - ptestPSALI) * ( 1 / tSA) -> PW_IA_LI",
                         "PSA_IM_LI -> PSA_IM_LI * (1 - ptestPSAsymp) * ( 1 / tSA) -> PW_IM_LI",
                         "PSA_IS_LI -> PSA_IS_LI * (1 - ptestPSAsymp) * ( 1 / tSA) -> PW_IS_LI",
                         ## High immunity
                         "PSA_S_HI -> PSA_S_HI * (1 - ptestPSAHI) * ( 1 / tSA) -> PW_S_HI",
                         "PSA_E_HI -> PSA_E_HI * (1 - ptestPSAHI) * ( 1 / tSA) -> PW_E_HI",
                         "PSA_EA_HI -> PSA_EA_HI * (1 - ptestPSAHI) * ( 1 / tSA) -> PW_EA_HI",
                         "PSA_ES_HI -> PSA_ES_HI * (1 - ptestPSAHI) * ( 1 / tSA) -> PW_ES_HI",
                         "PSA_IA_HI -> PSA_IA_HI * (1 - ptestPSAHI) * ( 1 / tSA) -> PW_IA_HI",
                         "PSA_IM_HI -> PSA_IM_HI * (1 - ptestPSAsymp) * ( 1 / tSA) -> PW_IM_HI",
                         "PSA_IS_HI -> PSA_IS_HI * (1 - ptestPSAsymp) *  ( 1 / tSA) -> PW_IS_HI",
                         ### Infection
                         # no immunity
                         paste("PSA_S_NI -> ", PSAEMPTY,"? 0 : PSA_S_NI * ",lambdaPSA," -> PSA_E_NI + infP"),
                         # low immunity
                         paste("PSA_S_LI -> ", PSAEMPTY,"? 0 : PSA_S_LI * ",lambdaPSA," -> PSA_E_LI + infP"),
                         # high immunity
                         paste("PSA_S_HI -> ", PSAEMPTY,"? 0 : PSA_S_HI * ",lambdaPSA," -> PSA_E_HI + infP"),
                         ##########
                         ####### Professionals
                         ##########
                         ### Infection
                         ###
                         # pinfH
                         paste("H_S_NI ->  (",nPSA," + ",nPWctc,") < 1 ? 0 : (", nPSA,") < 1  ?  H_S_NI *  pconta * ((1 - epsPHW) *  ctcHPW * (",
                               nPWctc,") / (",nH,") * wpropInfPWdest + (1 - epsHHW) * ctcHH * wpropInfHdest) : (", nPWctc,
                               ") < 1  ?  H_S_NI *  pconta * ((1 - epsHHW) * ctcHH * wpropInfHdest + (nH_SA / (", nH, ")) * (1 - epsPHSA) * ctcHPSA * (",
                               nPSA,") / nH_SA * wpropInfPSAdest ) : H_S_NI *  pconta * ( (1 - epsHHW) * ctcHH * wpropInfHdest + (nH_SA / (", nH,
                               ")) * ((1 - epsPHSA) * ctcHPSA * (", nPSA, ") / nH_SA * wpropInfPSAdest + (1 - epsPHW)  * ((ctcHPSA * (", nPSA,
                               ") + ctcHPW * (", nPWctc, "))/(", nH, ") - (ctcHPSA * (", nPSA, ") / nH_SA)) * wpropInfPWdest) +  ((", nH,
                               ") - nH_SA) / (", nH, ") * (1 - epsPHW) * (ctcHPSA * (", nPSA, ") + ctcHPW * (", nPWctc, "))/(", nH, ") * wpropInfPWdest) -> H_E_NI + infH"),
                         paste("H_S_NI_T ->  (",nPSA," + ",nPWctc,") < 1 ? 0 : (", nPSA,") < 1  ?  H_S_NI_T *  pconta * ((1 - epsPHW) *  ctcHPW * (",
                               nPWctc,") / (",nH,") * wpropInfPWdest + (1 - epsHHW) * ctcHH * wpropInfHdest) : (", nPWctc,
                               ") < 1  ?  H_S_NI_T *  pconta * ((1 - epsHHW) * ctcHH * wpropInfHdest + (nH_SA / (", nH, ")) * (1 - epsPHSA) * ctcHPSA * (",
                               nPSA,") / nH_SA * wpropInfPSAdest ) : H_S_NI_T *  pconta * ( (1 - epsHHW) * ctcHH * wpropInfHdest + (nH_SA / (", nH,
                               ")) * ((1 - epsPHSA) * ctcHPSA * (", nPSA, ") / nH_SA * wpropInfPSAdest + (1 - epsPHW)  * ((ctcHPSA * (", nPSA,
                               ") + ctcHPW * (", nPWctc, "))/(", nH, ") - (ctcHPSA * (", nPSA, ") / nH_SA)) * wpropInfPWdest) + ((", nH,
                               ") - nH_SA) / (", nH, ") * (1 - epsPHW) * (ctcHPSA * (", nPSA, ") + ctcHPW * (", nPWctc, "))/(", nH, ") * wpropInfPWdest) -> H_E_NI_T + infH"),
                         paste("H_S_LI ->  (",nPSA," + ",nPWctc,") < 1 ? 0 : (", nPSA,") < 1  ?  H_S_LI *  pconta * rinfLI * ((1 - epsPHW) *  ctcHPW * (",
                               nPWctc,") / (",nH,") * wpropInfPWdest + (1 - epsHHW) * ctcHH * wpropInfHdest) : (", nPWctc,
                               ") < 1  ?  H_S_LI *  pconta * rinfLI * ((1 - epsHHW) * ctcHH * wpropInfHdest + (nH_SA / (", nH, ")) * (1 - epsPHSA) * ctcHPSA * (",
                               nPSA,") / nH_SA * wpropInfPSAdest ) : H_S_LI *  pconta * rinfLI * ( (1 - epsHHW) * ctcHH * wpropInfHdest + (nH_SA / (", nH,
                               ")) * ((1 - epsPHSA) * ctcHPSA * (", nPSA, ") / nH_SA * wpropInfPSAdest + (1 - epsPHW)  * ((ctcHPSA * (", nPSA,
                               ") + ctcHPW * (", nPWctc, "))/(", nH, ") - (ctcHPSA * (", nPSA, ") / nH_SA)) * wpropInfPWdest) + ((", nH,
                               ") - nH_SA) / (", nH, ") * (1 - epsPHW) * (ctcHPSA * (", nPSA, ") + ctcHPW * (", nPWctc, "))/(", nH, ") * wpropInfPWdest) -> H_E_LI + infH"),
                         paste("H_S_LI_T ->  (",nPSA," + ",nPWctc,") < 1 ? 0 : (", nPSA,") < 1  ?  H_S_LI_T *  pconta * rinfLI * ((1 - epsPHW) *  ctcHPW * (",
                               nPWctc,") / (",nH,") * wpropInfPWdest + (1 - epsHHW) * ctcHH * wpropInfHdest) : (", nPWctc,
                               ") < 1  ?  H_S_LI_T *  pconta * rinfLI * ((1 - epsHHW) * ctcHH * wpropInfHdest + (nH_SA / (", nH, ")) * (1 - epsPHSA) * ctcHPSA * (",
                               nPSA,") / nH_SA * wpropInfPSAdest ) : H_S_LI_T *  pconta * rinfLI * ( (1 - epsHHW) * ctcHH * wpropInfHdest + (nH_SA / (", nH,
                               ")) * ((1 - epsPHSA) * ctcHPSA * (", nPSA, ") / nH_SA * wpropInfPSAdest + (1 - epsPHW)  * ((ctcHPSA * (", nPSA,
                               ") + ctcHPW * (", nPWctc, "))/(", nH, ") - (ctcHPSA * (", nPSA, ") / nH_SA)) * wpropInfPWdest) + ((", nH,
                               ") - nH_SA) / (", nH, ") * (1 - epsPHW) * (ctcHPSA * (", nPSA, ") + ctcHPW * (", nPWctc, "))/(", nH, ") * wpropInfPWdest) -> H_E_LI_T + infH"),
                         paste("H_S_HI ->  (",nPSA," + ",nPWctc,") < 1 ? 0 : (", nPSA,") < 1  ?  H_S_HI *  pconta * rinfHI * ((1 - epsPHW) *  ctcHPW * (",
                               nPWctc,") / (",nH,") * wpropInfPWdest + (1 - epsHHW) * ctcHH * wpropInfHdest) : (", nPWctc,
                               ") < 1  ?  H_S_HI *  pconta * rinfHI * ((1 - epsHHW) * ctcHH * wpropInfHdest + (nH_SA / (", nH, ")) * (1 - epsPHSA) * ctcHPSA * (",
                               nPSA,") / nH_SA * wpropInfPSAdest ) : H_S_HI *  pconta * rinfHI * ( (1 - epsHHW) * ctcHH * wpropInfHdest + (nH_SA / (", nH,
                               ")) * ((1 - epsPHSA) * ctcHPSA * (", nPSA, ") / nH_SA * wpropInfPSAdest + (1 - epsPHW)  * ((ctcHPSA * (", nPSA,
                               ") + ctcHPW * (", nPWctc, "))/(", nH, ") - (ctcHPSA * (", nPSA, ") / nH_SA)) * wpropInfPWdest) + ((", nH,
                               ") - nH_SA) / (", nH, ") * (1 - epsPHW) * (ctcHPSA * (", nPSA, ") + ctcHPW * (", nPWctc, "))/(", nH, ") * wpropInfPWdest) -> H_E_HI + infH"),
                         paste("H_S_HI_T ->  (",nPSA," + ",nPWctc,") < 1 ? 0 : (", nPSA,") < 1  ?  H_S_HI_T *  pconta * rinfHI * ((1 - epsPHW) *  ctcHPW * (",
                               nPWctc,") / (",nH,") * wpropInfPWdest + (1 - epsHHW) * ctcHH * wpropInfHdest) : (", nPWctc,
                               ") < 1  ?  H_S_HI_T *  pconta * rinfHI * ((1 - epsHHW) * ctcHH * wpropInfHdest + (nH_SA / (", nH, ")) * (1 - epsPHSA) * ctcHPSA * (",
                               nPSA,") / nH_SA * wpropInfPSAdest ) : H_S_HI_T *  pconta * rinfHI * ( (1 - epsHHW) * ctcHH * wpropInfHdest + (nH_SA / (", nH,
                               ")) * ((1 - epsPHSA) * ctcHPSA * (", nPSA, ") / nH_SA * wpropInfPSAdest + (1 - epsPHW)  * ((ctcHPSA * (", nPSA,
                               ") + ctcHPW * (", nPWctc, "))/(", nH, ") - (ctcHPSA * (", nPSA, ") / nH_SA)) * wpropInfPWdest) + ((", nH,
                               ") - nH_SA) / (", nH, ") * (1 - epsPHW) * (ctcHPSA * (", nPSA, ") + ctcHPW * (", nPWctc, "))/(", nH, ") * wpropInfPWdest) -> H_E_HI_T + infH")
                       )
    ) else
      # Add admission - HCWS infections
      transitions %<>% c(.,
                         c(
                           ####
                           #### screening area
                           ####
                           ### New patient (if there is an available bed)
                           ## Non Immune
                           # Susceptible
                           paste("@ ->", SPACE ,"? (1 - pLI - pHI) * (1 - prev) : 0 -> PW_S_NI + adm"),
                           # Exposed
                           paste("@ ->", SPACE ,"? (1 - pLI - pHI) *  prev * rtE : 0 -> PW_E_NI + adm + admE"),
                           # Exposed infectious future asymptomatic
                           paste("@ ->", SPACE ,"? (1 - pLI - pHI) *  prev * rtEA : 0 -> PW_EA_NI + adm + admEA"),
                           # Exposed infectious future symptomatic
                           paste("@ ->", SPACE ,"? (1 - pLI - pHI) *  prev * rtES : 0 -> PW_ES_NI + adm + admES"),
                           # Infectious asymptomatic
                           paste("@ ->", SPACE ,"? (1 - pLI - pHI) *  prev * rtIA : 0 -> PW_IA_NI + adm + admIA"),
                           # Infectious with mild symptoms
                           paste("@ ->", SPACE ,"? (1 - pLI - pHI) *  prev * rtIM : 0 -> PW_IM_NI + adm + admIM"),
                           # Infectious with severe symptoms
                           paste("@ ->", SPACE ,"? (1 - pLI - pHI) *  prev * rtIS : 0 -> PW_IS_NI + adm + admIS"),
                           ## Partially Immune
                           # Susceptible
                           paste("@ ->", SPACE ,"? pLI * (1 - prev) : 0 -> PW_S_LI + adm"),
                           # Exposed
                           paste("@ ->", SPACE ,"? pLI *  prev * rtE : 0 -> PW_E_LI + adm + admE"),
                           # Exposed infectious future asymptomatic
                           paste("@ ->", SPACE ,"? pLI *  prev * rtEA : 0 -> PW_EA_LI + adm + admEA"),
                           # Exposed infectious future symptomatic
                           paste("@ ->", SPACE ,"? pLI *  prev * rtES : 0 -> PW_ES_LI + adm + admES"),
                           # Infectious asymptomatic
                           paste("@ ->", SPACE ,"? pLI *  prev * rtIA : 0 -> PW_IA_LI + adm + admIA"),
                           # Infectious with mild symptoms
                           paste("@ ->", SPACE ,"? pLI *  prev * rtIM : 0 -> PW_IM_LI + adm + admIM"),
                           # Infectious with severe symptoms
                           paste("@ ->", SPACE ,"? pLI *  prev * rtIS : 0 -> PW_IS_LI + adm + admIS"),
                           ## Fully Immune
                           # Susceptible
                           paste("@ ->", SPACE ,"? pHI * (1 - prev) : 0 -> PW_S_HI + adm"),
                           # Exposed
                           paste("@ ->", SPACE ,"? pHI *  prev * rtE : 0 -> PW_E_HI + adm + admE"),
                           # Exposed infectious future asymptomatic
                           paste("@ ->", SPACE ,"? pHI *  prev * rtEA : 0 -> PW_EA_HI + adm + admEA"),
                           # Exposed infectious future symptomatic
                           paste("@ ->", SPACE ,"? pHI *  prev * rtES : 0 -> PW_ES_HI + adm + admES"),
                           # Infectious asymptomatic
                           paste("@ ->", SPACE ,"? pHI *  prev * rtIA : 0 -> PW_IA_HI + adm + admIA"),
                           # Infectious with mild symptoms
                           paste("@ ->", SPACE ,"? pHI *  prev * rtIM : 0 -> PW_IM_HI + adm + admIM"),
                           # Infectious with severe symptoms
                           paste("@ ->", SPACE ,"? pHI *  prev * rtIS : 0 -> PW_IS_HI + adm + admIS"),
                           ##########
                           ####### Professionals
                           ##########
                           ### Infection
                           ###
                           # pinfH
                           paste("H_S_NI ->  (",nPWctc,") < 1 ? 0 : H_S_NI *  pconta * ((1 - epsPHW) *  ctcHPW * (",
                                 nPWctc,") / (",nH,") * wpropInfPWdest + (1 - epsHHW) * ctcHH * wpropInfHdest) -> H_E_NI + infH"),
                           paste("H_S_NI_T ->  (",nPWctc,") < 1 ? 0 : H_S_NI_T *  pconta * ((1 - epsPHW) *  ctcHPW * (",
                                 nPWctc,") / (",nH,") * wpropInfPWdest + (1 - epsHHW) * ctcHH * wpropInfHdest) -> H_E_NI_T + infH"),
                           paste("H_S_LI ->  (", nPWctc,") < 1 ? 0 : H_S_LI *  pconta * rinfLI * ((1 - epsPHW) *  ctcHPW * (",
                                 nPWctc,") / (",nH,") * wpropInfPWdest + (1 - epsHHW) * ctcHH * wpropInfHdest) -> H_E_LI + infH"),
                           paste("H_S_LI_T ->  (", nPWctc,") < 1 ? 0 : H_S_LI_T *  pconta * rinfLI * ((1 - epsPHW) *  ctcHPW * (",
                                 nPWctc,") / (",nH,") * wpropInfPWdest + (1 - epsHHW) * ctcHH * wpropInfHdest) -> H_E_LI_T + infH"),
                           paste("H_S_HI ->  (", nPWctc,") < 1 ? 0 : H_S_HI *  pconta * rinfHI * ((1 - epsPHW) *  ctcHPW * (",
                                 nPWctc,") / (",nH,") * wpropInfPWdest + (1 - epsHHW) * ctcHH * wpropInfHdest) -> H_E_HI + infH"),
                           paste("H_S_HI_T ->  (", nPWctc,") < 1 ? 0 : H_S_HI_T * pconta * rinfHI * ((1 - epsPHW) *  ctcHPW * (",
                                 nPWctc,") / (",nH,") * wpropInfPWdest + (1 - epsHHW) * ctcHH * wpropInfHdest) -> H_E_HI_T + infH")
                         )
      )

  return(transitions)

}
