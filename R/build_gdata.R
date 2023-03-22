#' Generate global parameters dataset
#'
#' @description It calculates 1) the prevalence from daily incidence in the community and average disease duration,
#' 2) the probability of transmission per day spent in contact based on R0 and average disease duration,
#' 3) the time ratios of the epidemiological state based on average states duration,
#' 4) the proportion of time outside of work, and
#' 5) the proportion of probabilities of developing mild and severe symptoms for patients.
#'
#' Then the functions formats all global parameters required for the transitions as a named list readable by SimInf package.
#'
#' @usage build_gdata(
#'   n_ctcH_PSA = 2, t_ctcH_PSA = 10 / 60 / 24, n_ctcP_PSA = 0, t_ctcP_PSA = 5 / 60 / 24,
#'   n_ctcH_PW = 4, t_ctcH_PW = 15 / 60 / 24, n_ctcP_PW = 4, t_ctcP_PW = 30 / 60 / 24,
#'   n_ctcH_H = 5, t_ctcH_H = 3 / 60 / 24, t_ctcV_PW = 20 / 60 / 24,
#'   I = 185 / 100000, d = 10, R0 = 13, tw = 35,
#'   tSA  = 2 / 24, tIC  = 15, tSL  = 14, tESL = 28, tE  = 5,
#'   tEA = 2, tES = 2, tIA = 7, tIM = 8, tIS = 9, tLI = 60, tHI = 150,
#'   epsPPSA = 0.1, epsHPSA = 0.1, epsHPW = 0.1, epsPPW = 0.1,
#'   epsVPW = 0.1, epsPHSA = 0.1, epsPHW = 0.1, epsHHW = 0.1,
#'   ttestSA = 2 / 24, ttestPW = 2 / 24, ttestHW = 2 / 24, ttestsymp = 2 / 24,
#'   tbtwtestP = 14, tbtwtestH = 30, tbeftestPsymp = 2 / 24, tbeftestHsymp = 1,
#'   psympNI = 0.5, psympLI = 0.2, psympHI = 0.1, psevNI = 0.5, psevLI = 0.3, psevHI = 0.1,
#'   pISO = TRUE, pSL = 0.3, pESL = 1, pSLT = 0.01, pIC = 0.3, pdieIC = 0.005,
#'   pLI = 0.20, pHI = 0.5, hNI2LI = 1 / 30, hLI2HI = 1 / 60, rinfLI = 0.7, rinfHI = 0.5,
#'   rsymp = 1, rsev = 1,
#'   ptestPSAsymp = 1, ptestPSANI = .75, ptestPSALI = 0.50, ptestPSAHI = 0.1,
#'   ptestPWsymp = 0.95, ptestPWNI = 0.75, ptestPWLI = 0.50, ptestPWHI = 0.10,
#'   ptestHsymp = 0.85, ptestHNI = 0.75, ptestHLI = 0.50, ptestHHI = 0.20,
#'   senSA = 0.85, speSA = 0.95, senW = 0.85, speW = 0.95,
#'   senH = 0.85, speH = 0.95, sensymp = 0.85, spesymp = 0.95)
#'
#' @importFrom magrittr %>%
#'
#' @param disease string. only Covid and Influenza are available
#' @param n_ctcH_PSA a positive number, the daily number of contacts with professionals per patient in the screening area
#' @param t_ctcH_PSA a positive number, the average duration of contact between professional and patient per day in the screening area (in days, ex: if 15 min, t_ctcH_PSA = 15/60/24)
#' @param n_ctcP_PSA a positive number, the daily number of contacts with other patients per patient in the screening area
#' @param t_ctcP_PSA a positive number, the average duration of contact between patients per day in the screening area (in days)
#' @param n_ctcH_PW a positive number, the number of contact between professional and patient per day in the ward
#' @param t_ctcH_PW a positive number, the average duration of contact between patients per day in the ward (in days)
#' @param n_ctcP_PW a positive number, the number of contacts with other patients per patient per day, in the ward
#' @param t_ctcP_PW a positive number, the average duration of contact between patients per day in the ward (in days)
#' @param n_ctcH_H a positive number, the number of contacts with professionals per professional per day
#' @param t_ctcH_H a positive number, the average duration of contact between professionals in the ward (in days)
#' @param t_ctcV_PW a positive number, the average duration of contact between patients per day in the ward (in days)
#' @param I a positive number, the daily incidence
#' @param d a positive number, the average disease duration (in days)
#' @param R0 a positive number, the basic reproduction number
#' @param tw a positive number, the average number of working hours per week (in hours)
#' @param tSA  a positive number, the average duration (in days) before full admission (in screening area for clinical exam, administrative procedure, etc)
#' @param tIC  a positive number, the average duration (in days) of stay in intensive care
#' @param tSL  a positive number, the average duration (in days) of sick leave
#' @param tESL a positive number, the average duration (in days) of extended sick leave
#' @param tE a positive number, the average duration (in days) of epidemiological state E (exposed - non contagious)
#' @param tEA a positive number, the average duration (in days) of epidemiological state EA (exposed - contagious pre-asymptomatic)
#' @param tES a positive number, the average duration (in days) of epidemiological state ES (exposed - contagious pre-symptomatic)
#' @param tIA a positive number, the average duration (in days) of epidemiological state IA (infectious asymptomatic)
#' @param tIM a positive number, the average duration (in days) of epidemiological state IM (infectious with mild symptoms)
#' @param tIS a positive number, the average duration (in days) of epidemiological state IS (infectious with severe symptoms)
#' @param tLI a positive number, the average duration (in days) of low immunity persistence before return to non immune status
#' @param tHI a positive number, the average duration (in days) of high immunity persistence before return to low immune status
#' @param epsPPSA a positive ratio, the average infection-reducing ratio for patient infection during patient-to-patient contacts in the screening area (0 = no protection, 1 = complete protection/no infection possible)
#' @param epsHPSA a positive ratio, the average infection-reducing ratio for patient infection during professional-to-patient contacts in the screening area (0 = no protection, 1 = complete protection/no infection possible)
#' @param epsHPW a positive ratio, the average infection-reducing ratio for patient infection during professional-to-patient contacts in the ward (0 = no protection, 1 = complete protection/no infection possible)
#' @param epsPPW a positive ratio, the average infection-reducing ratio for patient infection during patient-to-patient contacts in the ward (0 = no protection, 1 = complete protection/no infection possible)
#' @param epsVPW a positive ratio, the average infection-reducing ratio for patient infection during visitor-to-patient contacts (0 = no protection, 1 = complete protection/no infection possible)
#' @param epsPHSA a positive ratio, the average infection-reducing ratio for professional infection during patient-to-professional contacts in the screening area (0 = no protection, 1 = complete protection/no infection possible)
#' @param epsPHW a positive ratio, the average infection-reducing ratio for professional infection during patient-to-professional contacts in the ward (0 = no protection, 1 = complete protection/no infection possible)
#' @param epsHHW a positive ratio, the average infection-reducing ratio for professional infection during professional-to-professional contacts (0 = no protection, 1 = complete protection/no infection possible)
#' @param ttestSA a positive number, the average duration (in days) of test in the screening area (from decision to test to action - such as transfer -  after obtaining the test result)
#' @param ttestPW a positive number, the average duration (in days) of test in the ward for patient screening (from decision to test to action - such as transfer -  after obtaining the test result)
#' @param ttestHW a positive number, the average duration (in days) of test for professional screening (from decision to test to action - such as sick leave -  after obtaining the test result)
#' @param ttestsymp a positive number, the average duration (in days) of test for symptomatic individuals (from decision to test to action - such as sick leave or transfer -  after obtaining the test result)
#' @param tbtwtestP a positive number, the average duration (in days) between two patient screening testing-events in the ward
#' @param tbtwtestH a positive number, the average duration (in days) between two professional screening testing-events in the ward
#' @param tbeftestPsymp a positive number, the average duration (in days) of symptomatic patients detection (before test)
#' @param tbeftestHsymp a positive number, the average duration (in days) of symptomatic professional detection (before test)
#' @param psympNI a positive probability, the conditional probability to be symptomatic when non immune
#' @param psympLI a positive probability, the conditional probability to be symptomatic despite a low immunity
#' @param psympHI a positive probability, the conditional probability to be symptomatic despite a high immunity
#' @param psevNI a positive probability, the conditional probability to develop severe symptoms when symptomatic and non immune
#' @param psevLI a positive probability, the conditional probability to develop severe symptoms when symptomatic and despite a low immunity
#' @param psevHI a positive probability, the conditional probability to develop severe symptoms when symptomatic and despite a high immunity
#' @param pISO probability, level of contact restrictions (confinement/quarantine/isolation) in case of positive test
#' @param pSL a positive probability, the probability for professional with mild symptoms of taking sick leave
#' @param pESL a positive probability, the probability for professional with severe symptoms  of taking extended sick leave
#' @param pSLT positive probability, the additional probability for professionals of taking sick leave after positive test
#' @param pIC a positive probability, the probability for patient with severe symptoms to be transfer in intensive care
#' @param pdieIC a positive probability, the probability to die in intensive care
#' @param pLI a positive probability, the probability to have low immunity at the admission (proportion of individuals with low immunity in the community)
#' @param pHI a positive probability, the probability to have high immunity at the admission (proportion of individuals with high immunity in the community)
#' @param hNI2LI a positive probability, the daily probability to gain low immunity when non immune
#' @param hLI2HI a positive probability, the daily probability to gain high immunity when having low immunity
#' @param rinfLI a positive ratio, the average infection-reducing ratio for individuals with low immunity compared to non immune (can be interpreted as low immunity efficiency)
#' @param rinfHI a positive ratio, the average infection-reducing ratio for individuals with high immunity compared to non immune (can be interpreted as high immunity efficiency)
#' @param rsymp a ratio, the average ratio increasing or reducing the probability to develop symptoms for patients compared to general population (professionals)
#' @param rsev a ratio, the average ratio increasing or reducing the probability to develop severe symptoms for symptomatic patients compared to general population (professionals)
#' @param rEA a ratio, the ratio of excretion for individuals in epidemiological stage EA (exposed - contagious pre-asymptomatic)
#' @param rES a ratio, the ratio of excretion for individuals in epidemiological stage ES (exposed - contagious pre-symptomatic)
#' @param rIA a ratio, the ratio of excretion for individuals in epidemiological stage IA (infectious asymptomatic)
#' @param rIM a ratio, the ratio of excretion for individuals in epidemiological stage IM (infectious with mild symptoms)
#' @param rIS a ratio, the ratio of excretion for individuals in epidemiological stage IS (infectious with severe symptoms)
#' @param ptestPSAsymp a positive probability, the probability to test symptomatic patients in the screening area
#' @param ptestPSANI a positive probability, the probability to test non immune patients in the screening area
#' @param ptestPSALI a positive probability, the probability to test patients with low immunity in the screening area
#' @param ptestPSAHI a positive probability, the probability to test patients with high immunity in the screening area
#' @param ptestPWsymp a positive probability, the probability to test symptomatic patients in the ward (Diagnostic testing)
#' @param ptestPWNI a positive proportion, the proportion of non immune patients tested during screening testing in the ward
#' @param ptestPWLI a positive proportion, the proportion of patients with low immunity tested during screening testing in the ward
#' @param ptestPWHI a positive proportion, the proportion of patients with high immunity tested during screening testing in the ward
#' @param ptestHsymp a positive probability, the probability to test symptomatic professionals (Diagnostic testing)
#' @param ptestHNI a positive proportion, the proportion of non immune professionals tested during screening testing
#' @param ptestHLI a positive proportion, the proportion of professionals with low immunity tested during screening testing
#' @param ptestHHI a positive proportion, the proportion of professionals with high immunity tested during screening testing
#' @param senSA a positive probability, the sensibility of tests use within the screening area
#' @param speSA a positive probability, the specificity of tests use within the screening area
#' @param senW a positive probability, the sensibility of tests use for patient screening in the ward
#' @param speW a positive probability, the specificity of tests use for patient screening in the ward
#' @param senH a positive probability, the sensibility of tests use for professional screening
#' @param speH a positive probability, the specificity of tests use for professional screening
#' @param sensymp a positive probability, the sensibility of tests use for symptomatic individuals
#' @param spesymp a positive probability, the specificity of tests use for symptomatic individuals
#'
#' @return Named list of parameters
#'
#' @examples build_gdata()
#'
#' @export

build_gdata <- function(
    disease = "Covid",
    ##### Infection
  n_ctcH_PSA = NULL,
  t_ctcH_PSA = NULL,
  n_ctcP_PSA = NULL,
  t_ctcP_PSA = NULL,
  n_ctcH_PW = NULL,
  t_ctcH_PW = NULL,
  n_ctcP_PW = NULL,
  t_ctcP_PW = NULL,
  n_ctcH_H = NULL,
  t_ctcH_H = NULL,
  t_ctcV_PW = NULL,
  I = NULL,
  d = NULL,
  R0 = NULL,
  tw = NULL,
  # https://www.gouvernement.fr/info-coronavirus/carte-et-donnees
  tSA  = NULL,
  # average duration before full admission (in screening area for clinical exam, administrative procedure, etc)
  tIC  = NULL,
  # average duration of stay in intensive care
  tSL  = NULL,
  # average duration of sick leave
  tESL = NULL,
  # average duration of extended sick leave
  tE  = NULL,
  # duration epidemiological state E
  tEA = NULL,
  # duration epidemiological state EA
  tES = NULL,
  # duration epidemiological state ES
  tIA = NULL,
  # duration epidemiological state IA
  tIM = NULL,
  # duration epidemiological state IM
  tIS = NULL,
  # duration epidemiological state IS
  tLI = NULL,
  # duration of partial immunity before return to non immune status
  tHI = NULL,
  # duration of full immunity before return to partial immune status
  # patient protection
  epsPPSA = NULL,
  epsHPSA = NULL,
  epsHPW = NULL,
  epsPPW = NULL,
  epsVPW = NULL,
  # healthcare workers protection
  epsPHSA = NULL,
  #from patient in SA
  epsPHW = NULL,
  #from patient in W
  epsHHW = NULL,
  #from HW in W
  ## Test in ward
  ttestSA = NULL,
  # test duration in screening area
  ttestPW = NULL,
  # test duration in ward for screening patients
  ttestHW = NULL,
  # test duration in ward for screening professionals
  ttestsymp = NULL,
  # test duration for symptomatic

  tbtwtestP = NULL,
  # duration between two tests for patient
  tbtwtestH = NULL,
  # duration between two tests for HCWS

  tbeftestPsymp = NULL,
  # duration before test of symp patient
  tbeftestHsymp = NULL,
  # duration before test of symp HCWS

  psympNI = NULL,
  # probability to be symptomatic when non immune
  psympLI = NULL,
  # probability to be symptomatic when partially immune
  psympHI = NULL,
  # probability to be symptomatic when fully immune

  psevNI = NULL,
  # probability to develop severe symptoms when non immune
  psevLI = NULL,
  # probability to develop severe symptoms when partially immune
  psevHI = NULL,
  # probability to develop severe symptoms when fully immune

  pISO = NULL,
  # contact restriction in case of positive test

  pSL = NULL,
  # probability to take sick leave
  pESL = NULL,
  # probability to take extended sick leave
  pSLT = NULL,
  # probability to take EL/ESL after positive test

  pIC = NULL,
  # probability to be transfer in intensive care
  pdieIC = NULL,
  # probability to die in intensive care

  ###################################
  pLI = NULL,
  # probability to be PI at the admission (proportion of PI in the population)
  pHI = NULL,
  # probability to be FI at the admission (proportion of FI in the population)
  hNI2LI = NULL,
  # daily probability to become partially immune
  hLI2HI = NULL,
  # daily probability to become fully immune

  rinfLI = NULL,
  # partial immunity efficiency % FIX ME better explain that this is the ratio of reduction of probability to be infected compared to non immune
  rinfHI = NULL,
  # partial immunity efficiency % FIX ME better explain that this is the ratio of reduction of probability to be infected compared to non immune

  rsymp = NULL,
  # Ratio adjusting probability of symptoms for patients compared to general population (professionals)
  rsev = NULL,
  # Ratio adjusting probability of severity if symptoms for patients compared to general population (professionals)

  rEA = NULL,
  # Ratio  of excretion for individuals in epidemiological stage EA (exposed - contagious pre-asymptomatic)
  rES = NULL,
  # Ratio of excretion for individuals in epidemiological stage ES (exposed - contagious pre-symptomatic)
  rIA = NULL,
  # Ratio of excretion for individuals in epidemiological stage IA (infectious asymptomatic)
  rIM = NULL,
  # Ratio of excretion for individuals in epidemiological stage IM (infectious with mild symptoms)
  rIS = NULL,
  # Ratio of excretion for individuals in epidemiological stage IS (infectious with severe symptoms)


  ptestPSAsymp = NULL,
  # probability to test symptomatic patients in the screening area
  ptestPSANI = NULL,
  # probability to test NI patients in the screening area
  ptestPSALI = NULL,
  # probability to test PI patients in the screening area
  ptestPSAHI = NULL,
  # probability to test FI patients in the screening area

  ptestPWsymp = NULL,
  # probability to test symptomatic patients in the ward
  ptestPWNI = NULL,
  # probability to test NI patients in the ward
  ptestPWLI = NULL,
  # probability to test PI patients in the ward
  ptestPWHI = NULL,
  # probability to test FI patients in the ward

  ptestHsymp = NULL,
  # probability to test symptomatic HCWS in the ward
  ptestHNI = NULL,
  # proportion of non immune professionals (healthcare workers) tested during screening testing
  ptestHLI = NULL,
  # probability to test PI HCWS
  ptestHHI = NULL,
  # probability to test FI HCWS

  senSA = NULL,
  speSA = NULL,
  senW = NULL,
  speW = NULL,
  senH = NULL,
  speH = NULL,
  sensymp = NULL,
  spesymp = NULL) {
  if(! disease %in% c("Covid", "Influenza"))
    stop(
    "Disease parameter must be Covid or Influenza."
  )

  if(disease == "Covid"){
    ##### Infection
  if(is.null(n_ctcH_PSA)) n_ctcH_PSA = 2
  if(is.null(t_ctcH_PSA)) t_ctcH_PSA = 10 / 60 / 24
  if(is.null(n_ctcP_PSA)) n_ctcP_PSA = 0
  if(is.null(t_ctcP_PSA)) t_ctcP_PSA = 5 / 60 / 24
  if(is.null(n_ctcH_PW)) n_ctcH_PW = 4
  if(is.null(t_ctcH_PW)) t_ctcH_PW = 15 / 60 / 24
  if(is.null(n_ctcP_PW)) n_ctcP_PW = 4
  if(is.null(t_ctcP_PW)) t_ctcP_PW = 30 / 60 / 24
  if(is.null(n_ctcH_H)) n_ctcH_H = 5
  if(is.null(t_ctcH_H)) t_ctcH_H = 3 / 60 / 24
  if(is.null(t_ctcV_PW)) t_ctcV_PW = 20 / 60 / 24
  if(is.null(I)) I = 185 / 100000
  if(is.null(d)) d = 10
  if(is.null(R0)) R0 = 13
  if(is.null(tw)) tw = 35
  # https://www.gouvernement.fr/info-coronavirus/carte-et-donnees
  if(is.null(tSA)) tSA  = 2 / 24
  # average duration before full admission (in screening area for clinical exam administrative procedure etc)
  if(is.null(tIC)) tIC  = 15
  # average duration of stay in intensive care
  if(is.null(tSL)) tSL  = 14
  # average duration of sick leave
  if(is.null(tESL)) tESL = 28
  # average duration of extended sick leave
  if(is.null(tE)) tE  = 5
  # duration epidemiological state E
  if(is.null(tEA)) tEA = 2
  # duration epidemiological state EA
  if(is.null(tES)) tES = 2
  # duration epidemiological state ES
  if(is.null(tIA)) tIA = 7
  # duration epidemiological state IA
  if(is.null(tIM)) tIM = 8
  # duration epidemiological state IM
  if(is.null(tIS)) tIS = 9
  # duration epidemiological state IS
  if(is.null(tLI)) tLI = 60
  # duration of partial immunity before return to non immune status
  if(is.null(tHI)) tHI = 150
  # duration of full immunity before return to partial immune status
  # patient protection
  if(is.null(epsPPSA)) epsPPSA = 0.1
  if(is.null(epsHPSA)) epsHPSA = 0.1
  if(is.null(epsHPW)) epsHPW = 0.1
  if(is.null(epsPPW)) epsPPW = 0.1
  if(is.null(epsVPW)) epsVPW = 0.1
  # healthcare workers protection
  if(is.null(epsPHSA)) epsPHSA = 0.1
  #from patient in SA
  if(is.null(epsPHW)) epsPHW = 0.1
  #from patient in W
  if(is.null(epsHHW)) epsHHW = 0.1
  #from HW in W
  ## Test in ward
  if(is.null(ttestSA)) ttestSA = 2 / 24
  # test duration in screening area
  if(is.null(ttestPW)) ttestPW = 2 / 24
  # test duration in ward for screening patients
  if(is.null(ttestHW)) ttestHW = 2 / 24
  # test duration in ward for screening professionals
  if(is.null(ttestsymp)) ttestsymp = 2 / 24
  # test duration for symptomatic

  if(is.null(tbtwtestP)) tbtwtestP = 14
  # duration between two tests for patient
  if(is.null(tbtwtestH)) tbtwtestH = 30
  # duration between two tests for HCWS

  if(is.null(tbeftestPsymp)) tbeftestPsymp = 2 / 24
  # duration before test of symp patient
  if(is.null(tbeftestHsymp)) tbeftestHsymp = 1
  # duration before test of symp HCWS

  if(is.null(psympNI)) psympNI = 0.5
  # probability to be symptomatic when non immune
  if(is.null(psympLI)) psympLI = 0.2
  # probability to be symptomatic when partially immune
  if(is.null(psympHI)) psympHI = 0.1
  # probability to be symptomatic when fully immune

  if(is.null(psevNI)) psevNI = 0.5
  # probability to develop severe symptoms when non immune
  if(is.null(psevLI)) psevLI = 0.3
  # probability to develop severe symptoms when partially immune
  if(is.null(psevHI)) psevHI = 0.1
  # probability to develop severe symptoms when fully immune

  if(is.null(pISO)) pISO = 1
  # contact restriction in case of positive test

  if(is.null(pSL)) pSL = 0.3
  # probability to take sick leave
  if(is.null(pESL)) pESL = 1
  # probability to take extended sick leave
  if(is.null(pSLT)) pSLT = 0.01
  # probability to take EL/ESL after positive test

  if(is.null(pIC)) pIC = 0.3
  # probability to be transfer in intensive care
  if(is.null(pdieIC)) pdieIC = 0.005
  # probability to die in intensive care

  ###################################
  if(is.null(pLI)) pLI = 0.20
  # probability to be PI at the admission (proportion of PI in the population)
  if(is.null(pHI)) pHI = 0.5
  # probability to be FI at the admission (proportion of FI in the population)
  if(is.null(hNI2LI)) hNI2LI = 1 / 30
  # daily probability to become partially immune
  if(is.null(hLI2HI)) hLI2HI = 1 / 60
  # daily probability to become fully immune

  if(is.null(rinfLI)) rinfLI = 0.7
  # partial immunity efficiency % FIX ME better explain that this is the ratio of reduction of probability to be infected compared to non immune
  if(is.null(rinfHI)) rinfHI = 0.5
  # partial immunity efficiency % FIX ME better explain that this is the ratio of reduction of probability to be infected compared to non immune

  if(is.null(rsymp)) rsymp = 1
  # Ratio adjusting probability of symptoms for patients compared to general population (professionals)
  if(is.null(rsev)) rsev = 1
  # Ratio adjusting probability of severity if symptoms for patients compared to general population (professionals)

  if(is.null(rEA)) rEA = 0.35
  # Ratio  of excretion for individuals in epidemiological stage EA (exposed - contagious pre-asymptomatic)
  if(is.null(rES)) rES = 1
  # Ratio of excretion for individuals in epidemiological stage ES (exposed - contagious pre-symptomatic)
  if(is.null(rIA)) rIA = 0.35
  # Ratio of excretion for individuals in epidemiological stage IA (infectious asymptomatic)
  if(is.null(rIM)) rIM = 1
  # Ratio of excretion for individuals in epidemiological stage IM (infectious with mild symptoms)
  if(is.null(rIS)) rIS = 1
  # Ratio of excretion for individuals in epidemiological stage IS (infectious with severe symptoms)


  if(is.null(ptestPSAsymp)) ptestPSAsymp = 1
  # probability to test symptomatic patients in the screening area
  if(is.null(ptestPSANI)) ptestPSANI = .75
  # probability to test NI patients in the screening area
  if(is.null(ptestPSALI)) ptestPSALI = 0.50
  # probability to test PI patients in the screening area
  if(is.null(ptestPSAHI)) ptestPSAHI = 0.1
  # probability to test FI patients in the screening area

  if(is.null(ptestPWsymp)) ptestPWsymp = 0.95
  # probability to test symptomatic patients in the ward
  if(is.null(ptestPWNI)) ptestPWNI = 0.75
  # probability to test NI patients in the ward
  if(is.null(ptestPWLI)) ptestPWLI = 0.50
  # probability to test PI patients in the ward
  if(is.null(ptestPWHI)) ptestPWHI = 0.10
  # probability to test FI patients in the ward

  if(is.null(ptestHsymp)) ptestHsymp = 0.85
  # probability to test symptomatic HCWS in the ward
  if(is.null(ptestHNI)) ptestHNI = 0.75
  # proportion of non immune professionals (healthcare workers) tested during screening testing
  if(is.null(ptestHLI)) ptestHLI = 0.50
  # probability to test PI HCWS
  if(is.null(ptestHHI)) ptestHHI = 0.20
  # probability to test FI HCWS

  if(is.null(senSA)) senSA = 0.85
  if(is.null(speSA)) speSA = 0.95
  if(is.null(senW)) senW = 0.85
  if(is.null(speW)) speW = 0.95
  if(is.null(senH)) senH = 0.85
  if(is.null(speH)) speH = 0.95
  if(is.null(sensymp)) sensymp = 0.85
  if(is.null(spesymp)) spesymp = 0.95
  }


  if(disease == "Influenza"){
    ##### Infection
    if(is.null(n_ctcH_PSA)) n_ctcH_PSA = 2
    if(is.null(t_ctcH_PSA)) t_ctcH_PSA = 10 / 60 / 24
    if(is.null(n_ctcP_PSA)) n_ctcP_PSA = 0
    if(is.null(t_ctcP_PSA)) t_ctcP_PSA = 5 / 60 / 24
    if(is.null(n_ctcH_PW)) n_ctcH_PW = 4
    if(is.null(t_ctcH_PW)) t_ctcH_PW = 15 / 60 / 24
    if(is.null(n_ctcP_PW)) n_ctcP_PW = 4
    if(is.null(t_ctcP_PW)) t_ctcP_PW = 30 / 60 / 24
    if(is.null(n_ctcH_H)) n_ctcH_H = 5
    if(is.null(t_ctcH_H)) t_ctcH_H = 3 / 60 / 24
    if(is.null(t_ctcV_PW)) t_ctcV_PW = 20 / 60 / 24
    if(is.null(I)) I = 185 / 100000
    if(is.null(d)) d = 10
    if(is.null(R0)) R0 = 13
    if(is.null(tw)) tw = 35
    # https://www.gouvernement.fr/info-coronavirus/carte-et-donnees
    if(is.null(tSA)) tSA  = 2 / 24
    # average duration before full admission (in screening area for clinical exam administrative procedure etc)
    if(is.null(tIC)) tIC  = 15
    # average duration of stay in intensive care
    if(is.null(tSL)) tSL  = 14
    # average duration of sick leave
    if(is.null(tESL)) tESL = 28
    # average duration of extended sick leave
    if(is.null(tE)) tE  = 5
    # duration epidemiological state E
    if(is.null(tEA)) tEA = 2
    # duration epidemiological state EA
    if(is.null(tES)) tES = 2
    # duration epidemiological state ES
    if(is.null(tIA)) tIA = 7
    # duration epidemiological state IA
    if(is.null(tIM)) tIM = 8
    # duration epidemiological state IM
    if(is.null(tIS)) tIS = 9
    # duration epidemiological state IS
    if(is.null(tLI)) tLI = 60
    # duration of partial immunity before return to non immune status
    if(is.null(tHI)) tHI = 150
    # duration of full immunity before return to partial immune status
    # patient protection
    if(is.null(epsPPSA)) epsPPSA = 0.1
    if(is.null(epsHPSA)) epsHPSA = 0.1
    if(is.null(epsHPW)) epsHPW = 0.1
    if(is.null(epsPPW)) epsPPW = 0.1
    if(is.null(epsVPW)) epsVPW = 0.1
    # healthcare workers protection
    if(is.null(epsPHSA)) epsPHSA = 0.1
    #from patient in SA
    if(is.null(epsPHW)) epsPHW = 0.1
    #from patient in W
    if(is.null(epsHHW)) epsHHW = 0.1
    #from HW in W
    ## Test in ward
    if(is.null(ttestSA)) ttestSA = 2 / 24
    # test duration in screening area
    if(is.null(ttestPW)) ttestPW = 2 / 24
    # test duration in ward for screening patients
    if(is.null(ttestHW)) ttestHW = 2 / 24
    # test duration in ward for screening professionals
    if(is.null(ttestsymp)) ttestsymp = 2 / 24
    # test duration for symptomatic

    if(is.null(tbtwtestP)) tbtwtestP = 14
    # duration between two tests for patient
    if(is.null(tbtwtestH)) tbtwtestH = 30
    # duration between two tests for HCWS

    if(is.null(tbeftestPsymp)) tbeftestPsymp = 2 / 24
    # duration before test of symp patient
    if(is.null(tbeftestHsymp)) tbeftestHsymp = 1
    # duration before test of symp HCWS

    if(is.null(psympNI)) psympNI = 0.9
    # probability to be symptomatic when non immune
    if(is.null(psympLI)) psympLI = 0.8
    # probability to be symptomatic when partially immune
    if(is.null(psympHI)) psympHI = 0.7
    # probability to be symptomatic when fully immune

    if(is.null(psevNI)) psevNI = 0.6
    # probability to develop severe symptoms when non immune
    if(is.null(psevLI)) psevLI = 0.5
    # probability to develop severe symptoms when partially immune
    if(is.null(psevHI)) psevHI = 0.4
    # probability to develop severe symptoms when fully immune

    if(is.null(pISO)) pISO = 1
    # contact restriction in case of positive test

    if(is.null(pSL)) pSL = 0.3
    # probability to take sick leave
    if(is.null(pESL)) pESL = 1
    # probability to take extended sick leave
    if(is.null(pSLT)) pSLT = 0.01
    # probability to take EL/ESL after positive test

    if(is.null(pIC)) pIC = 0.3
    # probability to be transfer in intensive care
    if(is.null(pdieIC)) pdieIC = 0.005
    # probability to die in intensive care

    ###################################
    if(is.null(pLI)) pLI = 0.20
    # probability to be PI at the admission (proportion of PI in the population)
    if(is.null(pHI)) pHI = 0.5
    # probability to be FI at the admission (proportion of FI in the population)
    if(is.null(hNI2LI)) hNI2LI = 1 / 30
    # daily probability to become partially immune
    if(is.null(hLI2HI)) hLI2HI = 1 / 60
    # daily probability to become fully immune

    if(is.null(rinfLI)) rinfLI = 0.7
    # partial immunity efficiency % FIX ME better explain that this is the ratio of reduction of probability to be infected compared to non immune
    if(is.null(rinfHI)) rinfHI = 0.5
    # partial immunity efficiency % FIX ME better explain that this is the ratio of reduction of probability to be infected compared to non immune

    if(is.null(rsymp)) rsymp = 1
    # Ratio adjusting probability of symptoms for patients compared to general population (professionals)
    if(is.null(rsev)) rsev = 1
    # Ratio adjusting probability of severity if symptoms for patients compared to general population (professionals)

    if(is.null(rEA)) rEA = 0.35
    # Ratio  of excretion for individuals in epidemiological stage EA (exposed - contagious pre-asymptomatic)
    if(is.null(rES)) rES = 1
    # Ratio of excretion for individuals in epidemiological stage ES (exposed - contagious pre-symptomatic)
    if(is.null(rIA)) rIA = 0.35
    # Ratio of excretion for individuals in epidemiological stage IA (infectious asymptomatic)
    if(is.null(rIM)) rIM = 1
    # Ratio of excretion for individuals in epidemiological stage IM (infectious with mild symptoms)
    if(is.null(rIS)) rIS = 1
    # Ratio of excretion for individuals in epidemiological stage IS (infectious with severe symptoms)


    if(is.null(ptestPSAsymp)) ptestPSAsymp = 1
    # probability to test symptomatic patients in the screening area
    if(is.null(ptestPSANI)) ptestPSANI = .75
    # probability to test NI patients in the screening area
    if(is.null(ptestPSALI)) ptestPSALI = 0.50
    # probability to test PI patients in the screening area
    if(is.null(ptestPSAHI)) ptestPSAHI = 0.1
    # probability to test FI patients in the screening area

    if(is.null(ptestPWsymp)) ptestPWsymp = 0.95
    # probability to test symptomatic patients in the ward
    if(is.null(ptestPWNI)) ptestPWNI = 0.75
    # probability to test NI patients in the ward
    if(is.null(ptestPWLI)) ptestPWLI = 0.50
    # probability to test PI patients in the ward
    if(is.null(ptestPWHI)) ptestPWHI = 0.10
    # probability to test FI patients in the ward

    if(is.null(ptestHsymp)) ptestHsymp = 0.85
    # probability to test symptomatic HCWS in the ward
    if(is.null(ptestHNI)) ptestHNI = 0.75
    # proportion of non immune professionals (healthcare workers) tested during screening testing
    if(is.null(ptestHLI)) ptestHLI = 0.50
    # probability to test PI HCWS
    if(is.null(ptestHHI)) ptestHHI = 0.20
    # probability to test FI HCWS

    if(is.null(senSA)) senSA = 0.85
    if(is.null(speSA)) speSA = 0.95
    if(is.null(senW)) senW = 0.85
    if(is.null(speW)) speW = 0.95
    if(is.null(senH)) senH = 0.85
    if(is.null(speH)) speH = 0.95
    if(is.null(sensymp)) sensymp = 0.85
    if(is.null(spesymp)) spesymp = 0.95
  }

  ### Checks
  ## Duration must be positive number
  ## Probabilities must be between 0 and 1

  # Proportion of tested in dividuals
  sapply(c(
    ptestPSAsymp,
    ptestPSANI,
    ptestPSALI,
    ptestPSAHI,
    ptestPWsymp,
    ptestPWNI,
    ptestPWLI,
    ptestPWHI,
    ptestHsymp,
    ptestHNI,
    ptestHLI,
    ptestHHI
  ), function(x)
    if (x < 0 | x > 1)
      stop(
        "Proportion of tested individuals must fall between 0 (nobody is tested) and 1 (all group is tested)."
      ))

  # Epidemiological stages durations
  sapply(c(tE, tEA, tES, tIA, tIM, tIS, tLI, tHI), function(x)
    if (x <= 0)
      stop(
        "Epidemiological stages and immunity durations must be positive numbers."
      ))

    if (d <= 0)
      stop(
        "Epidemiological stages and immunity durations must be positive numbers."
      )

    if (d > sum(tE, tEA, tES, tIA, tIM, tIS))
      stop(
        "Total disease duration (d) must be inferior or equal to the sum of durations of all epidemiological stages (tE, tEA, tES, tIA, tIM, tIS)."
      )

  sapply(c(rinfLI, rinfHI), function(x)
    if (x <= 0)
      stop(
        "Infection-reducing ratios related to immunity levels (rinfLI, rinfHI) must fall between 0 (no infection) and 1 (same probability to be infected that non immune)."
      ))

  if (pLI + pHI < 0 | pLI + pHI > 1)
    stop(
      "The sum of probabilities to carry immunity (pLI and pHI) must fall between 0 (only non immune individuals at the admission) and 1 (only immune individuals at the admission)."
    )

  # Screening testing of patients

  if (ptestPWNI > 0 | ptestPWLI > 0 | ptestPWHI > 0) {
    if (tbtwtestP <= 0)
      stop(
        "If one of the proportions of patients tested during screening testing is positive (ptestPWNI + ptestPWLI + ptestPWHI > 0), the average duration between two screening programs (tbtwtestP) must be positive"
      )

    if (ttestPW <= 0)
      stop(
        "If one of the proportions of patients tested during screening testing is positive (ptestPWNI + ptestPWLI + ptestPWHI > 0), the average duration from test to reaction to result (ttestPW) must be positive"
      )

    if (senW < 0 | senW > 1 | speW < 0 | speW > 1)
      stop(
        "If one of the proportions of patients tested during screening testing is positive (ptestPWNI + ptestPWLI + ptestPWHI > 0), the test sensibilities and specificities must fall between 0 and 1 (senW and speW)."
      )
  }

  if (tbtwtestP <= 0)
    tbtwtestP <- 1

  if (ttestPW <= 0)
    ttestPW <- 1


  # Screening testing of professionals
  if (ptestHNI > 0 | ptestHLI > 0 | ptestHHI > 0) {
    if (tbtwtestH <= 0)
      stop(
        "If one of the proportions of professionals tested during screening testing is positive (ptestHNI + ptestHLI + ptestHHI > 0), the average duration between two screening programs (tbtwtestH) must be positive"
      )

    if (ttestHW <= 0)
      stop(
        "If one of the proportions of professionals tested during screening testing is positive (ptestHNI + ptestHLI + ptestHHI > 0), the average duration from test to reaction to result (ttestH) must be positive"
      )

    if (senH < 0 | senH > 1 | speH < 0 | speH > 1)
      stop(
        "If one of the proportions of professionals tested during screening testing is positive (ptestHNI + ptestHLI + ptestHHI > 0), the test sensibilities and specificities must fall between 0 and 1 (senH and speH)."
      )

  }

  if (tbtwtestH <= 0)
    tbtwtestH <- 1
  if (ttestHW <= 0)
    ttestHW <- 1

  # Screening area at the admission
  if (ptestPSANI > 0 | ptestPSALI > 0 | ptestPSAHI > 0) {
    if (ttestSA <= 0)
      stop(
        "If one of the proportions of patients tested in the screening area is positive (ptestPSANI + ptestPSALI + ptestPSAHI > 0), the average duration from test to reaction to result (ttestSA) must be positive"
      )

    if (senSA < 0 | senSA > 1 | speSA < 0 | speSA > 1)
      stop(
        "If one of the proportions of patients tested in the screening area is positive (ptestPSANI + ptestPSALI + ptestPSAHI > 0), the test sensibilities and specificities must fall between 0 and 1 (senSA and speSA)."
      )

  }

  if (ttestSA <= 0)
    ttestSA <- 1


  if (tSA <= 0)
    stop("The duration of screening at the admission (tSA) must be positive (in days).")

  # Diagnostic testing
  if (ptestPSAsymp > 0 | ptestPWsymp > 0 | ptestHsymp > 0) {
    condi <-
      "If one of the proportions of individuals tested for diagnostic testing is positive (ptestPSAsymp + ptestPWsymp + ptestHsymp > 0),"
    if (ttestsymp <= 0)
      stop(
        paste(
          condi,
          "the average duration from test to reaction to result (ttestsymp) must be positive."
        )
      )

    if (tbeftestPsymp <= 0 | tbeftestHsymp <= 0)
      stop(
        paste(
          condi,
          "the average duration before suspicion (tbeftestPsymp and tbeftestHsymp) must be positive."
        )
      )

    if (sensymp < 0 | sensymp > 1 | spesymp < 0 | spesymp > 1)
      stop(
        "If one of the proportions of individuals tested for diagnostic testing is positive (ptestPSAsymp + ptestPWsymp + ptestHsymp > 0), the test sensibilities and specificities must fall between 0 and 1 (sensymp and spesymp)."
      )

  }

  if (ttestsymp <= 0)
    ttestsymp <- 1

  if (tbeftestPsymp <= 0)
    tbeftestPsymp <- 1

  if (tbeftestHsymp <= 0)
    tbeftestHsymp <- 1

  ## Contact restriction

  # if (!is.logical(pISO))
  #   stop(
  #     "pISO must be logical. TRUE: patients are isolated and contacts are restricted in case of a positive test, FALSE: contact are not restricted but test is counted."
  #   )

  ## Intensive care
  if (pIC < 0 | pIC > 1 | pdieIC < 0 | pdieIC > 1)
    stop(
      "The probability of being transfered and die in intensive care must fall between 0 (never transfered) and 1 (always transfered)."
    )

  if (pIC > 0 & tIC <= 0)
    stop("Duration of intensive care stay (tIC) must be positive.")

  ## Sick leave
  if (pSL < 0 | pSL > 1 | pESL < 0 | pESL > 1 | pSLT < 0 | pSLT > 1)
    stop(
      "The probabilities of taking sick leave (pSL, pESL, pSLT) must fall between 0 (never) and 1 (always)."
    )

  if (pSL + pESL + pSLT > 0 & (tSL <= 0 | tESL <= 0))
    stop("Duration of sick leave (tSL and tESL) must be positive.")

  ### Build gdata

  # p = R0 / (dCtc * nCtc * dInf)
  # where
  # p is the probability of transmission per minute spent in contact,
  # dCtc is the average contact duration (in minutes),
  # nCtc is the average number of contacts per person per day, and
  # dInf is the average duration of infectivity (in days): approximately 10 days for COVID-19 [10].

  bet01 <- function(x) {
    if (x < 0)
      x <- 0
    if (x > 1)
      x <- 1
    x
  }

  psympPNI <- psympNI * rsymp %>% bet01
  psevPNI <- psevNI * rsev %>% bet01

  psympPLI <- psympLI * rsymp %>% bet01
  psevPLI <- psevLI * rsev %>% bet01

  psympPHI <- psympHI * rsymp %>% bet01
  psevPHI <- psevHI * rsev %>% bet01

  gdata = c(
    tSA  = tSA,
    # average duration before full admission (in screening area for clinical exam, administrative procedure, etc)
    tIC  = tIC,
    # average duration of stay in intensive care
    tSL  = tSL,
    # average duration of sick leave
    tESL = tESL,
    # average duration of extended sick leave

    tE  = tE,
    # duration epidemiological state E
    tEA = tEA,
    # duration epidemiological state EA
    tES = tES,
    # duration epidemiological state ES
    tIA = tIA,
    # duration epidemiological state IA
    tIM = tIM,
    # duration epidemiological state IM
    tIS = tIS,
    # duration epidemiological state IS

    tLI = tLI,
    # duration of partial immunity before return to non immune status
    tHI = tHI,
    # duration of full immunity before return to partial immune status

    td = d,
    # disease duration

    ## Contacts
    ctcHPSA = n_ctcH_PSA  * t_ctcH_PSA,
    ctcPPSA = n_ctcP_PSA  * t_ctcP_PSA,

    ctcHPW = n_ctcH_PW  * t_ctcH_PW,
    ctcPPW = n_ctcP_PW  * t_ctcP_PW,
    ctcHH = n_ctcH_H  * t_ctcH_H,

    # n_ctcV_PW => see ldata
    ctcV = t_ctcV_PW,

    # Epsilon
    # patient protection
    epsPPSA = epsPPSA,
    epsHPSA = epsHPSA,
    epsHPW = epsHPW,
    epsPPW = epsPPW,
    epsVPW = epsVPW,

    # healthcare workers protection
    epsPHSA = epsPHSA,
    #from patient in SA
    epsPHW = epsPHW,
    #from patient in W
    epsHHW = epsHHW,
    #from HW in W

    ## Test in ward
    ttestSA = ttestSA,
    # test duration in screening area
    ttestPW = ttestPW,
    # test duration in ward for screening of patients
    ttestHW = ttestHW,
    # test duration in ward for screening of professionals
    ttestsymp = ttestsymp,
    # test duration for symptomatic

    tbtwtestP = tbtwtestP,
    # duration between two tests for patient
    tbtwtestH = tbtwtestH,
    # duration between two tests for HCWS

    tbeftestPsymp = tbeftestPsymp,
    # duration before test of symp patient
    tbeftestHsymp = tbeftestHsymp,
    # duration before test of symp HCWS

    # daily incidence in the community
    hinc = I /100000,
    # daily incidence in the community
    prev = hinc * d,
    # prevalence (probability to be infected at the admission)
    pconta = R0 / (8 * (30 / 60 / 24) * d),
    # https://doi.org/10.1093/cid/ciaa682
    # proportion of time outside of work
    ptow = 1 - tw / ((24 - 8) * 7),
    # 24 hours per days minus 8 sleeping hours time 7 days


    psympNI = psympNI,
    # probability to be symptomatic when non immune
    psympLI = psympLI,
    # probability to be symptomatic when partially immune
    psympHI = psympHI,
    # probability to be symptomatic when fully immune

    psevNI = psevNI,
    # probability to develop severe symptoms when non immune
    psevLI = psevLI,
    # probability to develop severe symptoms when partially immune
    psevHI = psevHI,
    # probability to develop severe symptoms when fully immune

    pISO = pISO,

    pSL = pSL,
    # probability to take sick leave
    pESL = pESL,
    # probability to take extended sick leave
    pSLT = pSLT,
    # probability to take EL/ESL after positive test

    pIC = pIC,
    # probability to be transfer in intensive care
    pdieIC = pdieIC,
    # probability to die in intensive care

    ###################################

    pLI = pLI,
    # probability to be PI at the admission (proportion of PI in the population)
    pHI = pHI,
    # probability to be FI at the admission (proportion of FI in the population)
    hNI2LI = hNI2LI,
    # daily probability to become partially immune
    hLI2HI = hLI2HI,
    # daily probability to become fully immune

    rinfLI = rinfLI,
    # partial immunity efficiency % FIX ME better explain that this is the ratio of reduction of probability to be infected compared to non immune
    rinfHI = rinfHI,
    # partial immunity efficiency % FIX ME better explain that this is the ratio of reduction of probability to be infected compared to non immune

    rEA = rEA,
    # Ratio  of excretion for individuals in epidemiological stage EA (exposed - contagious pre-asymptomatic)
    rES = rES,
    # Ratio of excretion for individuals in epidemiological stage ES (exposed - contagious pre-symptomatic)
    rIA = rIA,
    # Ratio of excretion for individuals in epidemiological stage IA (infectious asymptomatic)
    rIM = rIM,
    # Ratio of excretion for individuals in epidemiological stage IM (infectious with mild symptoms)
    rIS = rIS,
    # Ratio of excretion for individuals in epidemiological stage IS (infectious with severe symptoms)

    # probability of symptoms for patients compared to general population (professionals)
    psympPNI = psympPNI,
    psevPNI = psevPNI,

    psympPLI = psympPLI,
    psevPLI = psevPLI,

    psympPHI = psympPHI,
    psevPHI = psevPHI,

    ptestPSAsymp = ptestPSAsymp,
    # probability to test symptomatic patients in the screening area
    ptestPSANI = ptestPSANI,
    # probability to test NI patients in the screening area
    ptestPSALI = ptestPSALI,
    # probability to test PI patients in the screening area
    ptestPSAHI = ptestPSAHI,
    # probability to test FI patients in the screening area

    ptestPWsymp = ptestPWsymp,
    # probability to test symptomatic patients in the ward
    ptestPWNI = ptestPWNI,
    # probability to test NI patients in the ward
    ptestPWLI = ptestPWLI,
    # probability to test PI patients in the ward
    ptestPWHI = ptestPWHI,
    # probability to test FI patients in the ward

    ptestHsymp = ptestHsymp,
    # probability to test symptomatic HCWS in the ward
    ptestHNI = ptestHNI,
    # probability to test NI HCWS
    ptestHLI = ptestHLI,
    # probability to test PI HCWS
    ptestHHI = ptestHHI,
    # probability to test FI HCWS

    senSA = senSA,
    speSA = speSA,
    senW = senW,
    speW = speW,
    senH = senH,
    speH = speH,
    sensymp = sensymp,
    spesymp = spesymp
  )

  # Use psevNI >> worst case scenario
  with(data.frame(gdata %>% t),
       cumd <<-
         (1 - psympNI) * (tE + tEA + tIA) + psympNI * ((1 - psevNI) * (tE + tES + tIM) + psevNI * (tE + tES + tIS)))

  gdata <- c(gdata,
             with(
               data.frame(gdata %>% t),
               c(
                 rtE = tE / cumd,
                 # time ratio of the epidemiological state E over the whole infectious period (probability to be E at the admission when non susceptible)
                 rtEA = (1 - psympNI) * (tEA / cumd),
                 # time ratio of the epidemiological state EA over the whole infectious period (probability to be EA at the admission when non susceptible)
                 rtES = psympNI * (tES / cumd),
                 # time ratio of the epidemiological state ES over the whole infectious period (probability to be ES at the admission when non susceptible)
                 rtIA = (1 - psympNI) * (tIA / cumd),
                 # time ratio of the epidemiological state IA over the whole infectious period (probability to be IA at the admission when non susceptible)
                 rtIM = psympNI * (1 - psevNI) * (tIM / cumd),
                 # time ratio of the epidemiological state IM over the whole infectious period (probability to be IM at the admission when non susceptible)
                 rtIS = psympNI * psevNI * (tIS / cumd) # time ratio of the epidemiological state IS over the whole infectious period (probability to be IS at the admission when non susceptible)
               )
             ))


  return(gdata)

}
