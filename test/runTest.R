rm(list = ls())

######
######  Load packages
######

# library(igraph)
# library(magrittr)
# library(data.table)
# library(SimInf)
library(mwss)

######
######  initialization
######


# Number wards
nwards <- 5
# Ward names
ward_names <- letters[1:nwards]
# Number of patients per day
pop_size_P <- rep(20,nwards)
# Number of HCWS per day
pop_size_H <- rep(15,nwards)
# Number of visitors per day
nVisits <- rep(10,nwards)
# Length of stay
LS <- rep(27,nwards)


# toydata <- list(nwards = nwards,
#                ward_names = ward_names,
#                pop_size_P = pop_size_P,
#                pop_size_H = pop_size_H,
#                nVisits = nVisits,
#                LS = LS)

# save(nwards, file="data/nwards.rda")
# save(ward_names, file="data/ward_names.rda")
# save(pop_size_P, file="data/pop_size_P.rda")
# save(pop_size_H, file="data/pop_size_H.rda")
# save(nVisits, file="data/nVisits.rda")
# save(LS, file="data/LS.rda")

# save(toydata, file="data/toydata.rda")

# data("toydata")

# Generate random contact
matContact <- randomContacts(pop_size_H, ward_names)$contactMat / 100

gdata <- build_gdata(
  ##### Infection
  n_ctcH_PSA = 2,
  t_ctcH_PSA = 10/60/24,
  n_ctcP_PSA = 0,
  t_ctcP_PSA = 5/60/24,
  n_ctcH_PW = 4,
  t_ctcH_PW = 15/60/24,
  n_ctcP_PW = 4,
  t_ctcP_PW = 30/60/24,
  n_ctcH_H = 5,
  t_ctcH_H = 3/60/24,
  t_ctcV_PW = 20/60/24,
  I = 185/100000, # Daily incidence (https://www.gouvernement.fr/info-coronavirus/carte-et-donnees)
  d = 10,  # Average disease duration (days)
  R0 = 13, # Basic reproduction number - https://www.gouvernement.fr/info-coronavirus/carte-et-donnees
  tw = 35, # Average number of working hours per week (in hours)
  tSA  = 2/24, # average duration before full admission (in screening area for clinical exam, administrative procedure, etc)
  tIC  = 15,   # average duration of stay in intensive care
  tSL  = 14,   # average duration of sick leave
  tESL = 28,   # average duration of extended sick leave
  tE  = 5, # duration epidemiological state E
  tEA = 2, # duration epidemiological state EA
  tES = 2, # duration epidemiological state ES
  tIA = 7, # duration epidemiological state IA
  tIM = 8, # duration epidemiological state IM
  tIS = 9, # duration epidemiological state IS
  tLI = 60, # duration of partial immunity before return to non immune status
  tHI = 150, # duration of full immunity before return to partial immune status
  # patient protection
  epsPPSA = 0.1,
  epsHPSA = 0.1,
  epsHPW = 0.1,
  epsPPW = 0.1,
  epsVPW = 0.1,
  # healthcare workers protection
  epsPHSA = 0.1, #from patient in SA
  epsPHW = 0.1, #from patient in W
  epsHHW = 0.1, #from HW in W
  ## Test in ward
  ttestSA = 2/24, # test duration in screening area
  ttestPW = 2/24, # test duration in ward for screening for patients
  ttestHW = 2/24, # test duration in ward for screening for professionals
  ttestsymp = 2/24, # test duration for symptomatic

  tbtwtestP = 14, # duration between two tests for patient
  tbtwtestH = 30, # duration between two tests for HCWS

  tbeftestPsymp = 2/24, # duration before test of symp patient
  tbeftestHsymp = 1, # duration before test of symp HCWS

  psympNI = 0.5, # probability to be symptomatic when non immune
  psympLI = 0.2, # probability to be symptomatic when partially immune
  psympHI = 0.1, # probability to be symptomatic when fully immune

  psevNI = 0.5, # probability to develop severe symptoms when non immune
  psevLI = 0.3, # probability to develop severe symptoms when partially immune
  psevHI = 0.1, # probability to develop severe symptoms when fully immune

  pISO = 1, # probability, level of contact restriction if confirmed case

  pSL = 0.3, # probability to take sick leave
  pESL = 1, # probability to take extended sick leave
  pSLT = 0.01, # probability to take EL/ESL after positive test

  pIC = 0.3, # probability to be transfer in intensive care
  pdieIC = 0.005, # probability to die in intensive care

  ###################################
  pLI = 0.20, # probability to be PI at the admission (proportion of PI in the population)
  pHI = 0.5, # probability to be FI at the admission (proportion of FI in the population)
  hNI2LI = 1/30, # daily probability to become partially immune
  hLI2HI = 1/60, # daily probability to become fully immune

  rinfLI = 0.7, # partial immunity efficiency % FIX ME better explain that this is the ratio of reduction of probability to be infected compared to non immune
  rinfHI = 0.5, # partial immunity efficiency % FIX ME better explain that this is the ratio of reduction of probability to be infected compared to non immune

  rsymp = 1, # Ratio adjusting probability of symptoms for patients compared to general population (professionals)
  rsev = 1, # Ratio adjusting probability of severity if symptoms for patients compared to general population (professionals)

  rEA = 0.35, # Ratio  of excretion for individuals in epidemiological stage EA (exposed - contagious pre-asymptomatic)
  rES = 1, # Ratio of excretion for individuals in epidemiological stage ES (exposed - contagious pre-symptomatic)
  rIA = 0.35,# Ratio of excretion for individuals in epidemiological stage IA (infectious asymptomatic)
  rIM = 1, # Ratio of excretion for individuals in epidemiological stage IM (infectious with mild symptoms)
  rIS = 1, # Ratio of excretion for individuals in epidemiological stage IS (infectious with severe symptoms)

  ptestPSAsymp = 1, # probability to test symptomatic patients in the screening area
  ptestPSANI = .75,  # probability to test NI patients in the screening area
  ptestPSALI = 0.50, # probability to test PI patients in the screening area
  ptestPSAHI = 0.1, # probability to test FI patients in the screening area

  ptestPWsymp = 0.95, # probability to test symptomatic patients in the ward
  ptestPWNI = 0.75,# probability to test NI patients in the ward
  ptestPWLI = 0.50, # probability to test PI patients in the ward
  ptestPWHI = 0.10, # probability to test FI patients in the ward

  ptestHsymp = 0.85, # probability to test symptomatic HCWS in the ward
  ptestHNI = 0.75, # probability to test NI HCWS
  ptestHLI = 0.50, # probability to test PI HCWS
  ptestHHI = 0.20, # probability to test FI HCWS

  senSA = 0.85,
  speSA = 0.95,
  senW = 0.85,
  speW = 0.95,
  senH = 0.85,
  speH = 0.95,
  sensymp = 0.85,
  spesymp = 0.95
)

plot_connectivity(matContact, pop_size_P, verbose = FALSE)

# initial vector

u0 <- startvec(ward_names,
              pop_size_P,
              pop_size_H,
              nVisits,
              LS,
              matContact = matContact)$u0

# with screening area

model <- mwss(ward_names,
              pop_size_P,
              pop_size_H,
              nVisits,
              LS,
              gdata = gdata,
              matContact = matContact,
              IMMstate = NULL,
              EPIstate = NULL,
              SA = TRUE,
              nH_SA = 3,
              tSim =  365,
              verbose = FALSE)

trajmwss <- multisim(model, 50, ward_names)
class(trajmwss)

# without screening area☻

model <- mwss(ward_names,
              pop_size_P,
              pop_size_H,
              nVisits,
              LS,
              gdata = gdata,
              matContact = matContact,
              IMMstate = NULL,
              EPIstate = NULL,
              SA = FALSE,
              tSim =  365,
              verbose = FALSE)

trajmwss <- multisim(model, 50, ward_names)
class(trajmwss)

outputsummary <- keyoutput(trajmwss,
                         scale = 0,
                         focus = NULL)

outputsummary <- keyoutput(trajmwss,
                         scale = 1,
                         focus = NULL,
                         outb_Thhold = 0)

# percentage of simulation with at least 5 nosocomial infections
keyoutput(trajmwss,
        scale = 1,
        focus = "infections",
        outb_Thhold=5)$p_outbreak

################
keyoutput(trajmwss,
        scale = 0,
        focus = "infections")$P$quantiles_noso[["50%"]]

keyoutput(trajmwss,
        scale = 0,
        focus = "infections")$H$quantiles_noso[["50%"]]

keyoutput(trajmwss,
        scale = 0,
        focus = "test")$quantilesP[["50%"]]

keyoutput(trajmwss,
        scale = 0,
        focus = "test")$quantilesH[["50%"]]

keyoutput(trajmwss,
        scale = 0,
        focus = "incidence")$incidence[, incPS] %>%  median

keyoutput(trajmwss,
        scale = 0)$ISO$quantiles[["50%"]]

keyoutput(trajmwss,
        scale = 0)$SL$quantiles[["50%"]]

################

plot_pOutbreak(trajmwss,
               pop_size_P,
               matContact,
               outb_Thhold = 2,
               maxcolors = FALSE,
               addtitle = TRUE,
               verbose = FALSE)

plot_testcount(trajmwss, daysint = 30, iter = FALSE)
plot_testcount(trajmwss, daysint = 7, iter = 1, scale = 1)
plot_testcount(trajmwss, daysint = 7, iter = 1, scale = 0)
plot_testcount(trajmwss, daysint = 7, iter = 1, ward = "a", scale = 1)
plot_testcount(trajmwss, daysint = 30, iter = 1, ward = "a", scale = 1, pop = "P")
plot_testcount(trajmwss, daysint = 30, iter = 1, ward = "a", scale = 1, pop = "H")
plot_testcount(trajmwss, daysint = 2, iter = 3, ward = "a", scale = 1)
# daysint = 1, iter = FALSE, ward = FALSE, pop = NULL, scale = 0

plot_incidence(trajmwss, scale = 0, iter = FALSE, display_sd = F)
plot_incidence(trajmwss, scale = 1, pop = "Hsymp", iter = FALSE, ward = FALSE, display_sd = F)
plot_incidence(trajmwss, scale = 1, pop = "Hsymp", iter = FALSE, ward = FALSE, display_sd = T)
plot_incidence(trajmwss, scale = 1, pop = "P", ward = "a", display_sd = T)
plot_incidence(trajmwss, scale = 1, pop = "P", iter = 5, ward = "a", display_sd = T)

res <- trajmwss[[1]]
setDT(res)
res[time == 365]
# with screening area
res[wpropInfHorig > 0 | wpropInfHdest > 0 | wpropInfPSAdest > 0 | wpropInfPWdest > 0]
# without screening area
res[wpropInfHorig > 0 | wpropInfHdest > 0 | wpropInfPWdest > 0]

# max number of beds in contact restriction
res[, sum(ISO), by = time][, max(V1)]

# max number of HW in sick leave at the same time
res[, sum(SL+ESL), by = time][, max(V1)]

P <- prevalence(run(model), PW_E_NI + PW_EA_NI + PW_ES_NI + PW_IA_NI + PW_IM_NI + PW_IS_NI ~ PW_S_NI + PW_E_NI + PW_EA_NI + PW_ES_NI + PW_IA_NI + PW_IM_NI + PW_IS_NI)
data.table::setDT(P)
P

plot_nosoHazard(trajmwss,
           ward_names,
           pop_size_P,
           LS,
           matContact,
           addtitle = FALSE,
           verbose = FALSE)

