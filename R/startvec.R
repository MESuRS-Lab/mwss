#' Build initial population vector
#'
#' @description \code{startvec} returns two objects: u0, a data.frame of initial demographic, epidemiological and immunity
#' states of the population, and ldata: matrix with local (ward-level) parameters required by SimInf package.
#' ldata contains for each ward the proportion of time spent by professionals of the ward X in the different wards (\code{H_dest_X}) and
#' the origin ward proportions for the professionals acting in the ward X (\code{H_origine_X}). In a disconnected
#' facility, \code{H_dest_X} and \code{H_origin_X} will be equal to 1 (professionals of the ward only work in ward X
#' and all professionals working in the ward X are affiliated to ward X).
#' \code{nP} the number of beds in each ward, \code{nV} the average number of visit per patient per day,
#' \code{tLS} the average length of stay in each ward.
#' \code{u0} describes the distribution of patients and professionals in the
#' different wards and compartments (compartments are detailed in the vignette).
#' By default all patients and professionals are susceptible and non immune.
#' Other states can be specified using \code{IMMstate} and \code{EPIstate}.
#'
#' @usage startvec(ward_names, pop_size_P, pop_size_H, nVisits, LS,
#'           matContact = NULL, IMMstate = NULL, EPIstate = NULL,
#'           SA = FALSE, nH_SA = NULL, verbose = TRUE)
#'
#' @param ward_names String vector. Contains ward ids.
#' @param pop_size_P Numerical vector. Contains the number of beds per ward (ordered as ward_names).
#' @param pop_size_H Numerical vector. Contains the number of professionals affiliated to each ward (ordered as ward_names).
#' @param nVisits Numerical vector. Contains the average daily numbers of visits per ward (ordered as ward_names).
#' @param LS Numerical vector. Contains the average length of stay per ward (in days, ordered as ward_names).
#' @param matContact Square matrix. Contains the average proportion of time (%) spent in each ward (column) by professionals of each ward (row).
#' Wards order in rows and columns must be ordered as ward_names. Row sums should be equal to 1. Use \code{randomContacts} to generate an example.
#' Default is NULL (wards are disconnected; professionals are not shared between wards).
#' @param IMMstate Data.frame. Contains 4 columns specifying immunity of patients and professionals:
#' \code{ward}: defines the ward as identified in \code{ward_names} vector.
#' \code{pop}: defines the subpopulation either patients ("P") or professionals ("H").
#' \code{imm}: defines the immunity level either non immunity ("NI"), low immunity ("LI") or high immunity ("HI").
#' \code{n}: defines the number of individuals in that state.
#' Default is NULL (all individuals are considered non immune).
#' @param EPIstate Data.frame. Contains 5 columns specifying epidemiological state for patients and professionals and any subpopulation:
#' \code{ward}: defines the ward as identified in \code{ward_names} vector.
#' \code{pop}: defines the subpopulation either patients ("P") or professionals ("H").
#' \code{imm}: defines the immunity level either non immunity ("NI"), low immunity ("LI") or high immunity ("HI").
#' \code{epi}: defines the epidemiological state: incubating non infectious ("E"), incubating infectious future asymptomatic ("EA"), incubating infectious future symptomatic ("ES"), infectious asymptomatic ("IA"), infectious with mild symptoms ("IM"), infectious with severe symptoms ("IS").
#' \code{n}: defines the number of individuals in that state.
#' Default is NULL (all individuals are considered susceptible).
#' @param SA Logical. Activate the implementation of a screening area at the admission: contact are restricted (no contact with other patients of the ward) before clinical diagnostic and test.
#' Default is FALSE.
#' @param nH_SA Numerical vector. Contains the number of professionals in charge of admissions in the screening area when implemented (\code{SA} = TRUE).
#' @param verbose Logical. Activate messages display.
#'
#' @importFrom data.table setDT
#' @importFrom magrittr '%<>%'
#' @importFrom magrittr add
#'
#' @return List
#'
#' @examples
#' data("toydata")
#' list2env(toydata,envir=.GlobalEnv)
#' gdata <- build_gdata()
#'
#' matContact <- randomContacts(pop_size_H, ward_names)$contactMat
#'
#' IMMstate <- data.frame(
#' ward = c("a", "c", "b"),
#' pop = c("P","P", "H"),
#' imm = c("LI", "HI", "HI"),
#' n = c(3, 2, 10)
#' )
#' EPIstate <- data.frame(
#' ward = c("b", "d"),
#' pop = c("P","H"),
#' imm = c("NI", "NI"),
#' epi = c("E", "E"),
#' n = c(1, 1)
#' )
#'
#' ## Disconnected wards all susceptible and non immune, no screening area at the admission
#' startvec(ward_names, pop_size_P, pop_size_H, nVisits, LS,
#'          matContact = NULL, IMMstate = NULL, EPIstate = NULL,
#'          SA = FALSE, nH_SA = NULL, verbose = FALSE)
#'
#' ## Connected wards all susceptible and non immune, no screening area at the admission
#' startvec(ward_names, pop_size_P, pop_size_H, nVisits, LS,
#'          matContact = matContact, IMMstate = NULL, EPIstate = NULL,
#'          SA = FALSE, nH_SA = NULL, verbose = FALSE)
#'
#' ## Connected wards all susceptible but with immune individuals, no screening area at the admission
#' startvec(ward_names, pop_size_P, pop_size_H, nVisits, LS,
#'          matContact = matContact, IMMstate = IMMstate, EPIstate = NULL,
#'          SA = FALSE, nH_SA = NULL, verbose = FALSE)
#'
#' ## Connected wards with immune and infected individuals, no screening area at the admission
#' startvec(ward_names, pop_size_P, pop_size_H, nVisits, LS,
#'          matContact = matContact, IMMstate = IMMstate, EPIstate = EPIstate,
#'          SA = FALSE, nH_SA = NULL, verbose = FALSE)
#'
#' ## Connected wards with immune and infected individuals, including the implementation of a
#' ## screening area at the admission with one professional in charge of admission per ward
#' startvec(ward_names, pop_size_P, pop_size_H, nVisits, LS,
#'          matContact = matContact, IMMstate = IMMstate, EPIstate = EPIstate,
#'          SA = TRUE, nH_SA = rep(1, 5), verbose = FALSE)
#'
#' @export

startvec <- function(ward_names,
                     pop_size_P,
                     pop_size_H,
                     nVisits,
                     LS,
                     matContact = NULL,
                     IMMstate = NULL,
                     EPIstate = NULL,
                     SA = FALSE,
                     nH_SA = NULL,
                     verbose = TRUE) {
  # Checks.
  # vector format
  if (!is.vector(ward_names) |
      !is.vector(pop_size_P) |
      !is.vector(pop_size_H) | !is.vector(nVisits) | !is.vector(LS))
    stop("ward_names, pop_size_P, pop_size_H, nVisits and LS must be vectors.")

  # logical format
  if (!is.logical(SA))
    stop("SA must be logical")

  # vector lengths
  if (!identical(
    length(ward_names),
    length(pop_size_P),
    length(pop_size_H),
    length(LS),
    length(nVisits)
  )) {
    stop("ward_names, pop_size_P, pop_size_H, nVisits and LS vectors must be of equal length.")
  }

  if (!is.null(IMMstate)) {
    if (NA %in% match(IMMstate$pop, c("P", "H")))
      stop("IMMstate population column can only contains 'p' or 'h'")

    if (NA %in% match(IMMstate$imm  , c("NI", "LI", "HI")))
      stop("IMMstate immunity column can only contains 'NI', 'LI' or 'HI'")

    if (NA %in% match(IMMstate$ward, ward_names))
      stop("IMMstate ward column can only contains ward names present in the ward_names vector")
  }

  if (!is.null(EPIstate)) {
    if (NA %in% match(EPIstate$pop, c("P", "H")))
      stop("EPIstate population column can only contains 'p' or 'h'")

    if (NA %in% match(EPIstate$imm  , c("NI", "LI", "HI")))
      stop("EPIstate immunity column can only contains 'NI', 'LI' or 'HI'")

    if (NA %in% match(EPIstate$ward, ward_names))
      stop("EPIstate ward column can only contains ward names present in the ward_names vector")


    if (NA %in% match(EPIstate$epi  , c("S", "E", "EA", "ES", "IA", "IM", "IS")))
      stop(
        "In EPIstate, the currently implemented epidemiological states are: susceptible ('S'), Incubating not contagious ('E'), Incubating contagious to become asymptomatic ('EA'), Incubating contagious to become symptomatic ('ES'), Infectious asymptomatic ('IA'), Infectious symptomatic with mild symptoms ('IM'), Infectious symptomatic with severe symptoms ('IS')"
      )

    if (TRUE %in% (c("LI", "HI") %in% EPIstate$imm)){
      if (is.null(IMMstate))
        stop(
          "Immunity states described in EPIstate must be included in IMMstate."
        )
    }

  }

  # Check nH_SA if SA is TRUE
  if (isTRUE(SA)) {
    if (!length(nH_SA) %in% c(1, length(ward_names)))
      stop(
        "nH_SA must be either a unique value, commun to all wards, or one value per ward. The vector length must be 1 or identical to wards names vector length."
      )
    if (FALSE %in% (nH_SA > 0))
      stop("nH_SA must be positive value (>0).")
  }

  if (isTRUE(verbose))
    message("Warning: ward_names, pop_size_P, pop_size_H and LS vectors must be in the same order.")

  compartments <-
    expand.grid(
      pop = c("PW", "H"),
      epistate = c("S", "E", "EA", "ES", "IA", "IM", "IS"),
      IMMstate = c("NI", "LI", "HI")
    )

  if (isTRUE(SA))
    compartments <-
    expand.grid(
      pop = c("PSA", "PW", "H"),
      epistate = c("S", "E", "EA", "ES", "IA", "IM", "IS"),
      IMMstate = c("NI", "LI", "HI")
    )

  setDT(compartments)

  compartments %<>% .[order(pop, epistate, IMMstate)]
  compartments %<>% .[, paste(pop, epistate, sep = "_") %>% paste(., IMMstate, sep = "_")]

  compartments <-
    c(compartments, paste0(compartments[!grepl("PSA",compartments)], "_T"))

  compartments %<>% c(
    .,
    c(
      "IC",
      "SL",
      "ESL",
      "nTestP",
      "nTestH",
      "infP",
      "infH",
      "infHout",
      "incPA",
      "incPM",
      "incPS",
      "incHA",
      "incHM",
      "incHS",
      "adm",
      "admE",
      "admEA",
      "admES",
      "admIA",
      "admIM",
      "admIS"
    )
  )

  # build matrix
  xstart <- matrix(0,
                   ncol = length(compartments),
                   nrow = length(ward_names))

  # add row and column names
  rownames(xstart) <- ward_names
  colnames(xstart) <- compartments

  # Set population size
  xstart[, "PW_S_NI"] <- pop_size_P
  xstart[, "H_S_NI"] <- pop_size_H

  # update immunity levels based on IMMstate
  if (!is.null(IMMstate))
    for (row in IMMstate %>% nrow %>% seq) {
      row %<>% IMMstate[., ]

      if (row[["pop"]] == "P") {
        xstart[row[["ward"]], paste0(c("PW", "S", "NI"), collapse = "_")] %<>% subtract(as.numeric(row[["n"]]))
        xstart[row[["ward"]], paste0(c("PW", "S", row[["imm"]]), collapse = "_")] %<>% add(as.numeric(row[["n"]]))
      }
      if (row[["pop"]] == "H") {
        xstart[row[["ward"]], paste0(c("H", "S", "NI"), collapse = "_")] %<>% subtract(as.numeric(row[["n"]]))
        xstart[row[["ward"]], paste0(c("H", "S", row[["imm"]]), collapse = "_")] %<>% add(as.numeric(row[["n"]]))
      }

      if (TRUE %in% (xstart < 0))
        stop(
          paste(
            "In IMMState, the number of individuals with non null immunity level cannot be superior to the population size. See ward",
            row[["ward"]]
          )
        )

    }

  # update epidemiological levels based on EPIstate
  if (!is.null(EPIstate))
    for (row in EPIstate %>% nrow %>% seq) {
      row %<>% EPIstate[., ]


      if (row[["pop"]] == "P") {
        xstart[row[["ward"]], paste0(c("PW", "S", row[["imm"]]), collapse = "_")] %<>% subtract(as.numeric(row[["n"]]))
        xstart[row[["ward"]], paste0(c("PW", row[["epi"]], row[["imm"]]), collapse = "_")] %<>% add(as.numeric(row[["n"]]))
      }
      if (row[["pop"]] == "H") {
        xstart[row[["ward"]], paste0(c("H", "S", row[["imm"]]), collapse = "_")] %<>% subtract(as.numeric(row[["n"]]))
        xstart[row[["ward"]], paste0(c("H", row[["epi"]], row[["imm"]]), collapse = "_")] %<>% add(as.numeric(row[["n"]]))
      }

      if (TRUE %in% (xstart < 0))
        stop(
          paste(
            "In EPIstate, the number of non susceptible individuals cannot be superior to the population size of a specific compartment.
            Immunity states described in EPIstate must be included in IMMstate.
            See ward",row[["ward"]], "."
          )
        )

    }

  u0 <- xstart

  # set local data/parameters
  ldata <- matrix(
    data = 0,
    nrow = 4L,
    ncol = nrow(xstart),
    dimnames = list(c("nP", "tLS", "nV", "nH_SA"),
                    NULL)
  )

  if (isTRUE(SA)) {
    if (is.null(nH_SA))
      ldata["nH_SA", ] <- pop_size_H
    else
      ldata["nH_SA", ] <- nH_SA

  } else
    ldata %<>% .[rownames(.) != "nH_SA", ]

  ldata["nP", ] <- pop_size_P
  # Number of visit per patient per day
  ldata["nV", ] <- nVisits / pop_size_P
  # Daily turnover
  ldata["tLS", ] <- LS

  if (is.null(matContact)){
    matContact <- rbind(diag(1, nrow(u0)), diag(1, nrow(u0)))
  } else {
    HinW <- t(matContact) %>% apply(., 2, function(x)x / sum(x))
    PwithHfromW <- apply(matContact, 2, function(x) x / sum(x))
    matContact <- rbind(HinW, PwithHfromW)
  }

  rownames(matContact) <-
    c(paste0("H_dest_", ward_names),
      paste0("H_orig_", ward_names))

  ldata %<>% rbind(matContact, .)

  colnames(ldata) <- ward_names

  return(list(u0 = u0,
              ldata = ldata))
}
