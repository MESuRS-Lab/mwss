#' Write pts_fun
#'
#' @description Internal function used by \code{mwss} function to write the pts_fun code driving SimInf package to implement ward connections through professionals activities.
#'  During simulation weighted proportion of infected in specific subpopulations will be calculated and stored in v0 at the end of each time step.
#'
#' @usage build_pts_fun(u0, SA, v0, gdata)
#'
#' @return String containing C code used as pts_fun by SimInf package
#'
#' @keywords internal
#' @noRd

build_pts_fun <- function(u0, SA, v0, gdata){

  nwards <- nrow(u0)
  nCpmt <- ncol(u0)
  nvvar <- ncol(v0)

  # infected patients in the screening area contributing to spread
  if(SA){
    # daily number of infectious individuals admitted  =
    # total number of infectious individuals admitted since the beginning of the simulation
    # minus the count of the previous day
    infMPSA =
      paste0(
        "rEA * (", paste0("u_0[",which(colnames(u0) == "admEA")-1, "+i*nCpmt] - v[", grep('admEA_',colnames(v0)) %>% min %>% subtract(1),"+i*7]"), ") + ",
        "rES * (", paste0("u_0[",which(colnames(u0) == "admES")-1, "+i*nCpmt] - v[", grep('admES_',colnames(v0)) %>% min %>% subtract(1),"+i*7]"), ") + ",
        "rIA * (", paste0("u_0[",which(colnames(u0) == "admIA")-1, "+i*nCpmt] - v[", grep('admIA_',colnames(v0)) %>% min %>% subtract(1),"+i*7]"), ") + ",
        "rIM * (", paste0("u_0[",which(colnames(u0) == "admIM")-1, "+i*nCpmt] - v[", grep('admIM_',colnames(v0)) %>% min %>% subtract(1),"+i*7]"), ") + ",
        "rIS * (", paste0("u_0[",which(colnames(u0) == "admIS")-1, "+i*nCpmt] - v[", grep('admIS_',colnames(v0)) %>% min %>% subtract(1),"+i*7]"), ")"
      )
    totMPSA = paste0("u_0[",which(colnames(u0) == "adm")-1, "+i*nCpmt] - v[", which(startsWith(colnames(v0), 'adm_')) %>% min %>% subtract(1),"+i*7]")
  }

  # infected patients contributing to spread
    infMWp  =
      paste0(
        paste0("rEA * u_0[",which(
          grepl("PW_", colnames(u0)) & grepl("_EA_", colnames(u0)) & ! endsWith(colnames(u0), "_T")
        ) %>% subtract(1), "+i*nCpmt]", collapse = " + "), " + ",
        paste0("rES * u_0[",which(
          grepl("PW_", colnames(u0)) & grepl("_ES_", colnames(u0)) & ! endsWith(colnames(u0), "_T")
        ) %>% subtract(1), "+i*nCpmt]", collapse = " + "), " + ",
        paste0("rIA * u_0[",which(
          grepl("PW_", colnames(u0)) & grepl("_IA_", colnames(u0)) & ! endsWith(colnames(u0), "_T")
        ) %>% subtract(1), "+i*nCpmt]", collapse = " + "), " + ",
        paste0("rIM * u_0[",which(
          grepl("PW_", colnames(u0)) & grepl("_IM_", colnames(u0)) & ! endsWith(colnames(u0), "_T")
        ) %>% subtract(1), "+i*nCpmt]", collapse = " + "), " + ",
        paste0("rIS * u_0[",which(
          grepl("PW_", colnames(u0)) & grepl("_IS_", colnames(u0)) & ! endsWith(colnames(u0), "_T")
        ) %>% subtract(1), "+i*nCpmt]", collapse = " + "), " + (1 - pISO) * (",
        paste0("rEA * u_0[",which(
          grepl("PW_", colnames(u0)) & grepl("_EA_", colnames(u0)) & endsWith(colnames(u0), "_T")
        ) %>% subtract(1), "+i*nCpmt]", collapse = " + "), " + ",
        paste0("rES * u_0[",which(
          grepl("PW_", colnames(u0)) & grepl("_ES_", colnames(u0)) & endsWith(colnames(u0), "_T")
        ) %>% subtract(1), "+i*nCpmt]", collapse = " + "), " + ",
        paste0("rIA * u_0[",which(
          grepl("PW_", colnames(u0)) & grepl("_IA_", colnames(u0)) & endsWith(colnames(u0), "_T")
        ) %>% subtract(1), "+i*nCpmt]", collapse = " + "), " + ",
        paste0("rIM * u_0[",which(
          grepl("PW_", colnames(u0)) & grepl("_IM_", colnames(u0)) & endsWith(colnames(u0), "_T")
        ) %>% subtract(1), "+i*nCpmt]", collapse = " + "), " + ",
        paste0("rIS * u_0[",which(
          grepl("PW_", colnames(u0)) & grepl("_IS_", colnames(u0)) & endsWith(colnames(u0), "_T")
        ) %>% subtract(1), "+i*nCpmt]", collapse = " + ")," )"
      )

  # infected healthcare workers contributing to spread
  infMWh  =
    paste0(
      "rEA * (", paste0("u_0[",which(
        grepl("H_", colnames(u0)) & grepl("_EA_", colnames(u0))
      ) %>% subtract(1), "+i*nCpmt]", collapse = " + "), ") + ",
      "rES * (", paste0("u_0[",which(
        grepl("H_", colnames(u0)) & grepl("_ES_", colnames(u0))
      ) %>% subtract(1), "+i*nCpmt]", collapse = " + "), ") + ",
      "rIA * (", paste0("u_0[",which(
        grepl("H_", colnames(u0)) & grepl("_IA_", colnames(u0))
      ) %>% subtract(1), "+i*nCpmt]", collapse = " + "), ") + ",
      "rIM * (", paste0("u_0[",which(
        grepl("H_", colnames(u0)) & grepl("_IM_", colnames(u0))
      ) %>% subtract(1), "+i*nCpmt]", collapse = " + "), ") + ",
      "rIS * (", paste0("u_0[",which(
        grepl("H_", colnames(u0)) & grepl("_IS_", colnames(u0))
      ) %>% subtract(1), "+i*nCpmt]", collapse = " + "), ")"
    )

  # Total number of non isolated patients
  totMWp  =
    paste("(",
    paste0("u_0[",which(grepl("PW_", colnames(u0))&
                                 ! endsWith(colnames(u0), "_T")) %>% subtract(1), "+i*nCpmt]", collapse = "+"),
  "+ (1 - pISO) * (",
  paste0("u_0[",which(grepl("PW_", colnames(u0))& endsWith(colnames(u0), "_T")) %>% subtract(1), "+i*nCpmt]", collapse = "+"), "))")
  # Total number of healthcare workers
  totMWh  = paste0("u_0[",grep("H_", colnames(u0)) %>% subtract(1), "+i*nCpmt]", collapse = "+")

  if(isTRUE(SA))
pts_fun <- paste0("
const int nW = (int) ",nwards, "L;
const int nCpmt = (int)",nCpmt, "L;
const int nvvar = (int)",nvvar, "L;
const int rEA = (int)",gdata[["rEA"]], "L;
const int rES = (int)",gdata[["rES"]], "L;
const int rIA = (int)",gdata[["rIA"]], "L;
const int rIM = (int)",gdata[["rIM"]], "L;
const int rIS = (int)",gdata[["rIS"]], "L;
const int pISO = (int)",as.numeric(gdata[["pISO"]]), "L;
const int npropinf = (int) 3L;
unsigned int i = 0;
double infMPSA, infMWp, infMWh, totMPSA, totMWp, totMWh, wpropInfHorig, wpropInfHdest, wpropInfPSAdest, wpropInfPWdest = 0.00;
double pinf[",nwards * 3,"] = {0};
const int * u_0 = &u[-node*nCpmt];

// for loop terminates after screening all wards
for(i = 0; i < nW ; i++){
// count ind in ward i
  infMPSA = (",infMPSA,");
  infMWp  = (",infMWp,");
  infMWh  = (",infMWh,");

  totMPSA  = (",totMPSA,");
  totMWp  = (",totMWp,");
  totMWh  = (",totMWh,");

// if at least 1 patient in screening area
  if(totMPSA > 0 ){
  pinf[0 + i * npropinf] = infMPSA/totMPSA;
  }

// if at least 1 patient in ward
if(totMWp > 0 ){
  pinf[1 + i * npropinf] = infMWp/totMWp;
  }

// if at least 1 professional in ward
  if(totMWh > 0 ){
  pinf[2 + i * npropinf] = infMWh/totMWh;
  }

}

// prop for each ward are define in pinf

  wpropInfPSAdest = 0;
  wpropInfPWdest = 0;
  wpropInfHorig = 0;
  wpropInfHdest = 0;

  for(i = 0; i < nW; ++i){
  wpropInfPSAdest += ldata[i] * pinf[0 + i*npropinf];
  wpropInfPWdest += ldata[i] * pinf[1 + i*npropinf];
  wpropInfHorig += ldata[i + nW] * pinf[2 + i*npropinf];
  wpropInfHdest += ldata[i] * pinf[2 + i*npropinf];
  }

  v_new[0] = wpropInfHorig;
  v_new[1] = wpropInfHdest;
  v_new[2] = wpropInfPSAdest;
  v_new[3] = wpropInfPWdest;

  for(i = 0; i < nW; ++i){
  v_new[4 + i*7] = u_0[",which(colnames(u0) == "adm")-1,"+ i*nCpmt];
  v_new[5 + i*7] = u_0[",which(colnames(u0) == "admE")-1,"+ i*nCpmt];
  v_new[6 + i*7] = u_0[",which(colnames(u0) == "admEA")-1,"+ i*nCpmt];
  v_new[7 + i*7] = u_0[",which(colnames(u0) == "admES")-1,"+ i*nCpmt];
  v_new[8 + i*7] = u_0[",which(colnames(u0) == "admIA")-1,"+ i*nCpmt];
  v_new[9 + i*7] = u_0[",which(colnames(u0) == "admIM")-1,"+ i*nCpmt];
  v_new[10 + i*7] = u_0[",which(colnames(u0) == "admIS")-1,"+ i*nCpmt];
  }

return 1;
") else
  pts_fun <- paste0("
const int nW = (int) ", nwards, "L;
const int nCpmt = (int)", nCpmt, "L;
const int rEA = (int)",gdata[["rEA"]], "L;
const int rES = (int)",gdata[["rES"]], "L;
const int rIA = (int)",gdata[["rIA"]], "L;
const int rIM = (int)",gdata[["rIM"]], "L;
const int rIS = (int)",gdata[["rIS"]], "L;
const int pISO = (int)",as.numeric(gdata[["pISO"]]), "L;
const int npropinf = (int) 2L;
unsigned int i = 0;
double infMWp, infMWh, totMWp, totMWh, wpropInfHorig, wpropInfHdest, wpropInfPWdest = 0.00;
double pinf[", nwards * 2,"] = {0};
const int * u_0 = &u[-node*nCpmt];

// set pinf content to 0
// memset(pinf, 0, nW * npropinf * sizeof(double));

// for loop terminates after screening all wards
for(i = 0; i < nW ; i++){

// count ind in ward i
  infMWp  = (",infMWp,");
  infMWh  = (",infMWh,");

  totMWp  = (",totMWp,");
  totMWh  = (",totMWh,");

// if at least 1 patient in ward
if(totMWp > 0 ){
  pinf[0 + i * npropinf] = infMWp/totMWp;
  }

// if at least 1 professional in ward
  if(totMWh > 0 ){
  pinf[1 + i * npropinf] = infMWh/totMWh;
  }

}

// prop for each ward are define in pinf

  wpropInfHorig = 0;
  wpropInfHdest = 0;
  wpropInfPWdest = 0;

  for(i = 0; i < nW; ++i){

  wpropInfHorig += ldata[i + nW] * pinf[1 + i*npropinf];
  wpropInfHdest += ldata[i] * pinf[1 + i*npropinf];
  wpropInfPWdest += ldata[i] * pinf[0 + i*npropinf];

  }

  v_new[0] = wpropInfHorig;
  v_new[1] = wpropInfHdest;
  v_new[2] = wpropInfPWdest;

return 1;
")


return(pts_fun)

}
