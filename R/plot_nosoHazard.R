#' Plot daily risk of nosocomial infection per patient
#'
#' @description \code{plot_nosoHazard} function returns a network plot of the wards whose colors represent
#' the average daily risk of contracting a nosocomial infection for one patient of the ward .
#' This average daily risk is calculated by dividing the average daily incidence by the average daily turnover (number of beds divided by the average length of stay).
#' Associated risk-colors grade from white to red.
#'
#' @usage plot_nosoHazard(
#'        trajmwss, ward_names, pop_size_P, LS,
#'        matContact, layout = "with_fr", vertexsize = 0.5,
#'        vertexlabelsize = 0.03, edgearrowsize = 0.4, addtitle = FALSE,
#'        maxcolors = FALSE, verbose = TRUE)
#'
#' @param trajmwss list of trajectories generated by \code{multisim} function
#' @param ward_names string vector of ward names
#' @param pop_size_P numerical vector of patient population sizes
#' @param LS numerical vector of length of stay
#' @param matContact Matrix reporting  the proportion of time spent by health care workers in the different wards.
#' @param layout String. Graph layout (details: ? igraph::layout_). Default is "with_fr". Options: "as_star","as_tree","in_circle","nicely","on_grid","on_sphere", "randomly","with_dh","with_fr","with_gem","with_graphopt", "with_kk","with_lgl", "with_mds", "with_sugiyama"
#' @param vertexsize Integer. Defines the nodes/ward size (default is 0.5).
#' @param vertexlabelsize Integer. Defines the nodes/ward names size (default is 0.03).
#' @param edgearrowsize Integer. Defines the edge arrows size (default is 0.4).
#' @param addtitle logical for optional title. Default is  FALSE,
#' @param maxcolors integer. Limit the number of colors displayed in the legend (default is FALSE)
#' @param verbose Default is TRUE
#'
#' @importFrom intergraph asIgraph
#' @importFrom scales rescale
#' @importFrom igraph V
#' @importFrom grDevices heat.colors
#' @importFrom graphics legend
#' @importFrom graphics title
#' @importFrom data.table ':='
#' @importFrom igraph layout_
#' @importFrom igraph as_star
#' @importFrom igraph as_tree
#' @importFrom igraph in_circle
#' @importFrom igraph nicely
#' @importFrom igraph on_grid
#' @importFrom igraph on_sphere
#' @importFrom igraph randomly
#' @importFrom igraph with_dh
#' @importFrom igraph with_fr
#' @importFrom igraph with_gem
#' @importFrom igraph with_graphopt
#' @importFrom igraph with_kk
#' @importFrom igraph with_lgl
#' @importFrom igraph with_mds
#' @importFrom igraph with_sugiyama
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
#' matContact <- randomContacts(pop_size_H, ward_names)$contactMat
#'
#' plot_nosoHazard(results,
#'                 ward_names,
#'                 pop_size_P,
#'                 LS,
#'                 matContact,
#'                 layout = "nicely",
#'                 vertexsize = 0.5,
#'                 vertexlabelsize = 0.03,
#'                 edgearrowsize = 0.4,
#'                 addtitle = TRUE,
#'                 maxcolors = FALSE,
#'                 verbose = FALSE)
#'
#' @return Igraph plot.
#'
#' @export


plot_nosoHazard <-
  function(trajmwss,
           ward_names,
           pop_size_P,
           LS,
           matContact,
           layout = "with_fr",
           vertexsize = 0.5,
           vertexlabelsize = 0.03,
           edgearrowsize = 0.4,
           addtitle = FALSE,
           maxcolors = FALSE,
           verbose = TRUE) {

    g <-
      plot_connectivity(matContact,
                        pop_size_P,
                        netobj = TRUE,
                        verbose = verbose)

    g %<>% asIgraph(.)

    ldata <- data.frame(node = ward_names,
                        nP = pop_size_P,
                        tLS = LS)

    nosoHaz <- lapply(trajmwss, function(sim) {
      x <- sim[, incP := incPA + incPM + incPS, by = node] %>%
        .[, dincP := c(0, diff(incP)), by = node] %>%
        .[, mdincP := mean(dincP), by = node] %>%
        .[, c("mdincP" , "node")] %>% unique %>%
        merge(., ldata, by = "node")

      x[, nosoHazard := mdincP / (nP * (1 / tLS))]
      x
    }) %>% do.call(rbind, .) %>% .[, mean(nosoHazard), by = node] %>% .[, V1] %>% multiply_by(100)

    if (0 %in% nosoHaz)
      colorsHnoso <- nosoHaz %>% rescale(., to = c(1, 100)) else
      colorsHnoso <- nosoHaz %>% rescale(., to = c(5, 100))

    colorsHnoso %<>% sapply(., function(vertexColor) {
      rev(heat.colors(100))[vertexColor]
    })

    V(g)$colorsHnoso <- colorsHnoso

    eval(parse(text=(paste0("coords <- layout_(g, ", layout, "())"))))

    plot(
      g,
      vertex.label        = V(g)$vertex.names,
      vertex.size         = pop_size_P * vertexsize,
      vertex.label.cex    = pop_size_P * vertexlabelsize,
      edge.arrow.size     = edgearrowsize,
      layout              = coords,
      vertex.frame.color  = "black",
      vertex.label.color  = "black",
      vertex.color        = V(g)$colorsHnoso,
      edge.curved         = FALSE,
      vertex.label.family = "sans"
    )

    if (isTRUE(addtitle))
      title(main = paste("Daily risk of contracting a \n nosocomial infection per patient"))

    ### Plot the legend

      keep <- order(round(nosoHaz, 2))[!duplicated(round(nosoHaz, 2)[order(round(nosoHaz, 2))])]

      dfleg <- data.frame(
        legendlab = nosoHaz[keep] %>% round(., 2) %>% paste0( ., "%"),
        legendcol = colorsHnoso[keep]) %>% .[!duplicated(.$legendlab),]


      if(!isFALSE(maxcolors) & nrow(dfleg) > maxcolors){

        dfleg$valuesint <- nosoHaz[keep] %>% round(., 2)

        lengthout <- nrow(dfleg)
        ncolors <- dfleg$valuesint

        while((ncolors %>% unique %>% length) > maxcolors){
          lengthout %<>% magrittr::subtract(1)
          ncolors <- nosoHaz[keep] %>% findInterval(., seq(min(.), max(.), length.out = lengthout))
        }

        dfleg$interv <- ncolors
        setDT(dfleg)[, legendlab := paste0(max(valuesint), "%"), by = interv]

        dfleg %<>% .[ !duplicated(.[, c("legendlab")], fromLast=T),]
      }


      legend(
        "topleft",
        bty = "n",
        legend = dfleg$legendlab,
        col = "black",
        pt.bg = dfleg$legendcol,
        cex = 1,
        pt.cex = 2,
        pch = 21,
        border = NA
      )
  }
