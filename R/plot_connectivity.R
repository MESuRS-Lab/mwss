#' Plot the network based on matrix of contact
#'
#' @description The function \code{plot_connectivity} build an igraph object representing the wards connected by the professionals activities.
#' The function also generate a generic png plot.
#'
#' @usage plot_connectivity(
#'         matContact, size,
#'         vertexcexrate = 3, edgewidthrate = 5,
#'         netobj = FALSE, verbose = TRUE)
#'
#' @param matContact Square matrix reporting the average proportion of time spent by professionals of a given ward in the different wards. Sum of rows must be equal to 1. The line width scale for edges will depend on this argument.
#' @param size Vector of population size in each ward (beds, HCWS or sum of both). The size of the nodes/vertex/wards will depend on this argument.
#' @param vertexcexrate Integer, proportional coefficient to adjust vertex names size.
#' @param vertexcol Character, vector defining the colors of vertices in the plot.
#' @param edgewidthrate Integer, proportional coefficient to adjust edge width.
#' @param netobj Logical, define if the function return an igraph oject (TRUE) or a plot (FALSE). Default is FALSE.
#' @param verbose Logical, activate production of details messages.
#'
#' @importFrom igraph graph_from_data_frame
#' @importFrom igraph E
#' @importFrom igraph E<-
#' @importFrom graphics plot
#' @importFrom network plot.network
#' @importFrom magrittr multiply_by
#' @importFrom magrittr divide_by
#' @importFrom network %e%
#' @importFrom network %e%<-
#'
#' @return If netobj is FALSE, the function returns a plot of wards network. If netobj is TRUE, the function returns an igraph object.
#'
#' @examples
#'
#' data("toydata")
#' list2env(toydata,envir=.GlobalEnv)
#' gdata <- build_gdata()
#'
#' matContact <- randomContacts(pop_size_H, ward_names)$contactMat
#' plot_connectivity(matContact, pop_size_P)
#'
#' @export

plot_connectivity <-
  function(matContact,
           size,
           vertexcexrate = 3,
           vertexcol = "grey",
           edgewidthrate = 5,
           netobj = FALSE,
           verbose = TRUE) {

    # Check

    if(length(vertexcol) > 1 & length(vertexcol) != length(size)) stop("vertexcol must be either a unique color or a vector attributing one color by node")

    names(vertexcol) <- colnames(matContact)

    # Add names if none in size vector

    if (is.null(names(size))) {
      names(size) <- colnames(matContact)
      if (verbose) {
        message("Names were attributed to the size values based on the contact matrix order.")
        print(size)
      }
    }

    # Checks

    if (!identical(colnames(matContact), rownames(matContact)) |
        !identical(colnames(matContact), names(size)))
      stop(
        "The contact matrix should have the same names in rows and columns. Those names have to be the same than in the size vector."
      )


    # Initialize edges list
    connections <- data.frame(
      ward1 = character(),
      ward2 = character(),
      nHCWS = numeric(),
      stringsAsFactors = FALSE
    )

    # Fill edges list
    connections <- lapply(seq(nrow(matContact)), function(ward) {
      from_ward <- matContact %>% rownames %>% .[ward]
      to_ward <- matContact[ward,] %>% names %>% .[. != from_ward]
      nHCWS <- matContact[from_ward, to_ward] %>% unlist

      data.frame(from_ward, to_ward, nHCWS)
    }) %>% do.call(rbind, .)

    g <- wardsNet(connections, size)

    g %e% "weight" %<>%  multiply_by(edgewidthrate)

    if (isTRUE(netobj))
      return(g)

    vnames <- network::get.vertex.attribute(g, "vertex.names")

    if(length(vertexcol) == 1) vertexcol %>% rep(., length(vnames))

    # If the nodes/wards are connected
    if (sum(connections$nHCWS != 0) > 0) {
      plot.network(
        g,
        # our network object
        vertex.col = vertexcol[vnames],
        # color nodes
        vertex.cex = (size[vnames] / max(size[vnames])) * vertexcexrate,
        edge.lwd = network::get.edge.attribute(g, "weight"),
        # size nodes
        displaylabels = T,
        # show node names
        label.pos = 0 # display the names directly over nodes
      )

    } else {
      plot.network(
        g,
        # our network object
        vertex.col = vertexcol[vnames],
        # color nodes
        vertex.cex = (size / max(size)) * vertexcexrate,
        # size nodes
        displaylabels = T,
        # show node names
        label.pos = 0 # display the names directly over nodes
      )

    }
  }

