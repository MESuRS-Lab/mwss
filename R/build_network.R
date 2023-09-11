#' Creates a synthetic hospital ward network
#'
#' @description This function creates a synthetic hospital ward network with additional information on the population size, visitors etc.
#' You can use this script to generate different input data to be used as examples of "network structures".
#'
#' @param n_buildings integer. number of buildings (="clusters")
#' @param n_wards numerical vector. Vector of length (n_buildings) with number of wards for each building
#' @param total_patients integer. Total number of patients in the hospital
#' @param total_HCW integer. Total number of HCW in the hospital
#' @param minLS integer. Minimal number of days for random length of stay
#' @param maxLS integer. Maximal number of days for random length of stay
#' @param clust_ratio_inout numeric. Ratio of intra-building clustering (relative to inter-building clustering)
#' @param within_clust_lev numeric. Probability of creating links between wards of the same building
#' @param between_clust_lev numeric. Probability of creating links between wards of different building
#' @param silent logical. Print plot if TRUE
#'
#' @importFrom igraph erdos.renyi.game
#' @importFrom igraph as_adjacency_matrix
#' @importFrom fields image.plot
#' @importFrom fields image.plot
#' @importFrom stringr str_sub
#' @importFrom stats rmultinom
#'
#' @return A list of 9 elements: 1. ward_names, 2. pop_size_P, 3. pop_size_H, nVisits, 5. LS (length of stay), 6. Hplanning, 7. matContact, 8. IMMstate, 9. EPIstate
#'
#' @export

build_network <- function(n_buildings = 5,
                          n_wards = c(3,4,5,8,9),
                          total_patients = 500,
                          total_HCW = 900,
                          minLS = 14,
                          maxLS = 28,
                          within_clust_lev = 0.8,
                          between_clust_lev = 0.1,
                          clust_ratio_inout = 0.8,
                          silent = F){

#######################################################################

## Checks
  if(length(n_wards) != n_buildings) stop("The vector of \'n_wards\' should be equal to \'n_buildings\'")
  if(clust_ratio_inout < 0 | clust_ratio_inout > 1) stop("\'clust_ratio_inout\' is a ratio and must be between 0 and 1")
  if(TRUE %in% (n_wards >= 100)) stop("Buildings cannot contain more than 100 wards (\'n_wards\' elements  must be <= 100)")

## total number of wards
tot_n_wards <- sum(n_wards)

########################################################################

## create empty list to store the characteristic of the synthetic network and population
network_input <- list()

########################################################################

## ward names
idmaker <- function(x)
{
  max.val = x*100
  count <- nchar(as.character(max.val))                       # find out how many 'numbers' each ID will have after the letter
  size <- paste("%0",count,"d",sep="")                        # set the variable to be fed into 'sprintf' to ensure we have leading 0's
  lets <- toupper(sample(letters,x, replace=T))               # randomizing the letters
  nums <- sprintf(size,sample(1:max.val)[1:x])                # randomizing the numbers, and ensuing they all have the same number of characters
  ids <- paste(lets,nums,sep="")                              # joining them together
  return(ids)
}

if(n_buildings <= 26){

  network_input$ward_names <- sapply(1:n_buildings, function(x){
    paste0(LETTERS[x], paste0("00", 1:n_wards[x]) %>% str_sub(start= -2))
  }) %>% unlist

} else
  network_input$ward_names <- idmaker(tot_n_wards)

########################################################################

## pop_size_P

## distribution of patients among wards
## for the moment this is an homogeneous distribution but it can be customised as needed
patient_distrib <- rep(c(1/tot_n_wards),each=tot_n_wards)

# we sample with a multinomial distribution to add some noise
network_input$pop_size_P <- rmultinom(1, size = total_patients, prob = patient_distrib)[,1]

########################################################################

## pop_size_H
## we construct this similarly to pop_size_P

## distribution of patients among wards
## for the moment this is an homogeneous distribution but it can be customised as needed
HCW_distrib <- rep(c(1/tot_n_wards),each=tot_n_wards)

# we sample with a multinomial distribution to add some noise
network_input$pop_size_H <- rmultinom(1, size = total_HCW, prob = HCW_distrib)[,1]

########################################################################
# nVisits

# for the moment we set this at zero but it can be customised
network_input$nVisits <- rep(c(0),each=tot_n_wards)

########################################################################
## LS (average length of stay of a patient in each ward)

## to initialize length of stay, we can for example assume a min and max LS and sample from a uniform distribution
## here we use min = 14 days and max = 28 days
## other distribution can be assumed depending on the need
network_input$LS <- runif(n = tot_n_wards, min = minLS, max = maxLS)

########################################################################

## matContact (contact matrix between wards)
## this is the matrix of the (proportion of) time spent by HCW in a given ward (row) in any other ward in the hospital (columns)
## sum of each row must be equal to 100 (i.e. 100%)

###############################################################################
# Function that takes a graph as input and generates adjacency matrix as output
###############################################################################

contact_matrix_generator <- function(g) {
  adj_M <- as_adjacency_matrix(g,sparse = F) %>% as.matrix()
  row_sums <- apply(adj_M,1,sum)
  res <- matrix(NA,nrow=length(row_sums),ncol=length(row_sums))
  for (i in 1:length(row_sums)) {
    if (row_sums[i] != 0) {
      res[i,] <- adj_M[i,]/row_sums[i]
    } else {
      res[i,] <- 0
      res[i,i] <- 1
    }
  }
  return(res)
}

# The idea in order to build the synthetic hospital ward network is to assume that hospital wards are clustered
# (for example because they share the same building).
# Wards within a building(=cluster) will be highly connected, while connection across buildings will be less frequent.
# To implement this, we create a first layer with connection within buildings, a second layer with links across the buildings,
# and we sum the two (with weights)

## the following function generates a matrix from the weighted sum of two networks
generate_network <- function(n_buildings,
                             n_wards,
                             p = c(within_clust_lev,between_clust_lev),
                             dist_within_between = c(clust_ratio_inout,(1-clust_ratio_inout))){
  # vector indicating the parameters of the erdos renyi graph for within building and at the hospital level networks
  networks_list <- list()
  # build a erdos.renyi.game network for each building
  for(i in 1:n_buildings){
    networks_list[[i]] <- erdos.renyi.game(n_wards[i],
                                           p.or.m = p[1],
                                           type = "gnp")
  }
  # build a erdos.renyi.game network for the whole hospital
  networks_full <- erdos.renyi.game(sum(n_wards),
                                    p.or.m = p[2],
                                    type = "gnp")
  # create contact matrix
  M_full <- contact_matrix_generator(networks_full)

  M <- list()
  for(i in 1:n_buildings){
    M[[i]] <- contact_matrix_generator(networks_list[[i]])
  }

  # create a matrix of size n_wards*n_buidings with the blocks only
  M_cluster <- matrix(0, nrow=sum(n_wards),ncol=sum(n_wards))
  loc_wards <- 0
  for(i in 1:n_buildings){
    x_range <- loc_wards + 1:n_wards[i]
    y_range <- loc_wards + 1:n_wards[i]
    M_cluster[x_range,y_range] <- M[[i]]
    # image(M_cluster)
    loc_wards <- loc_wards + n_wards[i]
  }

  # Generate final matrix as the sum of the two matrix
  # distribution of contacts that occur within cluster and outside cluster

  M_final = M_cluster*dist_within_between[1] + M_full*dist_within_between[2]
  # image(M_final)

  ## merge with a diagonal matrix so that final network has a strong diagonal component
  ## elements on the diagonal represent the time spent within its own word
  ## (which realistically should be higher than the time spent in any other ward; we assume at least 50% of the time)
  for(i in 1:nrow(M_final)){
    ## time spent within their own ward
    t_ward <- runif(n = 1, min = 0.5, max = 0.8)
    M_final[i,] <- M_final[i,]*(1.- t_ward)
    M_final[i,i] <- M_final[i,i]+t_ward
  }
  return(M_final*100)
}

output = generate_network(n_buildings=n_buildings, n_wards=n_wards)
if(silent)
image.plot(output)

#image.plot(saveInputs$matContact)

network_input$matContact <- output
rownames(network_input$matContact) <- network_input$ward_names
colnames(network_input$matContact) <- network_input$ward_names

########################################################################

# network_input$Hplanning <- NULL
#
# network_input$IMMstate <- NULL
#
# network_input$EPIstate <- NULL

## the above attributes are not needed right now


########################################################################
## SAVE FINAL DATASET as .rda

# save(network_input, file = "data/network_2.rda")

return(network_input)
}


