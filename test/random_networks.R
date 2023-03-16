devtools::install_github("MESuRS-Lab/mwss")

library(mwss)

net <- build_network(within_clust_lev = 0.9,
                     between_clust_lev = 0.1,
                     clust_ratio_inout = 0.9)

plot_connectivity(net$matContact, net$pop_size_P,
                         vertexcexrate = 3,
                         vertexcol = c(rep("red",3),rep("blue",4),rep("white",5),rep("yellow",8),rep("orange",9)),
                         edgewidthrate = 5, netobj = F, verbose = F)

g <- plot_connectivity(net$matContact, net$pop_size_P, netobj = T, verbose = F)
