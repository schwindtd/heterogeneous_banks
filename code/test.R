mahal_dist_pair_full <- mahal_dist_pair + t(mahal_dist_pair)
rand_select <- round(runif(n=10, min=1, max=nrow(mahal_dist_pair)))
mahal_dist_pair_select <- mahal_dist_pair_full[rand_select, rand_select]
g <- graph.adjacency(mahal_dist_pair_select, mode = "undirected", weighted = TRUE)

# set labels for nodes
V(g)$label <- 1:vcount(g)

# set node colors based on degree centrality
#node_colors <- rev(heat.colors(10))
#degree_centrality <- degree(g, mode = "all")
#V(g)$color <- node_colors[cut(degree_centrality, breaks = 10)]

# set edge colors based on weight
#edge_colors <- heat.colors(10)
#E(g)$color <- edge_colors[cut(E(g)$weight, breaks = 10)]
E(g)$color <- "black"

# plot graph
# cairo_ps("../output/bank_similarity.eps", width = 6.25, height = 4, pointsize = 12)
# plot(g, layout = layout.fruchterman.reingold, edge.width = 1/E(g)$weight,
#      vertex.size = 20, vertex.label.cex = 0.8)
# legend("topleft", legend=c("Bank", "Similarity"), col=c("orange", "black"), 
#        pch=c(19, NA), lty=c(NA,1), pt.cex=c(1, 2), bty="n", cex=0.8)
# dev.off()
