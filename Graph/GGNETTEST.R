ggnet2(net, color = "phono", legend.size = 12, legend.position = "bottom") +
  theme(panel.background = element_rect(color = "grey"))

ggnet2(net, label = TRUE)
ggnet2(net, label = c("a", "e", "i"), color = "phono", label.color = "black")
ggnet2(net, size = 6, color = rep(c("tomato", "steelblue"), 5))
ggnet2(net, size = 6, color = "black", edge.size = 1, edge.color = "grey")


ggnet2(net, size = 6, color =c("tomato", "steelblue","steelblue","steelblue","steelblue"))

ggnet2(net, mode = "circle")
ggnet2(net, mode = "kamadakawai")
ggnet2(net, mode = "fruchtermanreingold", layout.par = list(cell.jitter = 0.75))
ggnet2(net, mode = "target", layout.par = list(niter = 100))

net %v% "color" = ifelse(net %v% "phono" == "vowel", "steelblue", "tomato")
ggnet2(net, color = "color", label=TRUE)

ggnet2(net, size = "degree")
