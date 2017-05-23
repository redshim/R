library("tm")
data(crude)
couple.of.words <- c("embargo", "energy", "oil", "environment", "estimate")
tdm <- TermDocumentMatrix(crude, control = list(dictionary = couple.of.words))
tdm.matrix <- as.matrix(tdm)
tdm.matrix[tdm.matrix>=1] <- 1
tdm.matrix <- tdm.matrix %*% t(tdm.matrix)

tdm.matrix[tdm.matrix>=1]

tdm.matrix <- tdm.matrix %*% t(tdm.matrix)
tdm.matrix


edges <- as.data.frame(as.table(tdm.matrix))
nodes <- row.names(tdm.matrix)


write.csv(edges, file = "edges.csv") # Save the Result
write.csv(nodes, file = "nodes.csv") # Save the Result



##################################################################################
tdm.matrix <- as.matrix(tdm)

library(slam)
word.count = as.array(rollup(tdm, 2)) # Rolling up group by the term

# frequently used words
word.order = order(word.count, decreasing = T)
most100 = word.order[1:100]
tdm.matrix = as.matrix(tdm[most100,])
tdm.matrix <- tdm.matrix %*% t(tdm.matrix)



edges <- as.data.frame(as.table(tdm.matrix))
nodes <- row.names(tdm.matrix)

write.csv(edges, file = "edges.csv") # Save the Result
write.csv(nodes, file = "nodes.csv") # Save the Result



########################################################
#install.packages("GGally")
library(GGally)
library(network)
library(sna)
library(ggplot2)

m[m[,3] == 11,]


tdf.matrix.filter <- tdm.matrix[tdm.matrix[1:2,] > 300 , ]


length(tdm.matrix)
length(tdf.matrix.filter)


net = network(tdm.matrix, directed = FALSE)

# vertex names
network.vertex.names(net) = row.names(tdm.matrix)
ggnet2(net)
ggnet2(net, size = 12, label = TRUE, label.size = 5)



