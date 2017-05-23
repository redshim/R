require(igraph)
# Specify an undirected graph by hand, using a numeric
# vector of the pairs of vertices sharing an edge.
G <- graph( c(1,2,1,3,1,4,3,4,3,5,5,6,6,7,7,8,8,9,3,8,5,8), directed = FALSE )

# visualization
plot(G, layout = layout.fruchterman.reingold,
     vertex.size = 25,
     vertex.color="red",
     vertex.frame.color= "white",
     vertex.label.color = "white",
     vertex.label.family = "sans",
     edge.width=2,
     edge.color="black")

bsk<-read.table("http://www.dimiter.eu/Data_files/edgesdata3.txt", sep='\t', dec=',', header=T)#specify the path, separator(tab, comma, ...), decimal point symbol, etc.

# Transform the table into the required graph format:
bsk.network<-graph.data.frame(bsk, directed=F) #the 'directed' attribute specifies whether the edges are directed
# or equivelent irrespective of the position (1st vs 2nd column). For directed graphs use 'directed=T'

# Inspect the data:

V(bsk.network) #prints the list of vertices (people)
E(bsk.network) #prints the list of edges (relationships)
degree(bsk.network) #print the number of edges per vertex (relationships per people)

# First try. We can plot the graph right away but the results will usually be unsatisfactory:
plot(bsk.network, layout = layout.fruchterman.reingold,
     vertex.size = 40,
     vertex.color="grey",
     vertex.frame.color= "white",
     vertex.label.color = "white",
     vertex.label.family = "sans",
     edge.width=5,
     edge.color="black")




graph <- graph.adjacency(cormat>0.5, weighted=TRUE, mode="upper")

E(graph)$weight<-t(cormat)[abs(t(cor_mat))>0.5]
E(graph)[ weight>0.7 ]$color <- "black"
E(graph)[ weight>=0.65 & weight<0.7 ]$color <- "red"
E(graph)[ weight>=0.6 &weight<0.65 ]$color <- "green"
E(graph)[ weight>=0.55 &weight<0.6 ]$color <- "blue"
E(graph)[ weight<0.55  ]$color <- "yellow"
V(graph)$label<- row.names(cormat)#V(graph)$name
graph$layout <-
plot(graph
    ,layout = layout.fruchterman.reingold
    ,vertex.size = 20
    ,vertex.color="white"
    ,vertex.frame.color= "black"
    ,vertex.label.color = "black"
    ,vertex.label.family = "sans"
    ,     edge.width=5
    )


###################################
#https://www.r-bloggers.com/correlation-network/
require(xts)
require(quantmod)
require(igraph)
cor_mat<- matrix( runif(100), nr=10 )

cor_mat
cor_mat[ lower.tri(cor_mat, diag=TRUE) ]<- 0
cor_mat[ abs(cor_mat) < 0.5]<- 0

graph <- graph.adjacency(cor_mat>0.5, weighted=TRUE, mode="upper")
E(graph)$weight<-t(cor_mat)[abs(t(cor_mat))>0.5]
E(graph)[ weight>0.7 ]$color <- "black"
E(graph)[ weight>=0.65 & weight<0.7 ]$color <- "red"
E(graph)[ weight>=0.6 &weight<0.65 ]$color <- "green"
E(graph)[ weight>=0.55 &weight<0.6 ]$color <- "blue"
E(graph)[ weight<0.55  ]$color <- "yellow"
V(graph)$label<- seq(1:10)#V(graph)$name
graph$layout <- layout.fruchterman.reingold
factor<-as.factor(cut(E(graph)$weight*10,c(4,5,6,7,8),labels=c(1,10,20,30))) png('corr_network.png',width=500) plot(decompose.graph(graph)[[which.max(sapply(decompose.graph(graph), vcount))]],edge.width =as.numeric(factor)*1.5,frame=T) legend("bottomleft", title="Colors", cex=0.75, pch=16, col=c("black", "blue","red", "green","pink"), legend=c(">70%", "65-70","60-65","55-60","50-55"), ncol=2)

dev.off()



data(testdata)
spmatrix <- data2mat(testdata)
result <- sp.pair(spmatrix)
plotnetwork(result$Pearson)

plotnetwork(result$Pearson, linecol = c("orange", "blue"),
            number.label = FALSE)

title("Pearson Correlation Network")


cor_mat<- matrix( runif(100), nr=10 )

corrdata<-read.csv("C:/chetan/R/corr.csv",header=T)
cor_mat<-as.matrix(corrdata[,-1])
diag(cor_mat)<-0
graph<-graph.adjacency(cor_mat,weighted=TRUE,mode="upper")
E(graph)[ weight>0.7 ]$color <- "green"
E(graph)[ weight < -0.7 ]$color <- "red"
E(graph)[ weight>0.6 & weight < 0.7 ]$color <- "black"
E(graph)[ weight< -0.6 & weight > -0.7 ]$color <- "black"
plot(graph)


plot(graph, layout=layout.circle, vertex.color=V(graph)$color,

     vertex.label=V(graph)$number)
