# Social network of friendships between 34 members of a karate club at a US university in the 1970s. 
# Please cite W. W. Zachary, An information flow model for conflict and fission in small groups, 
# Journal of Anthropological Research 33, 452-473 (1977).

#use igraph library
library(igraph)




dev.off(2)
dev.off(3)
dev.off(4)
dev.off(5)
dev.off(6)
dev.off(7)

windows()
windows()
windows()
windows()
windows()
windows()



#read graph from file (gml format)
OriginalGraph <- read.graph("karate.gml", format = c("gml"))

#OriginalGraph <- read.graph("zachary.txt", format = c("edgelist"))







#cs <- leading.eigenvector.community.step(OriginalGraph)
#V(OriginalGraph)$color <- ifelse(cs$membership==0, "lightblue", "green")

#scale <- function(v, a, b) {
#  v <- v-min(v) ; v <- v/max(v) ; v <- v * (b-a) ; v+a
#}

#V(OriginalGraph)$size <- scale(abs(cs$eigenvector), 10, 20)
#E(OriginalGraph)$color <- "grey"
#E(g)[ V(g)[ color=="lightblue" ] %--% V(g)[ color=="green" ] ]$color <- "red"
#dev.set(2)
#tkplot(g, layout=layout.kamada.kawai, vertex.label.font=2)

#plot the origial graph
dev.set(2)
plot(OriginalGraph, layout=layout.kamada.kawai, vertex.label.font=2)





#delete some edges to simulate the observed data      
#plot the reduced graph


removalMethod <- 1

if (removalMethod == 0)	# use preselected "interesting" set of edges
{
  
	EdgesDelGraph <- delete.edges(OriginalGraph, E(OriginalGraph, P=c(24,25, 26,33 )) )

	EdgesDelGraph <- delete.edges(EdgesDelGraph, E(EdgesDelGraph, P=c(8,32, 32,22, 20,33 )) )
	EdgesDelGraph <- delete.edges(EdgesDelGraph, E(EdgesDelGraph, P=c(10,4, 14,33, 27,23 )) )
	EdgesDelGraph <- delete.edges(EdgesDelGraph, E(EdgesDelGraph, P=c(15,32, 7,2, 7,3, 6,16, 7,0 )) )

	EdgesDelGraph <- delete.edges(EdgesDelGraph, E(EdgesDelGraph, P=c(8,0, 13,3, 5,0, 2,8, 18,33, 30,33, 31,33, 33,13 )) )

}
if(removalMethod ==2)
{ 
  EdgesDelGraph <- OriginalGraph
  EdgesDelGraph["15","10"]<-NULL;
  EdgesDelGraph["32","22"]<-NULL;
  EdgesDelGraph["30","33"]<-NULL;
  EdgesDelGraph["23","34"]<-NULL;
  EdgesDelGraph["3","15"]<-NULL;
  EdgesDelGraph["20","18"]<-NULL;
  EdgesDelGraph["15","34"]<-NULL;
  EdgesDelGraph["10","16"]<-NULL;
  EdgesDelGraph["15","27"]<-NULL;
  EdgesDelGraph["32","17"]<-NULL;
  EdgesDelGraph["32","27"]<-NULL;#15 32 30 23  3 20 15 10 15 32 32 12  6 27 31 14 23  4  7 12
  EdgesDelGraph["12","20"]<-NULL;
  EdgesDelGraph["6","14"]<-NULL;
  EdgesDelGraph["27","31"]<-NULL;
  EdgesDelGraph["31","28"]<-NULL;
  EdgesDelGraph["14","3"]<-NULL;
  EdgesDelGraph["23","32"]<-NULL;
  EdgesDelGraph["4","12"]<-NULL;
  EdgesDelGraph["7","9"]<-NULL;
  EdgesDelGraph["12","12"]<-NULL;
  #10 22 33 34 15 18 34 16 27 17 27 20 14 31 28  3 32 12  9 12
  
}

if (removalMethod == 1)	# use randomly selected edges
{

	SampledGraph <- OriginalGraph

	v1 <- sample(1:34, 20, replace=T)
	v2 <- sample(1:34, 20, replace=T)
  print(v1)
  print(v2)
	for (i in 1:length(v1))
	{
		a <- v1[i] 
		b <- v2[i] 
    print (a)
    print (b)
		if (are.connected(SampledGraph, a, b)) 
			{ #print ("inside",a,b)
      SampledGraph[a,b] <-NULL 
      #delete.edges(SampledGraph, E(SampledGraph, P=c(a, b)))
	}
 }
	EdgesDelGraph <- SampledGraph

}



dev.set(3) 	# selects new plot window...
plot(EdgesDelGraph, layout=layout.kamada.kawai, vertex.label.font=2)
    
    

source("plotRoc.R")
source("glmScore.R")


p1 <- plotRoc(OriginalGraph, EdgesDelGraph, 1, 1001, 3, 1)	# Jaccard
dev.set(4)
head(p1)
plot(p1)
#plot(p1,ylim=c(0,1),xlim=c(0,1))

p2 <- plotRoc(OriginalGraph, EdgesDelGraph, 1, 1001, 3, 2)	# Dice
dev.set(5)
head(p2)
plot(p2)
#plot(p2,ylim=c(0,1),xlim=c(0,1))

p3 <- plotRoc(OriginalGraph, EdgesDelGraph, 1, 1001, 3, 3)	# Inv log
dev.set(6)
head(p3)
plot(p3)
#plot(p3,ylim=c(0,1),xlim=c(0,1))

p4 <- glmScore(OriginalGraph, EdgesDelGraph, 1, 1001, 10)	# Logistic regression: combined Jaccard, Dice, Inv log
dev.set(7)
head(p4)
plot(p4)
#plot(p4,ylim=c(0,1),xlim=c(0,1))




