
# Testing script:
# 1. generates a similarity matrix, 
# 2. then sorts the similarities into ordered list
# 3. predicts missing links in the graph by using certain amount of most similar vertex pairs
# 4. scores the prediction by using ROC method

testPred <- function(originalGraph, sampledGraph, numBestToTake, treshold, mode)
{
	options(error=recover)

	source("simList.R")
	source("predictLinks.R")
	source("roc.R")

	if (mode == 1)
	{
		SJ <- similarity.jaccard(sampledGraph, vids = V(sampledGraph), mode = c("all", "out", "in", "total"), loops = FALSE)
		sim <- SJ
	}
	else if (mode == 2)
	{
		SD <- similarity.dice(sampledGraph, vids = V(sampledGraph), mode = c("all", "out", "in", "total"), loops = FALSE)
		sim <- SD
	}
	else if (mode == 3)
	{
		SInLogW <- similarity.invlogweighted(sampledGraph, vids = V(sampledGraph), mode = c("all", "out", "in", "total"))
		sim <- SInLogW
	}
	else
		stop("unknown mode specified")


	list <- simList(sim)
	predictedGraph <- predictLinks(sampledGraph, list, numBestToTake, treshold)
	rocr <- roc(originalGraph, sampledGraph, predictedGraph)

	
	list
}