
# The script predicts missing links in the graph, given sampled graph and sorted list of vertex similarities

# set numBestToTake == -1 to use only treshold parameter
# reverse direction of "if (sim < treshold)" comparison if smaller similarity score means more similarity

predictLinks <- function(graph, simList, numBestToTake, treshold)
{
	rows <- nrow(simList)

	countAdded <- 1
	for (i in 1:rows)
	{
		if (numBestToTake != -1 && countAdded > numBestToTake)		# check here so we can have numBestToTake==0
			break	

		node1 <- simList[i, 1]
		node2 <- simList[i, 2]
		sim <- simList[i, 3]

		if (sim < treshold) 		# give up
			break

		if (are.connected(graph, node1, node2)) 	# dont count existing connections in numBest
			next

		graph <- add.edges(graph, c(node1, node2))	
		countAdded <- countAdded + 1
	}

	graph
}
