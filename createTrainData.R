
# Creates data for training logistic regression by using input from a group of vertex similarity measures

createTrainData <- function(sampledGraph)
{
	options(error=recover)

	
	# The Jaccard similarity coefficient of two vertices is the number of common neighbors divided by the number of vertices that are 
	# neighbors of at least one of the two vertices being considered. similarity.jaccard calculates the pairwise Jaccard similarities for some 
	# (or all) of the vertices.  

	SJ <- similarity.jaccard(sampledGraph, vids = V(sampledGraph), mode = c("all", "out", "in", "total"), loops = FALSE)
	
	
	# The Dice similarity coefficient of two vertices is twice the number of common neighbors divided by the sum of the degrees of the 
	# vertices. similarity.dice calculates the pairwise Dice similarities for some (or all) of the vertices.

	SD <- similarity.dice(sampledGraph, vids = V(sampledGraph), mode = c("all", "out", "in", "total"), loops = FALSE)
	
	
	# The inverse log-weighted similarity of two vertices is the number of their common neighbors, weighted by the inverse logarithm of their degrees. 
	# It is based on the assumption that two vertices should be considered more similar if they share a low-degree common neighbor, since high-
	# degree common neighbors are more likely to appear even by pure chance. Isolated vertices will have zero similarity to any other vertex. Self-
	# similarities are not calculated.
		
	SInLogW <- similarity.invlogweighted(sampledGraph, vids = V(sampledGraph), mode = c("all", "out", "in", "total"))
		
				
		
	simMatrix <- SJ
		
	rows <- nrow(simMatrix)
	cols <- ncol(simMatrix)


	list <- mat.or.vec(rows * cols, 6)

	i <- 1
	for (row in 1:rows)
	{
		for (col in row:cols)		# assume symmetric matrix
		{
			if (col == row)		# skip self-associations
				next

			sim_SJ <- SJ[row, col]
			sim_SD <- SD[row, col]
			sim_SInLogW <- SInLogW[row, col]

			list[i, 1] <- are.connected(sampledGraph, row , col )	# node indexes are zero-based, add -1 to both row and col
			
			# these two columns are only for debugging
			list[i, 2] <- row #- 1	# node indexes are zero-based
     
			list[i, 3] <- col #- 1	# node indexes are zero-based
			
			list[i, 4] <- sim_SJ
			list[i, 5] <- sim_SD
			list[i, 6] <- sim_SInLogW
			

			i <- i + 1
		}
	}

	i <- i - 1			# obtain last used index
	list <- list[1:i, ]		# truncate unused rows

			
	list
}