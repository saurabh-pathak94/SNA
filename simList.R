
# The script generates sorted list of vertex similarities given a vertex similarity matrix

# reverse direction of "order(list[,3], decreasing = TRUE)" sorting order if smaller similarity score means more similarity

simList <- function(simMatrix) {

	rows <- nrow(simMatrix)
	cols <- ncol(simMatrix)


	list <- mat.or.vec(rows * cols, 3)

	i <- 1
	for (row in 1:rows)
	{
		for (col in row:cols)		# assume symmetric matrix
		{
			if (col == row)		# skip self-associations
				next

			sim <- simMatrix[row, col]

			if (sim > 0) 	# filter out zero-similar nodes
			{
				list[i, 1] <- row #- 1	# node indexes are zero-based
				list[i, 2] <- col #- 1	# node indexes are zero-based
				list[i, 3] <- sim

				i <- i + 1
			}
		}
	}

	i <- i - 1			# obtain last used index
	list <- list[1:i, ]		# truncate unused rows
  
	o <- order(list[,3], decreasing = TRUE)		# sort by last column
	list <- list[o,]
#	print (list) 
	list
}