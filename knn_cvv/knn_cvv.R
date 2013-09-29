## ------------------------------------------------------------------------------------------------
## knn cross-validation assignment
##
##   - implement cross-validation for kNN
##   - measure the training, test and cross-validation error as a function of k on the iris dataset
##
## -------------------------------------------------------------------------------------------------
# Load required libraries
library(class)
library(ggplot2)

knn.cvv <- function(data, kmax, trainpct, nfolds)
{	
	set.seed(1) # random seed for consistency

	train.pct <- trainpct    # percentage of data to use for training set
	N <- nrow(data)     # total number of records 

	train.index <- sample(1:N, train.pct * N)       # random sample of records (training set)

	train.data <- data[train.index, ]       # perform train/test split
	test.data <- data[-train.index, ]       # note use of neg index...different than Python!

	train.labels <- as.factor(as.matrix(labels)[train.index, ])     # extract training set labels
	test.labels <- as.factor(as.matrix(labels)[-train.index, ])     # extract test set labels

	max.k <- kmax 
	err.rates <- matrix(runif(0), max.k, nfolds) # initialize a matrix initially with dummy err values
	
	if(nfolds == 0)
	{
		nfolds <- 1 # at least runs once (default nfolds=1)
	}
	
	for (i in seq(nfolds)) # run knn test for (nfolds X k) times
	{
		for (k in 1:max.k)   # perform fit for various values of k
		{
		    knn.fit <- knn(train = train.data,          # training set
				    test = test.data,           # test set
				    cl = train.labels,          # true labels
				    k = k                       # number of NN to poll
			       )

		    this.err <- sum(test.labels != knn.fit) / length(test.labels)    # store gzn err
		    err.rates[k,i] <- this.err # store err in the pre-defined matrix
		}
	}
	
	# Cross Validation by taking rowMeans of err matrix
	results <- data.frame(1:max.k, rowMeans(err.rates))   # create results summary data frame
	names(results) <- c('k', 'err.rate')        # label columns of results df

	# create title for results plot
	title <- paste('KNN Cross Validation (train.pct = ', train.pct, ', nfolds = ', nfolds, ' )', sep='')

	# create results plot
	results.plot <- ggplot(results, aes(x=k, y=err.rate)) + geom_point(colour='black') + geom_line(colour='red')
	results.plot <- results.plot + ggtitle(title) + theme(panel.background = element_rect(fill = "grey"))

	# save the plots to png
	png('plots/knn_cvv_%03d.png', width=6, height=6, units='in', res=300)
	plot(results.plot, ask=FALSE)
	dev.off()

	# draw results plot
	#results.plot

}


########################################################
# test the knn cross-validation function implementation
########################################################

data <- iris                # create copy of iris dataframe
labels <- data$Species      # store labels
data$Species <- NULL        # remove labels from feature set

knn.cvv(data=data, kmax=100, trainpct=0.7, nfolds=5)
knn.cvv(data=data, kmax=100, trainpct=0.7, nfolds=10)
knn.cvv(data=data, kmax=100, trainpct=0.7, nfolds=15)
knn.cvv(data=data, kmax=100, trainpct=0.7, nfolds=20)

