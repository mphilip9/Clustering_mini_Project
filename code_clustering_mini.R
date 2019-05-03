# This mini-project is based on the K-Means exercise from 'R in Action'
# Go here for the original blog post and solutions
# http://www.r-bloggers.com/k-means-clustering-from-r-in-action/

# Exercise 0: Install these packages if you don't have them already

# install.packages(c("cluster", "rattle.data","NbClust"))

# Now load the data and look at the first few rows
data(wine, package="rattle.data")
head(wine)

# Exercise 1: Remove the first column from the data and scale
# it using the scale() function

df <- scale(wine[-1])

# Now we'd like to cluster the data using K-Means. 
# How do we decide how many clusters to use if you don't know that already?
# We'll try two methods.

# Method 1: A plot of the total within-groups sums of squares against the 
# number of clusters in a K-means solution can be helpful. A bend in the 
# graph can suggest the appropriate number of clusters. 

wssplot <- function(data, nc=15, seed=1234){
	              wss <- (nrow(data)-1)*sum(apply(data,2,var))
               	      for (i in 2:nc){
		        set.seed(seed)
	                wss[i] <- sum(kmeans(data, centers=i)$withinss)}
	                
		      plot(1:nc, wss, type="b", xlab="Number of Clusters",
	                        ylab="Within groups sum of squares")
	   }

wssplot(df)

# Exercise 2:
#   * How many clusters does this method suggest? 

  # A: Notice the bend in the graph at 3 clusters. This would likely be the best k

#   * Why does this method work? What's the intuition behind it?

  # A: The within groups sum of squares basically shows you mean variance within the group (the variance of all the data in 1 cluster, 2 clusters, etc). 
  # We want to find the number of clusters where the sum of squares starts to curve and level out, which in this case is around 3. The variance 
  # would be smaller at 14 clusters, but not very informative on the relationship between the variables

#   * Look at the code for wssplot() and figure out how it works

  # A: The first part of the function has three arguments. the first is the data frame, the second sets the object nc = 15,
  # and the third sets the object seed = 1234. Inside the body of the function, the object wss (within group sum of squares) is defined as the number
  # of rows within the wine data set minus 1 (giving you the degrees of freedom) * the variance of the columns of the wine data set added together.
  # Next, a for loop. The seed of R's random number generator is set at 1234, and then object wss[i] is defined as the within group SS for i number of 
  # clusters. This is looped up until nc or 15, giving a within group SS for up to 15 clusters. Then, a plot is created with 1-15 as x and wss as y.
  # The other arguments define x and y titles and the type of plot, in this case a line and point plot ("b").

# Method 2: Use the NbClust library, which runs many experiments
# and gives a distribution of potential number of clusters.

library(NbClust)
set.seed(1234)
nc <- NbClust(df, min.nc=2, max.nc=15, method="kmeans")
barplot(table(nc$Best.n[1,]),
	          xlab="Number of Clusters", ylab="Number of Criteria",
		            main="Number of Clusters Chosen by 26 Criteria")


# Exercise 3: How many clusters does this method suggest?
  # A: 3 clusters


# Exercise 4: Once you've picked the number of clusters, run k-means 
# using this number of clusters. Output the result of calling kmeans()
# into a variable fit.km

fit.km <- kmeans(df, centers = 3)


# Now we want to evaluate how well this clustering does.

# Exercise 5: using the table() function, show how the clusters in fit.km$clusters
# compares to the actual wine types in wine$Type. Would you consider this a good
# clustering?
library(flexclust)
km <- table(wine$Type, fit.km$cluster)#Looks pretty good. Type and cluster appear well in agreement
randIndex(km) #This shows the agreement between the wine type and the cluster solution. ~0.9

# Exercise 6:
# * Visualize these clusters using  function clusplot() from the cluster library
# * Would you consider this a good clustering?

clusplot(df, fit.km$cluster, main = clusplot)
# It certainly appears to be clustering the wines properly. Also, the two principal
# components (though I can't say what those variables acutally are) explain 55.41% of  the variability.

