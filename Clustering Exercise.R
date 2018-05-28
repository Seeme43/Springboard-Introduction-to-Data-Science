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

df <- scale(wine[,-1])

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
#   * Why does this method work? What's the intuition behind it?
#   * Look at the code for wssplot() and figure out how it works

#  ANSWER: 
#  The method suggest a number of cluster included between 2-4. Presumibly, they should be 3.
#  Just like observing a dendogram the intuition is that once the "within groups sum of squares" starts to become too closer, the groups start to become too similar between each other,
#  and the aim of clustering is to obtain the minor possible number of groups with more similar characteristics between each other than with other clusters.



# Method 2: Use the NbClust library, which runs many experiments
# and gives a distribution of potential number of clusters.

library(NbClust)
set.seed(1234)
nc <- NbClust(df, min.nc=2, max.nc=15, method="kmeans")
barplot(table(nc$Best.n[1,]),
	          xlab="Numer of Clusters", ylab="Number of Criteria",
		            main="Number of Clusters Chosen by 26 Criteria")


# Exercise 3: How many clusters does this method suggest?

# ANSWER: This method suggests 3 cluster, after have experimented the number of clusters with 26 different criteria.

# Exercise 4: Once you've picked the number of clusters, run k-means 
# using this number of clusters. Output the result of calling kmeans()
# into a variable fit.km

fit.km <- kmeans(wine, centers = 3, iter.max = 1000 )

# Now we want to evaluate how well this clustering does.

# Exercise 5: using the table() function, show how the clusters in fit.km$clusters
# compares to the actual wine types in wine$Type. Would you consider this a good
# clustering?

ct.km <- table(fit.km$cluster, wine$Type)

install.packages("flexclust")
library(flexclust)
randIndex(ct.km)

# ANSWER: The model does not represent a good clustering, as shown in the table function, where, for example, none of the points in the second type have effectively been guessed in
# the second cluster. The  adjusted Rand index, which ranges from -1 (no agreement) to 1 (perfect agreement), is 0.3347, confirming the observation made with the table.


# Exercise 6:
# * Visualize these clusters using  function clusplot() from the cluster library
# * Would you consider this a good clustering?

install.packages("cluster")
library(cluster)

clusplot(wine, fit.km$cluster, shade = TRUE, color = TRUE )

# ANSWER: This is not a good clustering as shown from the image, where each cluster looks overlapped to the other. 
