
install.packages(c("cluster", "rattle","NbClust"))

# Now load the data and look at the first few rows
data(wine, package="rattle")
head(wine)

# Exercise 1: Remove the first column from the data and scale
# it using the scale() function

DF <- wine
str(DF)
DF <- scale(DF[-1])

wssplot <- function(data, nc=15, seed=1234){
	 wss <- (nrow(data)-1)*sum(apply(data,2,var))
               	      for (i in 2:nc){
		        set.seed(seed)
	   wss[i] <- sum(kmeans(data, centers=i)$withinss)}
	                
	plot(1:nc, wss, type="b", xlab="Num of Clusters",
	                        ylab="Within groups-SS")
	   }

wssplot(DF)

# Exercise 2:


library(NbClust)
set.seed(1234)
nc <- NbClust(DF, min.nc=2, max.nc=15, method="kmeans")
barplot(table(nc$Best.n[1,]),
	     xlab="Numer of Clusters", ylab="Num of Criteria",main="Num of clusters")


set.seed(1234)
fit.kmeans <- kmeans(DF, 3, nstart = 25)

compare <- table(fit.kmeans$cluster,wine$Type)
compare


clusplot(DF, fit.kmeans$cluster, color = TRUE, shade = TRUE)
