# iot telemetry analysis


# Import libraries
library(anytime)
library(plyr)
library(ggplot2)
library(xtable)
library(moments)
library(gamlss)
library(gamlss.mx)
library(tidyverse)
library(dplyr)
library(GGally)
library(gridExtra)
library(corrplot)
library(cluster)
library(factoextra)
library(mclust)
library(scales)
library(fpc)
library(hamlet)
library(hopkins)
library(NbClust)



set.seed(17)


# Import dataset from csv

# Online from github repo
#df = read.csv('https://raw.githubusercontent.com/Giovo17/iot-telemetry-analysis/main/iot_telemetry_data.csv')

# Local from disk
setwd("~/Documents/University/Data\ Science/1°\ Year\ (2022-2023)/Data\ Analysis\ (1)/Exam\ -\ Data\ Analysis/Report/iot-telemetry-analysis")
df = read.csv("iot_telemetry_data.csv")

# Randomly select 50,000 rows from dataset to speed up runtimes
df = df[sample(nrow(df), 50000), ]



head(df)

#print(xtable(head(df), type = "latex", digits=5))


df$ts = anytime::anytime(df$ts)

df$device = revalue(df$device, c("b8:27:eb:bf:9d:51"="Device 1", "00:0f:00:70:91:0a"="Device 2", "1c:bf:ce:15:ec:4d"="Device 3"))





str(df)

summary(df)

sum(is.na(df))




### ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

###---------------------###
#   Univariate Analysis   #
###---------------------###



### ------------------------------------------------------------------------ ###
# TS (Timestamp of readings) (not sure if mantaining this variable)











### ------------------------------------------------------------------------ ###
# Device (MAC address of the device, categorical variable)
# useful for cluster analysis given that each device is located in a different place

table(df$device)

#xtable(df$device)




### ------------------------------------------------------------------------ ###
# Light (binary variable) can be modeled as a bernoulli variable (need to check) ???
# need to convert to numeric variable, unclass function doesn't work


table(df$light)


jpeg(file="../LateX_project/images/chapter2/light_barplot.jpeg", width = 6, height = 6, units = 'in', res = 200)

ggplot(df, aes(x=as.factor(light) )) +
  geom_bar(color="#6b9bc3", fill=rgb(0.1,0.4,0.5,0.7) ) + 
  geom_text(stat='count', aes(label=..count..), vjust=-1) +
  theme_minimal()

dev.off()





### ------------------------------------------------------------------------ ###
# Motion (binary variable)

table(df$motion)


jpeg(file="../LateX_project/images/chapter2/motion_barplot.jpeg", width = 6, height = 6, units = 'in', res = 200)

ggplot(df, aes(x=as.factor(motion) )) +
  geom_bar(color="#6b9bc3", fill=rgb(0.1,0.4,0.5,0.7) ) + 
  geom_text(stat='count', aes(label=..count..), vjust=-1) +
  theme_minimal()

dev.off()






### ------------------------------------------------------------------------ ###
# CO (Carbon monoxide)

summary(df$co)
var(df$co)
moments::skewness(df$co)
moments::kurtosis(df$co)


# EDA

jpeg(file="../LateX_project/images/chapter2/co_boxplot.jpeg", width = 6, height = 6, units = 'in', res = 200)

ggplot(df, aes(y=co)) + 
  geom_boxplot(fill="#6b9bc3", alpha=0.7) + 
  theme(
    axis.text=element_text(size=6.5), 
    axis.title=element_text(size=14, face="bold")
  ) +
  theme_minimal()

dev.off()


jpeg(file="../LateX_project/images/chapter2/co_histogram.jpeg", width = 6, height = 6, units = 'in', res = 200)

ggplot(df, aes(x=co)) +
  geom_histogram( binwidth=0.0008, fill="#6b9bc3", color="#6b9bc3", alpha=0.7, position = 'identity') +
  theme(plot.title = element_text(size=15)) +
  theme_minimal()

dev.off()




### ------------------------------------------------------------------------ ###
# Humidity (%, so defined in [0,100])

summary(df$humidity)
var(df$humidity)
moments::skewness(df$humidity)
moments::kurtosis(df$humidity)


# EDA

jpeg(file="../LateX_project/images/chapter2/humidity_boxplot.jpeg", width = 6, height = 6, units = 'in', res = 200)

ggplot(df, aes(y=humidity)) + 
  geom_boxplot(fill="#6b9bc3", alpha=0.7) + 
  theme(
    axis.text=element_text(size=6.5), 
    axis.title=element_text(size=14, face="bold")
  ) +
  theme_minimal()

dev.off()


jpeg(file="../LateX_project/images/chapter2/humidity_histogram.jpeg", width = 6, height = 6, units = 'in', res = 200)

ggplot(df, aes(x=humidity)) +
  geom_histogram( binwidth=5, fill="#6b9bc3", color="#6b9bc3", alpha=0.7, position = 'identity') +
  theme(plot.title = element_text(size=15)) +
  theme_minimal()

dev.off()




### ------------------------------------------------------------------------ ###
# LPG (liquefied petroleum gas)

summary(df$lpg)
var(df$lpg)
moments::skewness(df$lpg)
moments::kurtosis(df$lpg)



# EDA

jpeg(file="../LateX_project/images/chapter2/lpg_boxplot.jpeg", width = 6, height = 6, units = 'in', res = 200)

ggplot(df, aes(y=lpg)) + 
  geom_boxplot(fill="#6b9bc3", alpha=0.7) + 
  theme(
    axis.text=element_text(size=6.5), 
    axis.title=element_text(size=14, face="bold")
  ) +
  theme_minimal()

dev.off()


jpeg(file="../LateX_project/images/chapter2/lpg_histogram.jpeg", width = 6, height = 6, units = 'in', res = 200)

ggplot(df, aes(x=lpg)) +
  geom_histogram( binwidth=0.0005, fill="#6b9bc3", color="#6b9bc3", alpha=0.7, position = 'identity') +
  theme(plot.title = element_text(size=15)) +
  theme_minimal()

dev.off()




### ------------------------------------------------------------------------ ###
# Smoke

summary(df$smoke)
var(df$smoke)
moments::skewness(df$smoke)
moments::kurtosis(df$smoke)


# EDA

jpeg(file="../LateX_project/images/chapter2/smoke_boxplot.jpeg", width = 6, height = 6, units = 'in', res = 200)

ggplot(df, aes(y=smoke)) + 
  geom_boxplot(fill="#6b9bc3", alpha=0.7) + 
  theme(
    axis.text=element_text(size=6.5), 
    axis.title=element_text(size=14, face="bold")
  ) +
  theme_minimal()

dev.off()


jpeg(file="../LateX_project/images/chapter2/smoke_histogram.jpeg", width = 6, height = 6, units = 'in', res = 200)

ggplot(df, aes(x=smoke)) +
  geom_histogram( binwidth=0.002, fill="#6b9bc3", color="#6b9bc3", alpha=0.7, position = 'identity') +
  theme(plot.title = element_text(size=15)) +
  theme_minimal()

dev.off()




### ------------------------------------------------------------------------ ###
# Temp (temperature, defined in R)

summary(df$temp)
var(df$temp)
moments::skewness(df$temp)
moments::kurtosis(df$temp)


# EDA

jpeg(file="../LateX_project/images/chapter2/temp_boxplot.jpeg", width = 6, height = 6, units = 'in', res = 200)

ggplot(df, aes(y=temp)) + 
  geom_boxplot(fill="#6b9bc3", alpha=0.7) + 
  theme(
    axis.text=element_text(size=6.5), 
    axis.title=element_text(size=14, face="bold")
  ) +
  theme_minimal()

dev.off()


jpeg(file="../LateX_project/images/chapter2/temp_histogram.jpeg", width = 6, height = 6, units = 'in', res = 200)

ggplot(df, aes(x=temp)) +
  geom_histogram( binwidth=1, fill="#6b9bc3", color="#6b9bc3", alpha=0.7, position = 'identity') +
  theme(plot.title = element_text(size=15)) +
  theme_minimal()

dev.off()






### ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

###------------------------------###
#   Principal Component Analysis   #
###------------------------------###


# Multivariate EDA

df_numeric = df %>% select_if(is.numeric)

df_numeric_scaled = data.frame(scale(df_numeric))





# Pairplot of all numerical variables
jpeg(file="../LateX_project/images/chapter3/pairplot_numeric_dataset.jpeg", width = 6, height = 6, units = 'in', res = 200)

ggpairs(df_numeric,
        upper = list(continuous = "density", combo = "box_no_facet"),
        lower = list(continuous = "points", combo = "dot_no_facet"))

dev.off()



# A better way to visualize correlation matrix
jpeg(file="../LateX_project/images/chapter3/correlation_matrix.jpeg", width = 6, height = 6, units = 'in', res = 200)

corrplot::corrplot.mixed(cor(df_numeric), upper = "ellipse")

dev.off()


# Pairplot of all numerical variables with device hue
jpeg(file="../LateX_project/images/chapter3/pairplot_numeric_dataset_hue.jpeg", width = 6, height = 6, units = 'in', res = 200)

ggpairs(df, columns = c(3,4,6,8,9), ggplot2::aes(colour=device), upper = list(continuous = wrap('cor', size = 1.5)))

dev.off()



miu = apply(df_numeric, 2, mean)
var = apply(df_numeric, 2, var)

# Evaluating the variance of Humidity and Temperature
print(var[2]/sum(var)*100)
print(var[5]/sum(var)*100)




# PCA


df_numeric_scaled.cov = cov(df_numeric_scaled)
df_numeric_scaled.eigen = eigen(df_numeric_scaled.cov)
str(df_numeric_scaled.eigen)


pr.out  = prcomp(df_numeric_scaled)

print(pr.out$sdev)
print(pr.out$rotation)
print(pr.out$center)
print(pr.out$scale)






# CPVE
PVE = df_numeric_scaled.eigen$values / sum(df_numeric_scaled.eigen$values)
round(PVE, 4)

# Kaiser Rule
kaiserVector = df_numeric_scaled.eigen$values - 1   # Positive values will be choosen


kaiserNumOfPC = 0
for(x in kaiserVector){
  if(x > 0)
    kaiserNumOfPC = kaiserNumOfPC + 1
}

print(paste("Number of PCs according to Kaiser Rule: ", kaiserNumOfPC))




data = data.frame(
  "x" = 1:5,
  "y" = df_numeric_scaled.eigen$values
)

# Scree plot
jpeg(file="../LateX_project/images/chapter3/scree_plot.jpeg", width = 6, height = 6, units = 'in', res = 200)

#plot(df_numeric_scaled.eigen$values, type="b")

ggplot(data, aes(x=x, y=y)) +
  geom_line( color="grey") +
  geom_point(shape=21, color="black", fill="#69b3a2", size=6) +
  theme_minimal()

dev.off()






jpeg(file="../LateX_project/images/chapter3/pca_plot.jpeg", width = 6, height = 6, units = 'in', res = 200)

factoextra::fviz_pca_ind(pr.out, legend="top", habillage=df$device, palette=c("green", "red", "blue"), geom="point", ggtheme=theme_minimal())

dev.off()



jpeg(file="../LateX_project/images/chapter3/biplot.jpeg", width = 6, height = 6, units = 'in', res = 200)

factoextra::fviz_pca_biplot(pr.out, geom="point", ggtheme=theme_minimal())

dev.off()






### ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

###-------------------###
#   Cluster Analysis    #
###-------------------###


# Cluster validation   


# Assessment of cluster tendency

# Generating the benchmark dataset from a uniform distribution
benchmark_df = apply(df_numeric, 2, function(x){runif(length(x), min(x), max(x))})
benchmark_df = as.data.frame(benchmark_df) # Transform benchmark_df in a data frame cause apply return a vector
benchmark_df_scaled = scale(benchmark_df)




jpeg(file="../LateX_project/images/chapter4/pairplot_benchmark_dataset.jpeg", width = 6, height = 6, units = 'in', res = 200)

ggpairs(df_numeric_scaled,
        upper = list(continuous = "density", combo = "box_no_facet"),
        lower = list(continuous = "points", combo = "dot_no_facet"))

dev.off()


jpeg(file="../LateX_project/images/chapter4/pairplot_benchmark_dataset.jpeg", width = 6, height = 6, units = 'in', res = 200)

ggpairs(benchmark_df_scaled,
        upper = list(continuous = "density", combo = "box_no_facet"),
        lower = list(continuous = "points", combo = "dot_no_facet"))

dev.off()






# Plot on the PC space

jpeg(file="../LateX_project/images/chapter4/pca_benchmark_dataset.jpeg", width = 6, height = 6, units = 'in', res = 200)

factoextra::fviz_pca_ind(prcomp(benchmark_df_scaled), title="PCA - Benchmark Dataset",
                         habillage=df$device, legend="bottom", geom="point", ggtheme=theme_minimal())

dev.off()


jpeg(file="../LateX_project/images/chapter4/pca__benchmark_dataset.jpeg", width = 6, height = 6, units = 'in', res = 200)

factoextra::fviz_pca_ind(prcomp(df_numeric_scaled), title="PCA - Original Dataset",
                         habillage=df$device, legend="bottom", geom="point", ggtheme=theme_minimal())

dev.off()




# Hopkins statistic
hopkins::hopkins(df_numeric)



# Determining the best number of clusters


# Elbow method


factoextra::fviz_nbclust(df_numeric_scaled, kmeans, nstart = 10, method = "wss") +
  geom_vline(xintercept = 3, linetype = 2) +
  labs(subtitle = "Elbow method")



# Silhouette method
factoextra::fviz_nbclust(df_numeric_scaled, kmeans, method = "silhouette") +
  labs(subtitle = "Silhouette method")



# Gap statistic
set.seed(454)
factoextra::fviz_nbclust(RSDataset.scaled, kmeans, nstart = 50, method = "gap_stat", nboot = 500) + # nboot = number of simulations
  labs(subtitle = "Gap statistic method")

# NbClust function
nb_km_eu = NbClust(RSDataset.scaled, distance = "euclidean", min.nc = 2,
                   max.nc = 10, method = "kmeans")
factoextra::fviz_nbclust(nb_km_eu)

nb_km_man = NbClust(RSDataset.scaled, distance = "manhattan", min.nc = 2,
                    max.nc = 10, method = "kmeans")
factoextra::fviz_nbclust(nb_km_man)

nb_avg_eu = NbClust(RSDataset.scaled, distance = "euclidean", min.nc = 2,
                    max.nc = 10, method = "average")
factoextra::fviz_nbclust(nb_avg_eu)

nb_single_eu = NbClust(RSDataset.scaled, distance = "euclidean", min.nc = 2,
                       max.nc = 10, method = "single")
factoextra::fviz_nbclust(nb_single_eu)



# External cluster validation

km = factoextra::eclust(RSDataset.scaled, k=3, FUNcluster="kmeans", nstart=25, graph=FALSE)
factoextra::fviz_cluster(km, geom = "point", ellipse.type = "norm",
                         palette = "jco", ggtheme = theme_minimal())

# Confusion matrix to compare the kmeans result with external information
table(km$cluster, shrinkedDataset$device)
# The results are decently good for each of the 3 clusters
# Missclassified point: 27+17=44 that is the 4.4% of the data

# Silhouette plot
factoextra::fviz_silhouette(km)
# The results are good


pam = factoextra::eclust(RSDataset.scaled, k=3, FUNcluster="pam", graph=FALSE)
factoextra::fviz_cluster(pam, geom = "point", ellipse.type = "norm",
                         palette = "jco", ggtheme = theme_minimal())

# Confusion matrix to compare the pam result with external information
table(pam$clustering, shrinkedDataset$device)
# The results are decently good for each of the 3 clusters, and they are sightly better with respect to the kmeans results
# Missclassified point: 20+15=35 that is the 3.5% of the data

# Silhouette plot
factoextra::fviz_silhouette(pam)
# In the first cluster there some units with a silhouette value lower than 0, this is a bad result for pam algorithm

head(pam$silinfo$widths)

missCvec = vector()   # Vector of missclassified units

for(k in seq(1,length(RSDataset[,1]))){
  if(pam$silinfo$widths[k,3]<0){
    #print(pam$silinfo$widths[k,])
    missCvec = append(missCvec, strtoi(rownames(pam$silinfo$widths[k,])))  # Add the missclassified units to missCvec
  }
}

# Visualizing the units that are badly assigned by pam algorithm
colvec = pam$clustering   # Assigning initial colors according to pam clusterization
colvec[missCvec] = 4      # Changing color to the missclassified units

# Visualize the missclassified units in the scatterplot
pairs(RSDataset.scaled, gap=0, pch=16, col=colvec, cex=c(0.8, 0.8, 0.8, 3)[colvec])
# As we can see the three blue missclassified units are difficult units to be clusterized cause there are in between two clusters


# Have to try clara with the complete dataset


hclust_average = factoextra::eclust(RSDataset.scaled, FUNcluster="hclust", k=3, metric="euclidean", 
                                    hc.method="average", graph=FALSE)
factoextra::fviz_dend(hclust_average, palette = "jco")

# Confusion matrix to compare the pam result with external information
table(pam$clustering, shrinkedDataset$device)

# Silhouette plot
factoextra::fviz_silhouette(km)


# Dunn index


















###---------------------------------------###
#   Hierarchical agglomerative clustering   #
###---------------------------------------###

diss = cluster::daisy(RSDataset.scaled)  # Dissimilarity triangular matrix computed with the Gower's index
round(as.matrix(diss), 2) # Transform the triangular matrix in a squared matrix

factoextra::fviz_dist(diss)  # Dissimilarity matrix graphic representation


# Need to apply all agglomerative clustering methods and compare them
# The average linkage method and Ward's method should perform better than the others (need to check)
hc.single = hclust(d = diss, method = "single")
hc.complete = hclust(d = diss, method = "complete")
hc.average = hclust(d = diss, method = "average")
hc.ward = hclust(d = diss, method = "ward.D2")  # Ward method with squared distances

# Dendogram
factoextra::fviz_dend(hc.single, cex=0.5)
factoextra::fviz_dend(hc.complete, cex=0.5)
factoextra::fviz_dend(hc.average, cex=0.5)
factoextra::fviz_dend(hc.ward, cex=0.5)

# Need to compare the cophenetic distance vector with the original distance vector for every method to select the best one
cor(diss, cophenetic(hc.single))  # Correlation between original distance vector and cophenetic of single method
cor(diss, cophenetic(hc.complete))  # Correlation between original distance vector and cophenetic of complete method
cor(diss, cophenetic(hc.average))  # Correlation between original distance vector and cophenetic of average method
cor(diss, cophenetic(hc.ward))  # Correlation between original distance vector and cophenetic of ward method
# According to the comparison of the correlations the best method is the average

# Clustering division according to the average method
groupsAverage = cutree(hc.average, h=3)   # Cut the dendogram based on k (number of clusters) or h (dendogram height)
table(groupsAverage)

# Clustering division according to the average method
groupsSingle = cutree(hc.single, h=3)   # Cut the dendogram based on k (number of clusters) or h (dendogram height)
table(groupsSingle)

# According to the two previous results I think there are outliers cause some clusters are containing only 1 or 2 points
# From the scatterplot it's clearer their presence


# Confusion matrix
table(groupsAverage, groupsSingle)  # Useful to compare two different clustering partitions


# Not sure if this is helpful...
diffClass = mclust::classError(groupsAverage, groupsSingle)  # Useful to know the points that are differently classified by the two methods
missClassifiedItems = dataset[diffClass$misclassified,]  # See the differently classified points in the original dataset
ggplot(missClassifiedItems, aes(x=device)) + geom_bar()   # Plot the device column to see which device contributes more to the "missclassification"




# Scatterplot with points colored according to the cluster of membership in groups
pairs(RSDataset.scaled, pch=16, gap=0, col=groupsAverage)  
pairs(RSDataset.scaled, pch=16, gap=0, col=groupsSingle)


# Clusters representation in the space of the principal components
factoextra::fviz_cluster(list(data = RSDataset.scaled, cluster = groupsAverage),
                         palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
                         ellipse.type = "convex", # Concentration ellipse
                         repel = TRUE, # Avoid label overplotting (slow)
                         show.clust.cent = FALSE, ggtheme = theme_minimal())
# Need to comment about the position of each cluster
# ex. The units in the green cluster have high level of the first PC and average level of the second PC


hc.agnes = cluster::agnes(x = RSDataset,
                          stand = TRUE,
                          metric = "euclidean",
                          method = "average")


factoextra::fviz_dend(hc.agnes, cex=0.6, k=3)




###-------------------------###
#   Partitioning Clustering   #
###-------------------------###




# Selecting the best number of clusters
factoextra::fviz_nbclust(RDataset.scaled, kmeans, nstart=25, method="wss") + 
  geom_vline(xintercept = 3, linetype = 2)
# Have to search for an elbow in this graph there's one for x=3

set.seed(342) # Seed for initial centroids random generation 
km = kmeans(RDataset.scaled, centers=3, nstart=10)

# Analizing km outputs

print(km$size)   # Important to evaluate the presence of outliers

print(km$centers)   # Very important to analize Cluster means to characterize the clusters
# Cluster means:
#           co   humidity        lpg      smoke        temp
# 1 -0.9683959  1.2863623 -0.9970905 -0.9927343 -0.92545025
# 2  0.8242563 -0.8612697  0.8256278  0.8261186 -0.09137922
# 3 -0.2313946 -0.1359184 -0.1945112 -0.2014368  1.44686904

# Data is scaled so the mean for each variable is 0
# Units in cluster 1 are characterized by an above average humidity and the other values are below average
# Units in cluster 2 are characterized by an above average temperature, slightly below average values co, lpg and smoke and more or less average humidity
# Units in cluster 3 are characterized by an above average co, lpg and smoke, an average temperature and a below average humidity

# This process is useful in order to assign new units to the clusters


clusterVariability = scales::label_percent()(km$betweenss/km$totss)   # Variability explained by the clustering
print(clusterVariability)
# So this clustering method explains the 71% of the original variability
# Useful to select the best clustering method given the number of clusters K (the higher the better)

# Visualize the clusters in the scatterplot
pairs(RDataset.scaled, pch=16, gap=0, col=km$cluster)

# Visualize the clusters in the scatterplot with centroid
RDatasetCentr.scaled = rbind(RDatasetCentr.scaled, km$centers)
clusterMembership = km$cluster
clusterMembership.new = c(km$cluster, rep(4,3)) # Adding 3 units with cluster membership 4
pairs(RDatasetCentr.scaled, pch=16, gap=0,
      col=c("red", "green", "blue", "black")[clusterMembership.new],  # Assigning points colors: black for centroids
      cex=c(rep(0.7,3), 1.3)[[clusterMembership.new]]) # Assigning points dimensions: 0.7 for all point, 1.3 for centroids


# Analizing the Cluster means of the original dataset 
aggregate(RDataset, by=list(cluster=km$cluster), mean)
#   cluster          co humidity         lpg      smoke     temp
# 1       1 0.003283287 76.07329 0.005634292 0.01474428 19.84161
# 2       2 0.005664408 51.34459 0.008408754 0.02258743 22.15495
# 3       3 0.004262222 59.69658 0.006855943 0.01815646 26.42137

# aggregate(RSDataset, by=list(cluster=km$cluster), sd)

# Adding the column of cluster of membership to the dataset
CRDataset = cbind(RDataset, cluster=km$cluster)

# Visualize the clusters in the PC space
fviz_cluster(km, 
             data = RDataset.scaled,
             palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
             ellipse.type = "euclid", # Concentration ellipse
             star.plot = TRUE, # Add segments from centroids to items
             repel = TRUE, # Avoid label overplotting (slow)
             ggtheme = theme_minimal())




# PARTITIONING CLUSTERING METHOD - KMEDOIDS
# Working with the whole dataset including categorical variables
# I expect a better result cause I think in the dataset there are some outliers

pam = cluster::pam(shrinkedDataset, 3, metric="euclidean", stand=FALSE)

pairs(shrinkedDataset, pch=16, gap=0, col=pam$clustering)

# Scatterplot for mixed variables
DeviceNumSDataset = subset(shrinkedDataset, select = -c(ts, light, motion))
hamlet::mixplot(DeviceNumSDataset, pch=16)  

# TO DO: Better to try Clara algorithm for kmedoids

# fpc::pamk()   # pam() variant which automatically selects the number of clusters





# Silhouette analysis

cluster::silhoutte()

# Searching the optimal number of clusters
fviz_nbclust(RSDataset.scaled, pam, method="silhouette")
# 3 is the best number of clusters

# TO DO: Define which variables contribute more in cluster formation
# Take a look also at the principal components











###------------------------###
#   Model based clustering   #
###------------------------###

# Applying gaussian mixture model to the numerical variables of the dataset
mbc = mclust::Mclust(RSDataset.scaled, G=1:10) # Considering K from 1 to 10

str(mbc)

summary(mbc$BIC)

plot(mbc, what = "BIC", ylim = range(mbc$BIC, na.rm = TRUE), legendArgs = list(x = "bottomleft"))

head(mbc$z)




