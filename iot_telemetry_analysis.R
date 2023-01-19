# iot-telemetry-analysis

# Import libraries
library(anytime)
library(plyr)
library(ggplot2)
library(xtable)
library(moments)
library(gamlss)
library(gamlss.mx)
library(gtools)
library(tidyverse)
library(dplyr)
library(GGally)
library(gridExtra)
library(corrplot)
library(cluster)
library(factoextra)
library(mclust)
library(clValid)
library(scales)
library(fpc)
library(hamlet)
library(hopkins)
library(NbClust)


set.seed(17)


# Import dataset

# Online from github repo
#df = read.csv('https://raw.githubusercontent.com/Giovo17/iot-telemetry-analysis/main/iot_telemetry_data.csv')

# Local from disk
setwd("~/Documents/University/Data\ Science/1°\ Year\ (2022-2023)/Data\ Analysis\ (1)/Exam\ -\ Data\ Analysis/Report/iot-telemetry-analysis")
df = read.csv("iot_telemetry_data.csv")

head(df)
print(xtable::xtable(head(df), type="latex", digits=5))


# Randomly select 5000 rows from dataset to speed up runtimes
df = df[sample(nrow(df), 5000), ]


df$ts = anytime::anytime(df$ts)

df$device = plyr::revalue(df$device, c("b8:27:eb:bf:9d:51"="Device 1", "00:0f:00:70:91:0a"="Device 2", "1c:bf:ce:15:ec:4d"="Device 3"))


head(df)
print(xtable::xtable(head(df), type="latex", digits=5))


str(df)

summary(df)
print(xtable::xtable(summary(df), type="latex", digits=5))


sum(is.na(df))




### ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

###---------------------###
#   Univariate Analysis   #
###---------------------###



### ------------------------------------------------------------------------ ###
# TS (Timestamp of readings)





### ------------------------------------------------------------------------ ###
# Device (MAC address of the device)

table(df$device)

jpeg(file="../LateX_project/images/chapter2/device_barplot.jpeg", width=6, height =6, units='in', res=200)

ggplot(df, aes(x=as.factor(device) )) +
  geom_bar(color="#6b9bc3", fill=rgb(0.1,0.4,0.5,0.7) ) + 
  geom_text(stat='count', aes(label=..count..), vjust=-1) +
  theme_minimal()

dev.off()



### ------------------------------------------------------------------------ ###
# Light (binary variable)


table(df$light)

jpeg(file="../LateX_project/images/chapter2/light_barplot.jpeg", width=6, height =6, units='in', res=200)

ggplot(df, aes(x=as.factor(light) )) +
  geom_bar(color="#6b9bc3", fill=rgb(0.1,0.4,0.5,0.7) ) + 
  geom_text(stat='count', aes(label=..count..), vjust=-1) +
  theme_minimal()

dev.off()



### ------------------------------------------------------------------------ ###
# Motion (binary variable)

table(df$motion)

jpeg(file="../LateX_project/images/chapter2/motion_barplot.jpeg", width=6, height =6, units='in', res=200)

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

jpeg(file="../LateX_project/images/chapter2/co_boxplot.jpeg", width=6, height =6, units='in', res=200)

ggplot(df, aes(y=co)) + 
  geom_boxplot(fill="#6b9bc3", alpha=0.7) + 
  theme(
    axis.text=element_text(size=6.5), 
    axis.title=element_text(size=14, face="bold")
  ) +
  theme_minimal()

dev.off()


jpeg(file="../LateX_project/images/chapter2/co_histogram.jpeg", width=6, height =6, units='in', res=200)

ggplot(df, aes(x=co)) +
  geom_histogram( binwidth=0.0008, fill="#6b9bc3", color="#6b9bc3", alpha=0.7, position = 'identity') +
  geom_rug(aes(x=co, y = NULL)) +
  theme(plot.title = element_text(size=15)) +
  theme_minimal()

dev.off()



### ------------------------------------------------------------------------ ###
# Humidity

summary(df$humidity)
var(df$humidity)
moments::skewness(df$humidity)
moments::kurtosis(df$humidity)


# EDA

jpeg(file="../LateX_project/images/chapter2/humidity_boxplot.jpeg", width=6, height =6, units='in', res=200)

ggplot(df, aes(y=humidity)) + 
  geom_boxplot(fill="#6b9bc3", alpha=0.7) + 
  theme(
    axis.text=element_text(size=6.5), 
    axis.title=element_text(size=14, face="bold")
  ) +
  theme_minimal()

dev.off()


jpeg(file="../LateX_project/images/chapter2/humidity_histogram.jpeg", width=6, height =6, units='in', res=200)

ggplot(df, aes(x=humidity)) +
  geom_histogram( binwidth=5, fill="#6b9bc3", color="#6b9bc3", alpha=0.7, position = 'identity') +
  geom_rug(aes(x=humidity, y = NULL)) +
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

jpeg(file="../LateX_project/images/chapter2/lpg_boxplot.jpeg", width=6, height =6, units='in', res=200)

ggplot(df, aes(y=lpg)) + 
  geom_boxplot(fill="#6b9bc3", alpha=0.7) + 
  theme(
    axis.text=element_text(size=6.5), 
    axis.title=element_text(size=14, face="bold")
  ) +
  theme_minimal()

dev.off()


jpeg(file="../LateX_project/images/chapter2/lpg_histogram.jpeg", width=6, height =6, units='in', res=200)

ggplot(df, aes(x=lpg)) +
  geom_histogram( binwidth=0.0005, fill="#6b9bc3", color="#6b9bc3", alpha=0.7, position = 'identity') +
  geom_rug(aes(x=lpg, y = NULL)) +
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

jpeg(file="../LateX_project/images/chapter2/smoke_boxplot.jpeg", width=6, height =6, units='in', res=200)

ggplot(df, aes(y=smoke)) + 
  geom_boxplot(fill="#6b9bc3", alpha=0.7) + 
  theme(
    axis.text=element_text(size=6.5), 
    axis.title=element_text(size=14, face="bold")
  ) +
  theme_minimal()

dev.off()


jpeg(file="../LateX_project/images/chapter2/smoke_histogram.jpeg", width=6, height =6, units='in', res=200)

ggplot(df, aes(x=smoke)) +
  geom_histogram( binwidth=0.002, fill="#6b9bc3", color="#6b9bc3", alpha=0.7, position = 'identity') +
  geom_rug(aes(x=smoke, y = NULL)) +
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

jpeg(file="../LateX_project/images/chapter2/temp_boxplot.jpeg", width=6, height =6, units='in', res=200)

ggplot(df, aes(y=temp)) + 
  geom_boxplot(fill="#6b9bc3", alpha=0.7) + 
  theme(
    axis.text=element_text(size=6.5), 
    axis.title=element_text(size=14, face="bold")
  ) +
  theme_minimal()

dev.off()


jpeg(file="../LateX_project/images/chapter2/temp_histogram.jpeg", width=6, height =6, units='in', res=200)

ggplot(df, aes(x=temp)) +
  geom_histogram( binwidth=1, fill="#6b9bc3", color="#6b9bc3", alpha=0.7, position = 'identity') +
  geom_rug(aes(x=temp, y = NULL)) +
  theme(plot.title = element_text(size=15)) +
  theme_minimal()

dev.off()



### ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

###------------------------------###
#   Principal Component Analysis   #
###------------------------------###


# Multivariate EDA

df_numeric = df %>% select_if(is.numeric)
df_numeric_scaled = as.data.frame(scale(df_numeric))


# Pairplot of all numerical variables
jpeg(file="../LateX_project/images/chapter3/pairplot_numeric_dataset.jpeg", width=6, height =6, units='in', res=200)

ggpairs(df_numeric,
        upper = list(continuous = "density", combo = "box_no_facet"),
        lower = list(continuous = "points", combo = "dot_no_facet"))

dev.off()


# A better way to visualize correlation matrix
jpeg(file="../LateX_project/images/chapter3/correlation_matrix.jpeg", width=6, height =6, units='in', res=200)

corrplot::corrplot.mixed(cor(df_numeric), upper = "ellipse")

dev.off()


# Pairplot of all numerical variables with device hue
jpeg(file="../LateX_project/images/chapter3/pairplot_numeric_dataset_hue.jpeg", width=6, height =6, units='in', res=200)

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


pr.out = prcomp(df_numeric_scaled)

print(pr.out$sdev)

print(pr.out$rotation)
print(xtable::xtable(pr.out$rotation, type="latex", digits=5))



# CPVE
PVE = df_numeric_scaled.eigen$values / sum(df_numeric_scaled.eigen$values)
round(PVE, 5)

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
jpeg(file="../LateX_project/images/chapter3/scree_plot.jpeg", width=6, height =6, units='in', res=200)

ggplot(data, aes(x=x, y=y)) +
  geom_line( color="grey") +
  geom_point(shape=21, color="black", fill="#69b3a2", size=6) +
  theme_minimal()

dev.off()




jpeg(file="../LateX_project/images/chapter3/pca_plot.jpeg", width=6, height =6, units='in', res=200)

factoextra::fviz_pca_ind(pr.out, legend="top", habillage=df$device, palette=c("green", "red", "blue"), geom="point", ggtheme=theme_minimal())

dev.off()


jpeg(file="../LateX_project/images/chapter3/biplot.jpeg", width=6, height =6, units='in', res=200)

factoextra::fviz_pca_biplot(pr.out, geom="point", ggtheme=theme_minimal())

dev.off()




### ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

###-------------------###
#   Cluster Analysis    #
###-------------------###


df_cl = df[sample(nrow(df), 500), ]

df_cl_numeric = df_cl %>% select_if(is.numeric)
df_cl_numeric_scaled = as.data.frame(scale(df_cl_numeric))



# 1. CLUSTER VALIDATION   

# 1a. Assessing cluster tendency


# Generating the benchmark dataset from a uniform distribution
benchmark_df = apply(df_cl_numeric, 2, function(x){runif(length(x), min(x), max(x))})
benchmark_df = as.data.frame(benchmark_df) # Transform benchmark_df in a data frame cause apply return a vector
benchmark_df_scaled = as.data.frame(scale(benchmark_df))



jpeg(file="../LateX_project/images/chapter4/chapter4.1/pairplot_numeric_dataset.jpeg", width=6, height =6, units='in', res=200)

ggpairs(df_cl_numeric_scaled,
        upper = list(continuous = "density", combo = "box_no_facet"),
        lower = list(continuous = "points", combo = "dot_no_facet"))

dev.off()


jpeg(file="../LateX_project/images/chapter4/chapter4.1/pairplot_benchmark_dataset.jpeg", width=6, height =6, units='in', res=200)

ggpairs(benchmark_df_scaled,
        upper = list(continuous = "density", combo = "box_no_facet"),
        lower = list(continuous = "points", combo = "dot_no_facet"))

dev.off()




# Plot on the PC space
jpeg(file="../LateX_project/images/chapter4/chapter4.1/pcaplot_benchmark_dataset.jpeg", width=6, height=6, units='in', res=200)

factoextra::fviz_pca_ind(prcomp(benchmark_df_scaled), title="PCA - Benchmark Dataset",
                         habillage=df_cl$device, legend="bottom", geom="point", ggtheme=theme_minimal())

dev.off()


jpeg(file="../LateX_project/images/chapter4/chapter4.1/pcaplot_numeric_dataset.jpeg", width=6, height=6, units='in', res=200)

factoextra::fviz_pca_ind(prcomp(df_cl_numeric_scaled), title="PCA - Original Dataset",
                         habillage=df_cl$device, legend="bottom", geom="point", ggtheme=theme_minimal())

dev.off()



# Hopkins statistic
hopkins::hopkins(df_cl_numeric)
hopkins::hopkins(benchmark_df)



# Visual assessment of cluster tendency
jpeg(file="../LateX_project/images/chapter4/chapter4.1/vat_df_numeric.jpeg", width=6, height=6, units='in', res=200)

factoextra::fviz_dist(dist(df_cl_numeric_scaled), show_labels=FALSE)

dev.off()


jpeg(file="../LateX_project/images/chapter4/chapter4.1/vat_benchmark_df.jpeg", width=6, height=6, units='in', res=200)

factoextra::fviz_dist(dist(benchmark_df), show_labels=FALSE)

dev.off()




# 1b. Determining the best number of clusters


# Elbow method
jpeg(file="../LateX_project/images/chapter4/chapter4.1/elbow_method_kmeans.jpeg", width=6, height=6, units='in', res=200)

factoextra::fviz_nbclust(df_cl_numeric_scaled, kmeans, nstart=10, method = "wss") +
  geom_vline(xintercept=3, linetype=2)

dev.off()

jpeg(file="../LateX_project/images/chapter4/chapter4.1/elbow_method_clara.jpeg", width=6, height=6, units='in', res=200)

factoextra::fviz_nbclust(df_cl_numeric_scaled, cluster::clara, method="wss") +
  geom_vline(xintercept=3, linetype=2)

dev.off()

jpeg(file="../LateX_project/images/chapter4/chapter4.1/elbow_method_hcut.jpeg", width=6, height=6, units='in', res=200)

factoextra::fviz_nbclust(df_cl_numeric_scaled, hcut, nstart=10, method="wss") +
  geom_vline(xintercept=3, linetype=2)

dev.off()


# Silhouette method
jpeg(file="../LateX_project/images/chapter4/chapter4.1/silhouette_method_kmeans.jpeg", width=6, height=6, units='in', res=200)

factoextra::fviz_nbclust(df_cl_numeric_scaled, kmeans, nstart=10, method="silhouette")

dev.off()

jpeg(file="../LateX_project/images/chapter4/chapter4.1/silhouette_method_clara.jpeg", width=6, height=6, units='in', res=200)

factoextra::fviz_nbclust(df_cl_numeric_scaled, cluster::clara, method="silhouette")

dev.off()

jpeg(file="../LateX_project/images/chapter4/chapter4.1/silhouette_method_hcut.jpeg", width=6, height=6, units='in', res=200)

factoextra::fviz_nbclust(df_cl_numeric_scaled, hcut, nstart=10, method="silhouette")

dev.off()


# Gap statistic
jpeg(file="../LateX_project/images/chapter4/chapter4.1/gap_method_kmeans.jpeg", width=6, height=6, units='in', res=200)

factoextra::fviz_nbclust(df_cl_numeric_scaled, kmeans, nstart=20, method='gap_stat', nboot=150)

dev.off()

jpeg(file="../LateX_project/images/chapter4/chapter4.1/gap_method_clara.jpeg", width=6, height=6, units='in', res=200)

factoextra::fviz_nbclust(df_cl_numeric_scaled, cluster::clara, method='gap_stat', nboot=150)

dev.off()

jpeg(file="../LateX_project/images/chapter4/chapter4.1/gap_method_hcut.jpeg", width=6, height=6, units='in', res=200)

factoextra::fviz_nbclust(df_cl_numeric_scaled, hcut, nstart=20, method='gap_stat', nboot=150)

dev.off()




# 1c. Cluster validation


# Internal cluster validation
single_eu = clValid::clValid(df_cl_numeric_scaled, nClust=c(3,4), clMethods=c("hierarchical", "kmeans", "clara"), 
                             validation=c("internal", "stability"), metric="euclidean", method="single")

summary(single_eu)



# External cluster validation
km_res = factoextra::eclust(df_cl_numeric_scaled, FUNcluster="kmeans", k=3, nstart=10, graph=FALSE)

jpeg(file="../LateX_project/images/chapter4/chapter4.1/kmeans_viz.jpeg", width=6, height=6, units='in', res=200)

factoextra::fviz_cluster(km_res, geom="point", ellipse.type="norm", palette="jco", ggtheme=theme_minimal())

dev.off()


# Confusion matrix to compare the kmeans result with external information
table(km_res$cluster, df_cl$device)
print(xtable::xtable(table(km_res$cluster, df_cl$device), type="latex", digits=5))


# Silhouette plot on kmeans
jpeg(file="../LateX_project/images/chapter4/chapter4.1/silhouette_kmeans.jpeg", width=6, height=6, units='in', res=200)

factoextra::fviz_silhouette(km_res, palette="jco", ggtheme=theme_minimal())

dev.off()



clara_res = factoextra::eclust(df_cl_numeric_scaled, k=3, FUNcluster="clara", graph=FALSE)

jpeg(file="../LateX_project/images/chapter4/chapter4.1/clara_viz.jpeg", width=6, height=6, units='in', res=200)

factoextra::fviz_cluster(clara_res, geom = "point", ellipse.type = "norm", palette = "jco", ggtheme = theme_minimal())

dev.off()



# Confusion matrix to compare the clara result with external information
table(clara_res$clustering, df_cl$device)
print(xtable::xtable(table(clara_res$clustering, df_cl$device), type="latex", digits=5))



# Silhouette plot on clara
jpeg(file="../LateX_project/images/chapter4/chapter4.1/silhouette_clara.jpeg", width=6, height=6, units='in', res=200)

factoextra::fviz_silhouette(clara_res, palette="jco", ggtheme=theme_minimal())

dev.off()





# ------------------------------------------------------------------------------

# 2. Hierarchical agglomerative clustering


diss = cluster::daisy(df_cl_numeric_scaled)  # Dissimilarity triangular matrix computed with the Gower's index

jpeg(file="../LateX_project/images/chapter4/chapter4.2/dissimilarity_matrix.jpeg", width=6, height =6, units='in', res=200)

factoextra::fviz_dist(diss)  # Dissimilarity matrix graphic representation

dev.off()



# The average linkage method and Ward's method should perform better than the others (need to check)
hc_single = hclust(d=diss, method="single")
hc_complete = hclust(d=diss, method="complete")
hc_average = hclust(d=diss, method="average")
hc_ward = hclust(d=diss, method="ward.D2")  # Ward method with squared distances


# Dendograms
jpeg(file="../LateX_project/images/chapter4/chapter4.2/dendogram_single.jpeg", width=6, height =6, units='in', res=200)

factoextra::fviz_dend(hc_single, k=3, cex=0.5)

dev.off()

jpeg(file="../LateX_project/images/chapter4/chapter4.2/dendogram_complete.jpeg", width=6, height =6, units='in', res=200)

factoextra::fviz_dend(hc_complete, k=3, cex=0.5)

dev.off()

jpeg(file="../LateX_project/images/chapter4/chapter4.2/dendogram_average.jpeg", width=6, height =6, units='in', res=200)

factoextra::fviz_dend(hc_average, k=3, cex=0.5)

dev.off()

jpeg(file="../LateX_project/images/chapter4/chapter4.2/dendogram_ward.jpeg", width=6, height =6, units='in', res=200)

factoextra::fviz_dend(hc_ward, k=3, cex=0.5)

dev.off()



# Need to compare the cophenetic distance vector with the original distance vector for every method to select the best one
cor(diss, cophenetic(hc_single))  # Correlation between original distance vector and cophenetic of single method
cor(diss, cophenetic(hc_complete))  # Correlation between original distance vector and cophenetic of complete method
cor(diss, cophenetic(hc_average))  # Correlation between original distance vector and cophenetic of average method
cor(diss, cophenetic(hc_ward))  # Correlation between original distance vector and cophenetic of ward method
# According to the comparison of the correlations the best method is the average



# Clustering division according to the average method
groupsAverage = cutree(hc_average, k=3)   # Cut the dendogram based on k (number of clusters) or h (dendogram height)
table(groupsAverage)
print(xtable::xtable(table(groupsAverage), type="latex"))


# Clustering division according to the ward method
groupsWard = cutree(hc_ward, k=3)   # Cut the dendogram based on k (number of clusters) or h (dendogram height)
table(groupsWard)
print(xtable::xtable(table(groupsWard), type="latex"))


# Confusion matrix
table(groupsAverage, groupsWard)  # Useful to compare two different clustering partitions
print(xtable::xtable(table(groupsAverage, groupsWard), type="latex"))




# Scatterplot with points colored according to the cluster of membership in groups
jpeg(file="../LateX_project/images/chapter4/chapter4.2/pairplot_cluster_average.jpeg", width=6, height=6, units='in', res=200)

pairs(df_cl_numeric_scaled, pch=16, gap=0, col=groupsAverage)

dev.off()

jpeg(file="../LateX_project/images/chapter4/chapter4.2/pairplot_cluster_ward.jpeg", width=6, height=6, units='in', res=200)

pairs(df_cl_numeric_scaled, pch=16, gap=0, col=groupsWard)

dev.off()



# Clusters representation in the space of the principal components
jpeg(file="../LateX_project/images/chapter4/chapter4.2/pcaplot_cluster_ward.jpeg", width=6, height=6, units='in', res=200)

factoextra::fviz_cluster(list(data = df_cl_numeric_scaled, cluster = groupsWard),
                         palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
                         ellipse.type = "convex", # Concentration ellipse
                         repel = TRUE, # Avoid label overplotting (slow)
                         show.clust.cent = FALSE, ggtheme = theme_minimal())

dev.off()

jpeg(file="../LateX_project/images/chapter4/chapter4.2/pcaplot_cluster_average.jpeg", width=6, height=6, units='in', res=200)

factoextra::fviz_cluster(list(data=df_cl_numeric_scaled, cluster=groupsAverage),
                         palette=c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
                         ellipse.type="convex", # Concentration ellipse
                         repel=TRUE, # Avoid label overplotting (slow)
                         show.clust.cent=FALSE, ggtheme=theme_minimal())

dev.off()




# ------------------------------------------------------------------------------

# 3. Partitioning clustering 

# KMEANS


km = kmeans(df_cl_numeric_scaled, centers=3, nstart=25)


# Analizing km outputs

print(km$size)   # Important to evaluate the presence of outliers

print(km$centers)   # Very important to analize Cluster means to characterize the clusters
print(xtable::xtable(km$centers, type="latex", digits=5))


clusterVariability = scales::label_percent()(km$betweenss/km$totss)   # Variability explained by the clustering
print(clusterVariability)


# Visualize the clusters in the scatterplot
jpeg(file="../LateX_project/images/chapter4/chapter4.3/pairplot_cluster_kmeans.jpeg", width=6, height=6, units='in', res=200)

pairs(df_cl_numeric_scaled, pch=16, gap=0, col=km$cluster)

dev.off()



# Visualize the clusters in the scatterplot with centroid
RDatasetCentr_scaled = rbind(df_cl_numeric_scaled, km$centers)
clusterMembership = km$cluster
clusterMembership_new = c(km$cluster, rep(4,3)) # Adding 3 units with cluster membership 4

jpeg(file="../LateX_project/images/chapter4/chapter4.3/pairplot_cluster_kmeans_centroids.jpeg", width=6, height=6, units='in', res=200)

pairs(RDatasetCentr_scaled, pch=16, gap=0,
      col=c("red", "green", "blue", "black")[clusterMembership_new],  # Assigning points colors: black for centroids
      cex=c(0.7, 0.7, 0.7, 1.3)[clusterMembership_new]) # Assigning points dimensions: 0.7 for all point, 1.3 for centroids

dev.off()



# Analizing the Cluster means of the original dataset 
aggregate(df_cl_numeric, by=list(cluster=km$cluster), mean)
print(xtable::xtable(aggregate(df_cl_numeric, by=list(cluster=km$cluster), mean), type="latex", digits=5))



# Visualize the clusters in the PC space
jpeg(file="../LateX_project/images/chapter4/chapter4.3/pcaplot_cluster_kmeans.jpeg", width=6, height=6, units='in', res=200)

fviz_cluster(km, data=df_cl_numeric_scaled, palette=c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
             ellipse.type="euclid", # Concentration ellipse
             star.plot=TRUE, # Add segments from centroids to items
             repel=TRUE, # Avoid label overplotting (slow)
             ggtheme=theme_minimal())

dev.off()




# KMEDOIDS


clara_cl = cluster::clara(df_cl_numeric_scaled, 3, metric="euclidean", stand=FALSE)


jpeg(file="../LateX_project/images/chapter4/chapter4.3/pairplot_cluster_clara.jpeg", width=6, height=6, units='in', res=200)

pairs(df_cl_numeric_scaled, pch=16, gap=0, col=clara_cl$clustering)

dev.off()



# Visualize the clusters in the PC space
jpeg(file="../LateX_project/images/chapter4/chapter4.3/pcaplot_cluster_kmedoids.jpeg", width=6, height=6, units='in', res=200)

fviz_cluster(clara_cl, data=df_cl_numeric_scaled, palette=c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
             ellipse.type="euclid", # Concentration ellipse
             star.plot=TRUE, # Add segments from centroids to items
             repel=TRUE, # Avoid label overplotting (slow)
             ggtheme=theme_minimal())

dev.off()





# Confusion matrix to compare the clara result with kmeans result
table(km_res$cluster, clara_cl$cluster)
print(xtable::xtable(table(km_res$cluster, clara_cl$cluster), type="latex", digits=5))



# Analizing the Cluster means of the original dataset 
aggregate(df_cl_numeric, by=list(cluster=clara_cl$cluster), mean)
print(xtable::xtable(aggregate(df_cl_numeric, by=list(cluster=clara_cl$cluster), mean), type="latex", digits=5))




# ------------------------------------------------------------------------------

# 4. Model based clustering 


mbc = mclust::Mclust(df_cl_numeric_scaled, G=1:10) # Considering K from 1 to 10

str(mbc)
summary(mbc$BIC)



jpeg(file="../LateX_project/images/chapter4/chapter4.4/bic_plot.jpeg", width=6, height=6, units='in', res=200)

plot(mbc, what = "BIC", ylim = range(mbc$BIC, na.rm = TRUE), legendArgs = list(x = "bottomleft"))

dev.off()


summary(mbc)

print(mbc$modelName)
print(mbc$G)


# head matrix of soft clusterization
head(round(mbc$z, 6), 20)
print(xtable::xtable(head(round(mbc$z, 6), 20), type="latex", digits=5))


# head matrix of hard clusterization
head(round(mbc$classification, 6), 20)


jpeg(file="../LateX_project/images/chapter4/chapter4.4/pairplot_cluster_modelbased.jpeg", width=6, height=6, units='in', res=200)

pairs(df_cl_numeric_scaled, pch=16, gap=0, col=mbc$classification)

dev.off()


# External cluster validation
table(df_cl$device, mbc$classification)
print(xtable::xtable(table(df_cl$device, mbc$classification), type="latex", digits=5))


mclust::adjustedRandIndex(df_cl$device, mbc$classification)
# 0 = random partition, 1 = perfect agreement


jpeg(file="../LateX_project/images/chapter4/chapter4.4/best_number_cluster.jpeg", width=6, height=6, units='in', res=200)

factoextra::fviz_mclust(mbc, "BIC", palette="jco")

dev.off()

jpeg(file="../LateX_project/images/chapter4/chapter4.4/pcaplot_classification.jpeg", width=6, height=6, units='in', res=200)

factoextra::fviz_mclust(mbc, "classification", geom="point", pointsize=1.5, palette="jco")

dev.off()

jpeg(file="../LateX_project/images/chapter4/chapter4.4/pcaplot_classification_uncertainty.jpeg", width=6, height=6, units='in', res=200)

factoextra::fviz_mclust(mbc, "uncertainty", palette="jco")

dev.off()

jpeg(file="../LateX_project/images/chapter4/chapter4.4/bivariate_marginalization.jpeg", width=6, height=6, units='in', res=200)

factoextra::fviz_mclust(mbc, "uncertainty", palette="jco", choose.vars=c("temp", "smoke"))

dev.off()



