library(factoextra)
library(NbClust)
# Elbow method 6 根據每個資料點的分散以及聚合來衡量分群的結果
traffic<-traffic[,-c(1,2,3)]
traffic<-scale(traffic)

fviz_nbclust(traffic, kmeans, method = "wss") +
  geom_vline(xintercept = 6, linetype = 2) + # add line for better visualisation
  labs(subtitle = "Elbow method") # add subtitle


#2
fviz_nbclust(traffic, kmeans, method = "silhouette") +
  labs(subtitle = "Silhouette method")


library(NbClust)
NbClust(data = traffic, distance = "euclidean", min.nc = 2, max.nc = 6, method = "kmeans", index = "all", alphaBeale = 0.1)

pseudoF (traffic)

# Gap statistic 10
set.seed(42)
fviz_nbclust(traffic, kmeans,
             nstart = 25,
             method = "gap_stat",
             nboot = 500
) + # reduce it for lower computation time (but less precise results)
  labs(subtitle = "Gap statistic method")



library(cluster)
set.seed(42)
km_res <- kmeans(traffic, centers = 4, nstart = 20)
sil <- silhouette(km_res$cluster, dist(traffic))
fviz_silhouette(sil)
library(factoextra)
fviz_cluster(km_res, traffic, ellipse.type = "norm")


index.1<-which(sil[,1]==1)
index.2<-which(sil[,1]==2)
index.3<-which(sil[,1]==3)
index.4<-which(sil[,1]==4)

traffic3<-traffic
colnames(traffic3) <- paste0("X", 4:17)

traffic3<-as.data.frame(traffic3)
all_df <- bind_rows(traffic3[index.1,], traffic3[index.2,], traffic3[index.3,], traffic3[index.4,], .id = "Source")


means <- all_df %>%
  group_by(Source) %>%
  summarise_all(mean)

means_df <- gather(means, key = "Column", value = "Mean", -Source)


means_df$Column <- factor(means_df$Column, levels = paste0("X", 4:17))
ggplot(means_df, aes(x = Column, y = Mean, color = Source)) +
  geom_point(size = 3) +
  labs(x = "Column", y = "Mean", color = "Source") +
  ggtitle("Mean Comparison") +
  theme_minimal()
## data ####







####################################

### HC tree ###
traffic<-as.data.frame(traffic)
library(dplyr)
sampled_df <- sample_n(traffic, 100)

E.dist <- dist(sampled_df, method="euclidean") # 歐式距離
M.dist <- dist(sampled_df, method="manhattan") # 曼哈頓距離
#other function: get_dist(iris[,1:4], method="euclidean"), 
#get_dist() has some correlation measures

# visualize
fviz_dist(E.dist,gradient = list(low = "red", mid = "pink", high = "white")) #heatmap

# By Euclidean Distance
tree1 <- hclust(E.dist, method="ward.D2")
plot(tree1, xlab="Euclidean",h=-1)
abline(h=40,col="red")

# Gap statistic for hierarchical clustering
fviz_nbclust(traffic, FUN = hcut, method = "wss")

# By Manhattan Distance
tree2 <- hclust(M.dist) 
plot(tree2, xlab="Manhattan")

cluster <- cutree(tree1, k=3)  # 分成三群, can cut by h=10 too
cluster    
table(cluster)
which(cluster==1)

rect.hclust(tree1,k=3,border="red")

table(cluster, traffic1$地形)       # 分群結果和實際結果比較


# find the best cluster size
p = fviz_nbclust(iris[,1:4], 
                 FUNcluster = hcut,  # hierarchical clustering
                 method = "wss",     # total within sum of square
                 k.max = 20          # max number of clusters to consider
) 
p
(p = p + labs(title="Elbow Method for HC") )

p + geom_vline(xintercept = 3,       # 在 X=3的地方 
               linetype = 2)   

# Agglomerative coefficient
# Compute with agnes
library(cluster)
ac_ward <- agnes(E.dist, method = "ward")
ac_ward$ac

pltree(ac_ward, hang = -1) #, main="tree"

m <- c( "average", "single", "complete", "ward")
names(m) <- c( "average", "single", "complete", "ward")

# function to compute coefficient
ac <- function(x) {
  agnes(E.dist, method = x)$ac
}

sapply(m,ac)


