library(factoextra)
library(gridExtra)
library(dplyr)
library(hrbrthemes)
library(ggdendro)
library(ggplot2)


#select file "customers.csv"
f=file.choose()


#select data
cus_dat = read.csv(f, header = TRUE)


#add total amount spent in dollars for each customer as this can be an important variable for the supermarkets to know
total_spent <- rowSums(cus_dat)


#var and mean for all columns, high difference between mean and var so variables are standardised, euclidean distance is not highly influenced by the higher unit variables 
apply(cus_dat, 2, var)
apply(cus_dat, 2, mean)
sc_cus_dat <- scale(cus_dat)


#data all numeric we use kmeans method for clustering.
#sstart means we start with 50 initial centroids and chooses the best one out of the 50. So we explore more options to get a better result 
#function to return total within cluster variation for any k where we start we 50 different iterations of the first centroid 
km_twss <- function(k) {
  clus <- kmeans(sc_cus_dat, k, nstart=50)
  return(clus$tot.withinss)
}


#see total within cluster variation for k cluster from 2:20
set.seed(123)
max_k <- 20
#perform k means clustering from 2:20 using the function created before
twss <- sapply(2:max_k, km_twss)


#plot graph to visualise elbow
#create a data frame for k cluster with the TWSS to plot relationship 
elbow <- data.frame(2:max_k, twss)
#plot the elbow and this shows that after about 5 clusters the fall in total WSS reduces
#WSS reduces this means the valuess observed in each cluster are more similar so the euclidean distance is less far apart
#from the plot shows that k=4 or k=5 as the best cluster
plot(x=elbow[,1], y=elbow[,2], type="b", ylab = "Total WSS", xlab = "k-Number of Clusters", 
     main="Total Within Cluster Variation for Each k-Number Cluster")


#look at the results of the plot and compare to see which gives a good clustering
#perform clustering from k= 4 and k=5
km_4 <- kmeans(sc_cus_dat, 4, nstart=50)
km_5 <- kmeans(sc_cus_dat, 5, nstart=50)
#plot the clustering results
pl_4 <- fviz_cluster(km_4, geom = "point", data = sc_cus_dat) + 
  ggtitle("Cluster Results for Consumer Spending Habits where K=4") +
  xlab("PC1") + 
  ylab("PC2")
pl_5 <- fviz_cluster(km_5, geom = "point", data = sc_cus_dat) + 
  ggtitle("Cluster Results for Consumer Spending Habits where K=5") + 
  xlab("PC1") + 
  ylab("PC2")
#compare the cluster results
grid.arrange(pl_4, pl_5)
#the within TSS show that k=5 is better and graphically it offers more district clusters with less overlapping of classification of observations  


#hierarchical clustering
hc.complete = hclust(dist(sc_cus_dat), method = "complete")
ggdendrogram(hc.complete, rotate = F, theme_dendro = FALSE,labels = FALSE)+ggtitle("Dendrogram for Consumer Spending Habits")+
  xlab("Number of Clusters")+
  ylab("Height of Dendrogram")
#Dendrogram supports cutting the tree at a cluster of around 5 as beyond this point the clusters become to complex


#convert total spent variable into a categorical variable and see if we can use this to infer info from the 5 clusters
summary(total_spent)
boxplot(total_spent)


#create 5 levels for total consumer spending. The level bands are determined by the LQ, Median, UQ, Min and Max. With UQ split between 2 as high range of values
total_spent_fac <- cut(total_spent, breaks=c(0, 17500, 27500, 41300, 6000, 200000), 
                       labels=c("Lowest Consumer Spending", "Low Consumer Spending ", "Medium Consumer Spending", "High Consumer Spending", "Highest Consumer Spending"))
clust_spend_table <- table(km_5$cluster, total_spent_fac)
#chi squared test for independence from the response variable and the indicator variable
#null hypothesis is that they independence We get a low p value less than 0.05 so we reject the null so accept the alternative that they are dependent
chisq.test(clust_spend_table)


#perform PCA and plot graph can assist in interpretation of the clusters
cus_pc = prcomp(cus_dat, scale = TRUE)
#PC1 is affected by spending on Grocery, Milk and Detergents PC2 affected by spending on Fresh, Frozen and Delicassen products.
cus_pc
pl_5
pc_plot <- biplot(cus_pc, scale = 0)


#analysis of products sales distribution per cluster----
final_clustering = cbind(km_5$cluster,cus_dat) %>% as.data.frame()
colnames(final_clustering)=c("Cluster",colnames(cus_dat))


summary(final_clustering %>% filter(Cluster==1))
summary(final_clustering %>% filter(Cluster==2))
summary(final_clustering %>% filter(Cluster==3))
summary(final_clustering %>% filter(Cluster==4))
summary(final_clustering %>% filter(Cluster==5))


gg_Fresh=ggplot(final_clustering,aes(x=Fresh,group=Cluster,color=Cluster))+
  scale_color_gradientn(colours = rainbow(5)) +
  geom_density(adjust=1.5, alpha=.4) +
  theme_ipsum()+
  ggtitle("Fresh")+
  xlab("Customer Spending in USD")+
  ylab("Probability of Spending ")+
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))


gg_Milk=ggplot(final_clustering,aes(x=Milk,group=Cluster,color=Cluster))+
  scale_color_gradientn(colours = rainbow(5)) +
  geom_density(adjust=1.5, alpha=.4) +
  theme_ipsum()+
  ggtitle("Milk")+
  xlab("Customer Spending in USD")+
  ylab("Probability of Spending ")+
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))


gg_Grocery=ggplot(final_clustering,aes(x=Grocery,group=Cluster,color=Cluster))+
  scale_color_gradientn(colours = rainbow(5)) +
  geom_density(adjust=1.5, alpha=.4) +
  theme_ipsum()+
  ggtitle("Groceries")+
  xlab("Customer Spending in USD")+
  ylab("Probability of Spending ")+
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))


gg_Frozen=ggplot(final_clustering,aes(x=Frozen,group=Cluster,color=Cluster))+
  scale_color_gradientn(colours = rainbow(5)) +
  geom_density(adjust=1.5, alpha=.4) +
  theme_ipsum()+
  ggtitle("Frozen")+
  xlab("Customer Spending in USD")+
  ylab("Probability of Spending ")+
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))


gg_Detergents_Paper=ggplot(final_clustering,aes(x=Detergents_Paper,group=Cluster,color=Cluster))+
  scale_color_gradientn(colours = rainbow(5)) +
  geom_density(adjust=1.5, alpha=.4) +
  theme_ipsum()+
  ggtitle("Detergents")+
  xlab("Customer Spending in USD")+
  ylab("Probability of Spending ")+
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))


gg_Delicassen=ggplot(final_clustering,aes(x=Delicassen,group=Cluster,color=Cluster))+
  scale_color_gradientn(colours = rainbow(5)) +
  geom_density(adjust=1.5, alpha=.4) +
  theme_ipsum()+
  ggtitle("Delicatessen")+
  xlab("Customer Spending in USD")+
  ylab("Probability of Spending ")+
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))


#plots show number sales of each product are distributed in each category
grid.arrange(gg_Delicassen ,gg_Frozen ,gg_Grocery ,gg_Milk ,gg_Fresh ,gg_Detergents_Paper)


