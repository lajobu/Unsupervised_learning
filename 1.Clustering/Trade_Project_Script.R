library(cluster)
library(factoextra)
library(flexclust)
library(fpc)
library(clustertend)
library(ClusterR)
library(NbClust)
library(readxl)
library(reshape)
library(psych)

getwd() 
setwd("/Users/lajobu/Desktop/Project Clustering/")
getwd()

#import of data

trade <- read_excel("Trade_Project.xlsx")
 
show(trade) #28x6

trade$T_TR_I<-(trade$I_TR_EX + trade$I_TR_IM) #Total intra-EU28 trade, column 7
trade$T_TR_E<-(trade$E_TR_EX + trade$E_TR_IM) #Total extra-EU28 trade, column 8
trade$T_TR_IM<-(trade$I_TR_IM + trade$E_TR_IM) #Total imports-EU28 trade, column 9
trade$T_TR_EX<-(trade$I_TR_EX + trade$E_TR_EX) #Total exports-EU28 trade, column 10
trade$T_GDP_R<-((trade$T_TR_IM + trade$T_TR_EX)/trade$GDP)*100 #Total trade to GDP ratio, column 11

show(trade) #28x11

air <- read_excel("Air transport of goods.xlsx", sheet = "Sheet2", col_types = c("text", "numeric"))
trade$A_T_G <- air$Values #Air transport of goods, column 12

show(trade) #28x12

#data description

summary(trade)

#data selection - the most highly clustered group

X1<-trade[,c(2,11,12)] 
X1 #GDP, T_GDP_R, A_T_G

fviz_pca_ind(prcomp(X1, title = "TOTAL TRADE INTRA VS. TOTAL TRADE EXTRA", geom = "point", ggtheme = theme_classic(),legend = "bottom"))

hop <- get_clust_tendency(X1, n = nrow(X1)-1, graph = FALSE)
hop$hopkins_stat #0.1134274

X2<-trade[,c(2,3,5)] 
X2 #GDP, I_TR_EX, E_TR_EX

fviz_pca_ind(prcomp(X2, title = "TOTAL TRADE INTRA VS. TOTAL TRADE EXTRA", geom = "point", ggtheme = theme_classic(),legend = "bottom"))

hop <- get_clust_tendency(X2, n = nrow(X2)-1, graph = FALSE)
hop$hopkins_stat #0.1777662

X3<-trade[,c(2,4,6)] 
X3 #GDP, I_TR_IM,E_TR_IM

fviz_pca_ind(prcomp(X3, title = "TOTAL TRADE INTRA VS. TOTAL TRADE EXTRA", geom = "point", ggtheme = theme_classic(),legend = "bottom"))

hop <- get_clust_tendency(X3, n = nrow(X3)-1, graph = FALSE)
hop$hopkins_stat #0.1862818

X4<-trade[,c(7,8,12)] 
X4 #T_TR_I, T_TR_E, A_T_G

fviz_pca_ind(prcomp(X4, title = "TOTAL TRADE INTRA VS. TOTAL TRADE EXTRA", geom = "point", ggtheme = theme_classic(),legend = "bottom"))

hop <- get_clust_tendency(X4, n = nrow(X4)-1, graph = FALSE)
hop$hopkins_stat #0.1374022

X5<-trade[,c(11,8,12)] 
X5#I_TR_EX, E_TR_EX, T_TR_E

fviz_pca_ind(prcomp(X5, title = "TOTAL TRADE INTRA VS. TOTAL TRADE EXTRA", geom = "point", ggtheme = theme_classic(),legend = "bottom"))

hop <- get_clust_tendency(X5, n = nrow(X5)-1, graph = FALSE)
hop$hopkins_stat #0.1448834


#To go further, I will select X1, as it has the less Hopkings value

get_clust_tendency(X1, 2, graph=TRUE, gradient=list(low="red", mid="white", high="blue"), seed = 123) 
get_clust_tendency(X1, 3, graph=TRUE, gradient=list(low="red", mid="white", high="blue"), seed = 123) 
get_clust_tendency(X1, 4, graph=TRUE, gradient=list(low="red", mid="white", high="blue"), seed = 123) 
get_clust_tendency(X1, 5, graph=TRUE, gradient=list(low="red", mid="white", high="blue"), seed = 123) 

#Selecting the number of clusters

NbClust(X1, distance="euclidean", min.nc=2, max.nc=10, method="ward.D2", index="dindex") #D index

NbClust(X1, distance="euclidean", min.nc=2, max.nc=10, method="ward.D2", index="hubert") #Hubert

fviz_nbclust(X1, FUNcluster=kmeans, method="silhouette")+theme_classic() #K-means - silhouette

fviz_nbclust(X1, kmeans, method="wss")+theme_classic() #K-means - wss

fviz_nbclust(X1, pam, method="silhouette")+theme_classic() #PAM - silhouette

fviz_nbclust(X1, pam, method="wss")+theme_classic() #PAM - wss

#I will consider 3 clusters

#Clustering - K-means

kmeans <- eclust(X1,FUNcluster="kmeans", k=3,hc_metric = "euclidean")

kmeans.sil<-silhouette(kmeans$cluster, dist(X1))
fviz_silhouette(kmeans.sil)

#Clustering - PAM

pam <- eclust(X1,FUNcluster="pam", k=3,hc_metric = "euclidean")

pam.sil<-silhouette(pam$cluster, dist(X1))
fviz_silhouette(pam.sil)

#I chose K-mean as the best method to continue

d1<-cclust(X1, 3, dist="euclidean")
shadow(d1)

plot(shadow(d1))

#Looks like that 3 is not the correct number of cluster, hence I will try with 2

d2<-cclust(X1, 2, dist="euclidean")
shadow(d2)

plot(shadow(d2))

#Two clusters, clustering K-means

kmeans1 <- eclust(X1,FUNcluster="kmeans", k=2,hc_metric = "euclidean")

kmeans1.sil<-silhouette(kmeans1$cluster, dist(X1))
fviz_silhouette(kmeans1.sil)

#The results are better with two clusters

#Results

stripes(d2)

#Boxplots

ToCluster.c<-cbind(X1, kmeans1$cluster)
colnames(ToCluster.c)[4]<-c("Group")

df.m <- melt(ToCluster.c, id.var = "Group")
df.m$Group <- as.character(df.m$Group)

p <- ggplot(data = df.m, aes(x=variable, y=value)) +
  geom_boxplot(aes(fill = Group),outlier.size = 1) +
  facet_wrap( ~ variable, scales="free", ncol = 2) +
  xlab(label = NULL) + ylab(label = NULL) + ggtitle("Boxplots for 3 Groups of Clients") +
  guides(fill=guide_legend(title="Groups"))

p 