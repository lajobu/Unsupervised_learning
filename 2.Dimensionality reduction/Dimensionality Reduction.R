install.packages("readxl")
install.packages("corrplot")
install.packages("GGally")
install.packages("smacof")
install.packages("labdsv")
install.packages("vegan")
install.packages("MASS")
install.packages("ape")
install.packages("ggfortify")
install.packages("pca3d")
install.packages("pls")
install.packages("ClusterR")
install.packages("ggrepel")
install.packages("MVN")
install.packages("clusterSim")
install.packages("dimRed")
install.packages('fastICA')
install.packages('umap')
install.packages('ica')

library(readxl) 
library(corrplot)
library(ggplot2)
library(GGally)
library(smacof)
library(labdsv)
library(vegan)
library(MASS)
library(ape)
library(ggfortify)
library(FactoMineR)
library(factoextra)
library(pca3d)
library(pls)
library(ClusterR)
library(ggrepel)
library(MVN)
library(clusterSim)
library(dimRed)
library(fastICA)
library(umap)
library(ica)

getwd()
setwd("/Users/lajobu/Desktop/Project DR/")
getwd()

#1)Manipulation of the data:

library(readxl)
trade <- read_excel("Trade_Project.xlsx")

show(trade) #28x8 - There are 7 features, plus one column with the description of the rows
summary(trade) #Data description

x.trade <- trade[,c(2:8)] #In order to select only the features
View(x.trade) #28x7

x.trade.df <- data.frame(x.trade)

#We are ready to begin with dimensionality reduction:

#1)Correlation between variables - x.trade

trade.cor<-cor(x.trade.df, method="pearson") 
print(trade.cor, digits=2)
corrplot(trade.cor, order ="alphabet") 

ggpairs(x.trade.df, axisLabels="none", columns = c(1:7), upper = list(continuous = wrap("cor", size = 9)))

#The variables are highly correlated

#2)MDS - Multidimensional Scaling:

x.trade.s <- scale(x.trade.df, center=TRUE, scale=TRUE) #Firs find standarized variables
head(x.trade.s)

distance <- dist(x.trade.s, method = "euclidean", upper = TRUE, diag = TRUE) #We computed the distance matrix of our standarized variables
distance <- as.matrix(distance)
head(distance, 5)

n = nrow(distance) #We performed the multidimensional scaling of our distance matrix with cmdscale 
MDS <- cmdscale(distance, k= 2, eig = TRUE, x.ret = TRUE)
X <- MDS$points[,1:2]

#Analazing the matrix, we obtain that the value 11 is more distinct

show(trade[11,1]) #As we can see the country 11 is Germany

g <- ggplot(data.frame(X, trade), aes(X[,2], X[,1], label = C_EU, group=C_EU)) #Plot of two dimensional solution of multidimensional scaling
g + geom_point(color="blue", size=2) + 
  geom_label_repel(box.padding = 0.35, point.padding = 0.5, segment.color = "grey50") +
  ggtitle("Multi-dimensional Scaling on Trade Data") +
  theme_classic()

#We obtain the same as the matrix, Germany is the country more distinct in C_EU

ev <- MDS$eig #Checking the eigen values
gof <- MDS$GOF
print(round(ev,digits=4))

print(gof) #We obtain 0.9544 as the goodness of fit

fitted <- as.matrix(dist(X, method = "euclidean")) #We will plot the fitted distances against the observed distances
fitted <- as.vector(fitted)
observed <- as.vector(distance)
reg <- lm(fitted~observed)
plot(observed, fitted,pch=19, cex=0.4)
abline(lm(fitted~observed), col="red")

#We can see that the regression line goes almost linearly with respect to the fitted and observed distances. This proves that there is a strong correlation between the observed and fitted distances which was our initial objective. Since the line is almost at 45 degree, we can say that correlation is strong and hence, visuall, it gives a good goodness of fit.

print(paste("Coefficient of determination:", summary(reg)$r.squared)) #The coefficent of determination has been stimated as 0.9956

#Now we will try non-metric MDS,

n <- nrow(distance)
init <- scale(matrix(runif(n*2),ncol=2),scale=FALSE)
nmmds.out <- isoMDS(distance, init, k=2, maxit = 100)

Y <- nmmds.out$points
g <- ggplot(data.frame(Y, trade), aes(Y[,2], Y[,1], label = C_EU, group=C_EU)) #Plot of two dimensional solution of multidimensional scaling
g + geom_point(color="blue", size=2) + 
  geom_label_repel(box.padding = 0.35, point.padding = 0.5, segment.color = "grey50") +
  ggtitle("Non-Metric Multi-dimensional Scaling on Trade Data") +
  theme_classic()

nmmds <- metaMDS(comm = distance, distance = "euclidean", k=2)

gof <- goodness(nmmds)

plot(nmmds, display = "sites", type = "n", ylab=c(-4,4))
points(nmmds, display = "sites", cex = 2*gof/mean(gof))

#We will check the stress in case of non-metric multidimensional scaling

stressplot(nmmds, pch = 19, cex=0.75, l.col = "tomato", p.col = "skyblue")

fitted <- as.vector(as.matrix(dist(Y, method = "euclidean")))
observed <- as.vector(as.matrix(distance))
reg <- lm(fitted~observed)
plot(observed, fitted,pch=19, cex=0.4)
abline(lm(fitted~observed), col="red")

#We can observe from the plot that the fitted distances are in almost perfectly linearly related with observed distances.

print(paste("Coefficient of determination:", summary(reg)$r.squared))

#We obtain the Coefficient of Determination of 0.9967

#Now, we will be computing the stress for the dimensions 1, 2, 3, 4, 5, …. and explain how many dimensions are required to obtain a good fit.

stress_vec <- numeric(10)
for(i in seq(10)){
  stress_vec[i] <- metaMDS(distance, distance = "euclidean", k=i)$stress
}

plot(seq(10),stress_vec, type = 'o', ylab = "Stress", xlab = "Number of dimensions",
     col="tomato", pch=19)
abline(h=0.2, lty=2)
abline(h=0.02, lty=3)

#Now in order to obtaing a good fit, we can consider as 2% the stress (as the data is highly correlated), in this case we should use 2 variables, because we obtain a little bit less than 2% of stress

print(paste("Stress Values", stress_vec, sep = ": "))

#3) PCA - Principal Component Analysis:

x.trade.n<-data.Normalization(x.trade.df, type="n1",normalization="column") #First we normalized the data
summary(x.trade.n)

x.trade.pca <- prcomp(x.trade.n, scale = TRUE) #PCA

fviz_eig(x.trade.pca) #Graoh with the eigenvalues, for each dimension

#We can see that the first dimension contains almost the 80% of the variance

fviz_pca_ind(x.trade.pca, #Plot of the columns for eigenvalue for each dimension
             col.ind = "cos2", 
             gradient.cols = c("#800000", "#FFA500", "#008000"),
             repel = TRUE)

fviz_pca_var(x.trade.pca, #Plot of the columns for eigenvalue for each variable
             col.var = "contrib", 
             gradient.cols = c("#800000", "#FFA500", "#008000"),
             repel = TRUE)     #
             
fviz_pca_biplot(x.trade.pca, repel = TRUE, #Plot for columns for each of the dimensions with the vectors
                col.var = "#D2691E",
                col.ind = "#000000")

var <- get_pca_var(x.trade.pca) #Plot of the importance of each variable
corrplot(var$cos2, is.corr = FALSE)

eigvalues <- get_eigenvalue(x.trade.pca) #The eigenvalues
eigvalues

resultsvariables <- get_pca_var(x.trade.pca)
resultsvariables$coord #In order to obtain the coordinates of the variables

resultsvariables$contrib #The contribution to the PCA

resultsvariables$cos2 #Results of the representation

resultsstations <- get_pca_ind(x.trade.pca) #Results for each country
resultsstations$coord
resultsstations$contrib
resultsstations$cos2

install.packages("jpeg")
library(jpeg)

