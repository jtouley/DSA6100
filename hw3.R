rm(list=ls())
#some of these may not be needed - this data set was used for a seperate project last semester that makes up a combination of plm, glm, lm and clustering using k-means
library(dplyr)
library(plotly)
library(stringr)
library(cluster)
library(FactoMineR)
library(factoextra)
library(ggplot2)
library(reshape2)
library(ggthemes)
library(NbClust)
# data available here: https://drive.google.com/file/d/1wqoU8Fi-ZyG_iiD5PdaDD1_J7VwebJLD/view
data  = read.csv("C:/Users/jason/Downloads/Updated Master Data.csv")

print(data)
View(data)
str(data)
#This allows us to see the curret structure of the data and identify objects that may need to be modified prior to building our model
attach(data)
#cleaning data
#we scale the data so that all the means of the nonfactor data are 0
#we also change the numbers that reresent the countrires to their names in characters. This is to allow for the map function we used in ggplot to match up and display the countries with their respective colors/groupings
data[, 3:13] <- scale(data[, 3:13])
data$geo_idx[data$geo_idx==1] <- "Brazil"
data$geo_idx[data$geo_idx==2] <- "China"
data$geo_idx[data$geo_idx==3] <- "France" 
data$geo_idx[data$geo_idx==4] <- "Germany"
data$geo_idx[data$geo_idx==5] <- "India"
data$geo_idx[data$geo_idx==6] <- "Italy"
data$geo_idx[data$geo_idx==7] <- "Japan"
data$geo_idx[data$geo_idx==8] <- "Mexico"
data$geo_idx[data$geo_idx==9] <- "Russia"
data$geo_idx[data$geo_idx==10] <- "South Africa"
data$geo_idx[data$geo_idx==11] <- "UK"
data$geo_idx[data$geo_idx==12] <- "USA"
summary(data)
head(data$geo_idx)

#Map
map <- map_data("world")
map <- left_join(map, data, by = c('region' = 'geo_idx'))
ggplot() + geom_polygon(data = map, aes(x = long, y = lat, group = group,fill = Category)) +
  labs(x=NULL, y=NULL)+theme_minimal()

#correlation and PCA(principle compoent analysis) 
#heatmap
qplot(x=Var1, y=Var2, data=melt(cor(data[, 3:13], use="p")), fill=value, geom="tile") +
  scale_fill_gradient2(limits=c(-1, 1)) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  labs(title="Heatmap of Correlation Matrix", 
       x=NULL, y=NULL)
data.pca <- PCA(data[, 3:13], graph=FALSE)
print(data.pca)
eigenvalues = data.pca$eig
data.pca$var$coord
head(eigenvalues)
#Eigenvalues correspond to the amount of the variation explained by each principal component (PC).
#Scree plots allows us to see the dimensions of variance as displays by the eigenvalues
fviz_screeplot(data.pca, addlabels = TRUE, ylim = c(0, 65))
head(data.pca$var$contrib)
#Almost 80% of the variances contained in the data are retained by the first three principal components.
#circle of correlations
fviz_pca_var(data.pca, col.var="contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)
#this allows us to see which variances are most closely correlated with PC1, PC2 ad PC3
#The Hubert index is a graphical method of determining the number of clusters. According to this the best number of clusters is 3
number <- NbClust(data[, 3:13], distance="euclidean",
                  min.nc=2, max.nc=15, method='ward.D', index='all', alphaBeale = 0.1)
head(data.pca$ind$coord)
#prcomp()
#The prcomp function returns an object of class prcomp, which have some methods available. The print method returns the standard deviation of each of the four PCs, and their rotation (or loadings), which are the coefficients of the linear combinations of the continuous variables.
data.pca1 = prcomp(data[, 3:13], scale. = TRUE)
print(data.pca1)
head(data.pca1$rotation)
# create data frame with scores
scores = as.data.frame(data.pca1$x)

# plot of observations
ggplot(data = scores, aes(x = PC1, y = PC2, label = rownames(scores))) +
  geom_hline(yintercept = 0, colour = "gray65") +
  geom_vline(xintercept = 0, colour = "gray65") +
  geom_text(colour = "tomato", alpha = 0.8, size = 4) +
  ggtitle("PCA plot of GDP Growth Factors")


#PAM - Partitioning Around Medoids
number <- NbClust(data[, 3:13], distance="euclidean",
                  min.nc=2, max.nc=10, method='ward.D', index='all', alphaBeale = 0.1)

set.seed(2017)
pam <- pam(data[, 3:11], diss=FALSE, 3, keep.data=TRUE)
fviz_silhouette(pam)
data$geo_idx[pam$id.med]
fviz_cluster(pam, stand = FALSE, geom = "point",
             ellipse.type = "norm")

set.seed(2017)
GDPCluster <- kmeans(data[, 3:13], 3, nstart = 15)
GDPCluster
table(GDPCluster$cluster, data$Category)
GDPCluster$cluster <- as.factor(GDPCluster$cluster)
ggplot(data, aes(geo_idx, GDPGROWTH, color = GDPCluster$cluster)) + geom_point()



#linear regression


datafit <- lm(data$GDPCAP~.,data = data)
summary(datafit)

# Standard error of 1925 and R2 of .9799
par(mfrow=c(2,2))
summary(datafit)
plot(datafit)
vif(datafit)

#Removing anything with VIF high than 10 - error goes up 
capfit2 = lm(GDPCAP~YEAR+Cell+INTERNET+PATENT+MIL++FEMEDU +GDPGROWTH+POP,data = data)
summary(capfit2)
plot(capfit2)
vif(capfit2)

#minus energy
capfit3 = lm(GDPCAP~1,data = data)
summary(capfit3)
plot(capfit3)
vif(capfit3)

#step for GDPCAP
#forward - lowest AIC is 2747.4. Variables selected were GDPCAP ~ Category + PATENT + MIL + ENERGY + HEALTH + GDPGROWTH + INTERNET
step(capfit3, scope=list(lower=capfit3, upper=datafit), direction="forward")
#backward - loest AIC is 2734.26
#lm(formula = data$GDPCAP ~ geo_idx + YEAR + ENERGY + INTERNET + 
#PATENT + HEALTH + MIL + INFANT + FEMEDU + GDPGROWTH + Category, 
#data = data)
step(datafit, scope=list(lower=capfit3), direction="backward")
#both - same result as forward selection.
step(capfit3, scope=list(upper=datafit), data=data, direction="both")

bestfit <- lm(formula = data$GDPCAP ~ geo_idx + YEAR + ENERGY + INTERNET + 
                PATENT + HEALTH + MIL + INFANT + FEMEDU + GDPGROWTH + Category, 
                data = data)
summary(bestfit)

#comparing models
anova(bestfit,datafit)
#best fit has a slight edge in degrees of freedom. Error is one unit higher though.
predict(bestfit,newdata = data$expected)
data["Prediction"] <- predict(bestfit,newdata = data$expected)
head(data[c(1,2,11,15)])
print(data[c(1,2,11,15)])
summary(data$GDPCAP)
summary(data$Prediction)
#summary - medians ar less than $1500 apart - mins and maxes show greater seperation.
par(mfrow=c(1,2))
hist(data$GDPCAP)
hist(data$Prediction)
#histograms are close, but there is clear seperation between the actual data and the prediction
#GDP side by side
plot(data$YEAR,data$GDPCAP, col = c("red"))
plot(data$YEAR,data$Prediction, col = c("blue"))


