##########################
# PROBLEM 1              #
#########################
# setup
# clear the environment
rm(list=ls())

DATA_DIR <- './data'
IMAGES_DIR <- './images'
OUTPUT_DIR <- './output'

make_dir <- function(d) {
    if (file.exists(d)) unlink(d, recursive=TRUE, force=TRUE)
    dir.create(d)
}
lapply(c(IMAGES_DIR, OUTPUT_DIR),make_dir)


## function that concatenates strings (useful for directory paths)
concat <- function(x1,x2) {
    result <- paste(x1,x2,sep="")
    return(result)
}

## function that checks to see if a package is installed and,if not,installs it
## portions of this code came from http://stackoverflow.com/questions/9341635/how-can-i-check-for-installed-r-packages-before-running-install-packages
load_package <- function(x) {
    if (x %in% rownames(installed.packages())) { 
        print(concat("package already installed: ", x))
    }
    else { 
        install.packages(x) 
    }
    library(x, character.only=TRUE)
}

# get the data
data <- read.table(concat(DATA_DIR, "/beetle.txt"))  # read text file 
# how many are n/a?
sum(is.na(data))
head(which(is.na(data)))
# how many are NULL?
sum(is.null(data))
# how many are blank?
length(which(data == ""))
str(data)
summary(data)
load_package("psych")
describe(data)

# data dictionary
# V1: flea beetle species; (a) Halticus oleracea (b) Halticus cardourum
# V2: thorax length (THORAX) in microns
# V3: elytra length (ELYTRA) in microns
# V4: length of second antennal joint (AJ2) in microns
# V5: length of third antennal joint (AJ3) in microns

# relabel the variables
colnames(data) <- c("species", "thorax", "elytra", "aj2", "aj3")
# label the classifier
data$species_name <- ifelse(data$species == "a", "oleracea","cardourum")
head(data)

# scatterplot matrix - no grouping
load_package("car")
scatterplotMatrix(data[,2:5], 
                  diagonal="density",
                  main="Scatterplot Matrix for Flea Beetle Species")
# grouped by species
scatterplotMatrix(~thorax + elytra + aj2 + aj3|species_name, 
                  data=data, 
                  diagonal="density",
                  main="Scatterplot Matrix for Flea Beetle Species")
# color-coded
pairs(data[c("thorax", "elytra", "aj2", "aj3")], 
      main="Scatterplot Matrix for Flea Beetle Species", 
      pch=22, 
      bg=c("red", "blue")[unclass(data$species)])

# create the covariance matrix
cm <- cov(data[2:5])
cov_eigs <- eigen(cm)
cov_eigs$values
cov_eigs$vectors
plot(cov_eigs$values, type="b")
# create the correlation matrix
cr <- cor(data[2:5])
cor_eigs <- eigen(cr)
cor_eigs$values
cor_eigs$vectors
plot(cor_eigs$values, type="b")

# Linear Discriminant Analysis with Jacknifed Prediction
load_package("MASS")
(lda <- lda(species_name ~ ., data=data[,2:6]))

############################
# answer to i.             #
############################
# Box's M: It performs the Box's M-test for homogeneity of covariance matrices 
# obtained from multivariate normal data according to one classification factor. 
# The test is based on the chi-square approximation.
load_package("biotools")
boxM(data[,2:5], grouping=data$species_name)
# Box's M-test for Homogeneity of Covariance Matrices
# 
# data:  data[, -1]
# Chi-Sq (approx.) = 8.4516, df = 10, p-value = 0.5848

############################
# answer to ii.            #
############################
ld_coefs <- lda$scaling
round(ld_coefs,3)
# Z = -0.093 * thorax + 0.039 * elytra + 0.024 * aj2 + 0.037 * aj3
# scaled so that their mean value is zero and variance is one

############################
# answer to iii.           #
############################
# make predictions
plda <- predict(lda)
ldahist(data=plda$x[,1], g=data$species_name)
new.beetle <- data.frame(thorax=184,
                         elytra=275,
                         aj2=143,
                         aj3=192)
predict(lda, newdata = new.beetle)
# we would classify this new specimen as oleracea

############################
# answer to iv.            #
############################
# Confusion Matrix:
(confusion_matrix <- table(plda$class, data$species_name))
# estimate the percentage of beetles that will be mis-classified
round(1 - diag(prop.table(confusion_matrix)), 4)
# total percent incorrect
round(1 - sum(diag(prop.table(confusion_matrix))), 4)
# answer to v.: 
# cross-validation with leave-one-out
lda2 <- lda(species_name ~ ., data=data[,2:6], CV=TRUE)
# Confusion Matrix:
(confusion_matrix2 <- table(lda2$class, data$species_name))
# estimate the percentage of beetles that will be mis-classified
round(1 - diag(prop.table(confusion_matrix2)), 4)
# total percent incorrect
round(1 - sum(diag(prop.table(confusion_matrix2))), 4)

# Just for fun, plot the Partitions
load_package("klaR")
partimat(species ~ thorax + elytra + aj2 + aj3,
         data=data[,1:5],
         method="lda",
         main="Flea Beetle Data Partitioned by Species")


############################
# PROBLEM 2                #
############################
# setup
# clear the environment
rm(list=ls())

DATA_DIR <- './data'
IMAGES_DIR <- './images'
OUTPUT_DIR <- './output'

make_dir <- function(d) {
    if (file.exists(d)) unlink(d, recursive=TRUE, force=TRUE)
    dir.create(d)
}
lapply(c(IMAGES_DIR, OUTPUT_DIR),make_dir)


## function that concatenates strings (useful for directory paths)
concat <- function(x1,x2) {
    result <- paste(x1,x2,sep="")
    return(result)
}

## function that checks to see if a package is installed and,if not,installs it
## portions of this code came from http://stackoverflow.com/questions/9341635/how-can-i-check-for-installed-r-packages-before-running-install-packages
load_package <- function(x) {
    if (x %in% rownames(installed.packages())) { 
        print(concat("package already installed: ", x))
    }
    else { 
        install.packages(x) 
    }
    library(x, character.only=TRUE)
}

# get the data
load_package("foreign")
data <- read.spss(concat(DATA_DIR, "/faculty.sav"), 
                  use.value.labels=TRUE,
                  to.data.frame = TRUE,
                  use.missings=FALSE)
keeps <- c("facrank", 
           "item13",
           "item14",
           "item15",
           "item16",
           "item17",
           "item18",
           "item19",
           "item20",
           "item21",
           "item22",
           "item23",
           "item24")
data <- data[keeps]
# how many are n/a?
sum(is.na(data))
head(which(is.na(data)))
# how many are NULL?
sum(is.null(data))
# how many are blank?
length(which(data == ""))
str(data)

# Linear Discriminant Analysis with Jacknifed Prediction
load_package("MASS")
(lda <- lda(data$facrank ~ ., data=data))
ld_coefs <- lda$scaling
(round(ld_coefs,3))

data[, 2:13] <- scale(data[,2:13])
str(data)
(lda <- lda(data$facrank ~ ., data=data))
ld_coefs <- lda$scaling
(round(ld_coefs,3))

# make predictions
plda <- predict(lda)
ldahist(data=plda$x[,1], g=data$facrank)
ldahist(data=plda$x[,2], g=data$facrank)

# plot the LDA projection
(prop.lda = lda$svd^2/sum(lda$svd^2))
lda_data <- data.frame(facrank = data[,"facrank"], lda = plda$x)
lda_data <- lda_data[,1:3]
load_package("ggplot2")
load_package("scales")
ggplot(lda_data) + 
    geom_point(aes(lda.LD1, lda.LD2, col = facrank, shape = facrank), size = 2.5) + 
    labs(x = paste("LD1 (", percent(prop.lda[1]), ")", sep=""),
         y = paste("LD2 (", percent(prop.lda[2]), ")", sep="")) +
    ggtitle("LDA Projection of Faculty Data") + 
    theme(plot.title = element_text(lineheight=.8, face="bold")) +
    xlim(-5,5) + 
    ylim(-5,5)

############################
# answer to a.             #
############################
# Determine number of clusters
wss <- (nrow(lda_data[, 2:3])-1)*sum(apply(lda_data[, 2:3],2,var))
for (i in 2:15) wss[i] <- sum(kmeans(lda_data[,2:3], 
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

# K-Means Cluster Analysis for k=4
fit4 <- kmeans(lda_data[,2:3], 4) # 4 cluster solution
# get cluster means 
aggregate(lda_data[,2:3],by=list(fit4$cluster),FUN=mean)
# append cluster assignment
lda_data <- data.frame(lda_data, fit4$cluster)
str(lda_data)

# cluster plot with ellipses
load_package("cluster")
clusplot(lda_data[,2:3], 
         fit4$cluster, 
         color=TRUE, 
         shade=TRUE, 
         labels=4, 
         lines=0,
         main="Cluster Plot of Faculty Data with k=4",
         xlab="LD1",
         ylab="LD2")

# Centroid Plot against 1st 2 discriminant functions
load_package("fpc")
plotcluster(lda_data[,2:3], 
            fit4$cluster,
            main="Cluster Plot of Faculty Data with k=4",
            xlab="LD1",
            ylab="LD2")

# K-Means Cluster Analysis for k=5
fit5 <- kmeans(lda_data[,2:3], 5) # 5 cluster solution
# get cluster means 
aggregate(lda_data[,2:3],by=list(fit5$cluster),FUN=mean)
# append cluster assignment
lda_data <- data.frame(lda_data, fit5$cluster)
str(lda_data)

# cluster plot with ellipses
load_package("cluster")
clusplot(lda_data[,2:3], 
         fit5$cluster, 
         color=TRUE, 
         shade=TRUE, 
         labels=4, 
         lines=0,
         main="Cluster Plot of Faculty Data with k=5",
         xlab="LD1",
         ylab="LD2")

# Centroid Plot against 1st 2 discriminant functions
load_package("fpc")
plotcluster(lda_data[,2:3], 
            fit5$cluster,
            main="Cluster Plot of Faculty Data with k=5",
            xlab="LD1",
            ylab="LD2")

# K-Means Cluster Analysis for k=6
fit6 <- kmeans(lda_data[,2:3], 6) # 6 cluster solution
# get cluster means 
aggregate(lda_data[,2:3],by=list(fit6$cluster),FUN=mean)
# append cluster assignment
lda_data <- data.frame(lda_data, fit6$cluster)
str(lda_data)

# cluster plot with ellipses
load_package("cluster")
clusplot(lda_data[,2:3], 
         fit6$cluster, 
         color=TRUE, 
         shade=TRUE, 
         labels=4, 
         lines=0,
         main="Cluster Plot of Faculty Data with k=6",
         xlab="LD1",
         ylab="LD2")

# Centroid Plot against 1st 2 discriminant functions
load_package("fpc")
plotcluster(lda_data[,2:3], 
            fit6$cluster,
            main="Cluster Plot of Faculty Data with k=6",
            xlab="LD1",
            ylab="LD2")

# K-Means Cluster Analysis for k=10
fit10 <- kmeans(lda_data[,2:3], 10) # 10 cluster solution
# get cluster means 
aggregate(lda_data[,2:3],by=list(fit10$cluster),FUN=mean)
# append cluster assignment
lda_data <- data.frame(lda_data, fit10$cluster)
str(lda_data)

# cluster plot with ellipses
load_package("cluster")
clusplot(lda_data[,2:3], 
         fit10$cluster, 
         color=TRUE, 
         shade=TRUE, 
         labels=4, 
         lines=0,
         main="Cluster Plot of Faculty Data with k=10",
         xlab="LD1",
         ylab="LD2")

# Centroid Plot against 1st 2 discriminant functions
load_package("fpc")
plotcluster(lda_data[,2:3], 
            fit10$cluster,
            main="Cluster Plot of Faculty Data with k=10",
            xlab="LD1",
            ylab="LD2")

############################
# answer to b.             #
############################
d <- dist(lda_data[,2:3])
fit <- hclust(d, method="ward.D2") 
load_package("sparcl")
y <- c("red","blue","green","yellow","cyan")[lda_data$facrank]
ColorDendrogram(fit, y = y, main = "Hierarchical Clustering of Faculty Data", 
                branchlength = 80)
#plot(fit, hang = -1, labels=FALSE, main="Hierarchical Clustering of Faculty Data") # display dendogram
groups <- cutree(fit, k=5) # cut tree into 5 clusters
# draw dendogram with red borders around the 5 clusters 
rect.hclust(fit, k=4, border="red")


############################
# answer to c.             #
############################
# Centroid Plot against 1st 2 discriminant functions
plotcluster(lda_data[,2:3], 
            fit4$cluster,
            col=lda_data$facrank,
            main="Cluster Plot of Faculty Data with k=4",
            xlab="LD1",
            ylab="LD2")

# Centroid Plot against 1st 2 discriminant functions
plotcluster(lda_data[,2:3], 
            fit5$cluster,
            col=lda_data$facrank,
            main="Cluster Plot of Faculty Data with k=5",
            xlab="LD1",
            ylab="LD2")

# Centroid Plot against 1st 2 discriminant functions
plotcluster(lda_data[,2:3], 
            fit6$cluster,
            col=lda_data$facrank,
            main="Cluster Plot of Faculty Data with k=6",
            xlab="LD1",
            ylab="LD2")

# Centroid Plot against 1st 2 discriminant functions
plotcluster(lda_data[,2:3], 
            fit10$cluster,
            col=lda_data$facrank,
            main="Cluster Plot of Faculty Data with k=10",
            xlab="LD1",
            ylab="LD2")