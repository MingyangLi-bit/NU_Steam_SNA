# Start with a clear environment
rm(list=ls())

# Load necessary packages
if (!"devtools" %in% installed.packages()) install.packages("devtools") ## for the install_version function
library(devtools)
if (!"RSiena" %in% installed.packages()) install.packages("RSiena") ## install latest version of RSiena if you don't have it already, it can take ~10 minutes

library(RSiena)
packageVersion("RSiena")
library(statnet)
library(texreg)

setwd('/Users/mingyangli/Desktop/NU Courses/2023Fall/CS496_SNA/Final Project/r_scripts/genres_unweighted_yearly')
list.files()

# Read in data and convert to matrix format
genres.data.t1 <- as.matrix(read.table("genres_edge_2013_newly_added.dat"))
genres.data.t2 <- as.matrix(read.table("genres_edge_2014_newly_added.dat"))
genres.data.t3 <- as.matrix(read.table("genres_edge_2015_newly_added.dat"))
genres.data.t4 <- as.matrix(read.table("genres_edge_2016_newly_added.dat"))
genres.data.t5 <- as.matrix(read.table("genres_edge_2017_newly_added.dat"))
genres.data.t6 <- as.matrix(read.table("genres_edge_2018_newly_added.dat"))
genres.data.t7 <- as.matrix(read.table("genres_edge_2019_newly_added.dat"))
genres.data.t8 <- as.matrix(read.table("genres_edge_2020_newly_added.dat"))
genres.data.t9 <- as.matrix(read.table("genres_edge_2021_newly_added.dat"))
genres.data.t10 <- as.matrix(read.table("genres_edge_2022_newly_added.dat"))

# Read node attributes
free <- as.matrix(read.table("free_yearly.dat"))
popularity <- as.matrix(read.table("popularity_yearly.dat")) 
reviews <- as.matrix(read.table("reviews_yearly.dat"))
review_score <- as.matrix(read.table("review_score_yearly.dat"))

# Visually inspect the adjacency matrices.
net1 <- as.network(genres.data.t1)
net2 <- as.network(genres.data.t2)
net3 <- as.network(genres.data.t3)
net4 <- as.network(genres.data.t4)
net5 <- as.network(genres.data.t5)
net6 <- as.network(genres.data.t6)
net7 <- as.network(genres.data.t7)
net8 <- as.network(genres.data.t8)
net9 <- as.network(genres.data.t9)
net10 <- as.network(genres.data.t10)

arr = c(genres.data.t1, genres.data.t2, genres.data.t3, genres.data.t4, genres.data.t5, genres.data.t6, genres.data.t7, genres.data.t8, genres.data.t9, genres.data.t10)
genres_data <- sienaNet(array(arr, dim=c(26, 26, 10)))
popu_beh <- sienaNet(popularity, type="behavior")
review_beh <- sienaNet(reviews, type="behavior")
score_beh <- sienaNet(review_score, type="behavior")
free_beh <- sienaNet(free, type="behavior")

# Fit model for popu_beh
mydata <- sienaDataCreate(genres_data)
mybehdata_popu <- sienaDataCreate(genres_data, popu_beh)
myeff_popu <- getEffects(mybehdata_popu)
myeff_popu <- includeEffects(myeff_popu, density, transTriads, between)
myeff_popu <- includeEffects(myeff_popu, sameX, interaction1 = "popu_beh")
myeff_popu <- includeEffects(myeff_popu, gwesp, parm = 69)

mymodel <- sienaModelCreate(useStdInits = FALSE, projname = 'steam_genres')
model_popu<- siena07(mymodel, data=mybehdata_popu, effects=myeff_popu, batch=FALSE, verbose=TRUE, returnDeps = TRUE)
model_popu
model_popu <- siena07(mymodel, data=mybehdata_popu, effects=myeff_popu, prevAns=model_popu, batch=FALSE, verbose=TRUE, returnDeps = TRUE)
model_popu

screenreg(model_popu)

# Fit model for review_beh
mydata <- sienaDataCreate(genres_data)
mybehdata_review <- sienaDataCreate(genres_data, review_beh)
myeff_review <- getEffects(mybehdata_review)
myeff_review <- includeEffects(myeff_review, density, transTriads, between)
myeff_review <- includeEffects(myeff_review, gwesp, parm = 69)
myeff_review <- includeEffects(myeff_review, sameX, interaction1 = "review_beh")

mymodel <- sienaModelCreate(useStdInits = FALSE, projname = 'steam_genres')
model_review<- siena07(mymodel, data=mybehdata_review, effects=myeff_review, batch=FALSE, verbose=TRUE, returnDeps = TRUE)
model_review
model_review <- siena07(mymodel, data=mybehdata_review, effects=myeff_review, prevAns=model_review, batch=FALSE, verbose=TRUE, returnDeps = TRUE)
model_review

screenreg(model_review)


# Fit model for score_beh
mydata <- sienaDataCreate(genres_data)
mybehdata_score <- sienaDataCreate(genres_data, score_beh)
myeff_score <- getEffects(mybehdata_score)
myeff_score <- includeEffects(myeff_score, density, transTriads, between)
myeff_score <- includeEffects(myeff_score, gwesp, parm = 69)
myeff_score <- includeEffects(myeff_score, sameX, interaction1 = "score_beh")

mymodel <- sienaModelCreate(useStdInits = FALSE, projname = 'steam_genres')
model_score<- siena07(mymodel, data=mybehdata_score, effects=myeff_score, batch=FALSE, verbose=TRUE, returnDeps = TRUE)
model_score
model_score <- siena07(mymodel, data=mybehdata_score, effects=myeff_score, prevAns=model_score, batch=FALSE, verbose=TRUE, returnDeps = TRUE)
model_score

screenreg(model_score)

# Fit model for free_beh
mydata <- sienaDataCreate(genres_data)
mybehdata_free <- sienaDataCreate(genres_data, free_beh)
myeff_free <- getEffects(mybehdata_free)

mymodel <- sienaModelCreate(useStdInits = FALSE, projname = 'steam_genres')
model_free <- siena07(mymodel, data=mybehdata_free, effects=myeff_free, batch=FALSE, verbose=TRUE, returnDeps = TRUE)
model_free
model_free <- siena07(mymodel, data=mybehdata_free, effects=myeff_free, prevAns=model_free, batch=FALSE, verbose=TRUE, returnDeps = TRUE)
model_free

screenreg(model_free)
