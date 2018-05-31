############Network analysis for low human footprint areas in Puerto Rico, ####
##potential habitat for certain species##

###############################################################
###############################################################
####        From Adjacency Matrices to Networks
####        Borja Esteve-Altava - Transmitting Science Course  
####        Edited by Diana K. Guzman-Colon
###############################################################
###############################################################

##### Download and install the package#######
install.packages("igraph")
install.packages("rgeos") #to import .shp and other map files
install.packages("gdata")#to import .csv files
#the problem is that perl.exe is missing. 
#In my case it worked after getting it installed from: http://www.activestate.com/activeperl/downloads 
#then point to it:
#read.xls("bla.xlsx", perl = "C:\\Perl64\\bin\\perl.exe")
perl<-"C:/Perl64/bin/perl.exe" #needed for package gdata
installXLSXsupport(perl = perl, verbose = TRUE)
install.packages("ggplot2")

##### Load package####
library(igraph)
library(gdata)
library(ggplot2)
library(rgeos)

####Load Adjacency matrix ####
#workdir<-setwd()
#getwd()
nohu.data<-gdata::read.xls("V:/dguzmancolon/dguzmancolon/igraph/igraph/NoHU_resistances.xls", sheet=1, perl=perl, header=TRUE)
#nohu.data<-read.csv("./NoHU_resistances.csv", header=TRUE, sep="") #zap
#remove blank names in nodes
nohu.data<-nohu.data[-(153:159)]
nohu.nodes<-names(nohu.data)[-1] #get node IDs from first column

#Import and convert into adjacency matrix
nohu.mat<-data.matrix(nohu.data, rownames.force=TRUE)#Import and convert into adjacency matrix
nohu.mat<-nohu.mat[,-1] #The matrix should include the first row (which is data),
#but not the first column (which too contains node-names).
#to remove NA's: 
#nohu.mat<-nohu.mat[ , ! apply( nohu.mat , 2 , function(x) all(is.na(x)) ) ] 

#As the matrix is now of the size N by N, row- and colnames makes for a neat matrix. Row names need to be 
#the same as nodes names from the column, below is the code for that#
rownames(nohu.mat) <- colnames(nohu.mat) <- nohu.nodes 
#Check
nohu.mat
#save matrix 
write.csv(nohu.mat, "V:/dguzmancolon/dguzmancolon/igraph/igraph/NohuMat.csv", sep = "\t")

#####Example load edges list####
el.euc=read.csv("Z:/HumanInfluence/HFCircuitscape/HF_EucledianCores/distances_HFCores_NoHUResamp_cores_2.txt", 
                sep="", header=FALSE) # read  the 'el.with.weights.csv' file
names(el.euc) <- c("V1","V2","weight") ##add column names

el.euc[,1]=as.character(el.euc[,1]) #Because the vertex IDs in this dataset are numbers, we make sure igraph knows these should be treated as characters. Otherwise, it'll create problems (see page on data import)
el.euc[,2]=as.character(el.euc[,2])

el.mat<-el.euc[,-3]
el.mat=as.matrix(el.mat) #igraph needs the edgelist to be in matrix format
euc.g=graph.edgelist(el.mat[,1:2], directed=FALSE) #We first greate a network from the first two columns, which has the list of vertices
E(euc.g)$weight=as.numeric(el[,3]) #We then add the edge weights to this network by assigning an edge attribute called 'weight'.
#weights in the list are distances in (m)#

#visualize
plot(euc.g,layout=layout.fruchterman.reingold,edge.width=E(euc.g)$weight/100000)

#To get adjecency matrix from network
el.adj=get.adjacency(euc.g,attr= 'weight') #attr='weight' makes sure that the weights are shown in the adjacency matrix. 
el.adj


####Original Graph - weights by resistance distance (or Cost of moving)#####
#nohu.graph <- igraph::graph_from_adjacency_matrix(nohu.mat, mode="undirected", weighted=NULL) #with no weight
Wnohu.graph=igraph::graph_from_adjacency_matrix(nohu.mat,mode="undirected",weighted=TRUE,diag=FALSE) ###weighted
#Weuc.copy=get.adjacency(Weuc.graph.copy,attr= 'Weight') #
#Weuc.copy.g<-igraph::graph_from_adjacency_matrix(Weuc.copy,mode="undirected",weighted=TRUE,diag=FALSE)

Weuc.g<- igraph::graph_from_adjacency_matrix(el.adj,mode="undirected",weighted=TRUE,diag=FALSE) #this is.. 
#from 'Example load edges list section'#
#Weuc.graph1 <- graph_from_edgelist(el.mat, directed=FALSE)
#view name of nodes(vertices)
#V(Wnohu.graph)
#V(Weuc.graph)
#V(Weuc.copy.g)
V(Weuc.g)
#Weuc.graph
#Weuc.copy.g
#E(Weuc.graph)$Weight=as.numeric(el[,3])
E(Weuc.g)$weight

##load Node attribute table##
node_attr<-read.table("V:/dguzmancolon/dguzmancolon/igraph/igraph/CoreProp.txt", header=TRUE, sep = "\t")
n.attr.name <- node_attr[["core_ID"]] <- nohu.nodes  ##replaced node names to match current adjacency matrix#
n.attr.name<-node_attr[,-1] ##first column (which too contains node-names)#
rownames(n.attr.name) <- nohu.nodes

## This code says to create a vertex attribute called "AREA" 
#by extracting the value of the column "AREA" in the attributes file 
#when the core_ID number matches the vertex name.
V(Wnohu.graph)$AREA=as.character(node_attr$AREA[match(V(Wnohu.graph)$name,node_attr$core_ID)])

V(Wnohu.graph)$size=V(Wnohu.graph)$AREA
v.size=(V(Wnohu.graph)$size)
v.size=as.numeric(v.size)

#V(Weuc.graph)$AREA=as.character(node_attr$AREA[match(V(Weuc.graph)$name,node_attr$core_ID)])
#V(Weuc.graph)$AREA<-V(Weuc.graph)$AREA[!is.na(V(Weuc.graph)$AREA)] #remove NA's?
#V(Weuc.graph)$AREA
#V(Weuc.graph)$size=V(Weuc.graph)$AREA
#v.size=(V(Weuc.graph)$size)
#v.size=as.numeric(v.size)

#V(Weuc.copy.g)$AREA=as.character(node_attr$AREA[match(V(Weuc.copy.g)$name,node_attr$core_ID)])
#V(Weuc.copy.g)$AREA<-V(Weuc.copy.g)$AREA[!is.na(V(Weuc.copy.g)$AREA)] #remove NA's?
#V(Weuc.copy.g)$AREA
#V(Weuc.copy.g)$size=V(Weuc.copy.g)$AREA
#v.size=(V(Weuc.copy.g)$size)
#v.size=as.numeric(v.size)

#V(Weuc.g)$AREA=as.character(node_attr$AREA[match(V(Weuc.g)$name,node_attr$core_ID)])
#V(Weuc.g)$AREA<-V(Weuc.g)$AREA[!is.na(V(Weuc.g)$AREA)] #remove NA's?
#V(Weuc.g)$AREA
#V(Weuc.g)$size=V(Weuc.g)$AREA
#v.size=(V(Weuc.g)$size)
#v.size=as.numeric(v.size)

##edges
#E(Weuc.graph)$weight<-E(Weuc.graph)$weight[!is.na(E(Weuc.graph)$weight)]##remove NA's?
#E(Weuc.graph)$weight
#E(Weuc.graph)$weight

#E(Weuc.g)$weight<-E(Weuc.g)$weight[!is.na(E(Weuc.g)$weight)]##remove NA's?
#E(Weuc.g)$weight

E(Wnohu.graph)$weight

## Add coordinates to nodes#
#You can add the coordinates directly to the graph object in igraph by setting the an X and Y attributes to the vertices
coords <- layout.norm(as.matrix(node_attr[, c("Longitude","Latitude")]))

#View
plot(Wnohu.graph, vertex.size=v.size/1000000, vertex.label.cex=c(0.5), layout=coords, rescale=T, edge.width=E(Wnohu.graph)$weight)
str(Wnohu.graph)

#plot(Weuc.graph, vertex.size=v.size/1000000, vertex.label.cex=c(0.5), layout=coords, rescale=T, edge.width=E(Weuc.graph)$Weight/100000)
#str(Weuc.graph)

#######Delete unwanted (non-tansitable) edges from original resistance distance-weighted graph#####

cut.off<- 0.6
E(Wnohu.graph)$weight[is.na(E(Wnohu.graph)$weight)] <- 0 ##I have NA values, changed them to 0##
Wnohu.graph.copy <- delete_edges(Wnohu.graph, E(Wnohu.graph)[weight>cut.off])
#Verify
E(Wnohu.graph.copy)$weight

#for plotting
# Set edge width based on weight:
E(Wnohu.graph.copy)$width <- E(Wnohu.graph.copy)$weight*3

plot(Wnohu.graph.copy,vertex.size=v.size/1000000, vertex.label.cex=c(0.5), vertex.color=c( "dark green"),layout=coords, rescale=T, 
     edge.width=E(Wnohu.graph)$weight, edge.curved=0.5, edge.width=E(Wnohu.graph.copy)$width)

#Weuc.graph.copy<-delete.edges(Weuc.graph, which(E(Weuc.graph)$Weight > 79000)-1)
#Weuc.graph.copy=delete.edges(Weuc.graph, which(E(Weuc.graph)$Weight <=79000)) # here's my condition.
#Weuc.graph.copy=delete.vertices(Weuc.graph.copy,which(degree(Weuc.graph.copy)<1))
#Weuc.graph.copy<-delete.edges(Weuc.graph, which(E(Weuc.graph)$Weight >= 79000))

#E(Weuc.graph.copy)$Weight
#plot(Weuc.graph.copy,vertex.size=v.size/1000000, vertex.label.cex=c(0.5), layout=lgl, rescale=T, 
#     edge.width=E(Weuc.graph.copy)$Weight/1000, edge.curved=0.5  )


#Calc degree dist,edge density, transitivity
dd<-degree_distribution(Wnohu.graph.copy, v = V(Wnohu.graph.copy), cumulative = FALSE, mode = c("all"))
ed<-edge_density(Wnohu.graph.copy, loops = FALSE)
t<-transitivity(Wnohu.graph.copy, v = V(Wnohu.graph.copy), type = c("weighted"), isolates = c("NaN", "zero"))

#dd<-degree_distribution(Weuc.copy.g.copy, v = V(Weuc.copy.g.copy), cumulative = FALSE, mode = c("all"))
#ed<-edge_density(Weuc.copy.g.copy, loops = FALSE)
#t<-transitivity(Weuc.copy.g.copy, v = V(Weuc.copy.g.copy), type = c("weighted"), isolates = c("NaN", "zero"))

####Calculate clustering C(k)####
# calculate the number of connections per node
# calculate the clustering of each node
k<-degree(Wnohu.graph.copy)
C <- transitivity(Wnohu.graph.copy, type = "weighted")
L <- mean_distance(Wnohu.graph.copy)
Pk<-degree(Wnohu.graph.copy)

#Weuc.copy.g.copy
#k<-degree(Weuc.copy.g.copy)
#C <- transitivity(Weuc.copy.g.copy, type = "weighted")
#L <- mean_distance(Weuc.copy.g.copy)
#Pk<-degree(Weuc.copy.g.copy)

# mean clustering of all nodes with k links
Ck <- rep(0, max(k))
for (i in 1:max(k)){
  Ck[i] <- mean(C[k==i], na.rm = TRUE)
}
plot(Ck)

##### Randomize your network####
RWnohu.graph.copy <- sample_degseq(Pk, in.deg=NULL, method="simple")
# Calculate C and L
CR <- transitivity(RWnohu.graph.copy, type="average")
LR <- mean_distance(RWnohu.graph.copy)
# Compare yours with random
(C/CR)/(L/LR) >= 0.012*vcount(Wnohu.graph.copy)^1.11

######Check if graph is small world######
## Function to estimate the presence of a small-world organization
## by Borja Esteve-Altava, April 2015
## Requires package "igraph"
# Input: an igraph object
# Outputs: TRUE/FALSE presence of small-workd and score

is.smallworld = function(graph, rep=1000){
  
  # function that compares the C and L of empirical and random equivalent networks
  test.sw = function(graph){
    deg <- degree(graph)
    Crand <- matrix(0,nrow=1, ncol=rep)
    Lrand <- matrix(0,nrow=1, ncol=rep)
    for (i in 1:rep){
      # generate random equivalent network
      Grand <- degree.sequence.game(deg, method="simple")
      # calculate C and L
      Crand[i] <- transitivity(Grand, type="average", isolates="NaN")
      Lrand[i] <- average.path.length(Grand)
    }
    # C and L empirical and the average of the random ones
    C <- transitivity(graph, type="average", isolates="NaN")
    L <- average.path.length(graph)
    CR <- mean(Crand, na.rm=TRUE)
    LR <- mean(Lrand, na.rm=TRUE)
    smallwordness <- (C/CR)/(L/LR)
    smallwordness
  }
  
  # check if the graph is connected
  check1 <- is.connected(graph)
  # if connected: calculate small-world
  if (check1==TRUE){
    print("This graph is connected")
    print(paste("Is small-world?", test.sw(graph)>=0.012*vcount(graph)^1.11))
    print(paste("score =", round(test.sw(graph), 3)))
    # if not connected: calculate small-world for each cluster 
  } else{
    print("This graph is not connected")
    for (i in 1:no.clusters(graph)){
      subgraph <- induced.subgraph(graph, v=clusters(graph)$membership==i)
      print(paste("Cluster", i))
      print(paste("Is small-world?", test.sw(subgraph)>=0.012*vcount(subgraph)^1.11))
      print(paste("score =", round(test.sw(subgraph), 3)))
    }
  }
}

is.smallworld(Wnohu.graph.copy)
#is.smallworld(Weuc.copy.g.copy)

#####Scale-free####
degree = 1:max(degree(Wnohu.graph.copy, mode = "all"))
dd = degree_distribution(Wnohu.graph.copy, mode="all", cumulative=T)
probability = dd[-1]
# delete 0s
nonzero.position = which(probability != 0)
probability = probability[nonzero.position]
degree = degree[nonzero.position]
# linear fit of logarithms
reg = lm(log(probability) ~ log(degree))
cozf = coef(reg)
power.law.fit = function(x) exp(cozf[[1]] + cozf[[2]] * log(x))
# alpha and r
alpha = -cozf[[2]]
R.square = summary(reg)$r.squared
# linear fit of logarithms
reg = lm(log(probability) ~ log(degree))
cozf = coef(reg)
power.law.fit = function(x) exp(cozf[[1]] + cozf[[2]] * log(x))
# alpha and r
alpha = -cozf[[2]]
R.square = summary(reg)$r.squared

print(paste("Alpha =", round(alpha, 3)))
print(paste("R square =", round(R.square, 3)))

#plot
plot(probability~degree, log="xy", xlab="ki", ylab="P(k)", col=1)
curve(power.law.fit, col="red", add=T, n=length(degree))

###### Function to estimate if network is scale-free#####
## by Borja Esteve-Altava, April 2015
## Requires package "igraph"
# Input: an igraph object, cumulative TRUE/FALSE
# Outputs: TRUE (r > 0.7) / FALSE, alpha and r

is.scalefree = function(graph, cumulative = c(TRUE, FALSE)){
  d <- degree(graph, mode = "all")
  # Fit P(k) to power-law
  # initialize vectors
  dd = degree.distribution(graph, mode = "all", cumulative = cumulative)
  probability = dd[-1]
  degree = 1:max(d)
  # delete blank values
  nonzero.position = which(probability != 0)
  probability = probability[nonzero.position]
  degree = degree[nonzero.position]
  # linear fit of logarithms
  reg = lm(log(probability) ~ log(degree))
  cozf = coef(reg)
  power.law.fit = function(x) exp(cozf[[1]] + cozf[[2]] * log(x))
  alpha = -cozf[[2]]
  R.square = summary(reg)$r.squared
  # output
  print(paste("Is scale-free?", R.square>0.7))
  print(paste("Alpha =", round(alpha, 3)))
  print(paste("R square =", round(R.square, 3)))
  dev.new()
  plot(probability ~ degree, log = "xy", xlab = "Connections (log)", ylab = "Probability (log)", col = 1, main = "Connectivity Distribution P(k)")
  curve(power.law.fit, col = "red", add = T, n = length(d)) 
}

#scalefree<-is.scalefree(Weuc.copy.g.copy, cumulative = c(TRUE))
scalefree<-is.scalefree(Wnohu.graph.copy, cumulative = c(TRUE))

####Is my Network Hierarchical?#####
#The script is the same, 
#only that instead of frequency now we calculate the average C of nodes with the same k

#cluster = transitivity(graph, type="local", isolates="NaN")
cluster = transitivity(Wnohu.graph.copy, type="local", isolates="NaN")
d=degree(Wnohu.graph.copy)

for (i in 1:max(d)){
  probability[i]=mean(cluster[d==i], na.rm=TRUE)
}

## Function to estimate if network is hierarchical
## by Borja Esteve-Altava, April 2015
## Requires package "igraph"
# Input: an igraph object, cumulative Pk TRUE/FALSE
# Outputs: TRUE (r > 0.7) / FALSE, alpha and r

is.hierarchy = function(graph, cumulative = c(TRUE, FALSE)){
  d <- degree(graph, mode = "all")
  dd = degree.distribution(graph, mode = "all", cumulative = cumulative)
  
  # initialize vectors
  probability = dd[-1]
  degree = 1:max(d)
  # delete blank values
  nonzero.position = which(probability != 0)
  probability = probability[nonzero.position]
  degree = degree[nonzero.position]
  # linear fit of logarithms
  reg = lm(log(probability) ~ log(degree))
  cozf = coef(reg)
  power.law.fit = function(x) exp(cozf[[1]] + cozf[[2]] * log(x))
  
  alpha1 = -cozf[[2]]
  R.square1 = summary(reg)$r.squared
  
  # Fit C(k) to power-law
  cluster = transitivity(graph, type = "local", isolates = "NaN")
  for (i in 1:max(d)) {probability[i] = mean(cluster[d==i], na.rm=TRUE)}	
  # delete blank values
  nonzero.position = which(probability != 0)
  probability = probability[nonzero.position]
  degree = degree[nonzero.position]
  # linear fit of logarithms
  reg = lm(log(probability) ~ log(degree))
  cozf = coef(reg)
  power.law.fit = function(x) exp(cozf[[1]] + cozf[[2]] * log(x))
  
  alpha2 = -cozf[[2]]
  R.square2 = summary(reg)$r.squared
  
  
  print(paste("Is hierarchical?", (R.square1>0.7)&&(R.square2>0.7)))
  
  print("P(k) fit to Power-Law")
  print(paste("Alpha =", round(alpha1, 3)))
  print(paste("R square =", round(R.square1, 3)))
  
  print("C(k) fit to Power-Law")
  print(paste("Alpha =", round(alpha2, 3)))
  print(paste("R square =", round(R.square2, 3)))
  
}

is.hierarchy(Wnohu.graph.copy) 
#is.hierarchy(Weuc.copy.g.copy) 

plot(probability~degree, log="xy", xlab="ki", ylab="C(k)", col=1)
curve(power.law.fit, col="red", add=T, n=length(d))

###Types of Network Null Models####
#Regular networks (k-degree model)
#Random networks (Erdös-Rényi model)
#Small-world networks (Watts-Strogatz model)
#Scale-free networks (Barabási-Albert model)
barabasi<-barabasi.game(n=152,power=1)
barabasi
plot(barabasi)
#Geometric networks (Proximity model)

####Delete nodes, pre-final Graph Object#### 
#Delete edges to nodes that are too far away for a species of conservation concern to reach
#Based on distance in meters#
cut.off <- 95541
#max(E(Weuc.g)$weight)
net.sp <- delete_edges(Weuc.g, E(Weuc.g)[weight>cut.off])
plot(net.sp,vertex.size=v.size/1000000, vertex.label.cex=c(0.5), layout=coords, rescale=T, edge.width=E(Weuc.g)$weight/10000000)
#plot divided by high numbers for illustration purposes#

# calculate the number of connections per node
# calculate the clustering of each node
net.sp
k<-degree(net.sp)
C <- transitivity(net.sp, type = "weighted")
L <- mean_distance(net.sp)
Pk<-degree(net.sp)

#Weuc.copy.g.copy
#k<-degree(Weuc.copy.g.copy)
#C <- transitivity(Weuc.copy.g.copy, type = "weighted")
#L <- mean_distance(Weuc.copy.g.copy)
#Pk<-degree(Weuc.copy.g.copy)

# mean clustering of all nodes with k links
Ck <- rep(0, max(k))
for (i in 1:max(k)){
  Ck[i] <- mean(C[k==i], na.rm = TRUE)
}
plot(Ck)

##Randomize your network###
Rnet.sp <- sample_degseq(Pk, in.deg=NULL, method="simple")
# Calculate C and L
CR <- transitivity(Rnet.sp, type="average")
LR <- mean_distance(Rnet.sp)
# Compare yours with random
(C/CR)/(L/LR) >= 0.012*vcount(Rnet.sp)^1.11

#Check if graph is small world###
# Input: an igraph object
# Outputs: TRUE/FALSE presence of small-workd and score

is.smallworld = function(graph, rep=1000){
  
  # function that compares the C and L of empirical and random equivalent networks
  test.sw = function(graph){
    deg <- degree(graph)
    Crand <- matrix(0,nrow=1, ncol=rep)
    Lrand <- matrix(0,nrow=1, ncol=rep)
    for (i in 1:rep){
      # generate random equivalent network
      Grand <- degree.sequence.game(deg, method="simple")
      # calculate C and L
      Crand[i] <- transitivity(Grand, type="average", isolates="NaN")
      Lrand[i] <- average.path.length(Grand)
    }
    # C and L empirical and the average of the random ones
    C <- transitivity(graph, type="average", isolates="NaN")
    L <- average.path.length(graph)
    CR <- mean(Crand, na.rm=TRUE)
    LR <- mean(Lrand, na.rm=TRUE)
    smallwordness <- (C/CR)/(L/LR)
    smallwordness
  }
  
  # check if the graph is connected
  check1 <- is.connected(graph)
  # if connected: calculate small-world
  if (check1==TRUE){
    print("This graph is connected")
    print(paste("Is small-world?", test.sw(graph)>=0.012*vcount(graph)^1.11))
    print(paste("score =", round(test.sw(graph), 3)))
    # if not connected: calculate small-world for each cluster 
  } else{
    print("This graph is not connected")
    for (i in 1:no.clusters(graph)){
      subgraph <- induced.subgraph(graph, v=clusters(graph)$membership==i)
      print(paste("Cluster", i))
      print(paste("Is small-world?", test.sw(subgraph)>=0.012*vcount(subgraph)^1.11))
      print(paste("score =", round(test.sw(subgraph), 3)))
    }
  }
}

is.smallworld(net.sp)

#Scale-free##
degree = 1:max(degree(net.sp, mode = "all"))
dd = degree_distribution(net.sp, mode="all", cumulative=T)
probability = dd[-1]
# delete 0s
nonzero.position = which(probability != 0)
probability = probability[nonzero.position]
degree = degree[nonzero.position]
# linear fit of logarithms
reg = lm(log(probability) ~ log(degree))
cozf = coef(reg)
power.law.fit = function(x) exp(cozf[[1]] + cozf[[2]] * log(x))
# alpha and r
alpha = -cozf[[2]]
R.square = summary(reg)$r.squared
# linear fit of logarithms
reg = lm(log(probability) ~ log(degree))
cozf = coef(reg)
power.law.fit = function(x) exp(cozf[[1]] + cozf[[2]] * log(x))
# alpha and r
alpha = -cozf[[2]]
R.square = summary(reg)$r.squared

print(paste("Alpha =", round(alpha, 3)))
print(paste("R square =", round(R.square, 3)))

#plot
plot(probability~degree, log="xy", xlab="ki", ylab="P(k)", col=1)
curve(power.law.fit, col="red", add=T, n=length(degree))

# Function to estimate if network is scale-free##
# Input: an igraph object, cumulative TRUE/FALSE
# Outputs: TRUE (r > 0.7) / FALSE, alpha and r

is.scalefree = function(graph, cumulative = c(TRUE, FALSE)){
  d <- degree(graph, mode = "all")
  # Fit P(k) to power-law
  # initialize vectors
  dd = degree.distribution(graph, mode = "all", cumulative = cumulative)
  probability = dd[-1]
  degree = 1:max(d)
  # delete blank values
  nonzero.position = which(probability != 0)
  probability = probability[nonzero.position]
  degree = degree[nonzero.position]
  # linear fit of logarithms
  reg = lm(log(probability) ~ log(degree))
  cozf = coef(reg)
  power.law.fit = function(x) exp(cozf[[1]] + cozf[[2]] * log(x))
  alpha = -cozf[[2]]
  R.square = summary(reg)$r.squared
  # output
  print(paste("Is scale-free?", R.square>0.7))
  print(paste("Alpha =", round(alpha, 3)))
  print(paste("R square =", round(R.square, 3)))
  dev.new()
  plot(probability ~ degree, log = "xy", xlab = "Connections (log)", ylab = "Probability (log)", col = 1, main = "Connectivity Distribution P(k)")
  curve(power.law.fit, col = "red", add = T, n = length(d)) 
}

scalefree<-is.scalefree(net.sp, cumulative = c(TRUE))
#Is my Network Hierarchical?###
#The script is the same, only that instead of frequency now we calculate the average C of nodes with the same k

#cluster = transitivity(graph, type="local", isolates="NaN")
cluster = transitivity(net.sp, type="local", isolates="NaN")
d=degree(net.sp)

for (i in 1:max(d)){
  probability[i]=mean(cluster[d==i], na.rm=TRUE)
}

#Function to estimate if network is hierarchical
# Input: an igraph object, cumulative Pk TRUE/FALSE
# Outputs: TRUE (r > 0.7) / FALSE, alpha and r

is.hierarchy = function(graph, cumulative = c(TRUE, FALSE)){
  d <- degree(graph, mode = "all")
  dd = degree.distribution(graph, mode = "all", cumulative = cumulative)
  
  # initialize vectors
  probability = dd[-1]
  degree = 1:max(d)
  # delete blank values
  nonzero.position = which(probability != 0)
  probability = probability[nonzero.position]
  degree = degree[nonzero.position]
  # linear fit of logarithms
  reg = lm(log(probability) ~ log(degree))
  cozf = coef(reg)
  power.law.fit = function(x) exp(cozf[[1]] + cozf[[2]] * log(x))
  
  alpha1 = -cozf[[2]]
  R.square1 = summary(reg)$r.squared
  
  # Fit C(k) to power-law
  cluster = transitivity(graph, type = "local", isolates = "NaN")
  for (i in 1:max(d)) {probability[i] = mean(cluster[d==i], na.rm=TRUE)}	
  # delete blank values
  nonzero.position = which(probability != 0)
  probability = probability[nonzero.position]
  degree = degree[nonzero.position]
  # linear fit of logarithms
  reg = lm(log(probability) ~ log(degree))
  cozf = coef(reg)
  power.law.fit = function(x) exp(cozf[[1]] + cozf[[2]] * log(x))
  
  alpha2 = -cozf[[2]]
  R.square2 = summary(reg)$r.squared
  
  
  print(paste("Is hierarchical?", (R.square1>0.7)&&(R.square2>0.7)))
  
  print("P(k) fit to Power-Law")
  print(paste("Alpha =", round(alpha1, 3)))
  print(paste("R square =", round(R.square1, 3)))
  
  print("C(k) fit to Power-Law")
  print(paste("Alpha =", round(alpha2, 3)))
  print(paste("R square =", round(R.square2, 3)))
  
}

#is.hierarchy(Wnohu.graph.copy) 
is.hierarchy(net.sp) 

plot(probability~degree, log="xy", xlab="ki", ylab="C(k)", col=1)
curve(power.law.fit, col="red", add=T, n=length(d))

#to expoert as adjacency#
#matrix packg for sparse matrices#
install.packages('Matrix')
library(Matrix)
net.sp.export<-as_adjacency_matrix(net.sp, type = c("both"), attr = "weight",
                                   edges = TRUE, names = TRUE, sparse = igraph_opt("sparsematrices"))
net.sp.expoert<-as.matrix(net.sp.export)
write.csv(net.sp.expoert, "V:/dguzmancolon/dguzmancolon/igraph/igraph/eucPrunedgraph.csv", sep = "\t")

#Degree distribution#
net.sp.deg <- degree(net.sp) # List of degrees
#Wnohu.graph.copy.deg <- degree(Wnohu.graph.copy)
net.sp.hist <- as.data.frame(table(net.sp.deg)) # Let's count the frequencies of each degree
#Wnohu.graph.copy.histogram <- as.data.frame(table(Wnohu.graph.copy.deg))

# Need to convert the first column to numbers, otherwise
# the log-log thing will not work (that's fair...)
net.sp.hist[,1] <- as.numeric(net.sp.hist[,1])
#Wnohu.graph.copy.histogram[,1] <- as.numeric(Wnohu.graph.copy.histogram[,1])
# Now, plot it!
ggplot(net.sp.hist, aes(x = net.sp.deg, y = Freq)) +
  geom_point() +
  scale_x_continuous("Degree\n(nodes with this amount of connections)",
                     breaks = c(1, 3, 10, 30, 100, 300),
                     trans = "log10") +
  scale_y_continuous("Frequency\n(how many of them)",
                     breaks = c(1, 3, 10, 30, 100, 300, 1000),
                     trans = "log10") +
  ggtitle("Degree Distribution (log-log)") +
  theme_bw()

###### FINAL graph ...after prune - edit reachable nodes with resistance distance values####

g <-gdata::read.xls("V:/dguzmancolon/dguzmancolon/igraph/igraph/eucPrunedgraph.xlsx", sheet=1, perl=perl, header=TRUE)
g.nodes<-names(nohu.data)[-1] #get node IDs from first column
#remove blank names in nodes

#Import and convert into adjacency matrix
g.mat<-data.matrix(g, rownames.force=TRUE)#Import and convert into adjacency matrix
g.mat<-g.mat[,-1] #The matrix should include the first row (which is data),
#but not the first column (which too contains node-names).

#As the matrix is now of the size N by N, row- and colnames makes for a neat matrix. Row names need to be 
#the same as nodes names from the column, below is the code for that#
rownames(g.mat) <- colnames(g.mat) <- g.nodes 
#Check
g.mat
#column and row names that do not match between two matrices
g.mat.1<-g.mat[!rownames(g.mat)%in%rownames(nohu.mat),!colnames(g.mat)%in%colnames(nohu.mat)]

g.mat<-g.mat[-c(35, 38,44, 91, 99, 100, 105, 158), -c(35, 38,44, 91, 99, 100, 105, 158)]

g.graph=igraph::graph_from_adjacency_matrix(g.mat,mode="undirected",weighted = NULL ,diag=FALSE) #graph object

#saved pruned matrix to edit in excel
write.csv(g.mat, "V:/dguzmancolon/dguzmancolon/igraph/igraph/eucPrunedgraph1.csv", sep = "\t")

###replace values of connected nodes that were from pruned matrix. New values come from resistance distance matrix
g.mat[g.mat > 0] <- 1
g.dataframe<-as.data.frame(g.mat)
nohu.dataframe<-as.data.frame(nohu.mat)
weighted.dataframe<-g.dataframe*nohu.dataframe
weighted.mat<-as.matrix(weighted.dataframe)
write.csv(weighted.mat, "V:/dguzmancolon/dguzmancolon/igraph/igraph/weightedprunedmat.csv", sep = "\t")

##igraph object and plot #
#Graph filtered by both distacne metrics (resistance and eucledian)#
weighted.mat[is.na(weighted.mat)] <- 0
weight.graph=igraph::graph_from_adjacency_matrix(weighted.mat,mode="undirected",weighted = TRUE ,diag=FALSE)
cut.off <- 0.6
#max(E(weight.graph)$weight)
weight.graph.copy <- delete_edges(weight.graph, E(weight.graph)[weight>cut.off])

#For plotting#
# Set edge width based on weight:
E(weight.graph.copy)$width <- E(weight.graph.copy)$weight*3

#plot(weight.graph,vertex.size=v.size/1000000, vertex.label.cex=c(0.5), vertex.color="gray50", layout=coords, rescale=T, edge.width=E(weight.graph)$weight)

plot(weight.graph.copy,vertex.size=v.size/1000000, vertex.label.cex=c(0.5), vertex.color="gray50", layout=coords, rescale=T, edge.width=E(weight.graph.copy)$weight)

E(weight.graph.copy)$weight

#####Diagnostics with FINAL pruned/resistance graph####
#Calc degree dist,edge density, transitivity
dd<-degree_distribution(weight.graph.copy, v = V(weight.graph.copy), cumulative = FALSE, mode = c("all"))
ed<-edge_density(weight.graph.copy, loops = FALSE)
t<-transitivity(weight.graph.copy, v = V(weight.graph.copy), type = c("weighted"), isolates = c("NaN", "zero"))

k<-degree(weight.graph.copy)
C <- t
L <- mean_distance(weight.graph.copy)
Pk<-degree(weight.graph.copy)

# mean clustering of all nodes with k links
Ck <- rep(0, max(k))
for (i in 1:max(k)){
  Ck[i] <- mean(C[k==i], na.rm = TRUE)
}
plot(Ck)

# Randomize your network###
Rweight.graph.copy <- sample_degseq(Pk, in.deg=NULL, method="simple")

# Calculate C and L
CR <- transitivity(Rweight.graph.copy, type="average")
LR <- mean_distance(Rweight.graph.copy)

# Compare yours with random
(C/CR)/(L/LR) >= 0.012*vcount(weight.graph.copy)^1.11

#is.smallworld(Wnohu.graph.copy)
is.smallworld(weight.graph.copy)

##Scale-free###
degree = 1:max(degree(weight.graph.copy, mode = "all"))
dd = degree_distribution(weight.graph.copy, mode="all", cumulative=T)
probability = dd[-1]
# delete 0s
nonzero.position = which(probability != 0)
probability = probability[nonzero.position]
degree = degree[nonzero.position]
# linear fit of logarithms
reg = lm(log(probability) ~ log(degree))
cozf = coef(reg)
power.law.fit = function(x) exp(cozf[[1]] + cozf[[2]] * log(x))
# alpha and r
alpha = -cozf[[2]]
R.square = summary(reg)$r.squared
# linear fit of logarithms
reg = lm(log(probability) ~ log(degree))
cozf = coef(reg)
power.law.fit = function(x) exp(cozf[[1]] + cozf[[2]] * log(x))
# alpha and r
alpha = -cozf[[2]]
R.square = summary(reg)$r.squared

print(paste("Alpha =", round(alpha, 3)))
print(paste("R square =", round(R.square, 3)))

#plot
plot(probability~degree, log="xy", xlab="ki", ylab="P(k)", col=1)
curve(power.law.fit, col="red", add=T, n=length(degree))

#Scale-free????##
is.scalefree<-is.scalefree(weight.graph.copy, cumulative = c(TRUE))

#Is my Network Hierarchical?###
#The script is the same, only that instead of frequency now we calculate the average C of nodes with the same k

#cluster = transitivity(graph, type="local", isolates="NaN")
cluster = transitivity(weight.graph.copy, type="local", isolates="NaN")

for (i in 1:max(d)){
  probability[i]=mean(cluster[d==i], na.rm=TRUE)
}

#is.hierarchy(Wnohu.graph.copy) 
is.hierarchy(weight.graph.copy) 

plot(probability~degree, log="xy", xlab="ki", ylab="C(k)", col=1)
curve(power.law.fit, col="red", add=T, n=length(d))

#Modular##
modular <- cluster_optimal(weight.graph.copy)
modular

v.connect<-igraph::vertex_connectivity(weight.graph.copy, source = NULL, target = NULL, checks = TRUE)

    
  
###Modular####
modular <- cluster_optimal(graph)
modular

try({
  ## The calculation only takes a couple of seconds
  oc <- cluster_optimal(g)
  
  ## Double check the result
  print(modularity(oc))
  print(modularity(g, membership(oc)))
  
  ## Compare to the greedy optimizer
  fc <- cluster_fast_greedy(g)
  print(modularity(fc))
}, silent=TRUE)

##try with my network##
modular <- cluster_optimal(weight.graph.copy)
modular

try({
  ## The calculation only takes a couple of seconds
  oc <- cluster_optimal(Wnohu.graph.copy)
  
  ## Double check the result
  print(modularity(oc))
  print(modularity(Wnohu.graph.copy, membership(oc)))
  
  ## Compare to the greedy optimizer
  fc <- cluster_fast_greedy(Wnohu.graph.copy)
  print(modularity(fc))
}, silent=TRUE)



modular <- cluster_optimal(net.sp) #troubles!#
modular

#can try this#
try({
  ## The calculation only takes a couple of seconds
  oc <- cluster_optimal(g)
  
  ## Double check the result
  print(modularity(oc))
  print(modularity(g, membership(oc)))
  
  ## Compare to the greedy optimizer
  fc <- cluster_fast_greedy(g)
  print(modularity(fc))
}, silent=TRUE)

#modular<-cluster_spinglass(weight.graph.copy, weights = E(weight.graph.copy)$weight, vertex = NULL, spins = 30,
#parupdate = FALSE, start.temp = 1, stop.temp = 0.01, cool.fact = 0.99,
#update.rule = c("config"), gamma = 1,
#implementation = c("orig"), gamma.minus = 1)

#The vertex and edge betweenness are (roughly) defined by the number of geodesics (shortest paths) 
#going through a vertex or an edge#
between<-betweenness(weight.graph.copy)
central<-centralization.betweenness(weight.graph.copy)$centralization #see igraph::centralize  
#...bc grpah was 'scale -free' - im interested in node centrality#
com <- edge.betweenness.community(weight.graph.copy)
V(weight.graph.copy)$memb <- com$membership
modularity(com)

plot(com, weight.graph.copy, color = "greens", vertex.size=v.size/1000000, 
     vertex.label.cex=c(0.5), layout=coords, rescale=T, 
     edge.width=E(weight.graph.copy)$width)


#By default the edges within communities are colored green and other edges are red.
png(filename="V:/dguzmancolon/dguzmancolon/igraph/igraph/module_com.png")

plot(com, weight.graph.copy, col = membership(com),mark.groups = communities(com),vertex.size=v.size/1000000, 
     vertex.label.cex=c(0.5), layout=coords, rescale=T, edge.width=E(weight.graph.copy)$width,  
     edge.color = c("black", "red")[crossing(com,weight.graph.copy) + 1], mark.expand=5)

dev.off()

#color the edges of the graph based on their source node color
edge.start <- get.edges(weight.graph.copy, 1:ecount(weight.graph.copy))[,1]
edge.col <- V(weight.graph.copy)$color[edge.start]


##########END OF SCRIPT#####
