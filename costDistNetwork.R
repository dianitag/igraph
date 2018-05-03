###############################################################
###############################################################
####        From Adjacency Matrices to Networks
####        Borja Esteve-Altava - Transmitting Science Course  
####        Edited by Diana K. Guzman-Colon
###############################################################
###############################################################

##### Download and install the package#######
install.packages("igraph")
install.packages("gdata")#to import .csv files
#the problem is that perl.exe is missing. 
#In my case it worked after getting it installed from: http://www.activestate.com/activeperl/downloads 
#then point to it:
#read.xls("bla.xlsx", perl = "C:\\Perl64\\bin\\perl.exe")
perl<-"C:/Perl64/bin/perl.exe" #needed for package gdata
installXLSXsupport(perl = perl, verbose = TRUE)

##### Load package####
library(igraph)
library(gdata)


####Load Adjacency matrix ####
getwd()
nohu.data<-gdata::read.xls("V:/dguzmancolon/dguzmancolon/igraph/igraph/NoHU_resistances.xls", sheet=1, perl=perl, header=TRUE)
nohu.nodes<-names(nohu.data)[-1] #get node IDs from first column
#remove blank names in nodes
#nohu.nodes<-nohu.nodes[-(153:159)]

nohu.mat<-data.matrix(nohu.data, rownames.force=TRUE)#Import and convert into adjacency matrix
nohu.mat<-nohu.mat[,-1] #The matrix should include the first row (which is data),
#but not the first column (which too contains node-names).
#to remove NA's: 
#nohu.mat<-nohu.mat[ , ! apply( nohu.mat , 2 , function(x) all(is.na(x)) ) ] 

#As the matrix is now of the size N by N, row- and colnames makes for a neat matrix
rownames(nohu.mat) <- colnames(nohu.mat) <- nohu.nodes 
#Check
nohu.mat

####test Graph #####
nohu.graph <- igraph::graph_from_adjacency_matrix(nohu.mat, mode="undirected", weighted=NULL) #with no weight
Wnohu.graph=igraph::graph_from_adjacency_matrix(nohu.mat,mode="undirected",weighted=TRUE,diag=FALSE) ###weighted
#view name of nodes(vertices)
#V(Wnohu.graph)
Wnohu.graph
#load Node attribute table#
node_attr<-read.table("V:/dguzmancolon/dguzmancolon/igraph/igraph/CoreProp.txt", header=TRUE, sep = "\t")
n.attr.name <- node_attr[["core_ID"]] <- nohu.nodes  ##replaced node names to match current adjacency matrix#
n.attr.name<-node_attr[,-1] ##first column (which too contains node-names)#
rownames(n.attr.name) <- nohu.nodes

## This code says to create a vertex attribute called "AREA" 
#by extracting the value of the column "AREA" in the attributes file 
#when the core_ID number matches the vertex name.
V(Wnohu.graph)$AREA=as.character(node_attr$AREA[match(V(Wnohu.graph)$name,node_attr$core_ID)])
V(Wnohu.graph)$AREA
V(Wnohu.graph)$size=V(Wnohu.graph)$AREA
v.size=(V(Wnohu.graph)$size)
v.size=as.numeric(v.size)

##edges
E(Wnohu.graph)$weight

## Add coordinates to nodes#
#You can add the coordinates directly to the graph object in igraph by setting the an X and Y attributes to the vertices
coords <- layout.norm(as.matrix(node_attr[, c("Longitude","Latitude")]))

plot(Wnohu.graph, vertex.size=v.size/1000000, vertex.label.cex=c(0.5), layout=coords, rescale=T, edge.width=E(Wnohu.graph)$weight)
str(Wnohu.graph)

#speficific nodes
Wnohu.graph.copy <- delete.edges(Wnohu.graph, which(E(Wnohu.graph)$weight >= 1)-1)
plot(Wnohu.graph.copy,vertex.size=v.size/1000000, vertex.label.cex=c(0.5), layout=coords, rescale=T, 
     edge.width=E(Wnohu.graph)$weight, edge.curved=0.5  )

#Calc degree dist,edge density, transitivity
dd<-degree_distribution(Wnohu.graph.copy, v = V(Wnohu.graph.copy), cumulative = FALSE, mode = c("all"))
ed<-edge_density(Wnohu.graph.copy, loops = FALSE)
t<-transitivity(Wnohu.graph.copy, v = V(Wnohu.graph.copy), type = c("weighted"), isolates = c("NaN", "zero"))

####Calculate clustering C(k)####
# calculate the number of connections per node
# calculate the clustering of each node
k<-degree(Wnohu.graph.copy)
C <- transitivity(Wnohu.graph.copy, type = "weighted")
L <- mean_distance(Wnohu.graph.copy)
Pk<-degree(Wnohu.graph.copy)
# mean clustering of all nodes with k links
Ck <- rep(0, max(k))
for (i in 1:max(k)){
  Ck[i] <- mean(C[k==i], na.rm = TRUE)
}
plot(Ck)

##### Randomize your network####
RWnohu.graph <- sample_degseq(Pk, in.deg=NULL, method="simple")
# Calculate C and L
CR <- transitivity(RWnohu.graph, type="average")
LR <- mean_distance(RWnohu.graph)
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

scalefree<-is.scalefree(Wnohu.graph.copy, cumulative = c(TRUE))
####Is my Network Hierarchical?#####
#The script is the same, only that instead of frequency now we calculate the average C of nodes with the same k

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

plot(probability~degree, log="xy", xlab="ki", ylab="C(k)", col=1)
curve(power.law.fit, col="red", add=T, n=length(d))

###Network Null Models####
#Regular networks (k-degree model)
#Random networks (Erdös-Rényi model)
#Small-world networks (Watts-Strogatz model)
#Scale-free networks (Barabási-Albert model)
barabasi<-barabasi.game(n=152,power=1)
barabasi
plot(barabasi)
#Geometric networks (Proximity model)

###Modular####
modular <- cluster_optimal(Wnohu.graph.copy)
modular