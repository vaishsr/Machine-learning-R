## EPIK Network Descriptive Statistics  
## February 13, 2018 
## General Note: Be careful runnning iGraph and Statnet simultaneously, commands overlap 

#Clearing Old Data
rm(list = ls())
gc()

#setting network directory on dropbox 
setwd("C:/Users/soren/Google Drive/Dropbox/EPIK Documents/Social Network Analyses/Important Hearings Network Cuts - 2018")

############# The following reads the data file as a graph in the STATNET package ####################
############# Following https://dnac.ssri.duke.edu/r-labs/2017/02_descriptive_statistics.php #########
############# Note: Statnet is a structural update to the older SNA package in R #####################

library(statnet)
dat=read.csv(file.choose(),header=TRUE)
el=as.matrix(dat)            
el[,1]=as.character(el[,1])  
el[,2]=as.character(el[,2])
n=network(dat,matrix.type="edgelist",directed=TRUE) 
plot(n)
# detach(packages:statnet)

############# The following reads the data file as a graph in the IGRAPH package #####################
############# http://kateto.net/netscix2016 ##########################################################

library(igraph)

#creating weights for links in igraph network object 
links <- read.csv(file.choose(),header=TRUE)
library(plyr)                                                          #
table(links)                                                           #
count(links)                                                           # creates a table to find the weights 
links2 <- count(links)                                                 # 
links2                                                                 #
class(links2)                                                          #
write.csv(links2,file = 'EPIK_TestFile.csv')                           # exports to .csv file 
links3 <- read.csv(file.choose(),header=TRUE)                          # grab new csv      
links3$X <- NULL                                                       # deletes the 2nd column
links3 

nodes <- links$Source                                                  # creates nodes from links list
head(nodes)
duplicated(nodes)                                                      # finds duplicate nodes 
nodes
nodes[duplicated(nodes)]                                               # list names of duplicates nodes
nodes2 <- nodes[!duplicated(nodes)]                                    # deduplicates nodes
nodes2        
class(nodes2)

g <- graph_from_data_frame(d=links2, vertices=nodes2, directed=TRUE)
plot(g)
plot(g, vertex.label=NA)
plot(g, remove.multiple=F, remove.loops=T)



#from kateya
net <- graph_from_data_frame(d=links, vertices=nodes, directed=T) 
class(net)

#from matthew 
g=graph.data.frame(dat,directed=FALSE)

# detach(packages:igraph)

################################################################################################
################################################################################################
# Network Measures Using Statnet 
# https://dnac.ssri.duke.edu/r-labs/2017/02_descriptive_statistics.php
# https://cran.r-project.org/web/packages/sna/sna.pdf 
################################################################################################
################################################################################################

#network size, number of edges, number of dyads 
network.size(n)
network.edgecount(n)
network.dyadcount(n)

#density 
gden(n, mode='digraph')
ndegree <- degree(n, cmode="indegree")              #only appropriate for directed network 
outdegree <- degree(n, cmode="outdegree")           #only appropriate for directed network 

#Global Clustering Coefficient: Transitivity
gtrans(n)

#Weak and Weak Concensus Transitivity Measures 
gtrans(n, mode='digraph', measure='weak')
gtrans(n, mode='digraph', measure='weakcensus')

#Degree Centrality: Total, In-Degree, Out-Degree ###########################################

#Restoring Our Directed Network
set.network.attribute(n, "directed", TRUE)

#In-Degree Centrality
indegree <- degree(n, cmode="indegree")
#InDegree <- InDegree * .15                #Scaling, probably not needed for EPIK project 
set.vertex.attribute(n, "InDegree", indegree)

#Out-Degree Centrality
outdegree <- degree(n, cmode="outdegree")
#OutDegree <- OutDegree * .5                #Scaling, probably not needed for EPIK project 
set.vertex.attribute(n, "OutDegree", outdegree)

#Total Degree Centrality: The Total Number of Adjacent Nodes (In-Degree + Out-Degree)
totaldegree <- outdegree + indegree
#TotalDegree <- TotalDegree * .4            #Scaling, probably not needed for EPIK project
set.vertex.attribute(n, "TotalDegree", totaldegree)

#Try Sizing by the Different Degrees in a ggplot graph 
set.seed(12345)
ggnetwork(n) %>%
  ggplot(aes(x = x, y = y, xend = xend, yend = yend)) + 
  geom_edges(color = "lightgray") +
  geom_nodes(color = Color_Race, size = indegree) +       #must substitute Race variable#
  theme_blank() + 
  geom_density_2d()

#Path Centralities: Closeness Centrality, Information Centrality, Betweenness Centrality ####

#Closeness Centrality: geodesic distances of node i to all other nodes
n_Closeness <- closeness(n, gmode="digraph", cmode="directed")
n_Closeness
hist(n_Closeness , xlab="Closness", prob=TRUE)

#Information Centrality: measures the information flowing from node i
n_Info <- infocent(n, rescale=TRUE)
n_Info
hist(n_Info , xlab="Information Centrality", prob=TRUE)

#Betweenness Centrality: the actor between all the other actors, knowledge brokers
#measure based on Freeman's (1979) algorithm 
n_Betweenness <- betweenness(n, gmode="digraph")  
n_Betweenness
hist(AHS_Betweenness , xlab="Betweenness Centrality", prob=TRUE) 

gplot(n, vertex.cex=sqrt(n_Betweenness)/25, gmode="digraph") 

#Comparing Closeness and Betweenness Centralities
cor(n_Closeness, n_Betweenness)    #Correlate our adjusted measure of closeness with betweenness
plot(n_Closeness, n_Betweenness)   #Plot the bivariate relationship

#Bonacich Power Centrality Scores
bonpow(n, gmode="digraph")

#Eigenvetor Centrality Scores, plotting eigen scores 
n_eigen <- evcent(AHS_Network)
n_eigen
hist(n_eigen, xlab="Eigenvector Centrality", prob=TRUE) 
gplot(n, vertex.cex=n_eigen*10, gmode="digraph") 

# Burt's measures of brokerage are supported by igraph #######################################
# see http://igraph.org/r/doc/constraint.html    #############################################

constraint(g, nodes = V(g), weights=NULL)   ### must update the weighting 

#Brokerage Measure based on Gould and Fernandez's (1989) algortihm  ##########################
#See https://cran.r-project.org/web/packages/sna/sna.pdf page 25 for explanation of roles 
#See https://www.jstatsoft.org/article/view/v024i06/v24i06.pdf pages 22-24 for output summary

#first, we need to attach the attributes to our policymakers 
#memb <- sample( , replace=TRUE)           #This creates a vector of subgroups 

n_brokerage <- brokerage(n, Race)         #Race must be replaced with an attribute 
n_brokerage
hist(n$cl, xlab="Cumulative Brokerage", prob=TRUE)

n_cbrokerage <- (n_brokerage$cl)
gplot(n, vertext.cex=n_cbrokerage*.5, gmode="digraph") 

##guide to interpreting brokerage b/c there will be a range of output 
#Brokerage Roles: Group-Based Concept
#w_I: Coordinator Role (Mediates Within Group Contact)
#w_O: Itinerant Broker Role (Mediates Contact between Individuals in a group to which the actor   #does not belong)
#b_{IO}: Representative: (Mediates incoming contact from out-group members)
#b_{OI}: Gatekeeper: (Mediates outgoing contact from in-group members)
#b_O: Liason Role: (Mediates contact between individuals of two differnt groups, neither of which #the actor belongs)
#t: Total or Cumulative Brokerage (Any of the above paths)

################################################################################################
################################################################################################
# Network Measures Using iGraph  
# http://kateto.net/networks-r-igraph 
################################################################################################
################################################################################################

#global density in igraph 
edge_density(g, loops=F)

#reciprocity for directed graphs 
reciprocity(g)
dyad_census(g) # Mutual, asymmetric, and null node pairs
2*dyad_census(g)$mut/ecount(g) # Calculating reciprocity

#global and local transitivity 
transitivity(g, type="global") # net is treated as an undirected network
transitivity(as.undirected(g, mode="collapse")) # same as above
transitivity(g, type="local")
triad_census(g) # for directed networks

#skipped diameter measures 

#degree measures
deg <- degree(g, mode="all")
plot(g, vertex.size=deg*3)

#louvain modularity function (http://igraph.org/r/doc/cluster_louvain.html)
cluster_louvain(g, weights = NULL)

# other community functions (https://www.sixhat.net/finding-communities-in-networks-with-r-and-igraph.html)











