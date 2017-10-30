rm(list = ls(all = TRUE))
library(igraph)
library(data.table)
setwd("C:/Users/dplewi3/Dropbox")
 
 
# this network is a biology 2 class at a public high school
 
# load data:
# install.packages("NetData")
data(studentnets.S641, package = "NetData")

# check objects
ls()

# check data structure
s641_full_data_frame[1:20,]
dim(s641_full_data_frame)
 
# reduce to non-zero edges and build a graph object
s641_full_nonzero_edges = subset(s641_full_data_frame, (social_tie > 0 | task_tie > 0))
s641_full_nonzero_edges[1:20,]
dim(s641_full_nonzero_edges)
 
s641_full = graph.data.frame(s641_full_nonzero_edges) 
summary(s641_full)
 
# create separate graphs for each relationship type based on edge attributes and remove isolates
s641_social = delete.edges(s641_full, E(s641_full)[get.edge.attribute(s641_full,name = "social_tie")==0])
s641_social = delete.vertices(s641_social, V(s641_social)[degree(s641_social)==0])
summary(s641_social)
 
s641_task = delete.edges(s641_full, E(s641_full)[get.edge.attribute(s641_full,name = "task_tie")==0])
s641_task = delete.vertices(s641_task, V(s641_task)[degree(s641_task)==0])
summary(s641_task)
 
# plot each network
plot.igraph(s641_social, layout=layout.fruchterman.reingold, edge.arrow.size=.5)
 
plot(s641_task, layout=layout.fruchterman.reingold, edge.arrow.size=.5)
  
 
# in a directed network, we can think of in-closeness centrality as the average number of steps needed to get to a node from all the other nodes. out-closeness centrality measures the same thing with the directionality reversed
 
# in-closeness centrality
incloseness_social = closeness(s641_social, mode='in')
incloseness_social
 
# out-closeness
outcloseness_social = closeness(s641_social, mode='out')
outcloseness_social
 
# betweenness centrality measures the number of shortest paths going through a specific vertex and is returned by the betweenness() function
betweenness_social = betweenness(s641_social)
betweenness_social
 
# eigenvector centrality gives greater weight to a node the more it is connected to other highly connected nodes. a node connected to five high-scoring nodes will have higher eigenvector centrality than a node connected to five low-scoring
# nodes. it is often interpreted as measuring a node's network importance

# for these data, we will simply symmetrize to generate an undirected eigenvector centrality score
s641_social_undirected = as.undirected(s641_social, mode='collapse')
# returns an object, in which $vector contains the numerical scores
ev_obj_social = evcent(s641_social_undirected)
eigen_social = ev_obj_social$vector
eigen_social
 

 
# can write a function to get all of these at once
getNetStats=function(net)
{
  close_in = closeness(net, mode = "in")
  close_out = closeness(net, mode = "out")
  betw = betweenness(net)
  evc = evcent(as.undirected(net, mode = "collapse"))
  id=V(net)$name
  stats=as.data.table(list(id = id, close_in = close_in, close_out = close_out, betw = betw, evc = evc$vector))
  return(stats)
}
 
netstats_social = getNetStats(s641_social)
netstats_task = getNetStats(s641_task)

# examine the table to find the most central actors  according to the different measures

# keep the plot to visually chek the results
plot(s641_social, vertex.size=10, vertex.label=V(s641_social)$name, edge.arrow.size = 0.5, layout=layout.fruchterman.reingold,main='Classroom S641 Social Talk')
 
# show table sorted by decreasing indegree. the order() function returns a vector in ascending order and the minus sign flips it to be descending order
netstats_social[order(-close_in)] 
netstats_social[order(-close_out)] 
 
# NOTE: for some reason, this operation returns strange values, a visual inspection of the plot suggests that 11, 15, and 18 are not central actors at all. this could be a bug

netstats_social[order(-evc)] 
 
# let's make a plot or two with these summary statistics
 
# To visualize these data, we can create a barplot for each
# centrality measure. In all cases, the y-axis is the value of
# each category and the x-axis is the node number. 
barplot(netstats_social$close_in, names.arg=netstats_social$id)
barplot(netstats_social$close_out, names.arg=netstats_social$id)
barplot(netstats_social$betw, names.arg=netstats_social$id)
barplot(netstats_social$evc, names.arg=netstats_social$id)
 
# what can we say about the social actors if we compare the bar plots? who seems to run the show in sociable affairs? who seems to bridge sociable conversations? 
 
 
# now, compute correlations betwee the columns to determine how closely these measures of centrality are interrelated 
 
# generate a table of pairwise correlations
cor(netstats_social[,2:5])