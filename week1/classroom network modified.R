rm(list = ls(all = TRUE))
library(igraph)
library(data.table)
setwd("C:/Users/cyma9/Dropbox/SocialNetwork/week1")
#setwd("~/Dropbox")



data = fread(file="sample_generated_network_new.csv", header = TRUE, stringsAsFactors=FALSE)

# data<-data[ , apply(data, 2, function(x) !any(is.na(x)))]
# 1. remove "G_Like - " from column names
# colnames(data) = gsub("G_Like - ", "", colnames(data))

# 2. replace all names with a key
# names = data.table(name = colnames(data)[2:(ncol(data) - 5)])
# names[, key := sample(.N, replace = FALSE)]

column_wise_replace = function(DT, x, y) {
	for(i in seq_along(x)){
		 for (j in seq_len(ncol(DT))){
   		set(DT,which(DT[[j]] == x[i]),j,y[i])
		}
	}
}

#column_wise_replace(data, names$name, names$key)

#for(i in seq_along(names$name)){
#	colnames(data)[colnames(data) == names$name[i]] = names$key[i]
#}

# randomize order of inputted responses and order of response choices

data = data[sample(nrow(data), replace = FALSE),]
advice = data[,(ncol(data) - 4):ncol(data)]
namekey = data[,1]

data = data[,2:(ncol(data) - 5)]

setcolorder(data, sample(colnames(data), replace = FALSE))

data = cbind(namekey, data, advice)

# in class example allow diag for trust so degree to which trust self in attribute example


# getting adjacency data into igraph

# make the choice data numeric
scale = cbind(c("Extremely Distrust  1","Distrust  2","Slightly Distrust  3","Neither Distrust Nor Trust  4", "Slightly Trust  5","Trust  6", "Extremely Trust  7", "I don't know this person.", "This is my own name."), c(-3, -2, -1, 0, 1, 2, 3, 0, 0))

column_wise_replace(data, scale[,1], scale[,2])

# make adjacency matrix

# subset to just the trust choices, then sort on the columns and rows to make each entry match up
# data are directed, so matrix will not be symmetric
adj = as.data.frame(data[,1:(ncol(data) - 5)])
rownames(adj) = adj[,1]
adj = adj[,-1]

adj = adj[sort(rownames(adj)),sort(colnames(adj))]

trusting = adj
trusting[trusting==-3] = 0
trusting[trusting==-2] = 0
trusting[trusting==-1] = 0

trusting = graph.adjacency(as.matrix(trusting), "directed", weighted = TRUE)


plot.igraph(trusting,vertex.label=NA,layout=layout.fruchterman.reingold, vertex.label.color="black",edge.width=E(trusting)$weight,vertex.size = 12, edge.arrow.size=.3,edge.curved=FALSE)

# can also set attributes directly to the graph


# -------------------------------------------------------Question2-----------------------------------------------------
distrust = adj
distrust[distrust == 3] = 0
distrust[distrust == 2] = 0
distrust[distrust == 1] = 0
distrust[distrust == -3] = 3
distrust[distrust == -2] = 2
distrust[distrust == -1] = 1 
distrust_num = apply(distrust,2,as.numeric)
dev = colSums(distrust_num)
pal <- rainbow(max(dev) - min(dev) + 1 , alpha=.5)

distrust_graph = graph.adjacency(as.matrix(distrust_num), "directed", weighted = TRUE)
distrust_graph = set.vertex.attribute(distrust_graph, "dev", index = V(distrust_graph), dev) 
col = rep(0,30)
for ( i in c(1:30))
{
  col[i] = pal[dev[i] + 1]
}

plot.igraph(distrust_graph,vertex.label=NA,layout=layout.fruchterman.reingold, vertex.color = col, vertex.label.color="black",edge.width=E(distrust_graph)$weight,vertex.size = 12, edge.arrow.size=.3,edge.curved=FALSE)



# -------------------------------------------------------Question3-----------------------------------------------------






count = rep(0,38)

advice_edge1 = cbind(data[,1], data$V38)
for(i in c(1:30))
  {
  count[data$V38[i]] =  count[data$V38[i]] + 1   
  }

advice_edge2 = cbind(data[,1], data$V39)
for(i in c(1:30))
{
  count[data$V39[i]] =  count[data$V39[i]] + 1   
}

advice_edge3 = cbind(data[,1], data$V40)
for(i in c(1:30))
{
  count[data$V40[i]] =  count[data$V40[i]] + 1   
}

advice_edge4 = cbind(data[,1], data$V41)
for(i in c(1:30))
{
  count[data$V41[i]] =  count[data$V41[i]] + 1   
}

advice_edge5 = cbind(data[,1], data$V42)
for(i in c(1:30))
{
  count[data$V42[i]] =  count[data$V42[i]] + 1   
}
print(count)
advice_edges = rbind(advice_edge1, advice_edge2, advice_edge3, advice_edge4, advice_edge5)

advice_seeking = graph.data.frame(advice_edges, directed = TRUE)
for ( i in c(1:35))
{
  V(advice_seeking)$color[i] = 'grey'
  if (V(advice_seeking)$name[i] == which.max(count))
  {
    V(advice_seeking)$color[i] = 'red'
  }
}
# node = as.array(V(advice_seeking))
# V(advice_seeking)$color[node[] == which.max(count)] = 'red'

# make the plot
plot.igraph(advice_seeking,vertex.label=NA,layout=layout.fruchterman.reingold, vertex.label.color="black",edge.color="black",vertex.size = 12, edge.arrow.size=.3,edge.curved=FALSE)




#---------------------------------------------Question4----------------------------------------------------------------





# trust
count_trust = 0 
for (i in c(1:30))
{
  for (j in c(1:30))
  {
    if ((adj[i,j]>0)&(adj[j,i]>0)){
      count_trust = count_trust + 1 
      
    }
  }
}

count_distrust = 0 
for (i in c(1:30))
{
  for (j in c(1:30))
  {
    if ((adj[i,j]<0)&(adj[j,i]<0)){
      count_trust = count_trust + 1 
      
    }
  }
}


advice_adj = matrix(0,nrow = 37, ncol = 37)
for ( i in c(1:nrow(advice_edges)))
{
  x = advice_edges[i]$`To begin, please select your name.`
  y = advice_edges[i]$V2
  advice_adj[x,y] = 1
}
count_advice = 0
for ( i in c(1:37))
{
  for ( j in c(1:37))
    if ((advice_adj[i,j]==1)&(advice_adj[j,i]==1))
    {
      count_advice = count_advice + 1
    }
}






#---------------------------------------------------Question5--------------------------------------------------------



count_advice_trust = 0
count_advice_distrust = 0

for (i in c(1:30))
{
  for (j in c(1:30))
  {
    x = as.numeric(colnames(adj)[i])
    y = as.numeric(rownames(adj)[j])
    if ((adj[i,j]>0) & (advice_adj[x,y]>0))
    {
      count_advice_trust = count_advice_trust + 1
    }
    if ((adj[i,j]<0) & (advice_adj[x,y]>0))
    {
      count_advice_trust = count_advice_trust + 1
    }
    
  }
}


#------------------------------------------Extra-----------------------------------------------------------------------


trusting = adj
trusting[trusting==-3] = 0
trusting[trusting==-2] = 0
trusting[trusting==-1] = 0

trusting_extra = graph.adjacency(as.matrix(trusting), "directed", weighted = TRUE)
E(trusting_extra)$color = 'green'

# plot.igraph(trusting_extra,vertex.label=NA,layout=layout.fruchterman.reingold, vertex.label.color="black",edge.width=E(trusting_extra)$weight,vertex.size = 12, edge.arrow.size=.3,edge.curved=FALSE)





distrust = adj
distrust[distrust == 3] = 0
distrust[distrust == 2] = 0
distrust[distrust == 1] = 0
distrust[distrust == -3] = 3
distrust[distrust == -2] = 2
distrust[distrust == -1] = 1 


distrust_graph_extra = graph.adjacency(as.matrix(distrust_num), "directed", weighted = TRUE)
E(distrust_graph_extra)$color  = 'red'


# plot.igraph(distrust_graph_extra,vertex.label=NA,layout=layout.fruchterman.reingold, vertex.label.color="black",edge.width=E(distrust_graph_extra)$weight,vertex.size = 12, edge.arrow.size=.3,edge.curved=TRUE)


extra_graph = union(distrust_graph_extra, trusting_extra) 
E(extra_graph)$color <- ifelse(is.na(E(extra_graph)$color_1),E(extra_graph)$color_2,E(extra_graph)$color_1)
E(extra_graph)$weight <- ifelse(is.na(E(extra_graph)$weight_1),E(extra_graph)$weight_1,E(extra_graph)$weight_2)


advice_adj_extra = adj

for (i in c(1:30))
{
  for (j in c(1:30))
  {
    x = as.numeric(colnames(adj)[i])
    y = as.numeric(rownames(adj)[j])
    if (advice_adj[x,y] == 1) 
    {
      advice_adj_extra[i,j] = 1
    }
    else
    {
      advice_adj_extra[i,j] = 0 
    }
  }
}

advice_graph_extra = graph.adjacency(as.matrix(advice_adj_extra), "directed", weighted = TRUE)
E(advice_graph_extra)$color  = 'black'











advice_edge = as_edgelist(advice_graph_extra, names = T)
trust_edge = as_edgelist(extra_graph, names = T)
new_edge = rbind(trust_edge, advice_edge)
graph_new = graph_from_edgelist(new_edge)

for (i in c(1:424))
{
  E(graph_new)$color[i] = E(extra_graph)$color[i]
  E(graph_new)$weight[i] = E(extra_graph)$weight[i]
}

for (i in c(425:554))
{
  E(graph_new)$color[i] = E(advice_graph_extra)$color[i-424]
  E(graph_new)$weight[i] = E(advice_graph_extra)$weight[i-424]
}


plot.igraph(graph_new,vertex.label=NA,layout=layout.fruchterman.reingold, vertex.label.color="black", edge.width=E(extra_graph_full)$weight,edge.color = E(extra_graph_full)$color,vertex.size = 12, edge.arrow.size=.3,edge.curved=TRUE)


advice_matrix = as.matrix(as_adjacency_matrix(advice_seeking))

# mean density of advice network 
sum(advice_matrix)/nrow(advice_matrix)

# assign deviation as an attribute
dev = abs(colSums(advice_matrix) - sum(advice_matrix)/nrow(advice_matrix))

advice_seeking = set.vertex.attribute(advice_seeking, "dev", index = V(advice_seeking), dev) 

V(advice_seeking)$color = V(advice_seeking)$dev

plot.igraph(advice_seeking,vertex.label=NA,layout=layout.fruchterman.reingold, edge.color="black",vertex.size = 12, edge.arrow.size=.3,edge.curved=FALSE)
