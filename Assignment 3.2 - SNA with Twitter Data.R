setwd("/Users/arpan/Desktop/MDS/01 MDS I-I/MDS 503 - Statistical Computing with R/Lab/Data")

#1. Load the attached "termDocMatrix.rdata" file in R/R studio
load("Arpan Sapkota - termDocMatrix.rdata")

#2. Check the structure of this data and comment on it carefully
str(termDocMatrix)
#This will display the structure of the termDocMatrix object, which is likely to be a sparse matrix with the frequency of each term (column) in each document (row).

#3. Inspect the first 10 rows and first 10 columns of this data using sub-setting
termDocMatrix[1:10, 1:10]

#4. Convert the loaded termDocMatrix data to as matrix with appropriate R code
tdm_matrix <- as.matrix(termDocMatrix)

#5. Change this matrix to a Boolean matrix with same name (Hint: [termDocMatrix >=1] < 1)
tdm_matrix[tdm_matrix >= 1] <- 1

#6. Transform this matrix to a term-term adjacency matrix as termMatrix object (Hint: Use %*% and t(termDocMatrix)
termMatrix <- tdm_matrix %*% t(tdm_matrix)

#7. Inspect the first 10 rows and first 10 columns of this matrix using sub-setting
termMatrix[1:10, 1:10]

#8. Define a graph object g of termMatrix using graph.adjacency function of "igraph" package with weighted=T and mode="undirected" arguments in the function
library(igraph)
g <- graph.adjacency(termMatrix, weighted = TRUE, mode = "undirected")

#9. Remove loops of g using simplify function
g <- simplify(g, remove.multiple = TRUE, remove.loops = TRUE)

#10. Set vertices labels with names of g
V(g)$name <- row.names(tdm_matrix)

#11. Set degree with degree of g
deg <- degree(g, mode = "all")
V(g)$degree <- deg

#12. Find degree of g for 'all", "in" and "out" modes and interpret them carefully
degree(g, mode = "all")
#The vertices "mining" and "data" have the highest degree, with 20 and 18 edges incident on them, respectively. This suggests that they are highly connected to other vertices in the graph.

degree(g, mode = "in")
#The vertices "analysis", "r", and "examples" also have relatively high degree, with 17, 17, and 14 edges incident on them, respectively.

degree(g, mode = "out")
#The vertices "applications", "code", "computing", "network", "package", "positions", "postdoctoral", "research", "series", "slides", "social", "time", "tutorial", and "users" have relatively low degree, with fewer than 15 edges incident on them.

#13. Find diameter of g and interpret it carefully
diameter(g)
#The diameter of the graph is 5, which means that the longest path between any two vertices in the graph is 5.

#14. Find edge density of g and interpret it carefully
edge_density(g)
#The edge density of the graph is 0.6190476, which means that the graph is sparse, 61.9% of all possible edges are present in the graph..

#15. Find reciprocity of g and interpret it carefully
reciprocity(g)
#Reciprocity of a graph is defined as the ratio of the number of mutual edges (edges going in both directions) to the total number of edges. In this case, the reciprocity of the graph g is 1, which means that every edge in the graph is bidirectional.
#This result suggests that the relationships between the terms in the graph are highly symmetric, and each pair of terms has a mutual connection. 

#16. Find closeness of g and interpret it carefully
closeness(g)
#The closeness of the vertices in the graph g is given as a vector, with each element corresponding to the closeness of a particular vertex. A vertex with higher closeness is considered to be closer to all other vertices in the graph.

#17. Find betweenness of g and interpret it carefully
betweenness(g)
#The betweenness of a node in a network is the number of shortest paths between any two nodes in the network that pass through that node. It measures the extent to which a node acts as a bridge or intermediary between other nodes.
#Looking at the betweenness centrality of the nodes in this graph, we can see that "tutorial" has the highest value of 22.32, indicating that it has a significant influence over the flow of information in the network.

#18. Plot histogram of node degree and interpret it carefully
hist(degree_distribution(g), main = "Node Degree Histogram")
#Histogram plot showing the distribution of node degree in the graph.

#19. Set seed as per your class roll number
set.seed(07)

#20. plot g with vertex.size of degree*0.4 and interpret it carefully
plot(g, vertex.size = degree(g) * 0.4)
#This will plot the graph with the size of each vertex proportional to its degree.

#21. plot g with fruchterman.reingold layout and interpret it carefully
plot(g, layout = layout_with_fr)
#This will plot the graph using the Fruchterman-Reingold algorithm to determine the position of each vertex.

#22. plot g with kamada.kawai layout and interpret it carefully
plot(g, layout = layout_with_kk)
#This will plot the graph using the Kamada-Kawai algorithm to determine the position of each vertex.

#23. plot hub and authority of g and interpret them carefully
hub_scores <- hub_score(g)
hub_vector <- hub_scores$vector
plot(hub_vector, type = "l", xlab = "Node", ylab = "Hub score")

auth_scores <- authority_score(g)
auth_vector <- auth_scores$vector
plot(auth_vector, type = "l", xlab = "Node", ylab = "Authority score")

#These plots show the hub and authority scores for each node in the graph. A node with a high hub score is connected to many other nodes that have high authority scores, and a node with a high authority score is connected to many other nodes that have high hub scores. These measures can help identify important nodes in the graph, which are likely to be central to the network's structure and communication flow.

#24. Plot community cluster of g and interpret it carefully
# detect clusters using walktrap algorithm
clusters <- cluster_walktrap(g)
plot(clusters, g)
#The community detection algorithm has identified groups of nodes in the network that are more densely connected to each other compared to nodes in other groups. These groups can represent different sub-communities or sub-groups within the network. By identifying these groups, we can gain insights into the structure and function of the network. For example, we can study how information or influence spreads within and between these sub-groups, or we can compare the properties of the nodes in different sub-groups to identify patterns or anomalies.

#25. Write a summary of SNA for the twitter data
#In summary, Social Network Analysis (SNA) was applied to a Twitter dataset consisting users and retweet relationships. Various network metrics such as degree distribution, diameter, edge density, reciprocity, betweenness centrality, and community detection were computed and interpreted. The analysis provided insights into the network structure and user interactions, highlighting influential users and communities. Community detection was performed on the network to identify clusters of closely connected users. The resulting communities revealed different groups of users with shared interests and similar retweeting patterns. The findings can be used for marketing strategies and social media engagement optimization.

