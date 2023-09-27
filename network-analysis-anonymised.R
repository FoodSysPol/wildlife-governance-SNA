###Step 1: Install packages
.libPaths("/Library/Frameworks/R.framework/Versions/4.3-arm64/Resources/library")

library(tidyverse)
#install.packages('tidygraph')
library(tidygraph)
#install.packages('ggraph')
library(ggraph)
#install.packages('ggrepel')
library(ggrepel)
#install.packages('network')
library(network)
#install.packages('igraph')
library(igraph)
#install.packages('statnet')
library(statnet)
#install.packages('ergm')
library(ergm)

###Step 2: Set WD
setwd("/Users/chloecliffordastbury/Documents/Research/Wildlife project/SNA")

###Step 3: Load intermediate data 
#long version of link data from short survey with response options - participant-entered organisations not included yet!
intermediate <-read.csv('short-survey-intermediate.csv')

###Step 4: Clean and reshape intermediate data

#Redacted as includes organisation's names

intermediate <- intermediate %>%
  pivot_longer(!Organisation, names_to=NULL, values_to="To")

intermediate <- rename(intermediate, From = Organisation)

intermediate <- intermediate %>% drop_na()

###Step 5: Print intermediate data to file for manual editing
write.csv(intermediate, "intermediate_cleaned.csv")
#Combine with long survey and participant-entered organisations

###Step 6: Load cleaned data
links <- read.csv('links.csv')

###Step 7: Change duplicated names

#Redacted as includes organisation's names

###Step 8: Convert cleaned data to graph format

#Transform into igraph graph
links2 <- as.matrix(links)
links_graph <- graph_from_edgelist(links2)
links_graph <- as.undirected(links_graph)

#Remove self-loops and duplicate edges
links_graph <- simplify(links_graph)

#Make graph into igraph object
graph_links <- as_tbl_graph(links_graph, directed = FALSE)


###Step 9: Pull list of organisations and print to file to check for duplicates
orgs <- graph_links %>%
  activate(nodes) %>%
  as.data.frame()

#69 organisations
write.csv(orgs, "orgs.csv")

###Step 10: Load org attributes in igraph for visualisation
orgs_att <- read.csv('orgs attributes.csv')
graph_links_2 <- graph_links %>%
  activate(nodes) %>%
  mutate(
    sector = orgs_att$Sector,
    type = orgs_att$Orgnanization.type)

#Reduce number of sectors for visualisation
orgs_att2 <- orgs_att
orgs_att2$Sector[orgs_att2$Sector=="Animal Health" | orgs_att2$Sector=="Animal Welfare"] <- "Animal health and welfare"
orgs_att2$Sector[orgs_att2$Sector=="Nuclear energy" | orgs_att2$Sector=="Transport" | orgs_att2$Sector=="Crime" | orgs_att2$Sector== "Food and agriculture" | orgs_att2$Sector=="Zoos and aquariums" | orgs_att2$Sector=="Development" | orgs_att2$Sector== "Trade"] <- "Other"

#View sectors
table(orgs_att2$Sector)

#Descriptive characteristics of sample (Table 1)
table1 <- table(orgs_att2$Orgnanization.type, orgs_att2$Sector)
table1 <- addmargins(table1, c(1,2), sum)

freq <- orgs_att2 %>% 
  group_by(Orgnanization.type) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

freq <- orgs_att2 %>% 
  group_by(Sector) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

rm(table1, freq)

#Append attributes to nodes for visualisation
graph_links_3 <- graph_links %>%
  activate(nodes) %>%
  mutate(
    Sector = orgs_att2$Sector,
    Type = orgs_att2$Orgnanization.type,
    Degree = centrality_degree(mode = 'all'))

###Step 11: Plot figure
#With names
graph_links %>%
  ggraph(layout = "kk") +
  geom_node_text(aes(label = name), size = 3) +
  geom_edge_diagonal(color = "gray", alpha = 0.4)

#By sector
graph_links_2 %>%
  ggraph(layout = "graphopt") +
  geom_edge_link(color = "gray", alpha = 0.4) +
  geom_node_point(aes(color = sector), size = 3) 


#By type
graph_links_2 %>%
  ggraph(layout = "graphopt") +
  geom_edge_link(color = "gray", alpha = 0.4)+
  geom_node_point(aes(color = type), size = 3)


#By sector, type and degree centrality
graph_links_3 %>%
  ggraph(layout = "kk") +
  geom_edge_link(color = "gray", alpha = 0.4) +
  geom_node_point(aes(color = Type, shape = Sector, size = Degree)) +
  guides(size = "none")

#By sector, type and degree centrality (figure update)

#Reduce number of sectors to six or less for visualisation
orgs_att3 <- orgs_att2
orgs_att3$Orgnanization.type[orgs_att3$Orgnanization.type=="Voluntary partnership secretariat" | orgs_att3$Orgnanization.type=="Treaty secretariat"] <- "Agreement secretariat"
orgs_att3$Orgnanization.type[orgs_att3$Orgnanization.type=="Consultancy" | orgs_att3$Orgnanization.type=="Government department" | 
                               orgs_att3$Orgnanization.type=="Professional association" | orgs_att3$Orgnanization.type== "Regional economic initiative" | 
                               orgs_att3$Orgnanization.type=="Trade association"] <- "Other"

orgs_att3$Orgnanization.type[orgs_att3$Orgnanization.type=="Inter-governmental organization"] <- "Inter-governmental organisation"
orgs_att3$Orgnanization.type[orgs_att3$Orgnanization.type=="Non-governmental organization"] <- "Non-governmental organisation"


orgs_att3$Orgnanization.type <- as.factor(orgs_att3$Orgnanization.type)

levels(orgs_att3$Orgnanization.type)

orgs_att3$Orgnanization.type <- fct_relevel(orgs_att3$Orgnanization.type, "Inter-governmental organisation",
                                            "Non-governmental organisation", "Agreement secretariat",
                                            "Network", "Research institution", "Other")


levels(orgs_att3$Orgnanization.type)


graph_links_4 <- graph_links %>%
  activate(nodes) %>%
  mutate(
    Sector = orgs_att3$Sector,
    Type = orgs_att3$Orgnanization.type,
    Degree = centrality_degree(mode = 'all'))

graph_links_4 %>%
  ggraph(layout = "kk") +
  geom_edge_link(color = "gray", alpha = 0.4) +
  geom_node_point(aes(color = Sector, shape = Type, size = Degree)) +
  guides(size = "none")

###Step 12: Calculate whole-network statistics

#Density
edge_density(graph_links)

#Descriptive characteristics
graph_links_3 #number of edges
diameter(graph_links_3, directed = FALSE) #network diameter

280/69 #Average degree = total edges/total nodes

###Step 13: Calculate node statistics
#https://rpubs.com/pjmurphy/igraphCentrality 

#Order characteristics to match nodes
#Load organisational attributes
nodeInfo <- as.data.frame(orgs_att2[,-c(1)])
nodeInfo <- rename(nodeInfo, org_name = x)

#Make order of organisational attributes the same as vertex order
ordered_orgs <- as.data.frame(graph_links %>%
                                 activate(nodes))

ordered_orgs <- rename(ordered_orgs, org_name = "name")

nodeInfo <- full_join(ordered_orgs, nodeInfo, by = c("org_name"))

#Append betweenness and closeness to graph
graph_links_4 <- graph_links %>%
  activate(nodes) %>%
  mutate(
    Sector = nodeInfo$Sector,
    Type = nodeInfo$Orgnanization.type,
    Degree = centrality_degree(mode = 'all'),
    Close = centrality_closeness(mode = "all"),
    Between = centrality_betweenness(directed = FALSE))

#Create data frame with nodal characteristics
centrality_measures <- as.data.frame(graph_links_4 %>%
  activate(nodes))

#Print to paste in Table 2
write.csv(centrality_measures, "centrality_measures.csv")

###Step 14: Calculate sub-group statistics

#Cliques
###https://igraph.org/r/doc/cliques.html

#Histogram of clique sizes
clique_size_counts(graph_links_3)
#Cliques very numerous, but 12 cliques of 9 and 1 of 10

#Present membership of cliques of 9 or 10
clique <- cliques(graph_links_3, min = 8)

##Size of the largest clique = 10
clique_num(graph_links_3)

#Find largest clique (n=1)
largest_cliques(graph_links_3)

#Find maximal cliques
max_cliques(graph_links_3)

### Step 15: Assess homophily using ERGM
##https://statnet.org/Workshops/ergm_tutorial.html

#Create edge list as data from from igraph object (simplified, excluding self-loops and multiple edges)
edgelinks <- as.data.frame(as_edgelist(links_graph))

#Create network object graph from edgelist
edge_net <- network(edgelinks, matrix.type = "edgelist", directed = FALSE)

#### Add organisation attributes to network object #####
#Load organisational attributes
nodeInfo <- as.data.frame(orgs_att2[,-c(1)])
nodeInfo <- rename(nodeInfo, org_name = x)

#Make order of organisational attributes the same as vertex order
ordered_orgs <- as.data.frame(network.vertex.names(edge_net))
ordered_orgs <- rename(ordered_orgs, org_name = `network.vertex.names(edge_net)`)

nodeInfo <- full_join(ordered_orgs, nodeInfo, by = c("org_name"))


#Add vertex attributes
edge_net%v%"sector" <- nodeInfo$Sector
edge_net%v%"type" <- nodeInfo$Orgnanization.type

## Run exponential random graph model

#Homophily
model <- ergm(edge_net ~ edges + 
                       nodefactor('sector') + nodematch('sector',diff=T) + nodefactor('type'))

summary(model)

##Organisations are not more likely to associate with other orgs from the same sector
