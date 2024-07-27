###1
calNetwork <- function(data = data,filter = FALSE,
                       n = 3,method = "spearman",
                       cutoff_cor = 0.6, cutoff_p = 0.05){
  require(WGCNA)
  require("multtest")
  require(reshape2)
  require(tidyverse)
  
  cutoff_cor <- cutoff_cor
  cutoff_p <- cutoff_p
  if(filter){
    dat2 <- data[,colSums(data!= "0") > n]
  } else{
    dat2 <- data[,colSums(data!= "0") > round(nrow(data)/2,0)]
  }
  set.seed(123)
  dat_net <- corAndPvalue(dat2,method = method)#calculate correlation and significance
  dat_cor <- dat_net$cor#select correlation
  dat_p <- dat_net$p#select p values
  
  dat_cor[upper.tri(dat_cor,diag=TRUE)]=NA
  dat_p[upper.tri(dat_p,diag=TRUE)]=NA
  dat_cor_melt <- melt(dat_cor)
  dat_p_melt <- melt(dat_p)
  colnames(dat_cor_melt) <- c("from","to","correlation")
  colnames(dat_p_melt) <- c("from","to","p")
  
  dat_cor_melt %>%
    left_join(dat_p_melt, by = c("from","to")) %>%
    dplyr::filter(p!="NA") %>%
    arrange(p) -> dat_net
  #adjust p values
  procs <- c("ABH")#select benjamini
  p.adjust <- mt.rawp2adjp(dat_net$p,procs)
  
  dat_net2 <- data.frame(dat_net,p.adjust$adjp) %>%
    dplyr::filter(abs(correlation)> cutoff_cor,ABH < cutoff_p) %>%
    dplyr::select(from:correlation,ABH)
  
  colnames(dat_net2) <- c("from","to","correlation","adjust.p")
  return(dat_net2)
}

###2
calNetModule <- function(data = data){
  require(tidyverse)
  require(igraph)
  set.seed(123)
  
  modules <- cluster_fast_greedy(data)
  modules2 <- list()
  modules3 <- data.frame()
  for (i in seq_along(modules)) {
    modules2[[i]] <- modules[[i]] %>%
      as_tibble() %>%
      mutate(name = value) %>%
      select(-value) %>%
      mutate(module = i)
    modules3 <- rbind(modules3,
                      modules2[[i]])
  }
  dat <- list(modules,modules3)
  return(dat)
}

###3
calNetParameters <- function(data = data){
  require(igraph)
  set.seed(123)
  node_number <- length(V(data))#顶点数量
  network_edge_number <- length(E(data))#edge 数量
  network_connectance <- edge_density(data)
  network_degree <- mean(degree(data))
  dat <- data.frame(node_number,network_edge_number,
                    network_connectance,
                    network_degree,
                   )
  return(dat)
}

###4
extract_subNet <- function(graph = bac_graph,
                           data = bac,
                           node = bac_nodes,
                           node_id_position = 1){
  
  require(igraph)
  require(tidyverse)
  set.seed(123)
  
  colnames(node)[node_id_position] <- "name"
  data %>%
    as_tibble(rownames = "sample") %>%
    reshape2::melt(id.vars = "sample") %>%
    split(.$sample,sample) ->data_melt
  
  sub_network <- list()
  sub_network2 <- list()
  sub_node <- list()
  sub_edges <- list()
  sub_edge2 <- list()
  sub_netPara <- list()
  sub_netPara <- list()
  sub_netPara2 <- data.frame()
  
  for (i in seq_along(data_melt)) {
    node %>%
      left_join(data_melt[[i]] %>%
                  filter(value >0), by = c("name" = "variable")) %>%
      filter(value!="NA") ->sub_network[[i]]
    
    sub_network2[[i]] <- induced_subgraph(graph,sub_network[[i]][[1]])
    
    #parameters
    sub_netPara[[i]] <- calNetParameters(sub_network2[[i]])
    sub_netPara2 <- rbind(sub_netPara2,sub_netPara[[i]]) %>%
      data.frame()
    ##node
    sub_node[[i]] <- tibble(otu_ID = get.vertex.attribute(sub_network2[[i]])[[1]] ,
                            degree = get.vertex.attribute(sub_network2[[i]])[[2]])
    
    ##sub network edges
    get.edgelist(sub_network2[[i]]) %>%
      data.frame() %>%
      as_tibble() ->sub_edges[[i]]
    
    colnames(sub_edges[[i]]) <- c("from","to")
    
    sub_edge2[[i]] <- tibble(sub_edges[[i]],
                             correlation = get.edge.attribute(sub_network2[[i]])[[1]],
                             adjust.p = get.edge.attribute(sub_network2[[i]])[[2]])
  }
  
  names(sub_edge2) <- names(data_melt)
  names(sub_node) <- names(data_melt)
  rownames(sub_netPara2) <- rownames(data)
  
  sub_netPara2 %>%
    as_tibble(rownames = "sample") ->sub_netPara2
  sub_network_results <- list(sub_netPara2,sub_node,sub_edge2)
  names(sub_network_results) <- c("sub_networkParameters","node","edge")
  return(sub_network_results)
}

###5
setwd(readClipboard())
getwd()
bac <- read.csv("tax_aco2.csv",header = T,sep= ",",row.names = 1)
bac <-data.frame(t(bac))
dim(bac)
bac_net <- calNetwork(data = bac,method = "spearman",
                      cutoff_cor = 0.6, cutoff_p = 0.05)上
dim(bac_net)

library(tidyverse)
library(igraph)
c(as.character(bac_net$from), as.character(bac_net$to)) %>%
  as_tibble() %>%
  group_by(value) %>%
  summarize(n=n()) -> bac_nodes
colnames(bac_nodes) <- c("name", "degree")


bac_graph <- graph_from_data_frame(bac_net, vertices = bac_nodes, directed = FALSE )
bac_graph

####
bac_nodes2 <- calNetModule(data = bac_graph);bac_nodes2


####
bac_subNet <- extract_subNet(graph = bac_graph, data = bac, node = bac_nodes)
bac_subNet$sub_networkParameters
node<-bac_subNet$node
edge<-bac_subNet$edge

sample <- data.frame(bac_subNet$sub_networkParameters$sample)
node1 <- data.frame(bac_subNet$sub_networkParameters$node_number)
edge1 <- data.frame(bac_subNet$sub_networkParameters$network_edge_number)
Averagedegree <- data.frame(bac_subNet$sub_networkParameter$network_degree)
connectance <- data.frame(bac_subNet$sub_networkParameter$network_connectance)
dat <- data.frame(sample,node1,edge1,Averagedegree,connectance)

write.csv(dat,"result_aco2.csv")


###6
bac <- read.csv("tax_eco2.csv",header = T,sep= ",",row.names = 1)
bac <-data.frame(t(bac))
dim(bac)
bac_net <- calNetwork(data = bac,method = "spearman",
                      cutoff_cor = 0.6, cutoff_p = 0.05)上
dim(bac_net)

library(tidyverse)
library(igraph)
c(as.character(bac_net$from), as.character(bac_net$to)) %>%
  as_tibble() %>%
  group_by(value) %>%
  summarize(n=n()) -> bac_nodes
colnames(bac_nodes) <- c("name", "degree")


bac_graph <- graph_from_data_frame(bac_net, vertices = bac_nodes, directed = FALSE )
bac_graph

####
bac_nodes2 <- calNetModule(data = bac_graph);bac_nodes2


####
bac_subNet <- extract_subNet(graph = bac_graph, data = bac, node = bac_nodes)
bac_subNet$sub_networkParameters
node<-bac_subNet$node
edge<-bac_subNet$edge

sample <- data.frame(bac_subNet$sub_networkParameters$sample)
node1 <- data.frame(bac_subNet$sub_networkParameters$node_number)
edge1 <- data.frame(bac_subNet$sub_networkParameters$network_edge_number)
Averagedegree <- data.frame(bac_subNet$sub_networkParameter$network_degree)
connectance <- data.frame(bac_subNet$sub_networkParameter$network_connectance)
dat <- data.frame(sample,node1,edge1,Averagedegree,connectance)

write.csv(dat,"result_eco2.csv")

