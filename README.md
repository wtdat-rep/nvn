---
title: "Resilience of local railway networks - a network analytical approach"
author: "Richard Klimaczewski (wtdat)"
output:
  html_document:
    df_print: paged
  html_notebook: default
  pdf_document: default
---

# Preperations

```{r include=FALSE}
knitr::opts_chunk$set(message=FALSE, warning=FALSE)
```


```{r libraries, results='hide', echo=TRUE, message=FALSE, warning=FALSE}
library(tidyverse)
library(igraph)
library(readxl)
library(statnet)
library(kableExtra)
library(GGally)
library(RColorBrewer)
library(grid)
library(gridExtra)
library(gtable)
library(rlist)
library(ggpubr)
library(RGraphics)

```

```{r import, echo = FALSE, results='hide'}
input_nvn <- "014_Nahverkehrnetze/Nahverkehrsnetzwerke - V5.csv"
df.nvnSource <- read_csv2(input_nvn)
df.nvnSource <- df.nvnSource[df.nvnSource$Type %in% c("Tram", "S-Bahn", "U-Bahn") #Filter all RB, Seilbahnen, Zahnradbahnen etc.
                             & df.nvnSource$Service == 1,]

```

# Intro

The idea for this project came to my mind when I was about to be late for work. In order to get there, I have to transition from one tram line to the other at the main station. But the tram line I had to transition to did not arrive - and so did none other going in the same direction. There was some malfunction within the railway control center shutting down my tram and every other tram on that track. 

The thing with the city I work in is, that the railway network (for the tram/subway) has the shape of a star with one central hub station. The main station is right next to that central hub station, so every connection going north has to pass through the main station. The northbound tram/subway lines pass through the main station on two tracks - two lines per track. If there is something wrong with one of the tracks at the main station, it will effectively cut off two lines (and all their stops) from the rest of the network. Because of the structure of the railway network you can not simply take another route to your destination. There is none. Figure 1 illustrates this idea. If the red edge is severed, the nodes 2, 8, 9, and 10 are cut off from the rest of the network.

```{r pictures, echo=FALSE}
g1 <- network.initialize(14)
g1[1,2:5] <- 1
g1[4,11] <- 1
g1[5,6:7] <- 1
g1[2,8:9] <- 1
g1[9,10] <- 1
g1[3,12] <- 1
g1[12,13:14] <- 1

g2 <- network.initialize(14)
g2[1,2:4] <- 1
g2[5,6:8] <- 1
g2[2,c(5,9:10)] <- 1
#g2[6,15] <- 1
g2[3,c(4,13,6)] <- 1
g2[13,14] <- 1
g2[4,11:12] <- 1

par(mfrow = c(1,2))

gplot(g1, gmode = "graph", 
       edge.col = c("red", rep("grey",(network.edgecount(g1)-1))),
       vertex.col = "dark grey",
       displaylabels = TRUE,
       label.cex = .7,
       label.pos = 1,
       sub = "Figure 1: A vulnerable network."
       )

gplot(g2, gmode = "graph", 
       edge.col = c("red", rep("grey",(network.edgecount(g2)-1))),
       vertex.col = "dark grey",
       displaylabels = TRUE,
       label.cex = .7,
       label.pos = 1,
       sub = "Figure 2: A more resilient network."
       )

```

I could not get to work. Nobody could take a tram up north. None of the 3 lines on this route could enter the station. The platform was filling with people. As I was standing and waiting there on an increasingly crowded platform, the thought crossed my mind, that this would not have happened in my home town. Because of the way the railway network is laid out, the trains could have taken others routes between two points A and B - like it is shown in Figure 2. This network could have compensated for the loss of the red egde by diverting traffic over the path [1->3->6->5->2]. Of course there would be delays, but traffic could still flow around disturbances within the railway network, while the network from Figure 1 would remain seperated until the damage was undone. In other words, the railway network of my hometown seems to be more resillient than the one in which I was trying to reach my workplace.

# Good Spot for my Berlin Split anecdote

IDEA: Irgendwo am Anfang: Zeig die dt. Teilung mit dem Berliner Netz. Stelle das Foto mit der Straßenbeleuchtung daneben. Das wird cool. Verkaufe es als unerwarteten Nebeneffekt, der es ja auch war. 

# Research interest and specification

This is when I wondered how other railway networks would hold up. Which ones are more resillient then others? I wanted to compare them. My focus would be on the rails, so this project is not about public transportation networks in general. A lot of traffic is happening by other means like monorail, cable cars, cog railways, boats and ferries and of course by busses. I will intentionally miss out on all of them, so my results will have to be taken with a grain of salt. Let's say that I come to the conclusion that the network of a certain city is highly centralized, so that you cannot transition between terminal destinations without going back to a central hub, than this will be not accounted for bus traffic, which might in fact connect the terminal destinations. Even though people might be able to travel with ease between some points in reality, they will not be able to in my model without a connection by rails.

This decision is based on two reasons. First, I am interested in the resillience of networks. The vehicles need to be able to travel through the network. It is not realistic to assume that monorails, cable cars, boats, ferries and other vehicles can change to the rails trams/streetcars and subways use. Busses and other motor vehicles on the other hand can drive pretty much everywhere (except for [trolleybusses](https://en.wikipedia.org/wiki/Trolleybus)), so the analysis would turn out to be meaningless. Finally, the second and more practical reason is, that I want to limit the amount of data I need to gather in order to realize this project. I do not intend to simulate a whole cities traffic. I want to know how unlucky you can get on your way to work in certain German cities, if you depend railbound transportation.

There is an additional problem I need to address. There are three major types of vehicles on rails in German cities: trams, which are street cars, the U-Bahn, which is the subway/metro/tube and the S-Bahn. The S-Bahn is a German speciality, as I came to learn. It is a feature in many German city networks. Some of them connect to vast areas of urban hinterlands or even between cities. The Rhine-Ruhr region for example has a polycentric S-Bahn-network connecting over a dozen cities with each other. Including these connections would go beyond the scope of my research interest and increase the possibilty of absurd results. As a traveler within a cities network, you could find yourself still technically connected to your destination via a detour over several cities and a large distance, even though a critical edge has been severed. 

So, how to deal with the S-Bahn then? Technically the S-Bahn belongs to the national railyway company (the Deutsche Bahn). This certainly is an argument for not counting them in, but they need to be included. For many cities, e.g. Berlin or Hamburg, the local traffic networks would simply cease to function (and in large part: to exist) if one would remove the S-Bahn. In order to deal with this, I made the rule to only include connections, whose terminal stations lie within the network plan. If S-Bahn lines go beyond the edge of the network plan, I will consider them as interregional. This decision comes at a cost, but a compartively small one. Reality is messy.

A similar problem arose with regional trains (RB - "Regionalbahn"). My data source are network plans, for the most part. In many cases, regional trains are included in those, but they are not really part of the local network. Regional train lines usually just fade off at the edges of the network plan, so my rule seemed to be able to deal with them effortlessly. Or so I thought - as it turned out during the encoding of the data, there are some examples of regional trains being completely within local traffic networks. Luckily, there are just a few. I included them into my data set, but will exclude them for analysis, since they cause a myriad of problems.

# Analytical Approach

The local rail networks are conceptualized as graphs, consisting of nodes (the stations) and edges (the rails between them). I am interested in analyzing how well the different city networks deal with punctual stress, which I define as the loss of a single edge within the network. I decided against looking at the loss of a complete node, since this would constitute significantly heavier damage to the network. By focussing on an edge, I leave the possibility for an alternative route between its two adjacent nodes. Since I am interested in the fate of a hypothetical traveler within the network, this seems to be the more likely scenario.

In order to address this question, I can make some simplifications:

* I will assume an undirected graph. There are very little unidirectional edges in the networks I have encountered so far. Since they will comlicate things while adding not much insight, I have choosen to ignore them. They are however covered by a variable in my data set for later analysis.

* For this first version, I made the decision to treat the types of tranportation (Tram, U-Bahn and S-Bahn) equally. It is not realistic to assume, that the S-Bahn could just swap to the tracks of the Tram, but people might by walking from one to the other. 

The graphs will be tested by deleting an edge at a time and measuring if a) parts of the network are seperated, and if they are, b) how big these cut off parts are. In terms of network analysis, I am interested in the number of components and their sizes. The bigger the cut off parts are, the higher the damage suffered by the network will be. A coefficient will be calculated in order to measure the damage, by dividing the size of the cut off part by half the network size. Since all networks will experience at least some seperations (especially near the terminal stations, where stations usually are arranged like pearls on a necklace), I will calculate a second coefficient by multiplying the first one with the weight on an edge. This way, I compensate for the somewhat artificial cutoffs at the fringes of the network, since long stretching arms into the hinterlands of a city are usually only serviced by one or two lines. The weight of an edge is calculating the mean of the betweenness-centralities of the two adjacent nodes.

# The Data

The data are collected encoding the maps of local traffic networks. Where possible, I will webscrape this data, but in most cases it is encoded manually. The data are in form of an edgelist with some additional information. Each connection between two stations is encoded for each train line servicing between them. Although I will not use this information for this current project, this is done to achieve a greater depth of information while I am at it and to ensure some kind of failsave against errors (each line has to be complete in the end). My source data looks like this:

```{r show df.nvnSource, echo = FALSE}
# make it a nice table, that fits the page width
#knitr::kable(head(df.nvnSource)) %>% knitr::kable_styling(latex_options = "scale_down")
knitr::kable(head(df.nvnSource), format = "html") %>% kable_styling(full_width = FALSE)

```

My sample consists of German cities with railway networks that conform the the minimum size of 100.000 inhabitants, which is the requirement to be counted as a large city (ger. Großstadt). According to [this list](https://de.wikipedia.org/wiki/Liste_der_Gro%C3%9Fst%C3%A4dte_in_Deutschland) this encompasses 81 cities. Since data collection takes time, I decided to write this project with a smaller sample and to add the other cities as I encode them. As of this version, I have data on **`r sum(table(unique(df.nvnSource$City)))`** cities, which includes some big ones like Berlin and Munich. All code is written in a manner that it can deal with additional data effortlessly later on. Since I wrote the data myself, I do not have to worry about missing values, which is nice.

# Data wrangling

Even though I wrote the data myself, I cannot use them without performing some tranformations for which i wrote some functions. In my R-project they are saved in a seperate R-script file and run in a main function. In the following code chunks, I will introduce them. 

* splitting of the df + saving in list
* extract info on nodes, edges
* run transformation on the egdes


```{r split df}

# Determine distinct cities in the data set and save them in a vector.
nvn.makeSplitString <- function(df, splitCol = "City"){
       splitString <- unique(df[[splitCol]])
       return(splitString)
}

# Split data set by splitString and return a list.
nvn.splitdf <- function(df, splitString, splitCol = "City"){
       list.df <- list()
       for(i in seq_along(splitString)){
              name <- paste0("df.", splitString[i])
              df2 <- df[
                     df[[splitCol]] == splitString[i],
              ]
              name <- assign(name, df2)
              list.df[[i]] <- name
       }
       return(list.df)
}
```

Next I need to extract the information on the nodes and the edges. The pattern for this is always the same. Frist, I write a function that takes a data frame and extracts the information. Then I write a second function, which will apply the first one to the list objects in which I store my data frames. Let's get the nodes first.

```{r nodes}
# Take a data frame, get a list of all unique nodes and return them as a data frame.
nvn.getNodes <- function(df, ID.Source = ID.Source, ID.Target = ID.Target, Name.Source = Name.Source, Name.Target = Name.Target){
       nodes.sourceID <- c(df[[ID.Source]], df[[ID.Target]])
       nodes.sourceNames <- c(df[[Name.Source]], df[[Name.Target]])
       nodes.tempDf <- tibble(nodes.sourceID, nodes.sourceNames)
       df.nvnNodes <- distinct(nodes.tempDf, .keep_all = T)   
       names(df.nvnNodes)[1] <- "ID.Node"
       names(df.nvnNodes)[2] <- "Name.Node"
       return(df.nvnNodes)
}

# Applies nvn.getNodes to a list of data frames and returns a list of data frames. 
nvn.cityNodes <- function(cityList = "cityList", splitString, ID.Source = "ID.Source", ID.Target = "ID.Target", 
       Name.Source = "Name.Source", Name.Target = "Name.Target"){
       list.nodes <- list()
       for(i in seq_along(splitString)){
              df <- cityList[[i]]
              temp <- nvn.getNodes(df, ID.Source = ID.Source, ID.Target = ID.Target, 
                     Name.Source = Name.Source, Name.Target = Name.Target)
              list.nodes[[i]] <- temp
       }
       return(list.nodes)
}
```

The resulting data frames are a simple list of all nodes with their ID's and names. They look like this:

```{r show nodes, echo = FALSE}
splitString <- nvn.makeSplitString(df.nvnSource)
cityList <- nvn.splitdf(df.nvnSource, splitString)
list.nodes <- nvn.cityNodes(cityList, splitString)
knitr::kable(head(list.nodes[[1]]), format = "html") %>% kable_styling(full_width = FALSE)

```


Next, the edges.

```{r edges}
# Take a data frame, get all unique combinations of Source and Target, return them as a data frame 
# and keep all other information.
nvn.getEdges <- function(df, ID.Source = ID.Source, ID.Target = ID.Target){
       edges.spread <- df %>% 
                     select(ID.Source, ID.Target, everything())  

       df.nvnEdges <- edges.spread %>% 
                     group_by(ID.Source, ID.Target) %>% 
                     summarise(Weight = n())
       return(df.nvnEdges)
}

# Applies nvn.getEdges to a list of data frames and returns a list of data frames.
nvn.cityEdges <- function(cityList = "cityList", splitString, ID.Source = "ID.Source", ID.Target = "ID.Target"){
       list.edges <- list()
       for(i in seq_along(splitString)){
              df <- cityList[[i]]
              temp <- nvn.getEdges(df, ID.Source = ID.Source, ID.Target = ID.Target)
              list.edges[[i]] <- temp
       }
       return(list.edges)
}

```

The resulting data frames are edge list with weights. The weights correspond to the number of train lines servicing this edge. They look like this:

```{r show edges, echo = FALSE}
list.edges <- nvn.cityEdges(cityList, splitString)
knitr::kable(head(list.edges[[1]]), format = "html") %>% kable_styling(full_width = FALSE)
```


Next I have to do some wrangling with the list.edges, since it is not in the desired state at this stage. There are two problems with it: The first one is pretty straightforward. Since i encoded the data for each train line, there are a many redundant edges in the data set, which I need to filter. The second problem is related to the somewhat chaotic way the data was encoded. While dealing with the network plans, I usually folled the train lines through the network. This bears the possibility, that I encoded the same edge between two nodes A and B twice: once as [A,B] and once as [B,A]. I call these edges inverts. Inverts cannot be detected by looking for distinct edges, since they *are* distinct.

Both operations are necessary to ensure that the data set for analysis only contains only one edge per pair of nodes. If it does not, my results would be invalid, since the deletion of an edge (where there is an invert) would not result in a seperation of the network. 

```{r edge wrangling}
# Take a list of data frames, drop all non-distinct edges for each data frame and return a list of data frames.
nvn.distinctEdges <- function(list, ID.Source = "ID.Source", ID.Target = "ID.Target"){
       list2 <- list
       for(i in seq_along(list2)){
              list2[[i]] <- list[[i]] %>% distinct(ID.Source, ID.Target, .keep_all = TRUE)
       }
       return(list2)
}

# Take a data frame, detect inverts, remove them and return a data frame.
nvn.rmInverts <- function(df, col1, col2){
       df2 <- df
       df2$discard <- rep(FALSE, nrow(df2))
       for(i in 1:nrow(df2)){
              val1 <- df2[[i, col1]]
              val2 <- df2[[i, col2]]
              for(j in 1:nrow(df2)){
                     if((df2[[i, "discard"]] == FALSE) &
                                   (df2[[j,col2]] == val1) & (df2[[j, col1]] == val2)){
                            df2[j, "discard"] <- TRUE
                     }
              }
       }
       df2 <- df2[df2$discard == FALSE,]
       df2 <- df2[,colnames(df2) != "discard"]
       return(df2)
}

# Applies nvn.rmInverts to a list of data frames and returns a list of data frames.
nvn.rmInvertsOnList <- function(list, col1 = "ID.Source", col2 = "ID.Target"){
       list2 <- list
       for(i in seq_along(list2)){
              list2[[i]] <- nvn.rmInverts(list[[i]], col1 = col1, col2 = col2)
       }
       return(list2)
}

```

The next function calcultes an weight called **bCO** for each edge in a network. The coefficient is calculated as the mean of the edges two nodes. This weight is intended to accordingly refelct the importance of a particular edge for the whole network. Its value increases with the number of possible paths going through the edge. The second function simply applies the first to my list objects. 

```{r betweeness centrality coefficient}
# Take a data frame containing an edge list and calculate the betweenness-centrality of an edge as a mean.
nvn.betweennessEdges <- function(net, df.edges, node1 = node1, node2 = node2, rescale = FALSE){
       
       df.centrality <- data.frame(Names = network.vertex.names(net), val = betweenness(net, rescale = rescale))
       vec.out <- double()
       
       for(i in 1:nrow(df.edges)){
              src <- as.character(df.edges[i, node1])
              tar <- as.character(df.edges[i, node2])
              
              vec.out[i] <- (df.centrality[df.centrality$Names == src, 2] + df.centrality[df.centrality$Names == tar, 2])/2 
              
       }
       
       return(vec.out)
       
}

# Applies nvn.betweennessEdgesList to a list of data frames and returns a list of data frames.
nvn.betweennessEdgesList <- function(net, list, node1 = "ID.Source", node2 = "ID.Target", rescale = FALSE){
       list2 <- list
       for(i in seq_along(list2)){
              list2[[i]]$bCO <- nvn.betweennessEdges(net[[i]], list[[i]], node1 = node1, node2 = node2, rescale = rescale)
       }
       return(list2)
}
```


Lastly, I just need to import my data and run the functions just written. With the data in the right form, I can create the network objects needed for the analysis. This is done mapping the statnet::network function over the list.edges.undirected.  

```{r main data wrangling, results = 'hide'}
# Data import and data wrangling.
df.nvnSource <- read_csv2(input_nvn)
df.nvnSource <- df.nvnSource[df.nvnSource$Type %in% c("Tram", "S-Bahn", "U-Bahn") & df.nvnSource$Service == 1,]
splitString <- nvn.makeSplitString(df.nvnSource)
cityList <- nvn.splitdf(df.nvnSource, splitString)
list.nodes <- nvn.cityNodes(cityList, splitString)
list.edges <- nvn.cityEdges(cityList, splitString)
list.edges.undirected <- nvn.distinctEdges(list.edges)
list.edges.undirected <- nvn.rmInvertsOnList(list.edges.undirected)

# Creation of network-objects.
list.nets <- map(list.edges.undirected, network, directed = FALSE, matrix.type = "edgelist")

# Addition of betweeness-centrality based coefficient for edges.
list.edges.undirected <- nvn.betweennessEdgesList(list.nets, list.edges.undirected)
```


# Analyzing the networks

Bridges are edges that increase the number of components in a network when they are removed. Identifying the edges that are bridges within a local railway network means finding the networks weak points. Removing these edges damages the network by cutting off parts of it.  By measuring the size of the cut-off components, the damage suffered by the network can be quantified. I do this with two coefficients. The first coefficient compares the size of the cut-off components with the total network size divided by half. I will call this coefficient *CO*. Since I consider the smaller component as part that is cut-off from the rest of the network, the maximum size of the cut-off part equals half the network size. A value of 1 can be intepreted as the theoretical maximum damage inflicted to a network by the severance of a single edge, while 0 indicates that nothing was cut off. The advantage of the coefficient *CO* is, that it can be compared across different networks, but it is sensitive to cut-offs in the fringe areas of the network.

A second coefficient named *wCO* is introduced to deal with cut-offs at the fringes of the network by simply multiplying the first coefficient with an edge weight. This emphasizes the importance of central edges and decreases the (somewhat artificial) impact of the fringe areas, but comes at the cost of not having a fixed maximum value for this coefficient. The coefficient *wCO* cannot be compared across different networks, but it is useful to assess the damage to a particular network more precisely. Later on, when visualizing the networks, I am using the mean of *wCO* + *k\*standard deviations* for the color palette to indicate the most vulnurable edges of the networks.

```{r find bridges}
# Take a network object, delete an edge and check, if its number of components increases. If it does, measure the size of the cut-off.
nvn.bridgeImpact <- function(net){
       c.cnt <- components(net)
       c1.size <- rep(network.size(net), network.edgecount(net))
       c.rise <- rep(0, network.edgecount(net)) 
       c2.size <- rep(0, network.edgecount(net))  
       for(i in 1:network.edgecount(net)){
              net2 <- net
              net2 <- network::delete.edges(net2, i)
              c2.cnt <- sna::components(net2)
              if(c.cnt < c2.cnt){
                     c.rise[i] <- 1
                     comp <- component.dist(net2)
                     c2.size[i] <- min(comp$csize)
              }
       }
       cut.offRel <- c2.size/(c1.size*0.5) 
       df.result <- data.frame(c.rise, c1.size, c2.size, cut.offRel)
       return(df.result)
}

# Applies nvn.bridgeImpact to a list of data frames and returns a list of data frames.
nvn.getBridgeImpact <- function(list.edg, list.net){
     list2 <- list.edg
     for(i in seq_along(list2)){
          list2[[i]] <- cbind(as.data.frame(list2[[i]]), nvn.bridgeImpact(list.net[[i]]))
     }
     return(list2)
}

# Adds a calculated column to the data frames of list.results.
nvn.weightedCutOffs <- function(list, col1 = "Weight", col2 = "cut.offRel"){
       list2 <- list
       for(i in seq_along(list2)){
              list2[[i]]$weightedCutOff <- unlist(list2[[i]][,col1] * list2[[i]][,col2])
       }
       return(list2)
}
```

Now all the pieces are in place to run the main calculation. First, a data frame containing the data on the cut-offs for each network is created and saved into a list. In the next step the cut-offs are weighted. The resulting data frames for each network look like the example in the table below. 

```{r main analysis, eval=FALSE, results = 'hide'}
# Run nvn.bridgeImpact for all network objects and save the results
list.results <- nvn.getBridgeImpact(list.edges.undirected, list.nets)

# Calculate weighted coefficient by running nvn.weightedCutOffs for the results.
list.results <- nvn.weightedCutOffs(list.results, col1 = "bCO")

```

**CHANGE THE eval = ... between the two - run import for dev, but the real deal for publication**

```{r temp import, eval=TRUE}
nvn.importListResults <- function(input_dir = "014_Nahverkehrnetze/", input_file = "list.results[",
                                   num = 7, ending = "].csv"){
     list.results <- list()
     for(i in 1:num){
          input_string <- paste0(input_dir, input_file, i, ending)
          list.results[[i]] <- read_csv2(input_string, col_names = TRUE)
     }
     return(list.results)
}

list.results <- nvn.importListResults(num = length(unique(df.nvnSource$City)))

list.results <- nvn.weightedCutOffs(list.results, col1 = "bCO")

```


```{r show results data frame, echo = FALSE, results = 'hide'}
knitr::kable(head(list.results[[1]]), format = "html") %>% kable_styling(full_width = FALSE)
```

At this point, the main calculations are run and the question of the vulnurability of local railway networks could be answered. But that would be boring and ugly, at least at this point. In order to compare the cities networks, I want to do two things: firstly, the results will be shown in a simple table, since this is simply the most concise and comprehensive way to display things. Secondly, I want to show each network as a graph together with informations on the network and on its top n most critical edges. When I do this, I also want to provide an easy way of displaying the critical edges by coloring them. Additionally, I want to display the proper names of the stations, because they will not be recognisable by their ID's only. In short, some more coding is needed. Let's get to it.

# Preperation for presentation

All my data are currently saved in data frames within a list-object. I need to unnest them in order to compare them within one table. I wrote a helper-function to gather all the individual data frames into one table which is then aggregated on the city-level. The result will be a comprehensive table which is named *df.summary*, which displays the key measures for the cities railway networks. In a second step, I also round the values within *df.summary* and *list.results* for increased readability.


```{r gathering}
# Helper function for gathering data frames from list.results.

nvn.gatherResults <- function(list){
       for(i in seq_along(list)){
              if(i == 1){
                     df <- list[[i]]
              } else {
                     df <- rbind(df, list[[i]])
              }
       }
       return(df)
}

df.results <- nvn.gatherResults(list.results)

# Aggregation of measures per city.
df.summary <- df.results %>% 
       group_by(City) %>% 
       summarise(network.size = max(c1.size),
              c2.size_mean = mean(c2.size),
              c2.size_max = max(c2.size),
              CO_mean = mean(cut.offRel),
              CO_sd = sd(cut.offRel),
              CO_max = max(cut.offRel),
              wCO_mean = mean(weightedCutOff),
              wCO_sd = sd(weightedCutOff),
              wCO_max = max(weightedCutOff)) %>% 
       arrange(desc(wCO_mean)) %>% 
       add_column(. , Rank = 1:nrow(.), .before = "City")

# taken from https://stackoverflow.com/questions/29875914/rounding-values-in-a-dataframe-in-r
nvn.round_df <- function(x, digits) {
       numeric_columns <- sapply(x, mode) == 'numeric'
       x[numeric_columns] <-  round(x[numeric_columns], digits)
       x
}

df.summary <- nvn.round_df(df.summary, 4)

list.results <- lapply(list.results, nvn.round_df, 4)

```

With this code, the table for the results is ready. It will be shown in the next chapter along the other results. 

The preperations for the display of the network graph with additional information are a bit more extensive. Since this will be done for each individual network, the following functions are designed to work with my list-objects. They will loop through them and gather information on the top n edges and on the real world names of the stations. Also, I need some code for the visualization of the graph and its colorization. In the end, I am putting all the functions in a loop for procedural display of the results for all networks contained in my list-objects. This might not be the most elegant approach, but it gets the job done and makes the addition of new networks effortless.  

The following function defines the colors for the most vulnurable edges of a network. I decided to base this decision on the distribution of the weight based on betweeness-centrality of the two nodes of a given edge. The coloring scheme encompases 7 steps of colors ranging from yellow to dark red. Each step has the width of one standard deviation. Since the coloring shall show only vulnurabilites, every edge below the mean of the distribution will be simply grey. This way it should be obvious to the eye where the problematic edges lie in the network. The drawback to this solution is, that even the most robust networks will have some edges in warning colors. 

```{r color gradient}
nvn.makeColGradient <- function(vec){  
       vec2 <- rep("gray73", length(vec))
       for (i in seq_along(vec)){
              if (vec[i] >= mean(vec) & vec[i] < sd(vec)){
                     vec2[i] <- "yellow2"
              } else if (vec[i] >= sd(vec) & vec[i] < sd(vec)*2){
                     vec2[i] <- "orange1"
              } else if (vec[i] >= sd(vec)*2 &
                            vec[i] < sd(vec)*3){
                     vec2[i] <- "orange3"
              } else if (vec[i] >= sd(vec)*3 &
                            vec[i] < sd(vec)*4) {
                     vec2[i] <- "orangered1"
              } else if (vec[i] >= sd(vec)*4 &
                            vec[i] < sd(vec)*5) {
                     vec2[i] <- "red1"
              } else if (vec[i] >= sd(vec)*5 &
                            vec[i] < sd(vec)*6) {
                     vec2[i] <- "red3"
              } else if (vec[i] >= sd(vec)*6 ){
                     vec2[i] <- "red4"
              }
       }
       return(vec2)
}
```

Next, I wrote two simple functions that look up the top n edges in regard to the two coefficients *CO* and *wCO*. 

```{r top edges for the networks}
nvn.topNcutOffs1 <- function(list, listindex, n, col1 = "ID.Source", col2 = "ID.Target", col3 = "cut.offRel"){
       df <- list[[listindex]]
       df <- df[order(df[,col3], decreasing = TRUE),]
       df2 <- df[1:n, c(col1, col2, col3)]
       return(df2)
}

nvn.topNcutOffs2 <- function(list, listindex, n, col1 = "ID.Source", col2 = "ID.Target", col3 = "weightedCutOff"){
       df <- list[[listindex]]
       df <- df[order(df[,col3], decreasing = TRUE),]
       df2 <- df[1:n, c(col1, col2, col3)]
       return(df2)
}
```

The nodes in my network objects are identified by an ID, which is incomprehensible to the reader (or me). I intend to show some of the connections in the network explicitly. In order to provide a real-world reference, I neet a function for looking up the names of the nodes. I included 3 modes for displaying the information: "add" - get station names as an extra column; "replace" - get station names instead of the ID's; "concat" - display ID and station name together. 

```{r look up of station names}
## nvn.lookUpNames
nvn.lookUpNames <- function(df, listindex, mode = "add", trunc_by = 20){
       
       df2 <- list.nodes[[listindex]]
       
       if(mode == "add"){
              df <- left_join(df, df2, by = c("ID.Source" = "ID.Node"))
              colnames(df)[ncol(df)] <- "Name.Source"
              
              df <- left_join(df, df2, by = c("ID.Target" = "ID.Node"))
              colnames(df)[ncol(df)] <- "Name.Target"
              
              df <- df[,c(1,4,2,5,3)]
              df[,c(1,4,2,5,3)] <- map(df[,c(1,4,2,5,3)], str_trunc, trunc_by, side = "center", ellipsis = "...")
              
       } else if (mode == "replace"){
              df <- left_join(df, df2,  by = c("ID.Source" = "ID.Node"))
              colnames(df)[ncol(df)] <- "Name.Source"
              
              df <- left_join(df, df2, by = c("ID.Target" = "ID.Node"))
              colnames(df)[ncol(df)] <- "Name.Target"  
              
              df <- df[,3:ncol(df)]
              df <- df[,c(2,3,1)]
              df[,c(2,3,1)] <- map(df[,c(2,3,1)], str_trunc, trunc_by, side = "center", ellipsis = "...")
              
       } else if (mode == "concat"){
              df <- left_join(df, df2,  by = c("ID.Source" = "ID.Node"))
              df$Source <- str_c(df[["ID.Source"]], df[["Name.Node"]], sep = " ")
              df <- df[,-(ncol(df)-1)]

              df <- left_join(df, df2, by = c("ID.Target" = "ID.Node"))
              df$Target <- str_c(df[["ID.Target"]], df[["Name.Node"]], sep = " ")

              df <- df[,c(3,4,6)]
              df <- df[,c(2,3,1)]
              df[,c(2,3,1)] <- map(df[,c(2,3,1)], str_trunc, trunc_by, side = "center", ellipsis = "...")
       }

       return(df)
}
```

As a last prearrangement, I wrote two functions for sorting my list-objects. Since the sorting shall be a ranking that reflects the vulnurabilitsy of the networks, it can only be implemented after results are known. At this point, the table *df.summary* is already sorted by the coefficient *wCO*, while the list-objects are still in their original order. The two functions below create a sort index based on *df.summary* and apply it to all relevant objects. As a result, the networks can now be presented from the worst to be best (in terms of vulnurability).

```{r - sloppy pasted code}
## nvn.sortIndex
nvn.sortIndex <- function(list, df){
       oldList <- integer()
       newList <- integer()
       for(i in seq_along(list)){
             oldList <- c(oldList, i)
             
             temp <- unique((list[[i]]$City))
             index <- df[df$City == temp, 1]
             
             newList <- c(newList, as.integer(index))
       }
       df2 <- data.frame(newList, oldList)
       return(df2)
}

##nvn.sortList
nvn.sortList <- function(list, df.sort){
       list2 <- list
       for(i in seq_along(list)){
              list2[[df.sort[i,1]]] <- list[[i]]
       }
       return(list2)
}

sort_by <- nvn.sortIndex(list.results, df.summary)

list.results <- nvn.sortList(list.results, sort_by)
list.nets <- nvn.sortList(list.nets, sort_by)
list.nodes <- nvn.sortList(list.nodes, sort_by)
list.edges <- nvn.sortList(list.edges, sort_by)
list.edges.undirected <- nvn.sortList(list.edges.undirected, sort_by)
```

Now, everything is in place for the presentation of the results for each network in my data set.

Describe your stuff below


```{r network summary code display, eval=FALSE}
for(i in seq_along(list.results)){
       
       
       # Step 1: Create network graph.
       
       g <- list.nets[[i]]
       g.nodeLabels <- list.nodes[[i]]
       g.nodeLabels <- dplyr::arrange(g.nodeLabels, ID.Node)
       g.edgeAttr <- list.results[[i]]
       g.edgeAttr$col <- nvn.makeColGradient(g.edgeAttr$weightedCutOff)
       g.edgeAttr$size <- ifelse(g.edgeAttr$col == "gray83", 1, 1.5)
       g.edgeAttr$size <- ifelse(g.edgeAttr$col == "gray83", .5, .8)
       g1 <- ggnet2(g, node.color = "gray49", node.size = .5, edge.color = g.edgeAttr$col, 
              edge.size =   g.edgeAttr$size, mode = "kamadakawai", layout.par = list(niter = 5000)) +
              ggtitle(paste0("Rank ", which(df.summary$City == unique(g.edgeAttr$City)), ": ", g.edgeAttr$City)) +
              theme(plot.title = element_text(size = 20, face = "bold"))
       
       
       # Step 2: Create two tables with the top n network edges with regard to each of the two coefficients (CO/wCO).
       
       listindex <- i
       t.padding <- 36
       tbl1 <- nvn.topNcutOffs1(list.results, listindex, n = 5)
       tbl2 <- nvn.topNcutOffs2(list.results, listindex, n = 5)
       tbl1 <- nvn.lookUpNames(tbl1, listindex, mode = "replace")
       tbl2 <- nvn.lookUpNames(tbl2, listindex, mode = "replace")
       colnames(tbl1) <- c(str_pad("Source", t.padding, side = "both", pad = " "), 
              str_pad("Target", t.padding, side = "both", pad = " "), 
              str_pad("CO", t.padding/4, side = "right", pad = " "))
       colnames(tbl2) <- c(str_pad("Source", t.padding, side = "both", pad = " "), 
              str_pad("Target", t.padding, side = "both", pad = " "), 
              str_pad("wCO", t.padding/4, side = "right", pad = " "))
       
       tbl1 <- ggtexttable(tbl1, rows = NULL, theme = ttheme(
              colnames.style = colnames_style(color = "black", fill = "#ffffff", linewidth = 1, linecolor = "black"),
              tbody.style = tbody_style(color = "black",
                     fill = c("#dcdcdc", "#ffffff"), hjust=0, x=0.05)
       ))
       
       tbl2 <- ggtexttable(tbl2, rows = NULL, theme = ttheme(
              colnames.style = colnames_style(color = "black", fill = "#ffffff", linewidth = 1, linecolor = "black"),
              tbody.style = tbody_style(color = "black",
                     fill = c("#dcdcdc", "#ffffff"), hjust=0, x=0.05)
       ))
       
       
       # Step 3: Create six graphical objects to highlight measures.
       
       pos <- .5
       dim <- .85
       cap.cex <- 1
       num.cex <- 1.3
       vjust1 <- -1.35
       vjust2 <- 1
       col.fill <- "#dcdcdc"
       
       tile1 <- grobTree(rectGrob(pos,pos,dim,dim, gp = gpar(fill = col.fill)),
              textGrob("Network Size", vjust = vjust1, gp = gpar(cex = cap.cex, fontface = "bold")),
              textGrob(df.summary[listindex,3], vjust = vjust2, gp = gpar(cex = num.cex)))
       tile2 <- grobTree(rectGrob(pos,pos,dim,dim, gp = gpar(fill = col.fill)),
              textGrob("Max cut-off", vjust = vjust1, gp = gpar(cex = cap.cex, fontface = "bold")),
              textGrob(df.summary[listindex,5], vjust = vjust2, gp = gpar(cex = num.cex)))
       tile3 <- grobTree(rectGrob(pos,pos,dim,dim, gp = gpar(fill = col.fill)),
              textGrob("CO max", vjust = vjust1, gp = gpar(cex = cap.cex, fontface = "bold")),
              textGrob(df.summary[listindex,8], vjust = vjust2, gp = gpar(cex = num.cex)))
       tile4 <- grobTree(rectGrob(pos,pos,dim,dim, gp = gpar(fill = col.fill)),
              textGrob("CO mean", vjust = vjust1, gp = gpar(cex = cap.cex, fontface = "bold")),
              textGrob(df.summary[listindex,6], vjust = vjust2, gp = gpar(cex = num.cex)))
       tile5 <- grobTree(rectGrob(pos,pos,dim,dim, gp = gpar(fill = col.fill)),
              textGrob("wCO max", vjust = vjust1, gp = gpar(cex = cap.cex, fontface = "bold")),
              textGrob(df.summary[listindex,11], vjust = vjust2, gp = gpar(cex = num.cex)))
       tile6 <- grobTree(rectGrob(pos,pos,dim,dim, gp = gpar(fill = col.fill)),
              textGrob("wCO mean", vjust = vjust1, gp = gpar(cex = cap.cex, fontface = "bold")),
              textGrob(df.summary[listindex,9], vjust = vjust2, gp = gpar(cex = num.cex)))
       
       
       # Step 4: Pull it all together in one graphical object.
       
       spacer <- textGrob(" ")
       
       print(ggarrange(spacer,
              ggarrange(g1,
                     ggarrange(
                            ggarrange(
                                   tile1, tile3, tile5,  tile2, tile4, tile6, ncol = 3, nrow = 2), tbl1, tbl2,
                            ncol = 1, nrow = 3),
                     ncol = 2, widths = c(2,1.5)
              ), nrow = 2, heights = c(1, 30)
       )
       )
       
       
}
```

*before you run it and show the results, explain the output with an example. explain everything the reader can see
Show only 1 example here. Show the full thing in the next chapter!*

The results will be shown in the next chapter in tabular form and in the detailed view, which the code above produces for each network. But before we start, I want to demonstrate with one example, what we are about to see. Let's take a look at the city of Hannover, which is displayed below.

```{r - network display (example), echo=FALSE, fig.width = 12}
# This works only, if the lists have been sorted previously (which should have happened at this point).
for(i in which(df.summary$City == 'Hannover')){
       
       g <- list.nets[[i]]
       g.nodeLabels <- list.nodes[[i]]
       g.nodeLabels <- dplyr::arrange(g.nodeLabels, ID.Node)
       g.edgeAttr <- list.results[[i]]
       g.edgeAttr$col <- nvn.makeColGradient(g.edgeAttr$weightedCutOff)
       g.edgeAttr$size <- ifelse(g.edgeAttr$col == "gray83", 1, 1.5)
       g.edgeAttr$size <- ifelse(g.edgeAttr$col == "gray83", .5, .8)
       g1 <- ggnet2(g, node.color = "gray49", node.size = .5, edge.color = g.edgeAttr$col, 
              edge.size =   g.edgeAttr$size, mode = "kamadakawai", layout.par = list(niter = 5000)) +
              ggtitle(paste0("Rank ", which(df.summary$City == unique(g.edgeAttr$City)), ": ", g.edgeAttr$City)) +
              theme(plot.title = element_text(size = 20, face = "bold"))
       
       listindex <- i
       t.padding <- 36
       tbl1 <- nvn.topNcutOffs1(list.results, listindex, n = 5)
       tbl2 <- nvn.topNcutOffs2(list.results, listindex, n = 5)
       tbl1 <- nvn.lookUpNames(tbl1, listindex, mode = "replace")
       tbl2 <- nvn.lookUpNames(tbl2, listindex, mode = "replace")
       colnames(tbl1) <- c(str_pad("Source", t.padding, side = "both", pad = " "), 
              str_pad("Target", t.padding, side = "both", pad = " "), 
              str_pad("CO", t.padding/4, side = "right", pad = " "))
       colnames(tbl2) <- c(str_pad("Source", t.padding, side = "both", pad = " "), 
              str_pad("Target", t.padding, side = "both", pad = " "), 
              str_pad("wCO", t.padding/4, side = "right", pad = " "))
       
       tbl1 <- ggtexttable(tbl1, rows = NULL, theme = ttheme(
              colnames.style = colnames_style(color = "black", fill = "#ffffff", linewidth = 1, linecolor = "black"),
              tbody.style = tbody_style(color = "black",
                     fill = c("#dcdcdc", "#ffffff"), hjust=0, x=0.05)
       ))
       
       tbl2 <- ggtexttable(tbl2, rows = NULL, theme = ttheme(
              colnames.style = colnames_style(color = "black", fill = "#ffffff", linewidth = 1, linecolor = "black"),
              tbody.style = tbody_style(color = "black",
                     fill = c("#dcdcdc", "#ffffff"), hjust=0, x=0.05)
       ))
       
       
       pos <- .5
       dim <- .85
       cap.cex <- 1
       num.cex <- 1.3
       vjust1 <- -1.35
       vjust2 <- 1
       col.fill <- "#dcdcdc"
       
       tile1 <- grobTree(rectGrob(pos,pos,dim,dim, gp = gpar(fill = col.fill)),
              textGrob("Network Size", vjust = vjust1, gp = gpar(cex = cap.cex, fontface = "bold")),
              textGrob(df.summary[listindex,3], vjust = vjust2, gp = gpar(cex = num.cex)))
       tile2 <- grobTree(rectGrob(pos,pos,dim,dim, gp = gpar(fill = col.fill)),
              textGrob("Max cut-off", vjust = vjust1, gp = gpar(cex = cap.cex, fontface = "bold")),
              textGrob(df.summary[listindex,5], vjust = vjust2, gp = gpar(cex = num.cex)))
       tile3 <- grobTree(rectGrob(pos,pos,dim,dim, gp = gpar(fill = col.fill)),
              textGrob("CO max", vjust = vjust1, gp = gpar(cex = cap.cex, fontface = "bold")),
              textGrob(df.summary[listindex,8], vjust = vjust2, gp = gpar(cex = num.cex)))
       tile4 <- grobTree(rectGrob(pos,pos,dim,dim, gp = gpar(fill = col.fill)),
              textGrob("CO mean", vjust = vjust1, gp = gpar(cex = cap.cex, fontface = "bold")),
              textGrob(df.summary[listindex,6], vjust = vjust2, gp = gpar(cex = num.cex)))
       tile5 <- grobTree(rectGrob(pos,pos,dim,dim, gp = gpar(fill = col.fill)),
              textGrob("wCO max", vjust = vjust1, gp = gpar(cex = cap.cex, fontface = "bold")),
              textGrob(df.summary[listindex,11], vjust = vjust2, gp = gpar(cex = num.cex)))
       tile6 <- grobTree(rectGrob(pos,pos,dim,dim, gp = gpar(fill = col.fill)),
              textGrob("wCO mean", vjust = vjust1, gp = gpar(cex = cap.cex, fontface = "bold")),
              textGrob(df.summary[listindex,9], vjust = vjust2, gp = gpar(cex = num.cex)))
       
       
       spacer <- textGrob(" ")
       
       print(ggarrange(spacer,
              ggarrange(g1,
                     ggarrange(
                            ggarrange(
                                   tile1, tile3, tile5,  tile2, tile4, tile6, ncol = 3, nrow = 2), tbl1, tbl2,
                            ncol = 1, nrow = 3),
                     ncol = 2, widths = c(2,1.5)
              ), nrow = 2, heights = c(1, 30)
       )
       )
       
       
}
```

What can we see here?

1. **The Caption** (top left corner): The name of the city and it's rank according to the coefficient *wCO* is displayed. The higher the rank, the higher the vulnurability of the railway network of the city.
2. **The Network Graph** (left side): The railway network of the city as a network graph. Nodes (stations) are respresented by dots, railways between the stations as lines between the dots. ID's and names of the stations are not displayed to avoid clutter. The connections with the highest vulnurability are colored.
       a) As a rule of thumb it can be said, that colored edges in the center of the network are more problematic than on its periphery. Severed parts of the network become practically unavoidable closer to the fringe. In the case of Hannover, the most vulnurable parts of the network are at its very center and around it. This is bad, since this drastically decreases the networks ability to cope with stress.
       b) Note that only connections between the nodes (the dots in the picture) count. Edges, that are crossing other edges, are meaningless. They are just a random by-product of the network vizualization algorithm.
3. **The Tiles** (top right corner): The tiles highlight several measures key to understanding the network.
       a) *Network Size* is the count of nodes (stations) in the network. City size may play a role, so I display this measure for comparisson.
       b) *Max cut-off* shows the absolute number of nodes that are cut off from the rest of the network, when the most vulnurable edge is interrupted. In the case of Hannover up to 71 stations can be cut off from the network if a single connection is severed.
       c) *CO max* is the largest value of the first coefficient for this network.
              - As a reminder: *CO* is calculated for each edge by: $$ \text{Number of seperated Nodes}/(\frac{\text{Total Number of Nodes}}{2})$$ The coefficient *CO* can attain values ranging from 0 to 1. A value of 1 means, that the network can be split into two halfs by only one edge; a value of 0 means no nodes are split off when the edge in question is interrupted. *CO* can be compared across networks.
              - In the case of Hannover, the highest value of *CO* equals `r as.numeric(df.summary[df.summary$City == 'Hannover',8])`, which is a very high value. A look at *Network Size* confirms, that a substantial part (`r as.numeric(df.summary[df.summary$City == 'Hannover',5])` out of `r as.numeric(df.summary[df.summary$City == 'Hannover',3])` nodes) of the railway network can be cut off by a single edge.
       d) *CO mean* displays the average value of *CO* for the network. A higher value can indicate a higher number of problematic edges.
       e) *wCO max* is the largest value for the second coefficient for this network.
              - As a reminder: *wCO* is calculated by multiplying *CO* with the measure *bCO*, which is based on the in-betweenness centrality of the two nodes for a given edge. *wCO* is more sensitive to the interruption of important edges that to interruptions in the outer parts of the network. A higher value indicates central vulnurability. Note that that values of *wCO* are pretty low in general and that *wCO* has no fixed value range like *CO* has.
              - In the case of Hannover, the value of `r as.numeric(df.summary[df.summary$City == 'Hannover',11])` is among the highest in the data set and indicates central vulnurability of the network.
       f) Finally, *wCO mean* displays the average value of *wCO* for the network. A higher value can indicate a higher number of problematic edges.
4. **The Tables** (right side): In this area I show the top 5 edges for the two coefficients. The edges are represented by the two stations of the edge. "Source" and "Target" are just labels and can be interchanged. The most vulnurable edges are mostly identical for both coefficients but there are differences. Keep in mind, that *CO* is only sensible to the raw number of cut-off edges, while *wCO* factors in the centrality of the edge.

This dashboard-style is the format I have chosen for the presentation of the results. Basically, the code above puts it all together as a single picture, so that the same kind of dashboard-like can be generated procedurally. At the time of writing this, I still have to gather data on a number of networks, My approach allows it, that I can add them later on by not much more than a button-press.  

# Presentation

here show the df.summary and the stream of network graphs

```{r - network display (the real deal), echo=FALSE, fig.width = 12}
for(i in seq_along(list.results)){
       
       g <- list.nets[[i]]
       g.nodeLabels <- list.nodes[[i]]
       g.nodeLabels <- dplyr::arrange(g.nodeLabels, ID.Node)
       g.edgeAttr <- list.results[[i]]
       g.edgeAttr$col <- nvn.makeColGradient(g.edgeAttr$weightedCutOff)
       g.edgeAttr$size <- ifelse(g.edgeAttr$col == "gray83", 1, 1.5)
       g.edgeAttr$size <- ifelse(g.edgeAttr$col == "gray83", .5, .8)
       g1 <- ggnet2(g, node.color = "gray49", node.size = .5, edge.color = g.edgeAttr$col, 
              edge.size =   g.edgeAttr$size, mode = "kamadakawai", layout.par = list(niter = 5000)) +
              ggtitle(paste0("Rank ", which(df.summary$City == unique(g.edgeAttr$City)), ": ", g.edgeAttr$City)) +
              theme(plot.title = element_text(size = 20, face = "bold"))
       
       listindex <- i
       t.padding <- 36
       tbl1 <- nvn.topNcutOffs1(list.results, listindex, n = 5)
       tbl2 <- nvn.topNcutOffs2(list.results, listindex, n = 5)
       tbl1 <- nvn.lookUpNames(tbl1, listindex, mode = "replace")
       tbl2 <- nvn.lookUpNames(tbl2, listindex, mode = "replace")
       colnames(tbl1) <- c(str_pad("Source", t.padding, side = "both", pad = " "), 
              str_pad("Target", t.padding, side = "both", pad = " "), 
              str_pad("CO", t.padding/4, side = "right", pad = " "))
       colnames(tbl2) <- c(str_pad("Source", t.padding, side = "both", pad = " "), 
              str_pad("Target", t.padding, side = "both", pad = " "), 
              str_pad("wCO", t.padding/4, side = "right", pad = " "))
       
       tbl1 <- ggtexttable(tbl1, rows = NULL, theme = ttheme(
              colnames.style = colnames_style(color = "black", fill = "#ffffff", linewidth = 1, linecolor = "black"),
              tbody.style = tbody_style(color = "black",
                     fill = c("#dcdcdc", "#ffffff"), hjust=0, x=0.05)
       ))
       
       tbl2 <- ggtexttable(tbl2, rows = NULL, theme = ttheme(
              colnames.style = colnames_style(color = "black", fill = "#ffffff", linewidth = 1, linecolor = "black"),
              tbody.style = tbody_style(color = "black",
                     fill = c("#dcdcdc", "#ffffff"), hjust=0, x=0.05)
       ))
       
       
       pos <- .5
       dim <- .85
       cap.cex <- 1
       num.cex <- 1.3
       vjust1 <- -1.35
       vjust2 <- 1
       col.fill <- "#dcdcdc"
       
       tile1 <- grobTree(rectGrob(pos,pos,dim,dim, gp = gpar(fill = col.fill)),
              textGrob("Network Size", vjust = vjust1, gp = gpar(cex = cap.cex, fontface = "bold")),
              textGrob(df.summary[listindex,3], vjust = vjust2, gp = gpar(cex = num.cex)))
       tile2 <- grobTree(rectGrob(pos,pos,dim,dim, gp = gpar(fill = col.fill)),
              textGrob("Max cut-off", vjust = vjust1, gp = gpar(cex = cap.cex, fontface = "bold")),
              textGrob(df.summary[listindex,5], vjust = vjust2, gp = gpar(cex = num.cex)))
       tile3 <- grobTree(rectGrob(pos,pos,dim,dim, gp = gpar(fill = col.fill)),
              textGrob("CO max", vjust = vjust1, gp = gpar(cex = cap.cex, fontface = "bold")),
              textGrob(df.summary[listindex,8], vjust = vjust2, gp = gpar(cex = num.cex)))
       tile4 <- grobTree(rectGrob(pos,pos,dim,dim, gp = gpar(fill = col.fill)),
              textGrob("CO mean", vjust = vjust1, gp = gpar(cex = cap.cex, fontface = "bold")),
              textGrob(df.summary[listindex,6], vjust = vjust2, gp = gpar(cex = num.cex)))
       tile5 <- grobTree(rectGrob(pos,pos,dim,dim, gp = gpar(fill = col.fill)),
              textGrob("wCO max", vjust = vjust1, gp = gpar(cex = cap.cex, fontface = "bold")),
              textGrob(df.summary[listindex,11], vjust = vjust2, gp = gpar(cex = num.cex)))
       tile6 <- grobTree(rectGrob(pos,pos,dim,dim, gp = gpar(fill = col.fill)),
              textGrob("wCO mean", vjust = vjust1, gp = gpar(cex = cap.cex, fontface = "bold")),
              textGrob(df.summary[listindex,9], vjust = vjust2, gp = gpar(cex = num.cex)))
       
       
       spacer <- textGrob(" ")
       
       print(ggarrange(spacer,
              ggarrange(g1,
                     ggarrange(
                            ggarrange(
                                   tile1, tile3, tile5,  tile2, tile4, tile6, ncol = 3, nrow = 2), tbl1, tbl2,
                            ncol = 1, nrow = 3),
                     ncol = 2, widths = c(2,1.5)
              ), nrow = 2, heights = c(1, 30)
       )
       )
       
       
}
```

# Critical Evaluation

Look critically at your findings. 
-> Network size / city size vs. high coefficients.
       - scatterplot: network size vs. coefficient
       - rangkorrelation: network size vs. coefficient








