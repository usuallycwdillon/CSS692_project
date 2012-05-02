#
# Status: 28 April ... 
# 

################################################################################
#
# Purpose of this R source file is to maintain a record version of the steps 
# ...well, the successful ones, anyway... to build 'network', 'sna', and 'igraph'
# data for analysis of GitHub users and projects with an aim to make successful
# recommendations of projects to which users might make to make contributions as
# part of our work for Maksim Tsvetovat's GMU CSS692 Social Network Analysis 
# course, Spring 2012 semester project. 
#
# This work is being collaboratively developed with John Nelson, Ge Yan, and 
# Clarence Dillon--students of social complexity at the Center for Social 
# Complexity at GMU's Krasnow Institute for Advanced Research 
#
################################################################################
#
# After downloading and unzipping the data files into a working directory these 
# steps import the data into R as dataframes and build a network graph with
# attributes for further analysis. We start R (pref, R Studio) and establish an
# appropriate workspace (with version control, since many are contributing).
save.image("~/R/CSS692_workspace.RData")  #This creates an empty file, like 'touch'

# Some of the attributes are stored as text (true/false) but it's easier
# for me in R if these values are stored numerically as 0-false, 1-true.
# This is a quick conversion function that can be called anywhere once loaded.
# Functions must be defined and called up-front so that that they exist!
tfbinary <- function(a) {
  if ((a == 'true')||(a == 'TRUE')) return (1)
  else return(0)  
}

# Next, we want to build graphs of users (uG) and repos (rG) but, we must first
# open the library and bring in the igraph and sna packages
# install.packages("sna") # Uncomment this if you need to install sna first
# install.packages("igraph") # Uncomment this if you need to install igraph first
# library("sna") # not using sna, it turns out but I'm not ready to let it go yet
library("igraph")

# Some data file commented to save memory; but preserved for future analysis
# user_contrib2repo     <- read.delim("~/R/data/user_contributes_to_repository.tsv")
repo_meta             <- read.delim("~/R/data/repo_meta.tsv")
# user_meta             <- read.delim("~/R/data/user_meta.tsv")
# user_owns_repo        <- read.delim("~/R/data/user_owns_repository.tsv")
# user_watches_repo     <- read.delim("~/R/data/user_watches_repository.tsv")
contrib_linked_repos  <- read.delim("~/R/data/contributor_linked_repositories.tsv")
owner_linked_repos    <- read.delim("~/R/data/owner_linked_repositories.tsv")
watch_linked_repos    <- read.delim("~/R/data/watch_linked_repositories.tsv")

# The sna package is picky about the names of the columns, so might as well change
# them now before we get used to the data column names.
colnames(watch_linked_repos) <- c("source", "target", "watchWeight")
colnames(owner_linked_repos) <- c("source", "target", "ownWeight")
colnames(contrib_linked_repos) <- c("source", "target", "contribWeight")

# Prepare for some data manipulation; don't want to lose the original data
# This is where the True-False Binary translation function gets used. 
# Observed. RAM use dropped from 6.6GiB to 4.8GiB upon completion of this code
repo_copy <- repo_meta
repo_copy$hasWiki <- sapply(repo_meta$Has.Wiki, tfbinary)
repo_copy$hasIssues <- sapply(repo_meta$Has.Issues, tfbinary)
repo_copy$hasDownloads <- sapply(repo_meta$Has.Downloads, tfbinary)
repo_copy$isFork <- sapply(repo_meta$Is.Fork, tfbinary)
repo_metaB <- subset(repo_copy, select= -c(Has.Wiki, Has.Issues, Has.Downloads, Is.Fork))
rm(repo_copy) # make that memory available again

# This works well in Rstudio because of its tabbed interface; maybe not in R 
# alone. Very important to comment these out to match the 
# View(user_contrib2repo)
View(repo_meta)
# View(user_meta)
# View(user_owns_repo)
# View(user_watches_repo)
View(contrib_linked_repos)
View(owner_linked_repos)
View(watch_linked_repos)

# That's a bunch of data. Save the workspace. This time it has data. This step
# is time-consuming (there's a lot of data to write into that file) but safe.
save.image("~/R/CSS692_workspace.RData")

# Next, we actually create some graphs in igraph. I found the scripts to filter 
# subgraphs overly complicated so, I'm filtering the dataframes into smaller 
# data frames and making multiple graphs in the script further down. This way is
# memory intensive but allows to compare graphs; see the impact of filtering.
# Warning: by memory intensive, I mean nearly 10GiB RAM during the merges!
m <- merge(watch_linked_repos, owner_linked_repos, by=c("source", "target"))
mm <- merge(m, contrib_linked_repos, by=c("source", "target"))
# Now a graph that has at least 5 watchers and 2 contributors
mm.sub5w2c <- subset(mm, watchWeight > 5 & contribWeight > 2) 
# Now a graph that has at least 5 watchers and 5 contributors
mm.sub5w5c <- subset(mm, watchWeight > 5 & contribWeight > 5)
# These graphs have a minimum of 5 watchers & contribs and no more than 500 watchers
mm.sub5w5c5C <- subset(mm, (watchWeight > 5 & contribWeight > 5) & watchWeight < 500)
# ...and no more than 500 watchers or contributors (stupid robots!)
mm.sub5w5C5c5C <- subset(mm, (watchWeight > 5 & contribWeight > 5) & 
  (watchWeight < 500 & contribWeight < 500))
# Here's a smaller graph of medium project sizes
mm.sub5w1C5c1C <- subset(mm, (watchWeight > 5 & contribWeight > 5) & 
  (watchWeight < 100 & contribWeight < 100))
# Here's a smaller graph of smaller sizes
mm.sub5w2X5c2X <- subset(mm, (watchWeight > 5 & contribWeight > 5) & 
  (watchWeight < 20 & contribWeight < 20))

# To save some memory we can drop those *_linked_repos dataframes
rm(watch_linked_repos, owner_linked_repos, contrib_linked_repos)

# Now we turn all those dataframes into graphs
rGi           <- graph.data.frame(mm, directed=TRUE); 
set.graph.attribute(rGi, subtitle,
                    "Full graph of 216,147 vertices and 3,099,834 edges")                                                                          )
rGi.sub52     <- graph.data.frame(mm.sub5w2c, directed=TRUE)
set.graph.attribute(rGi.sub52, subtitle,
                    "Minimum of 5 Watchers and 2 Contributors")
rGi.sub55     <- graph.data.frame(mm.sub5w5c, directed=TRUE)
set.graph.attribute(rGi.sub55, subtitle, 
                    "Minimum of 5 Watchers and 5 Contributors")
rGi.sub55C    <- graph.data.frame(mm.sub5w5c5C, directed=TRUE)
set.graph.attribute(rGi.sub55C, subtitle, 
                    "Minimum of 5 Watchers and Contributors; No More Than 500 Contributors")
rGi.sub55C55C <- graph.data.frame(mm.sub5w5C5c5C, directed=TRUE)
set.graph.attribute(rGi.sub55C55C, subtitle, 
                    "Minimum of 5 and No More Than 500 Watchers or Contributors")
rGi.sub51C51C <- graph.data.frame(mm.sub5w1C5c1C, directed=TRUE)
set.graph.attribute(rGi.sub51C51C, subtitle, 
                    "Minimum of 5 and No More Than 100 Watchers or Contributors")
rGi.sub52X52X <- graph.data.frame(mm.sub5w2X5c2X, directed=TRUE)
rGi.sub52X52X <- set.graph.attribute(rGi.sub52X52X, "name", 
                    "Minimum of 5 and No More Than 10 Watchers or Contributors")
summary(rGi.sub52X52X)


# see summaries and create list of names
# summary(rGi)

#################################################################################
#
# Now that we have some graphs it's time to make some plots and begin to visualize
# the data in a few ways. With such a large graph and so many graps to plot, it's
# best to set up these plots as functions so that there isn't so much typing and  
# we can compare apples to apples without copying script. It may be a bit much
# to itteratviely plot all graphs (like, if we created a list of the graphs as 
# they are made). 
basicplot <- function (graph) { 
  plot(graph, vertex.size=1, vertex.label=NA, edge.arrow.size=.2)
  }

basicplot(rGi)
basicplot(rGi.sub52)
basicplot(rGi.sub55)
basicplot(rGi.sub55C)
basicplot(rGi.sub55C55C)
basicplot(rGi.sub51C51C)
basicplot(rGi.sub52X52X)

# This function is adapted from http://igraph.wikidot.com/r-recipes to produce 
# normalized centrality measures for each graph for which it's called. We do this
# just in case some of these common measures are useful (and to show that we were
# paying attention in the middle of the semester)
centrality.norm <- function(graph,type=c("degree","closeness","betweenness"),
                          centralization=FALSE) {    
  result<-NA
  g<-graph
  cent<-centralization
  if (!is.igraph(g)) {stop("Not a graph object")}
  if (type[[1]] == "degree") {
    if (!cent) result <- degree(g)/(vcount(g)-1)
    else result <- (sum(max(degree(g))-degree(g)))/((vcount(g)-1)*(vcount(g)-2))}
  else if (type[[1]] == "betweenness") {
    temp <- 2*betweenness(g)/((vcount(g)-1)*(vcount(g)-2))
    if (!cent) result <- temp
    else  result <- sum(max(temp)-temp)/(vcount(g)-1)}
  else if (type[[1]] == "closeness") {
    if (!cent) result <- closeness(g)
    else result <- (2*vcount(g)-3)*(sum(max(closeness(g))-closeness(g)))/((vcount(g)-1)*(vcount(g)-2))}
  else {stop("this type is unavailable or mispelled")}
  return(result)
}
# Call each of the graphs 
cm52     <- centrality.norm(rGi.sub52)
cm55     <- centrality.norm(rGi.sub55)
cm55C    <- centrality.norm(rGi.sub55C)
cm55C55C <- centrality.norm(rGi.sub55C55C)
cm52X52X <- centrality.norm(rGi.sub52X52X)

# Because we have taken/are taking CSS625 we feel compelled to look for power law
# distributions in the distribution of connectivity. This function from the 
# igraph library plots nonlinear preferential attachment; adapted here as a 
# function so that it can be applied to each graph, in turn.
pldist_test <- function(graph) { 
  d.graph <- degree(graph, mode="in")
  dd.graph <- degree.distribution(graph, mode="in", cumulative=TRUE)
  alpha <- power.law.fit(d.graph, xmin=20)
  plot(dd.graph, log="xy", xlab="degree", ylab="cumulative frequency",
       col=1, main="Nonlinear preferential attachment")
  lines(3:50, 10*(3:50)^(-coef(alpha)+1))
  }

pldist_test(rGi.sub52)
pldist_test(rGi.sub55)
pldist_test(rGi.sub55500)


# Plot the dendrogram and look for blocking 
dendrogram <- function (graph) {
  wt <- walktrap.community(graph, modularity=TRUE)
  dend <- as.dendrogram(wt, use.modularity=TRUE)
  plot(dend, nodePar=list(pch=c(NA, 80)))
}

# dendrogram(rGi)           # Not too bad, but why spend the time
dendrogram(rGi.sub52)
dendrogram(rGi.sub55)
dendrogram(rGi.sub55C)
dendrogram(rGi.sub55C55C)
dendrogram(rGi.sub52X52X)

# This code is also in the igraph library but I'm not sure we talked about 
# 'modularity'. Need to look back at the slide decks. It's supposed to be 
# animated but I can't get that part to work.
library(Cairo)
g <- rGi.sub52
l <- layout.kamada.kawai(g, niter=1000)
ebc <- edge.betweenness.community(g)
colbar <- rainbow(6)
colbar2 <- c(rainbow(10), rep("black",40))
for (i in 1:50) {
  g2 <- delete.edges(g, ebc$removed.edges[seq(length=i-1)])
  eb <- edge.betweenness(g2)
  cl <- clusters(g2)$membership
  q <- modularity(g, cl)
  E(g2)$color <- "grey"
  E(g2)[ order(eb, decreasing=TRUE)[1:10]-1 ]$color <- colbar2[1:10]
  E(g2)$width <- 1
  E(g2)[ color != "grey" ]$width <- 2
  
  # CairoPNG(sprintf("eb-community-%04d.png", i))
  plot(g2, layout=l, vertex.size=6, vertex.label=NA, edge.arrow.size=.2, #added this about edge.arrow.size
       edge.label.color="red", vertex.color=colbar[cl+2],
       edge.label.font=2)
  title(main=paste("Q=", round(q,3)), font=2)
  ty <- seq(1,by=-strheight("1")*1.5, length=20)
  text(-1.3, ty, adj=c(0,0.5), round(sort(eb, dec=TRUE)[1:20],2), col=colbar2,
       font=2)
#   dev.off()
  scan()
}

# These plots seem most useful
# Plotting the diameter: at least 5 watchers and contributors; no more than 500 
# watchers
dplot <- function(graph) {
  g <- graph
  d <- get.diameter(g)
  d.lab <- length(d)
  E(g)$color <- "grey"
  E(g)$width <- 1
  E(g, path=d)$color <- "red"
  E(g, path=d)$width <- 2
  V(g)$label.color <- "blue"
  V(g)$color  <- "SkyBlue2"
  V(g)[ d ]$label.color <- "black"
  V(g)[ d ]$color <- "red"
  
  # It's necessary here to edit the graph title to be appropriate for the graph
  plot(g, layout=layout.fruchterman.reingold, 
       vertex.label.dist=0, vertex.size=2, vertex.label=NA, edge.arrow.size=.2)
  title(main="Diameter of the GitHub Repository Network",
        # This doesn't work. I need to automate adding a subtitle with the graph name
        xlab=get.graph.attribute(graph, graph.name))
  axis(1, labels=FALSE, tick=TRUE)
  axis(2, labels=FALSE, tick=TRUE)
  }

# dplot(rGi) #Don't do that! It takes, ~, an hour
dplot(rGi.sub52)
dplot(rGi.sub55)
dplot(rGi.sub55C)
dplot(rGi.sub55C55C)
dplot(rGi.sub51C51C)
dplot(rGi.sub52X52X)

################################################################################
# So, these diameter plots are kind of interesting; show some open triads. Wonder
# how many triads could be closed by recommending repos to contributors of near-
# neighboring repos? Let's collect some data on triads and run some statistics
# Hint: that great-big-huge graph of everything is too big to census.
s52X <- triad.census(rGi.sub52X52X)
s51C <- triad.census(rGi.sub51C51C)
s55C <- triad.census(rGi.sub55C55C)
s55c <- triad.census(rGi.sub55C)
s55  <- triad.census(rGi.sub55)
s52  <- triad.census(rGi.sub52)
# full <- triad.census(rGi)
triad_cols <- c('003', '012', '102', '021D', '021U', '021C', '111D','111U', 
                '030T', '030C', '201', '120D', '120U', '120C', '210', '300')

# Here's a nice 2d vector of the triad census for each subgraph
comp_Triads <- rbind(c(s52X,s51C,s55C,s55c,s55,s52))
colnames(comp_Triads) <- triad_cols # canonical names as headers
comp_Triads
# print it out in LaTex
library(xtable)
xtable(t(comp_Triads))





# #minimum spanning tree of the small graph
# def distance(p1, p2):
#   return ((p1[0]-p2[0]) ** 2 + (p1[1]-p2[1]) ** 2) ** 0.5
# 
# xs, ys = get.coordinates(rGi.sub55500)
# g <- rGi.sub55500
# layout = Layout(zip(xs, ys))
# 
# weights = [distance(layout[edge.source], layout[edge.target]) for edge in g.es]
# max_weight = max(weights)
# g.es["width"] = [6 - 5*weight/max_weight for weight in weights]
# mst = g.spanning_tree(weights)
# 
# plot(g, layout=layout, opacity=0.25, vertex_label=None)
#add(mst, layout=layout, edge_color="red", vertex_label=None)


# tkplot version of typical plots; slightly more interactive. Cannot use until
# I install the tcltk support on my system.

# library(tkl)
# fancyplot <- function(graph) {
#   g <- graph
#   cs <- leading.eigenvector.community.step(g)
#   V(g)$color <- ifelse(cs$membership==0, "lightblue", "green")
#   scale <- function(v, a, b) {
#     v <- v-min(v) ; v <- v/max(v) ; v <- v * (b-a) ; v+a
#     }
#   V(g)$size <- scale(abs(cs$eigenvector), 10, 20)
#   E(g)$color <- "grey"
#   E(g)[ V(g)[ color=="lightblue" ] %--% V(g)[ color=="green" ] ]$color <- "red"
#   tkplot(g, layout=layout.kamada.kawai, vertex.label.font=2)
# }
# 
# fancyplot(rGi)
# fancyplot(rGi.sub52)
# fancyplot(rGi.sub55)
# fancyplot(rGi.sub55C)
# fancyplot(rGi.sub55C55C)
# fancyplot(rGi.sub51C51C)
# fancyplot(rGi.sub52X52X)




# Working on making different color edges in certain cases using edge attributes  
# E(rGi.sub55500)[ weight > 0.8 ]$color <- "red"
# plot.igraph(rGi.sub55500)


# this does not work yet
# alternatively, with vertex attributes from repo_metaB
# model: rows.to.keep<-which(rownames(data) %in% names.to.keep)
# repo_metaBf <- which(repo_metaB %in% rGiNames)
# head(repo_metaBf)
# model: x.sub1 <- subset(x.df, y > 2 & V1 > 0.6)
# mm.sub 
# rGi_va <- graph.data.frame(mm, vertices=repo_metaB, directed = TRUE)


# A failed experiment in coloring edges based on contribution weights
# colorize <- function(graph) { 
#   E(graph)$color <- "grey"
#   for (e in E(graph)) { 
#     if (graph$ownWeight > graph$watchWeight) graph$color = "red"
#       }
# }
# colorize(rGi.sub55500)

#   graph.es["color"] = "red" if (graph$ownWeight > graph$watchWeight) 
#   graph.es["color"] = ["red" if ()]
#   if (E(graph)$ownWeight > E(graph)$watchWeight) E(graph)$color <- "yellow"
#   if (E(graph)$contribWeight > E(graph)$watchWeight) E(graph)$color <- "red"
# 
#   vertex_set = set(vertices_in_cycle)
#   g.es["color"] = "black"
#   red_edges = g.es.select(_source_in=vertex_set, _target_in=vertex_set)
#   red_edges["color"] = "red"
#   
#   g.es["color"] = ["red" if (edge.source in vertex_set and \
#                              edge.target in vertex_set) else "black" \
#                    for edge in g.es]

#   }



# Generate a vector of the differneces between repo_metaB$ID and V(rGi)$name
# library(compare)
# rGiNames <- V(rGi)$name
# compareIDs <- compare(rGiNames, repo_metaB$ID, allowAll=TRUE)
# diffs <-  lapply(rGiNames,function(i)setdiff(rGiNames[i],compareIDs$tM[i]))
# colnames(diffs) <- colnames(rGiNames)
# 
# head(compareIDs$tM)


save.image("~/R/CSS692_project/.RData")
savehistory("~/R/CSS692_project/projectHistoryLog.Rhistory")
