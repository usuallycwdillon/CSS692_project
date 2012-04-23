#
# Status: 22 April 22:52. I switched from network to igraph as it seems to behave
# much more nicely with memory and computation resources. Network is just plain 
# slow. The repository graph works, with edge attributes for watching, owning, 
# and contributing. May need to start over by adding vertex attributes from the
# beginning as I cannot seem to be able to add them using the V(g)$name attribute.
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

user_contrib2repo <- read.delim("~/R/data/user_contributes_to_repository.tsv")
repo_meta <- read.delim("~/R/data/repo_meta.tsv")
user_meta <- read.delim("~/R/data/user_meta.tsv")
user_owns_repo <- read.delim("~/R/data/user_owns_repository.tsv")
user_watches_repo <- read.delim("~/R/data/user_watches_repository.tsv")
contrib_linked_repos <- read.delim("~/R/data/contributor_linked_repositories.tsv")
owner_linked_repos <- read.delim("~/R/data/owner_linked_repositories.tsv")
watch_linked_repos <- read.delim("~/R/data/watch_linked_repositories.tsv")

# The sna package is picky about the names of the columns, so might as well change
# them now before we get used to the data column names.
colnames(watch_linked_repos) <- c("source", "target", "watchWeight")
colnames(owner_linked_repos) <- c("source", "target", "ownWeight")
colnames(contrib_linked_repos) <- c("source", "target", "contribWeight")

# This works well in Rstudio because of its tabbed interface; maybe not in R alone.
View(user_contrib2repo)
View(repo_meta)
View(user_meta)
View(user_owns_repo)
View(user_watches_repo)
View(contrib_linked_repos)
View(owner_linked_repos)
View(watch_linked_repos)

# That's a bunch of data. Save the workspace. This time it has data.
save.image("~/R/CSS692_workspace.RData")

# Next, we want to build graphs of users (uG) and repos (rG) but, we must first
# open the library and bring in the sna, network, and igraph packages
library("igraph")
library("network")
 # library("sna") #We won't use this right away but I want to keep track of it 

# Some of the attributes are stored as text (true/false) but it's easier
# for me in R if these values are stored numerically as 0-false, 1-true.
# This is a quick conversion function that can be called anywhere once loaded.
tfbinary <- function(a) {
  if ((a == 'true')||(a == 'TRUE')) return (1)
  else return(0)  
}

# We need to create a vector of unique nodes in the repository edgelists, which 
# we can use as a dictionary of node attribues in future graphs. We do this first
# for each of the repos, then for user graphs:
# (1) Start with contributor-linked repositories
uniqueCLRa <- unique(contrib_linked_repos$source)
uniqueCLRb <- unique(contrib_linked_repos$target)
jointCLR  <- c(uniqueCLRa,uniqueCLRb)
uniqueCLR <- unique(jointCLR)
length(uniqueCLR)
# But what about the ability to cross-compare? What is the list of unique nodes
# from both vectors; say, for adding attributes to a complete set of nodes?
# So we create a frequency table for each time a repo is linked to another
# Adapted from a 2005 post from Dimitris Rizopoulos from the R-help listserv
# https://stat.ethz.ch/pipermail/r-help/2005-May/071907.html
CLRtab_a <- table(contrib_linked_repos$source) # frequency of A
CLRtab_b <- table(contrib_linked_repos$target) # frequency of B
CLRtab   <- c(CLRtab_a, CLRtab_b)                    # frequency of both together
CLRunique <- as.numeric(names(CLRtab))               # easier to plot numbers
CLRnumber <- as.vector(CLRtab)
# Note! CLRuNames is also a non-numeric list of unique nodes is whole CLR graph
# Created this way here to validate the previous method (it validates)
CLRuNames <- unique(names(CLRtab))                   # unique names in frequency
CLRuCount <- unique(CLRtab)                          # unique counts
# FYI: Another curious plot of number of times a repo appears in the edgelist
# plot(CLRnumber, CLRunique)                         # uncomment to create this
CLRvc <- length(CLRuNames); CLRvc                    # number of nodes in CLR graph
if (length(uniqueCLR) == CLRvc) print("So far, so good.") else (print
                                     ("Wait. There may be a problem here..."))

# (2) Now, for owner-linked repositories
uniqueOLRa <- unique(owner_linked_repos$source)
uniqueOLRb <- unique(owner_linked_repos$target)
length(uniqueOLRa)
length(uniqueOLRb)
jointOLR  <- c(uniqueOLRa,uniqueOLRb)
uniqueOLR <- unique(jointOLR)
length(uniqueOLR)
OLRtab_a <- table(owner_linked_repos$source) # frequency of A
OLRtab_b <- table(owner_linked_repos$target) # frequency of B
OLRtab   <- c(OLRtab_a, OLRtab_b)                    # frequency of both together
OLRunique <- as.numeric(names(OLRtab))               # easier to plot numbers
OLRnumber <- as.vector(OLRtab)
# Note! CLOuNames is also a non-numeric list of unique nodes is whole CLO graph
# Created this way here to validate the previous method (it validates)
OLRuNames <- unique(names(OLRtab))                   # unique names in frequency
OLRuCount <- unique(OLRtab)                          # unique counts
# FYI: Another curious plot of number of times a repo appears in the edgelist
plot(OLRnumber, OLRunique)                         # uncomment to create this
OLRvc <- length(OLRuNames); OLRvc                    # number of nodes in CLO graph
if (length(uniqueOLR) == OLRvc) print("So far, so good.") else (print
                                     ("Wait. There may be a problem here..."))

# (3) Finally, for watcher-linked repositories...
uniqueWLRa <- unique(watch_linked_repos$source)
uniqueWLRb <- unique(watch_linked_repos$target)
length(uniqueWLRa)
length(uniqueWLRb)
jointWLR  <- c(uniqueWLRa,uniqueWLRb)
uniqueWLR <- unique(jointWLR)
length(uniqueWLR)
WLRtab_a <- table(watch_linked_repos$source) # frequency of A
WLRtab_b <- table(watch_linked_repos$target) # frequency of B
WLRtab   <- c(WLRtab_a, WLRtab_b)                    # frequency of both together
WLRunique <- as.numeric(names(WLRtab))               # easier to plot numbers
WLRnumber <- as.vector(WLRtab)
# Note! CLWuNames is also a non-numeric list of unique nodes is whole WLR graph
# Created this way here to validate the previous method (it validates)
WLRuNames <- unique(names(WLRtab))                   # unique names in frequency
WLRuCount <- unique(WLRtab)                          # unique counts
# FYI: Another curious plot of number of times a repo appears in the edgelist
# plot(WLRnumber, WLRunique)                         # uncomment to create this
WLRvc <- length(WLRuNames); WLRvc                    # number of nodes in WLR graph
if (length(uniqueWLR) == WLRvc) print("So far, so good.") else (print
                                     ("Wait. There may be a problem here..."))

# We're not quite done here. We still need to merge these lists of uniqe nodes,
# and identify a vector of uniqe nodes included in all three sub-vetors. We'll
# use this later to merge all of the graphs together with contributor, watcher,
# and owner attribute edges. 
mergedLRtab <- c(WLRtab, OLRtab, CLRtab) # Remember, these are freq.tables
jointCombinedLRunique <- as.numeric(names(mergedLRtab))
length(jointCombinedLRunique)
# Let's try this again...
jointLR <- c(uniqueCLR, uniqueWLR, uniqueOLR)
uniqueJLR <- unique(jointLR)
length(uniqueJLR) # I get 1050493
# Again, it's a good idea to validate so we do this the other way around, too.
mergedLRtab <- c(WLRtab, OLRtab, CLRtab)
mergedLRtabNum <- as.numeric(names(mergedLRtab))
mergedLRunique <- unique(mergedLRtabNum)
length(mergedLRunique)

if (length(mergedLRunique) == length(uniqueJLR)) print(
  "So far, so good.") else (print("Wait. There may be a problem here..."))

# So, in the end we have some data about four sets of nodes: the repos linked by 
# watchers, repos linked by owners, repos linked by contributors, and the number 
# of nodes in the joint, combined graph.

################################################################################
# This is how to create the graph, starting with nodes, using the 'network' package.
# This took less than a minute to build and another half minute to add names

# network.vertex.names(uG) <- c(user_meta$Login)
# list.vertex.attributes(uG)
# isHir <- sapply(user_meta$Is.Hirable, tfbinary)
# set.vertex.attribute(uG, "isHirable", c(isHir))
# set.vertex.attribute(uG, "nCollaborators", c(user_meta$N.Collaborators))
# set.vertex.attribute(uG, "nFollowers", c(user_meta$N.Followers))
# set.vertex.attribute(uG, "nPubRepos", c(user_meta$N.Public.Repos))
# list.vertex.attributes(uG)

# A word of caution: it took 2GHz processor and 24GB RAM nearly 3 min to build. 
# Note. According to Carter Butts, the 'network' author, a graph of 100k nodes and 
# 100k edges and no attributes consumes about 78MB of RAM; so this graph is .7GB
# without attributes. 
# rG <- network.initialize(980453, multiple=TRUE)
# network.vertex.names(rG) <- c(repo_meta$ID)
# list.vertex.attributes(rG)
# hasWiki <- sapply(repo_meta$Has.Wiki, tfbinary)
# hasIssues <- sapply(repo_meta$Has.Issues, tfbinary)
# hasDownloads <- sapply(repo_meta$Has.Downloads, tfbinary)
# isFork <- sapply(repo_meta$Is.Fork, tfbinary)
# set.vertex.attribute(rG, "hasWiki", c(hasWiki))
# set.vertex.attribute(rG, "hasDownloads", c(hasDownloads))
# set.vertex.attribute(rG, "hasIssues", c(hasIssues))
# set.vertex.attribute(rG, "isFork", c(isFork))
# set.vertex.attribute(rG, "nWatchers", repo_meta$N.Watchers)
# set.vertex.attribute(rG, "nOpenIssues", repo_meta$N.Open.Issues)
# set.vertex.attribute(rG, "nForks", repo_meta$N.Forks)
# set.vertex.attribute(rG, "size", repo_meta$Size)
# list.vertex.attributes(rG)

################################################################################
# This part of the script takes the edgelists and adds the edges to the graphs

# But, there is some big trouble with our initial data: the watched repos edge list
# does not re
# watch_linked_repositories[with(watch_linked_repositories, order(-target, source)), ]
# repo_meta[with(repo_meta, order(-, ID)), ]

## Well this next bit won't work because Network is finding a mis-match between 
#  the nodes in the edgelist and the nodes in the graph; it's being too picky.
# rGw <- add.edges(rG, watch_linked_repositories$source, 
#                  watch_linked_repositories$target, edge.check=TRUE)
# but I found confirmation of this general problem elsewhere: 
# see, http://www.cybaea.net/Blogs/Data/SNA-with-R-Loading-your-network-data.html
# and, http://www.cybaea.net/Blogs/Data/SNA-with-R-Loading-large-networks-using-
#             the-igraph-library.html



#################################################################################
# Here is some script to just start with the edgelist using the 'sna' package. 
# The idea with this script is to start by defining the edges and their attribues
# then add attributes to the nodes in the edgelist later.
# contributor_linked_repositories <- read.delim("~/R/data/contributor_linked_repositories.tsv")
# crGsna <-as.edgelist.sna(contributor_linked_repositories, attrname = 'contributors')

# Next, need to get a vector of unique nodes in the .sna graph and attribute the 
# attributable attributes. 


################################################################################
# This is an iGraphv version of the above.
# This took less than a minute to build and another half minute to add names. 
# According to some user blogs, the igraph package is much more efficient at 
# making and storing graphs; just does not have so many methods for analysing 
# them. Fortunately, there are methods to convert them into network objects.

# Create an empty edgelist of the appropriate size (using the CLR, OLR and WLR 
# graphs. For example, this is an edgelist with 167863 edges but we first need 
# to know how many nodes/vertices

# This method is slow but, it works.
m <- merge(watch_linked_repos, owner_linked_repos, by=c("source", "target"))
mm <- merge(m, contrib_linked_repos, by=c("source", "target"))
# This won't work. ...and it's computationally expensive
# mm <- merge(watch_linked_repos, owner_linked_repos, contrib_linked_repos, 
#           by=c("source", "target"))
rGi <- graph.data.frame(mm, directed=TRUE)
summary(rGi)

# next step: figure out how to apply vertex names based on V(rgi)$name instead of 
# by vertex index values.
# rGi <- set.vertex.attribute(rGi, "ID", value=repo_meta$ID)
# rGi <- set.vertex.attribute(rGi, "hasWiki", value=c(hasWiki))
# rGi <- set.vertex.attribute(rGi, "hasDownloads",value= c(hasDownloads))
# rGi <- set.vertex.attribute(rGi, "hasIssues", value=c(hasIssues))
# rGi <- set.vertex.attribute(rGi, "isFork", value=c(isFork))
# rGi <- set.vertex.attribute(rGi, "nWatchers", value=c(repo_meta$N.Watchers))
# rGi <- set.vertex.attribute(rGi, "nOpenIssues", value=c(repo_meta$N.Open.Issues))
# rGi <- set.vertex.attribute(rGi, "nForks", Vc(repo_meta$N.Forks))
# rGi <- set.vertex.attribute(rGi, "projSize", value=c(repo_meta$Size))




################################################################################
#
# This is unused script that I can't find the emotional strenght to part with  #


# We need to create adjacency matrices for repos and users to define the edges 
# in the owning, watching, following and committing networks. There is no 'collaborator'
# network because all the values are 0.
# 
# I modeled this code to create an adjacency matrix from some that I found by a
# certain M.C. Fitzpatrick (mfitzpatrick@umces.edu) which he wrote to use WinBUGS
# on or around 21 April 2010 
# 
# Here's what the code should do:
# 1. Take a data frame (DF) as input. Do a loop for each row in DF. 
# 2. For each row in DF, create a column and give the cell value a 0.
# for each row in the graph, look to see if the row element is related to the column
# element; if so, add one to the value. 
# create adjacency information needed for WinBUGS
# mkAM <- function(DF) {
#   r <- nrow(DF)
#   aM <- 0
#   for(i in 1:r) {
#     for(j in 1:r) {
#       if(DF[i, j] == 1) {
#         aM <- append(r, j)
#       }
#     }
#   }
#   devVal <- defVal[-1]
#   return(aM)
# }

# Give this function a column of nodes to create a sparse matrix that increments
# once for each instance when a is comparable to b
# mkAM <- function(vec, a, b) {
#   r <- nrow(vec)
#   thisMatrix <- matrix(0, r, vec)
#   for (i in 1:r) {
#     v1 <- user_meta$Login[i]
#     i++
#   }
# }
# 
# r <- nrow(user_meta)
# r2 <- r*r
# watchdf <- data.frame(v1=numeric(r2), v2=numeric(r2), weight=numeric(r2)) {
#   for (i in V1 1:r2) {
#     for (v)
#   }
# }

# This is unusable because we don't have user linkages
# uGi <- set.vertex.attribute(uGi, "login", value=user_meta$Login)
# uGi <- set.vertex.attribute(uGi, "hirable", value=isHir)
# uGi <- set.vertex.attribute(uGi, "nCollaborators", value=c(user_meta$N.Collaborators))
# uGi <- set.vertex.attribute(uGi, "nFollowers", value=c(user_meta$N.Followers))
# uGi <- set.vertex.attribute(uGi, "nPublicRepos", value=c(user_meta$N.Public.Repos))
# uGi


# This is just for good housekeeping. I usually save my work upon completion and
# use these lines to save off from time to time.
save.image("~/R/CSS692_project/.RData")
savehistory("~/R/CSS692_project/projectHistoryLog.Rhistory")
