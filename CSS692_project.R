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
save.image("~/R/CSS692_workspace.RData")

user_contrib2repo <- read.delim("~/R/data/user_contributes_to_repository.tsv")
repo_meta <- read.delim("~/R/data/repo_meta.tsv")
user_meta <- read.delim("~/R/data/user_meta.tsv")
user_owns_repo <- read.delim("~/R/data/user_owns_repository.tsv")
user_watches_repo <- read.delim("~/R/data/user_watches_repository.tsv")

View(user_contrib2repo)
View(repo_meta)
View(user_meta)
View(user_owns_repo)
View(user_watches_repo)

save.image("~/R/CSS692_workspace.RData")

# Next, we want to build graphs of users (uG) and repos (rG) but, we must first
# open the library and bring in the sna, network, and igraph packages
library("igraph")
library("network")
library("sna")

# Some of the attributes are stored as text (true/false) but it's easier
# in R if these values are stored numerically as 0-false, 1-true.
tfbinary <- function(a) {
  if ((a == 'true')||(a == 'TRUE')) return (1)
  else return(0)  
}


# This took less than a minute to build and another half minute to add names
uG <- network.initialize(167863, multiple=TRUE)
network.vertex.names(uG) <- c(user_meta$Login)
list.vertex.attributes(uG)
isHir <- sapply(user_meta$Is.Hirable, tfbinary)
set.vertex.attribute(uG, "isHirable", c(isHir))
set.vertex.attribute(uG, "nCollaborators", c(user_meta$N.Collaborators))
set.vertex.attribute(uG, "nFollowers", c(user_meta$N.Followers))
set.vertex.attribute(uG, "nPubRepos", c(user_meta$N.Public.Repos))
list.vertex.attributes(uG)

# A word of caution: it took 2GHz processor and 24GB RAM nearly 3 min to build. 
# nb. According to Carter Butts, the 'network' author a graph of 100k nodes and 
# 100k edges and no attributes consumes about 78MB of RAM; so this graph is .7GB
rG <- network.initialize(980453, multiple=TRUE)
network.vertex.names(rG) <- c(repo_meta$ID)
list.vertex.attributes(rG)
hasWiki <- sapply(repo_meta$Has.Wiki, tfbinary)
hasIssues <- sapply(repo_meta$Has.Issues, tfbinary)
hasDownloads <- sapply(repo_meta$Has.Downloads, tfbinary)
isFork <- sapply(repo_meta$Is.Fork, tfbinary)
set.vertex.attribute(rG, "hasWiki", c(hasWiki))
set.vertex.attribute(rG, "hasDownloads", c(hasDownloads))
set.vertex.attribute(rG, "hasIssues", c(hasIssues))
set.vertex.attribute(rG, "isFork", c(isFork))
set.vertex.attribute(rG, "nWatchers", repo_meta$N.Watchers)
set.vertex.attribute(rG, "nOpenIssues", repo_meta$N.Open.Issues)
set.vertex.attribute(rG, "nForks", repo_meta$N.Forks)
set.vertex.attribute(rG, "size", repo_meta$Size)
list.vertex.attributes(rG)

save.image("~/R/CSS692_project/.RData")
savehistory("~/R/CSS692_project/projectHistoryLog.Rhistory")






