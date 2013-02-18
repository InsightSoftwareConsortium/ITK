# check that the points in the difference maps are equi-distant from closest
# seeds

di <- read.csv("diffpos.csv", header=F)
sd <- read.csv("sd.csv", header=F)

# want to compute a distance between each point in di and all the points in sd

require(proxy)
alldist <- proxy::dist(di[,-1], sd[,-1])
