library(mlbgm)
# BB-Ref updates their figures daily!
rwar <- read_rwar()
save(rwar, file = "data/rwar.rda", compress = "xz")
