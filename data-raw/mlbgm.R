# build internal databases

comps_hypercube <- mlbgm:::comps_hypercube_build()

save(comps_hypercube, file = "data/comps_hypercube.rda", compress = "xz")

