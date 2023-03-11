source('./process.R')

metapath <- getwd()
dbpath <- '../data/CovidShiny.sqlite'

process(metapath, dbpath)
