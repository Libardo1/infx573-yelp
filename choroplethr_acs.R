# a multiple-county geo.set
psrc=geo.make(state="WA", county=c(33,35,53,61))
# combine geo.sets
north.mercer.island=geo.make(state=53, county=33, tract=c(24300,24400))
optional.tract=geo.make(state=53, county=33, tract=24500)
# add in one more tract to create new, larger geo
north.mercer.island.plus=north.mercer.island + optional.tract
# created a nested geo.set
my.nested.geo.set=c(north.mercer.island.plus, psrc)
str(my.nested.geo.set)
length(my.nested.geo.set)
# .. and flatten in out
# note difference in structure and length
my.flat.geo.set=flatten.geo.set(my.nested.geo.set)
str(my.flat.geo.set)
length(my.flat.geo.set)

library(acs)
library(sqldf)
library(ggplot2)
library(maps)

api.key.install(key="3e66c5de9d6cfe4f933c62def2764b9a82075cd2")



geo.lookup(state="AZ", county="Ma", place="Ph")


acs.lookup(endyear = 2012, span = 5, dataset = "acs", keyword = "bachelor", case.sensitive=F )
census_poverty = acs.fetch(
  endyear = 2012, 
  span = 5, 
  geo = states, 
  table.name="POVERTY STATUS",
  col.names="pretty",
  )

library(choroplethr)
choroplethr_acs("B01001", "zip", endyear=2012, span=5, zoom="arizona") 

my.tracts=geo.make(state="AZ", county="Maricopa", tract="*") 
d <- acs.fetch(geography=my.tracts, table.number="B01003")
geography(d)

