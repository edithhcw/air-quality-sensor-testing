library(rworldmap)
# start with the entire world
newmap <- getMap(resolution = "low")
plot(newmap, main = "World")
# crop to the area desired (outside US)
# (can use maps.google.com, right-click, drop lat/lon markers at corners)
plot(newmap
     , xlim = c(-139.3, -58.8) # if you reverse these, the world gets flipped
     , ylim = c(13.5, 55.7)
     , asp = 1 # different aspect projections
     , main = "US from worldmap"
)

library(ggplot2)
map.world <- map_data(map = "world")
# map = name of map provided by the maps package.
# These include county, france, italy, nz, state, usa, world, world2.
str(map.world)
# how many regions
length(unique(map.world$region))

p1 <- ggplot(map.world, aes(x = long, y = lat, group = group))
p1 <- p1 + geom_polygon() # fill areas
p1 <- p1 + labs(title = "World, plain")
print(p1)

p2 <- ggplot(map.world, aes(x = long, y = lat, group = group, colour = region))
p2 <- p2 + geom_polygon() # fill areas
p2 <- p2 + theme(legend.position="none") # remove legend with fill colours
p2 <- p2 + labs(title = "World, colour borders")
print(p2)

p3 <- ggplot(map.world, aes(x = long, y = lat, group = group, fill = region))
p3 <- p3 + geom_polygon() # fill areas
p3 <- p3 + theme(legend.position="none") # remove legend with fill colours
p3 <- p3 + labs(title = "World, filled regions")
print(p3)

p4 <- ggplot(map.world, aes(x = long, y = lat, group = group, colour = region))
p4 <- p4 + geom_path() # country outline, instead
p4 <- p4 + theme(legend.position="none") # remove legend with fill colours
p4 <- p4 + labs(title = "World, path outlines only")
print(p4)

library(ggmap)
library(mapproj)
map <- get_map(
  location = "California" # google search string
  , zoom = 7 # larger is closer
  , maptype = "hybrid" # map type
)
plot(map)

p <- ggmap(map)
p <- p + labs(title = "California hybrid")
print(p)

# some options are cute, but not very informative
map <- get_map(
  location = "Berkeley, California" # google search string
  , zoom = 10 # larger is closer
  , maptype = "watercolor" # map type
)
p <- ggmap(map)
p <- p + labs(title = "SF Bay Area watercolor")
print(p)