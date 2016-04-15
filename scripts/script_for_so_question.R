require(ggmap)

cent <- c(lon = -122, lat = 47.5)
set.seed(127)
df <- data.frame(lon = rnorm(25, mean = -122.2, sd = 0.2),
                 lat = rnorm(25, mean = 47.5, sd = 0.1),
                 size = rnorm(25, mean = 15, sd = 5))

map_background <- get_map(cent, map = "toner-background")
map_lines <- get_map(cent, map = "toner-lines")
map_labels <- get_map(cent, map = "toner-labels")

ggmap(map_background) +
  geom_point(data = df,
             aes(x = lon, y = lat, size = size),
             color = "blue", alpha = 0.8) + 
  scale_size_identity(guide = "none") + 
  inset_ggmap(map_lines) + 
  inset_ggmap(map_labels)


style_string_background <- c("feature:road.highway.controlled_access|element:geometry.stroke|visibility:on|color:0x808080&style=element:labels|visibility:off&style=feature:road.highway|element:geometry|visibility:on|color:0x808080&style=feature:road.arterial|element:geometry.fill|color:0x808080|visibility:on&style=feature:poi.park|visibility:off&style=feature:landscape.man_made|visibility:on|color:0x808080|lightness:49&style=feature:administrative.province|element:geometry|visibility:on|color:0x808080|weight:2.8")
map_background_google <- get_googlemap(center=c(lon = -122, lat = 47.5), style=style_string_background, maptype="roadmap")

ggmap(map_background_google) +
  geom_point(data = df,
             aes(x = lon, y = lat, size = size),
             color = "blue", alpha = 0.8) + 
  scale_size_identity(guide = "none") + 
  inset_ggmap(map_lines) + 
  inset_ggmap(map_labels)


style_string_adminlabels <- c("feature:landscape|visibility:off&style=feature:poi|visibility:off&style=feature:road|visibility:off&style=feature:transit|visibility:off&style=feature:water|visibility:off&style=feature:administrative|element:geometry|visibility:off")
map_labels_google <- get_googlemap(center=c(lon = -122, lat = 47.5), style=style_string_adminlabels, maptype="roadmap")

ggmap(map_background_google) +
  geom_point(data = df,
             aes(x = lon, y = lat, size = size),
             color = "blue", alpha = 0.8) + 
  scale_size_identity(guide = "none") + 
  inset_ggmap(map_lines) + 
  inset_ggmap(map_labels_google)















