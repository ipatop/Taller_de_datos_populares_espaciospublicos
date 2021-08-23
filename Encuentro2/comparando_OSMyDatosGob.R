require(patchwork)
require(leaflet)
require(osmdata)
require(sf)

bb = getbb('Ciudad Autonoma de Buenos Aires, Argentina',format_out = 'polygon')
#plot(bb[[2]][[1]],type='l')
ciudad = st_polygon(list(bb[[2]][[1]]))
ciudad = st_sfc(ciudad)
plot(ciudad)
parks = opq(bb) %>% 
  add_osm_feature(key = 'leisure', value = "park") %>% 
  osmdata_sf() %>% trim_osmdata(bb)


parks_poly = parks$osm_polygons
parks_mpoly = parks$osm_multipolygons
cn = intersect(colnames(parks_poly),colnames(parks_mpoly))
#cn
parks = rbind(parks_poly[,cn],parks_mpoly[,cn])

plot(ciudad,reset=FALSE)
plot(parks[1],add=TRUE)

# Cargo datos de ciudad

parks_ciudad = read_sf('espacio-verde-publico/espacio-verde-publico.shp')

### Comparemos

## Cantidad
nrow(parks)
nrow(parks_ciudad)
par(mfrow=c(1,2))
plot(parks[1],reset=FALSE)
plot(parks_ciudad[1],reset=FALSE)

### Mapita comparativo
mapa_ba <- get_stamenmap(bbox_ba,  zoom = 12)

ggOSM = ggmap(mapa_ba)+ 
  geom_sf(data=parks,fill='darkgreen', size=0.5, color="black", inherit.aes = FALSE)

ggCiud = ggmap(mapa_ba)+ 
  geom_sf(data=parks_ciudad,fill='darkgreen', size=0.5, color="black", inherit.aes = FALSE)

ggOSM + ggCiud

### Mapita diferencia

##### Parques de OSM que tocan parques de la ciudad
interOSM_ciudad = st_intersects(st_make_valid(parks_ciudad),parks)
##### Parques de la ciudad que tocan parques de OSM
interciudad_OSM = st_intersects(parks,st_make_valid(parks_ciudad))

###### Cuantos son de cada uno
length(unique(unlist(interOSM_ciudad))) 
length(unique(unlist(interciudad_OSM)))
nrow(parks)
nrow(parks_ciudad)
length(unique(unlist(interOSM_ciudad)))/nrow(parks)  # Fraccion de parques de OSM que tocan ciudad
length(unique(unlist(interciudad_OSM)))/nrow(parks_ciudad)  # Fraccion de parques de ciudad que tocan OSM

####### Graficamos las diferencias

ggmap(mapa_ba)+ 
  geom_sf(data=parks[setdiff(1:nrow(parks),unique(unlist(interOSM_ciudad))),],fill='darkgreen', size=0.5, color="black", inherit.aes = FALSE) +

ggmap(mapa_ba)+ 
  geom_sf(data=parks_ciudad[setdiff(1:nrow(parks_ciudad),unique(unlist(interciudad_OSM))),],fill='darkgreen', size=0.5, color="black", inherit.aes = FALSE)


map = leaflet() %>% addProviderTiles(providers$OpenStreetMap) %>%
  addPolygons(data=st_make_valid(parks_ciudad),color='red',fillColor='red',weight=1)   %>% 
  addPolygons(data=parks,color='blue',fillColor='blue',weight = 1) 
map


