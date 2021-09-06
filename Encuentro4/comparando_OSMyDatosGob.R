require(patchwork)
require(leaflet)
require(osmdata)
require(htmlwidgets)
require(sf)
require(ggmap)

bb = getbb('Ciudad Autonoma de Buenos Aires, Argentina',format_out = 'polygon')
bbox_ba = getbb("Ciudad Autonoma de Buenos Aires, Argentina")
#plot(bb[[2]][[1]],type='l')
plot(bb[[2]][[1]])
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
parksOSM = rbind(parks_poly[,cn],parks_mpoly[,cn])

parksOSM = st_transform(parksOSM,crs=22195)


# Cargo datos de ciudad

parksCiu = st_make_valid(read_sf('../Encuentro2/espacio-verde-publico/espacio-verde-publico.shp'))
parksCiu = st_transform(parksCiu,crs=22195)


# Misiones:

# Agrego MAGNITUDES a SF
## AREAS
parksOSM$area = as.numeric(st_area(parksOSM))
parksCiu$area = as.numeric(st_area(st_make_valid(parksCiu)))



## PERIMETROS
require(lwgeom)
parksCiu$perimeter = as.numeric(st_perimeter(parksCiu))
parksOSM$perimeter = as.numeric(st_perimeter(parksOSM))

## AREA SOBRE PERIMETRO
parksCiu$ASP = parksCiu$area/parksCiu$perimeter
parksOSM$ASP = parksOSM$area/parksOSM$perimeter

palPeri = colorNumeric(palette = c('red','blue'),domain = c(parksCiu$perimeter,parksOSM$perimeter))

palArea = colorNumeric(palette = c('red','blue'),domain = c(parksCiu$area,parksOSM$area))

palASP = colorNumeric(palette = c('red','blue'),domain = c(parksCiu$ASP))


# Mapita para mirar

pal = palASP
map = leaflet() %>% addProviderTiles(providers$OpenStreetMap) %>% addPolygons(data=st_transform(parksCiu[parksCiu$clasificac=='PLAZOLETA' & parksCiu$area>5000,],4326),popup=~paste(clasificac,nombre_ev),color = ~palASP(ASP)) %>% addLegend(pal=palASP,values=parksCiu$ASP)
map
require(ggplot2)

ggplot(data=parksCiu,aes(x=area,fill=clasificac),alpha=0.5) + geom_density() + facet_wrap(~clasificac,scales = "free") + theme(legend.position = 'none') + xlim(0,100**2)

View(parksCiu[parksCiu$clasificac=='PLAZOLETA' & parksCiu$area>5000,])

View(parksCiu)


# Tiene sentido tirar:

catTirar = c('PLAZOLETA','CANTERO CENTRAL')




# Corte de area OSM
# Corte de área Ciudad
# A tener en cuenta para cortar
## Area, Diámetro, Perímetro


# Criterio para unificar x distancia (o generar red)





# Features parques