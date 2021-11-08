setwd("./Documents/CEDESu/")
require(sf)
require(tidyverse)
require(ggplot2)
require(patchwork)
require(ggmap)
require(osmdata)
bb = getbb('Ciudad Autonoma de Buenos Aires, Argentina',format_out = 'polygon')
bbox_ba = getbb("Ciudad Autonoma de Buenos Aires, Argentina")
mapa_ba <- get_stamenmap(bbox_ba,  zoom = 12)

ciudad = st_polygon(list(bb[[2]][[1]]))
ciudad = st_sfc(ciudad)
st_crs(ciudad) = 4326
parques_full = st_read('parquesCorteArea2_5perc_fullfeatures.geojson')
parques_full = parques_full %>% st_transform(crs=4326)

#options
names(parques_full)

# Todos los parques, mostrando ambas áreas
ggplot() + theme_minimal()+ theme(legend.position = c(.8,.1))+
  geom_sf(data=ciudad,fill='white') +
  geom_sf(data=parques_full,mapping=aes(fill=area>=5e3)) +
  scale_fill_discrete(name = "Area mayor a 1/2 ha",labels=c('No','Sí')) +
  ggtitle('Parques')

# Todos los parques, areas <0.5ha
ggplot() + theme_minimal()+ theme(legend.position = c(.8,.1))+
  geom_sf(data=ciudad,fill='white') +
  geom_sf(data=parques_full %>% filter(area<5e3)) +
  ggtitle('Parques con area < 1/2ha')

# Parques con JPN
ggplot() + theme_minimal()+ theme(legend.position = c(.8,.15))+
  geom_sf(data=ciudad,fill='white') +
  geom_sf(data=parques_full %>% filter(area>=5e3),mapping=aes(fill=JPN)) +
  ggtitle('Parques y Juegos para Niñes')

# Parques con fitness_station + canchas
ggplot() + theme_minimal()+ theme(legend.position = c(.8,.15))+
  geom_sf(data=ciudad,fill='white') +
  geom_sf(data=parques_full %>% filter(area>=5e3),mapping=aes(fill=fitness_station+canchas)) +
  scale_fill_continuous(name='Cantidad de espacios')+
  ggtitle('Fitness y canchas')


# Parques y ciclovía
ggplot() + theme_minimal() + theme(legend.position = c(.8,.15)) +
  geom_sf(data=ciudad,fill='white') +
  geom_sf(data=parques_full %>% filter(area>=5e3) %>% 
            mutate('DBB'=case_when(distanciaBicisenda<=1e2 ~ '<1 cuadra',
                                   distanciaBicisenda<=1e3 ~ '<1 km',
                                   distanciaBicisenda>1e3 ~ '>1 km')),mapping=aes(fill=DBB)) +
  scale_fill_brewer(name='Distancia a Ciclovía',palette = "YlOrRd") +
  ggtitle('Distancia a ciclovía')

# Paradas de colectivo
ggplot() + theme_minimal() + theme(legend.position = c(.8,.15)) +
  geom_sf(data=ciudad,fill='white') +
  geom_sf(data=parques_full %>% filter(area>=5e3),
          mapping=aes(fill=paradasA200m)) +
  scale_fill_continuous(name='Paradas de colectivo') +
  ggtitle('Paradas a menos de 200m')


precenso = new.env()
load('precenso2021_06.RData',envir = precenso)
precenso = as.list(precenso)
precenso = precenso$dataset
precenso = st_as_sf(precenso,coords=c('longitud_centroide','latitud_centroide'))
st_crs(precenso) = 4326

parques = parques_full %>% filter(area>=5e3)
D = st_distance(parques,precenso)


nrow(D)
nrow(parques)
parques$potencial_viviendas = sapply(1:nrow(parques),function(i){
  di = D[i,]
  di = as.numeric(di)
  di = ifelse(di<10,10,di)
  -sum(precenso$ind01/di)
})

i = 1

precenso$potencial_parques = sapply(1:nrow(precenso),function(i){
  di = D[,i]
  di = as.numeric(di)
  di[di<10] = 10
  sum(parques$area/1e4/di)
})


# Todos los parques, potencial
ggplot() + theme_minimal()+ theme(legend.position = c(.8,.1))+
  geom_sf(data=ciudad,fill='white') +
  geom_sf(data=parques,mapping=aes(fill=potencial_viviendas)) +
  scale_fill_gradient(low='red',high='yellow')+
  ggtitle('Potencial viviendas')

# Parques y radios censales, potencial
ggplot() + theme_minimal()+ theme(legend.position = c(.1,.86))+
  geom_sf(data=ciudad,fill='white') +
  geom_sf(data=precenso,mapping=aes(color=log(potencial_parques)),size=.5) +
  scale_color_gradient(high='red',low='yellow',name='lPoteParques')+
  geom_sf(data=parques,mapping=aes(fill=log(-potencial_viviendas))) +
  scale_fill_gradient(high='red',low='yellow',name='lPoteViviendas')

#Previos viviendas
precios<-read_sf("precios_geo.csv")
names(precios)
precios<-transform(precios, precio = as.numeric(precio), sup_total = as.numeric(sup_total))

#calculo precio por mt2
precios$preciosmt2 = as.numeric(precios$precio)/as.numeric(precios$sup_total)
#filtro cosas que no tienen senido
precios=precios[precios$sup_total>10,]
precios=precios[precios$precio>10000,]
precios=precios[precios$preciosmt2>60,]
precios=precios[precios$preciosmt2<2010027,]
unique(precios$fuente)
ggplot(precios,aes(x=preciosmt2,fill=BARRIO,alpha=0.5))+geom_density() +facet_wrap(~BARRIO) +theme(legend.position = "none")
ggplot(precios,aes(x=preciosmt2,fill=BARRIO,alpha=0.5))+geom_density() + labs(title = "Precio de propiedad por barrio", subtitle="Fuente: mercado libre, argenprop, zonaprop y properati")

names(precios)

precios_sf<-st_as_sf(precios, coords = c("lon", "lat"), crs = 4326, agr = "constant")

#Distancia de las propiedades a parques
D_prop = st_distance(parques,precios_sf)
ncol(D_prop)
nrow(precios_sf)

precios_sf$potencial_parques = sapply(1:nrow(precios_sf),function(i){
  di = D_prop[,i]
  di = as.numeric(di)
  di[di<10] = 10
  sum(parques$area/1e4/di)
})

hist(precios_sf$potencial_parques)

ggplot() + theme_minimal() + theme(legend.position = c(.1,.86))+
  geom_sf(data=ciudad,fill='white') +
  geom_sf(data=parques_full,mapping=aes(fill=area>=5e3)) +
  scale_fill_discrete(name = "Area mayor a 1/2 ha",labels=c('No','Sí')) +
  geom_sf(data = precios_sf,mapping=aes(color=log(preciosmt2)),size=.5,alpha=0.5) +
  scale_color_gradient(high='red',low='yellow',name='lPoteParques')

ggplot() + theme_minimal() +
  geom_sf(data=ciudad,fill='white') +
  geom_sf(data = precios_sf,mapping=aes(color=log(potencial_parques)),size=.8) +
  scale_color_gradient(high='red',low="darkgrey",name='lPoteParques') +
  geom_sf(data=parques_full, fill="darkgreen",alpha=0.9)

names(precios_sf)
ggplot(precios_sf,aes(log(preciosmt2),log(potencial_parques)))+geom_point()

ggplot() + theme_minimal() +
  geom_sf(data=ciudad,fill='white') +
  geom_sf(data=precenso,mapping=aes(color=log(potencial_parques)),size=.5) +
  scale_color_gradient(high='red',low='yellow',name='lPoteParques')+
  geom_sf(data=parques,mapping=aes(fill=log(-potencial_viviendas))) +
  scale_fill_gradient(high='red',low='yellow',name='lPoteViviendas')


parks = opq(bb) %>% 
  add_osm_feature(key = 'leisure', value = "park") %>% 
  osmdata_sf() %>% trim_osmdata(bb)
parks_poly = parks$osm_polygons
parks_mpoly = parks$osm_multipolygons
cn = intersect(colnames(parks_poly),colnames(parks_mpoly))
#cn
parks = rbind(parks_poly[,cn],parks_mpoly[,cn])


ggmap(mapa_ba)+ 
  geom_sf(data=parks,fill='darkgreen', size=0.5, color="black", inherit.aes = FALSE)

save.image("cedesu_precios.RData")

