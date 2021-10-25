require(sf)
require(tidyverse)
require(ggplot2)
require(patchwork)
require(ggmap)
require(osmdata)
bb = getbb('Ciudad Autonoma de Buenos Aires, Argentina',format_out = 'polygon')
bbox_ba = getbb("Ciudad Autonoma de Buenos Aires, Argentina")

ciudad = st_polygon(list(bb[[2]][[1]]))
ciudad = st_sfc(ciudad)
st_crs(ciudad) = 4326
parques_full = st_read('parquesCorteArea2_5perc_fullfeatures.geojson')
parques_full = parques_full %>% st_transform(crs=4326)
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
load('../Precenso_2021/precenso2021_06.RData',envir = precenso)
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

