require(tidyverse)
require(ggplot2)
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

parksOSM = st_make_valid(parksOSM)

# Cargo datos de ciudad

parksCiu = st_make_valid(read_sf('../Encuentro2/espacio-verde-publico/espacio-verde-publico.shp'))
parksCiu = st_transform(parksCiu,crs=22195)
especiales = c('AMEGHINO, FLORENTINO',
               'HOUSSAY, BERNARDO A., Dr.',
               'MISERERE',
               'MARTIN FIERRO',
               'BARRANCAS de BELGRANO',
               'PARQUE PDTE SARMIENTO',
               'CENTENARIO',
               'CHACABUCO',
               'DE LOS PATRICIOS',
               'AVELLANEDA, NICOLÁS, Pdte.',
               'ANDES, LOS',
               'LEZAMA',
               'ALMAGRO',
               'RESERVA ECOLÓGICA',
               'AV.9 DE JULIO')
parksCiu$especial = is.element(parksCiu$nombre_ev,especiales)

# Misiones:

# Agrego MAGNITUDES a SF
## AREAS
parksOSM$area = as.numeric(st_area(parksOSM))
parksCiu$area = as.numeric(st_area(parksCiu))

## PERIMETROS
require(lwgeom)
parksCiu$perimeter = as.numeric(st_perimeter(parksCiu))
parksOSM$perimeter = as.numeric(st_perimeter(parksOSM))

## AREA SOBRE PERIMETRO
parksCiu$ASP = parksCiu$area/parksCiu$perimeter
parksOSM$ASP = parksOSM$area/parksOSM$perimeter


# Paletas para graficar en leaflet
# Perimetro
palPeri = colorNumeric(palette = c('red','blue'),domain = c(parksCiu$perimeter,parksOSM$perimeter)) 

# Area
palArea = colorNumeric(palette = c('red','blue'),domain = c(parksCiu$area,parksOSM$area))

# Area sobre Perímetro
palASP = colorNumeric(palette = c('red','blue'),domain = c(parksCiu$ASP))


# Mapita para mirar
pal = palASP #Elegimos paleta
dataset = parksCiu[parksCiu$clasificac=='PLAZOLETA' & parksCiu$area>5000,] %>% st_transform(crs=4326) # Dataset a mirar (lo transformamos a latlon)
map = leaflet() %>% # Mapa
  addProviderTiles(providers$OpenStreetMap) %>% # Fondo de OSM
  addPolygons(data=dataset,
              popup=~paste(clasificac,nombre_ev),
              color = ~pal(ASP)) %>%
  addLegend(pal=palASP,values=parksCiu$ASP)
map

### Gráficos de distribución
## Exploración de áreas de los espacios (Ciudad) por categoría (clasificac):
parksCiu %>%
  ggplot(aes(x=area/100**2,fill=clasificac),alpha=0.5) +
  #geom_density() + # Para graficar histograma (density plot)
  stat_ecdf() + # Para graficar distribución acumulada
  facet_wrap(~clasificac,scales = "free") +
  theme(legend.position = 'none') +
  #xlim(0,1) + 
  xlab('Area / Manzana (1 ha)')+
  geom_vline(xintercept = 0.025)

parksCiu %>%
  ggplot(aes(x=area/100**2),alpha=0.5) +
  #geom_density() + # Para graficar histograma (density plot)
  stat_ecdf() + # Para graficar distribución acumulada
  theme(legend.position = 'none') +
  xlim(0,10) + 
  xlab('Area / Manzana (1 ha)') +
  scale_x_log10()

(parksCiu$area/100) %>% quantile(c(.1,.25))

parksCiu %>% filter(area/100>10) %>% # Agarro parques mayores a cierta proporcion de manzan
  #count(clasificac) %>%
  ggplot(aes(x=clasificac)) +
  geom_bar(las=2) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  

# Conclusiones
# * Barrio-Complejo y Jardín botánico son únicos
# * Los canteros centrales suelen tener un área muy pequeña, los jardines varían muchísimo
# * Paseos, patios y lugares de juegos inclusivos son todos pequeños.

# En general, lo más claro es que no tiene sentido hacer un corte de área muy duro, ya que de lo charlado en grupo en general concluímos que muchos lugares _pequeñitos_ son igualmente usados.

View(parksCiu)


# Tiene sentido tirar:

catTirar = c('PLAZOLETA','CANTERO CENTRAL')


### Gráficos de distribución
## Exploración de ASP (area sobre perímetro) de los espacios (Ciudad) por categoría (clasificac):
# Para entender la medida:
# En un cuadrado de lado l el área es l^2 y por lo tanto el ASP es l^2/(4*l) = l/4 = H/sqrt(8) (H la hipotenusa). En un círculo el área es pi*r^2 y el perímetro es 2*pi*r, y por lo tanto el ASP es r/2=D/4 (D el díametro). Entonces el ASP mide esencialmente el radio interno del polígono.
# Si pensamos en un rectangulo de lados a y b, el área es a*b y la diagonal es sqrt(a^2+b^2), con lo cual, ASP = a*b/2(a+b) = 1/2(1/a+1/b). Cuando tenemos un cuadrado, esto da l/4. Si mantenemos el área constante (A) entonces b = A/a, ASP=1/2(1/a + a/A).

parksCiu %>%
  ggplot(aes(x=ASP*4,fill=clasificac),alpha=0.5) + # Multiplico ASP*4 para que sea el diámetro efectivo
  geom_density() + # Para graficar histograma (density plot)
  #stat_ecdf() + # Para graficar distribución acumulada
  facet_wrap(~clasificac,scales = "free") +
  theme(legend.position = 'none') +
  #xlim(0,10) + 
  xlab('4*ASP (m, Deff)')
# Conclusión:
# Nuevamente, los radios más grandes los encontramos en en parques y parques semipúblicos.

# Scatterplot de área vs ASP
require(ggrepel)
require(tidyverse)
parksCiu %>% 
  mutate(nombre_ev=na_if(nombre_ev,'S/D')) %>%
  mutate(nombre_ev=na_if(nombre_ev,'SIN NOMBRE')) %>%
  ggplot(aes(x=area/100**2,y=4*ASP,color=clasificac,label=nombre_ev),alpha=0.5) +
  geom_point()+
  geom_label_repel(data=parksCiu[parksCiu$especial,],mapping=aes(x=area/100**2,y=4*ASP,color=clasificac,label=nombre_ev),max.overlaps = 10,cex=2)+
  xlab('Area (ha)') + ylab('4*ASP (Deff)')+
  xlim(0,10) # Jugar con los límites

# A área fija, ASP1>ASP2 significa parques más alargados
# A ASP fijo, Area1>Area2 implicaque P1>P2
# Conclusión: Los lugares más memorables tienen bajo ASP y alta área.


#### CONCLUSIÓN:

# Propongo retirar los lugares con área <0.025 ha (equivale a lugares con tamaño menor al 2.5% del área de una manzana).

# Esto equivale a tirar aproximadamente el 27% de parques de ciudad
sum(parksCiu$area/100**2<.025)
mean(parksCiu$area/100**2<.025)

### REPETIMOS LOS MISMOS GRÁFICOS PARA OSM

### Gráficos de distribución
## AREAS 
df = parksOSM %>% select(area) %>% mutate(fuente='OSM')
df = df %>% bind_rows(.,
                      parksCiu %>% select(area) %>% mutate(fuente='Ciud') )
df %>%
  ggplot(aes(x=area/100**2,col=fuente),alpha=0.5) +
  #geom_density() + # Para graficar histograma (density plot)
  stat_ecdf() + # Para graficar distribución acumulada
  xlim(0,10) + 
  xlab('Area / Manzana (1 ha)') +
  scale_x_log10() + 
  geom_vline(xintercept = .025)

# HAY AREAS NEGATIVAS, producto de errores en la carga de los polígonos %>% resuelto con st_make_valid

mean(parksOSM$area/100**2<.025) # El 11%

## Exploración de ASP (area sobre perímetro) de los espacios:

df = parksOSM %>% select(ASP) %>% mutate(fuente='OSM')
df = df %>% bind_rows(.,
                      parksCiu %>% select(ASP) %>% mutate(fuente='Ciud') )

df %>%
  ggplot(aes(x=ASP*4,color=fuente),alpha=0.5) +     
  #geom_density() +
  stat_ecdf() +
  #xlim(0,10) + 
  xlab('4*ASP (m, Deff)')
# Conclusión:
# Nuevamente, los radios más grandes los encontramos en en parques y parques semipúblicos.

# Scatterplot de área vs ASP
require(ggrepel)
require(tidyverse)
df = parksOSM %>% select(area,ASP) %>% mutate(fuente='OSM')
df = df %>% bind_rows(.,
                      parksCiu %>% select(area,ASP) %>% mutate(fuente='Ciud') )
df %>% 
  ggplot(aes(x=area/100**2,y=4*ASP,color=fuente),alpha=0.5) +
  geom_point()+
  xlab('Area (ha)') + ylab('4*ASP (Deff)')

#  Para pensar: ¿Por qué en OSM ASP sigue creciendo con el área y en Ciudad no? 

# Este mapita responde
df %>% 
  filter(area>50) %>% # Probar con 25 y con 50
  st_transform(crs=4326) %>%
  mutate(color=ifelse(fuente=='OSM','green','red')) %>%
  leaflet() %>% addProviderTiles(provider = providers$OpenStreetMap) %>%
  addPolygons(color=~color)

# Viendo este mapa, interpreto que es porque en Ciudad tiene el entramado itnerno, que aumenta "artificialmente" el perímetro. En otros casos, OSM considera enteramente un espacio que en Ciudad está sólo marcado la parte verde.