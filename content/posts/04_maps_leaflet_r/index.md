---
title: "Iteractive maps with Leaflet in R"
date: 2025-10-29 20:00:00
description: Iteractive maps with Leaflet in R
menu:
  sidebar:
    name: Iteractive maps (Leaflet | R)
    identifier: maps_leaflet_r
    weight: 40
tags: ["Programmig", "R", "Data"]
categories: ["Programmig"]
---

## Overview

An interactive map is a great tool to support spatial analysis. This
project shows how to create an interactive map using the `leaflet`
package (`library(leaflet)`) in R. The `leaflet` package works with
spatial data, such as coordinates to add markers or polygons to delimit
areas. Here, I present maps that shows the distribution of veterinary
services in Mexico City.

## Libraries

``` r
library(readr)
library(dplyr)
library(leaflet)
library(sf)
```

## Data

I use data from the National Statistical Directory of Economic Units
(DENUE in spanish) of the National Institute of Statistics and Geography
(INEGI in spanish). This directory contains information of all the
establishments in Mexico.

To download the data, you can visit the
<a href="https://www.inegi.org.mx/app/mapa/denue/default.aspx"
target="_blank">DENUE</a> website. The data can be filter by Economic
Activity, Size of Establishments and/or Geographic Area.

For this project, I focus on veterinary services in the Mexico City. I
use the exact locations provided by latitude and longitude, as well as
the name of establishments.

### Manage data

Read the data from DENUE. This file contains all establishments in
Mexico City.

``` r
denue_inegi <- read_csv("denue_inegi_09_.csv", show_col_types = F, locale = locale(encoding = "LATIN1"))
head(denue_inegi[c("nom_estab", "nombre_act","entidad", "municipio", "latitud", "longitud")])
```

    ## # A tibble: 6 × 6
    ##   nom_estab                   nombre_act      entidad municipio latitud longitud
    ##   <chr>                       <chr>           <chr>   <chr>       <dbl>    <dbl>
    ## 1 AJOLOTARIO APATLACO         Piscicultura e… Ciudad… Xochimil…    19.3    -99.1
    ## 2 AJOLOTARIO CARRIZAL         Otra acuicultu… Ciudad… Xochimil…    19.3    -99.1
    ## 3 AJOLOTARIO CHINAMPA ONKALI  Otra acuicultu… Ciudad… Xochimil…    19.3    -99.1
    ## 4 AJOLOTARIO TLAZOCAMA TONANA Otra acuicultu… Ciudad… Xochimil…    19.3    -99.1
    ## 5 AJOLOTERIA APANTLI          Otra acuicultu… Ciudad… Xochimil…    19.3    -99.1
    ## 6 AMILLI TLAPALLI             Piscicultura e… Ciudad… Xochimil…    19.3    -99.1

Filter by veterinary services. Based on the North American Industry
Classification System, Mexico (SCIAN 2023) used by INEGI, the codes for
these activities are 541941, 541942, 541943, and 541944.

``` r
data_vet <- denue_inegi %>% 
  filter(codigo_act >= 541941 & codigo_act <= 541944) 

head(data_vet[c("nom_estab", "nombre_act","entidad", "municipio", "latitud", "longitud")])
```

    ## # A tibble: 6 × 6
    ##   nom_estab                   nombre_act      entidad municipio latitud longitud
    ##   <chr>                       <chr>           <chr>   <chr>       <dbl>    <dbl>
    ## 1 A PATA DE PERRO             Servicios vete… Ciudad… Venustia…    19.4    -99.1
    ## 2 ACUA MASCOTA                Servicios vete… Ciudad… Iztapala…    19.3    -99.0
    ## 3 ACUAMASCOTAS VETERINARIA    Servicios vete… Ciudad… Azcapotz…    19.5    -99.2
    ## 4 ADOPTANDO UN CORAZON CANINO Servicios vete… Ciudad… Miguel H…    19.4    -99.2
    ## 5 ALFA PETS                   Servicios vete… Ciudad… Gustavo …    19.5    -99.1
    ## 6 AMALTEA                     Servicios vete… Ciudad… Coyoacán     19.3    -99.1

For a map by geographic area, I aggregate the data by municipality.

``` r
data_vet$vet <- 1 
data_vet_mun <- data_vet %>%
  group_by(municipio) %>%
  summarise(total_vet = sum(vet))

head(data_vet_mun)
```

    ## # A tibble: 6 × 2
    ##   municipio             total_vet
    ##   <chr>                     <dbl>
    ## 1 Azcapotzalco                107
    ## 2 Benito Juárez               199
    ## 3 Coyoacán                    170
    ## 4 Cuajimalpa de Morelos        40
    ## 5 Cuauhtémoc                  130
    ## 6 Gustavo A. Madero           286

### Polygons

Read the shapefile with the municipality polygons. You can download the
polygons from <a
href="https://datos.cdmx.gob.mx/dataset/alcaldias/resource/8648431b-4f34-4f1a-a4b1-19142f944300"
target="_blank">Data Portal of CDMX</a>.

``` r
shp_mun <- st_read("alcaldias/poligonos_alcaldias_cdmx.shp", quiet = TRUE)
```

Now, we merge the data with the polygons by municipality.

``` r
mun_sf <- merge(shp_mun , data_vet_mun, by.x = "NOMGEO", by.y = "municipio", all.x = TRUE)
```

Get the centroids; this step helps with the elements on the map.

``` r
centroides <- st_centroid(mun_sf)
coords <- st_coordinates(centroides)
mun_sf$lon <- coords[,1]
mun_sf$lat <- coords[,2]
```

Finally, the data by municipality is as follows.

``` r
head(mun_sf)
```

    ## Simple feature collection with 6 features and 7 fields
    ## Geometry type: POLYGON
    ## Dimension:     XY
    ## Bounding box:  xmin: -99.36492 ymin: 19.23257 xmax: -99.09908 ymax: 19.51514
    ## Geodetic CRS:  WGS 84
    ##                  NOMGEO CVEGEO CVE_ENT CVE_MUN total_vet
    ## 1        Álvaro Obregón  09010      09     010       155
    ## 2          Azcapotzalco  09002      09     002       107
    ## 3         Benito Juárez  09014      09     014       199
    ## 4              Coyoacán  09003      09     003       170
    ## 5 Cuajimalpa de Morelos  09004      09     004        40
    ## 6            Cuauhtémoc  09015      09     015       130
    ##                         geometry       lon      lat
    ## 1 POLYGON ((-99.18906 19.3955... -99.24683 19.33617
    ## 2 POLYGON ((-99.18231 19.5074... -99.18211 19.48533
    ## 3 POLYGON ((-99.14762 19.4040... -99.16113 19.38064
    ## 4 POLYGON ((-99.13427 19.3565... -99.15038 19.32667
    ## 5 POLYGON ((-99.25738 19.4011... -99.31094 19.32431
    ## 6 POLYGON ((-99.12951 19.4626... -99.14906 19.43137

## Interactive maps with Leaflet

The first map shows the total number of veterinary services by
municipality in Mexico City. Below, I explain how the code works:

- `leaflet` package uses %\>% as pipe to build the map step by step.

- `addProviderTiles(providers$CartoDB.Positron` adds the basemap.

- `addPolygons()` draws the municipality areas and sets their
  appearance. `popup = ~ NOMGEO` creates a pop-up label that shows the
  municipality name

- `addCircles()` adds the circles to the map. Each circle is centered on
  a municipality using the previously calculated centroid, and its
  radius represents the total number of veterinary services in the
  municipality. `popup = ~paste0(NOMGEO, ": ", total_vet)` works like
  the polygon pop-up but adds the total number of veterinary services.

- `addControl()` places the map title in the top-right corner.

``` r
map1 <- leaflet(mun_sf) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons( color = "darkblue", weight = 2,  opacity = 0.5,
    fillColor = "white", fillOpacity = 0.5,
    popup = ~ NOMGEO) %>%
  addCircles(lng = ~lon, lat = ~lat,radius = ~total_vet*10,      
    color = "white",fillColor = "blue",fillOpacity = 0.8,
    popup = ~paste0(NOMGEO, ": ", total_vet) ) %>%
  addControl(html = "<div style='text-align: center; font-size: 16px;'> <b> Total of veterinary Services in Mexico City </b> </div>", position = "topright" ) 

#map1 

htmlwidgets::saveWidget(map1, "html/map_01.html", 
                        title = "G1.Total of veterinary Services in Mexico City",  selfcontained = TRUE)
```

<iframe src="https://joelcae.github.io/interactive_maps_leaflet_r/html/map_01.html" width="100%" height="450">
</iframe>

This map shows the distribution of all veterinary establishments in
Mexico city. Below, I explain how the code works. Some functions were
already used in previous maps.

- `makeIcon()` upload a custom icon to use as marker on the map.

- `addMarkers` places markers on the map at the specified coordinates
  (latitude and longitude). Each marker represents a veterinary
  establishment.

- `setView()` sets the initial view of the map, defining the center
  coordinates and zoom level.

``` r
icon_gato <- makeIcon(iconUrl = "pets.png",  iconWidth = 15, iconHeight = 15)

map2 <- leaflet(data = data_vet) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addMarkers(lng = ~longitud, lat = ~latitud, icon = icon_gato, label = ~nom_estab) %>%
 addControl(html = "<div style='text-align: center; font-size: 16px;'> <b> veterinary Services in Mexico City </b> </div>", position = "topright" ) %>% 
  setView(lng = -99.140487, lat = 19.430072, zoom = 13)

#map2 

htmlwidgets::saveWidget(map2, "html/map_02.html", 
                        title = "G2.veterinary Services in Mexico City",  selfcontained = TRUE)
```

<iframe src="https://joelcae.github.io/interactive_maps_leaflet_r/html/map_02.html" width="100%" height="450">
</iframe>

Finally, I present a cluster map. The clusters are created based on the
proximity of establishments. Note that the clusters are adjusted as you
zoom in the map or click on a cluster.

In this map, I used the same functions as before, but added
`clusterOptions = markerClusterOptions()`, which creates the clusters on
the map.

``` r
map3 <- leaflet(data = data_vet) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons( data = mun_sf, color = "green", weight = 2, opacity = 0.5,
    fillColor = "white", fillOpacity = 0.5,
    popup = ~ NOMGEO) %>%
  addMarkers(lng = ~longitud, lat = ~latitud, icon = icon_gato, label = ~nom_estab, 
               clusterOptions = markerClusterOptions(showCoverageOnHover = F)) %>%
 addControl(html = "<div style='text-align: center; font-size: 16px;'> <b> veterinary Services in Mexico City </b> </div>", position = "topright" ) 

#map3 

htmlwidgets::saveWidget(map3, "html/map_03.html", 
                        title = "G3.veterinary Services in Mexico City",  selfcontained = TRUE)
```

<iframe src="https://joelcae.github.io/interactive_maps_leaflet_r/html/map_03.html" width="100%" height="450">
</iframe>

## Contact me

<hr>
<style>
.socials {
  text-align: center;
  margin-top: 30px;
  font-family: sans-serif;
}
.socials a {
  text-decoration: none;
  font-size: 18px;
  margin: 0 20px;
  color: #333;
  position: relative;
  font-weight: 500;
}
.socials a::after {
  content: attr(title);
  position: absolute;
  bottom: -25px;
  left: 50%;
  transform: translateX(-50%);
  background-color: #333;
  color: white;
  padding: 3px 8px;
  border-radius: 5px;
  font-size: 12px;
  opacity: 0;
  pointer-events: none;
  transition: opacity 0.3s;
}
.socials a:hover::after {
  opacity: 1;
}
</style>

<div class="socials">

<a href="https://joelcastillo.netlify.app" target="_blank" title="Website">Website</a>
<a href="https://github.com/JoelCae" target="_blank" title="GitHub">GitHub</a>
<a href="https://www.linkedin.com/in/joel-castillo-espinosa" target="_blank" title="LinkedIn">LinkedIn</a>

</div>

<hr>
