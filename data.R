library(tidyverse)
library(sf)
library(tmap)
library(janitor)
library(spatstat)
library(readxl)
library(sp)
library(spdep)

a = st_read('/Users/apple/Desktop/Digital\ Viz/w2/TubeEntryExit_/TubeEntryExit_.shp')
b = st_read('/Users/apple/Desktop/Digital\ Viz/w2/TfL_lines_update/TfL_lines_update.shp')

location = read_csv('/Users/apple/Desktop/dig_viz_individual/Stations_20180921.csv', na='')
location = location%>%
  filter(x!=0 & y!=0)%>%
  
  st_as_sf(., coords = c("x", "y"), 
           crs = 4326)


station = read_xlsx('/Users/apple/Desktop/dig_viz_individual/station_exit.xlsx',na = ' ')
station = station %>%
  rename(.,"NAME"="Station")

sta_loc = station %>%
  left_join(location, by="NAME") %>%
  dplyr::distinct(.,nlc,.keep_all=TRUE) %>%
  filter(Zone != "NA")
sta_loc = sta_loc %>%
  rename(.,weekday_entry = Weekday...5)%>%
  rename(.,saturday_entry = Saturday...6)%>%
  rename(.,sunday_entry = Sunday...7)%>%
  rename(.,weekday_exit = Weekday...8)%>%
  rename(.,saturday_exit = Saturday...9)%>%
  rename(.,sunday_exit = Sunday...10)

st_write(sta_loc, "station_with_location.shp", append=FALSE)
#writeOGR(out.df, dsn = '.', layer = 'poly', driver = "ESRI Shapefile")

test1 = st_read('/Users/apple/Desktop/entry_and_exit/entry_and_exit.shp')

sta_entry = sta_loc%>%
  select(-weekday_exit,-saturday_exit,-sunday_exit)

st_write(sta_entry, "station_entry.shp", append=FALSE)

sta_exit = sta_loc%>%
  select(-weekday_entry,-saturday_entry,-sunday_entry)

st_write(sta_exit, "station_exit.shp", append=FALSE)

sta_loc <- sta_loc%>%
  #mutate(new_col=weekday_entry+weekday_exit)%>%
  rename(.,weekday = new_col)

sta_loc <- sta_loc%>%
  mutate(new_col=saturday_entry+saturday_exit)%>%
  rename(.,saturday = new_col)

sta_loc <- sta_loc%>%
  mutate(new_col=sunday_entry+sunday_exit)%>%
  rename(.,sunday = new_col)
#zip -r exit.zip exit

st_write(sta_loc, "entry_and_exit.shp", append=FALSE)


osm<- st_read('/Users/apple/Desktop/ucl/CASA0013/osm_data/gis_osm_pois_a_free_1.shp')
attr <- osm %>%
  filter(.,fclass == 'attraction')
st_write(attr, "attractions.shp", append=FALSE)

as.data.frame(table(osm$fclass))

location <- osm %>%
  filter(.,fclass == 'attraction'|fclass =='mall'|stringr::str_detect(fclass, 'shop'))

st_write(location, "location.shp", append=FALSE)

sta_loc = sta_loc %>%
  rename(.,sat = saturday)%>%
  rename(.,sun = sunday)
sta_loc = sta_loc %>%
  mutate(., weekday = round(weekday))

sta_loc = sta_loc %>%
  rename(.,SAT = sat)%>%
  rename(.,SUN = sun)%>%
  rename(.,WEEKDAY = weekday)



