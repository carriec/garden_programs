#HIFLD Prison Boundaries data
#file_js = FROM_GeoJson(url_file_string = "https://opendata.arcgis.com/datasets/2d6109d4127d458eaf0958e4c5296b67_0.geojson", Average_Coordinates = TRUE)
#class(file_js)
#file_js
hifld<-getURL("https://opendata.arcgis.com/datasets/2d6109d4127d458eaf0958e4c5296b67_0.geojson")
hifld <- st_read(hifld)

#View shapefile spatial metadata:
#Geometry type
#st_geometry_type(hifld)
#Coordinate reference system
#st_crs(hifld)
#Extent of area
#st_bbox(hifld)
#View all metadata and attributes
#hifld

#Quick plot
#ggplot() + 
#  geom_sf(data = hifld, size = 0.5, color = "black", fill = "cyan1") + 
# ggtitle("Boundary Plot") + 
#  coord_sf()

#Convert HIFLD spatial data to data frame and save as Rdata
hifld.no_sf <- as.data.frame(hifld) 
class(hifld.no_sf) 
saveRDS(hifld.no_sf, file = "data/hifld_data/2020-09-28/hifld_no_sf.Rds")
rm(hifld)