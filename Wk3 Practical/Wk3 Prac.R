###Practical 03

##3.5.1 identify CRS_ Vector data
#see what is inside
library(sf)
library(here)
st_layers(here("Wk3 Practical", "AUS_gpkg", "gadm36_AUS.gpkg"))
#read gkpg for AUS
library(sf)
Ausoutline <- st_read(here("Wk3 Practical", "AUS_gpkg", "gadm36_AUS.gpkg"), 
                      layer='gadm36_AUS_0')
#check CRS
print(Ausoutline)
library(sf)
st_crs(Ausoutline)$proj4string
#to set a CRS
Ausoutline <- Ausoutline %>%
st_set_crs(., 4326)
#or more concisely
Ausoutline <- st_read(here("Wk3 Practical", "AUS_gpkg", "gadm36_AUS.gpkg"), 
                           layer='gadm36_AUS_0') %>% 
 st_set_crs(4326)
#transform to PRS
AusoutlinePROJECTED <- Ausoutline %>%
  st_transform(.,3112)
print(AusoutlinePROJECTED)
#transform sf to sp(?)
#From sf to sp
AusoutlineSP <- Ausoutline %>%
  as(., "Spatial")
#From sp to sf
AusoutlineSF <- AusoutlineSP %>%
  st_as_sf()

##Raster data
#read data
library(raster)
library(terra)
jan<-terra::rast(here("Wk3 Practical", "raster data source", "wc2.1_5m_tavg", "wc2.1_5m_tavg_01.tif"))
# have a look at the raster layer jan
jan
plot(jan)
#par("mar) > check the margin size
# set the proj 4 to a new object
pr1 <- terra::project(jan, "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
#or....
newproj<-"ESRI:54009"
# get the jan raster and give it the new proj4
pr1 <- jan %>%
  terra::project(., newproj)
plot(pr1)
#back to WGS84
pr1 <- pr1 %>%
  terra::project(., "EPSG:4326")
plot(pr1)

##Data loading
# look in our folder, find the files that end with .tif and 
library(fs)
dir_info("Wk3 Practical/raster data source/wc2.1_5m_tavg") 
library(tidyverse)
listfiles<-dir_info("Wk3 Practical/raster data source/wc2.1_5m_tavg") %>%
  filter(str_detect(path, ".tif")) %>%
  dplyr::select(path)%>%
  pull()
#have a look at the file names 
listfiles
#load all .tif into a SpatRaster
worldclimtemp <- listfiles %>%
  terra::rast()
#have a look at the raster stack
worldclimtemp
# access the january layer
worldclimtemp[[1]]
#rename the layers
month <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
               "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
names(worldclimtemp) <- month
worldclimtemp$Jan

##Raster location
#extract data when using raster stack
site <- c("Brisbane", "Melbourne", "Perth", "Sydney", "Broome", "Darwin", "Orange", 
              "Bunbury", "Cairns", "Adelaide", "Gold Coast", "Canberra", "Newcastle", 
              "Wollongong", "Logan City" )
lon <- c(153.03, 144.96, 115.86, 151.21, 122.23, 130.84, 149.10, 115.64, 145.77, 
         138.6, 153.43, 149.13, 151.78, 150.89, 153.12)
lat <- c(-27.47, -37.91, -31.95, -33.87, 17.96, -12.46, -33.28, -33.33, -16.92, 
         -34.93, -28, -35.28, -32.93, -34.42, -27.64)
#Put all of this inforamtion into one list 
samples <- data.frame(site, lon, lat, row.names="site")
# Extract the data from the Rasterstack for all points 
AUcitytemp<- terra::extract(worldclimtemp, samples)
#add city names
Aucitytemp2 <- AUcitytemp %>% 
  as_tibble()%>% 
  add_column(Site = site, .before = "Jan")


##3.6 descriptive statistics
#data preparation
#using row name
Perthtemp <- Aucitytemp2 %>%
  filter(site=="Perth")
#using row location
Perthtemp <- Aucitytemp2[3,]
#histogram
hist(as.numeric(Perthtemp))
#improve the aesthetics
library(tidyverse)
#define where you want the breaks in the historgram
userbreak<-c(8,10,12,14,16,18,20,22,24,26)
# remove the ID and site columns
Perthtemp <- Aucitytemp2 %>%
  filter(site=="Perth")
t<-Perthtemp %>%
  dplyr::select(Jan:Dec)
hist((as.numeric(t)), 
     breaks=userbreak, 
     col="red", 
     main="Histogram of Perth Temperature", 
     xlab="Temperature", 
     ylab="Frequency")
#check out the histogram info
histinfo <- as.numeric(t) %>%
  as.numeric()%>%
  hist(.)
histinfo
#using more data rather than numeric ones
plot(Ausoutline$geom)
#simplify the .shp
AusoutSIMPLE <- Ausoutline %>%
  st_simplify(., dTolerance = 1000) %>%
  st_geometry()%>%
  plot()
#ensure they have same CRS
print(Ausoutline)
crs(worldclimtemp)
#crop dataset to the map
Austemp <- Ausoutline %>%
  # now crop our temp data to the extent
  terra::crop(worldclimtemp,.)
# plot the output
plot(Austemp)
#cut the locations outside the Aus
exactAus<-terra::mask(Austemp, Ausoutline)
plot(exactAus)
#subset using the known location of the raster
hist(exactAus[[3]], col="red", main ="March temperature")
#Histogram with ggplot
#compatible with ggplot2, using df
exactAusdf <- exactAus %>%
  as.data.frame()
library(ggplot2)
# set up the basic histogram
gghist <- ggplot(exactAusdf, 
                 aes(x=Mar)) + 
  geom_histogram(color="black", 
                 fill="white")+
  labs(title="Ggplot2 histogram of Australian March temperatures", 
       x="Temperature", 
       y="Frequency")
# add a vertical line to the hisogram showing mean tempearture
gghist + geom_vline(aes(xintercept=mean(Mar, 
                                        na.rm=TRUE)),
                    color="blue", 
                    linetype="dashed", 
                    size=1)+
  theme(plot.title = element_text(hjust = 0.5))
#include all 12m data
squishdata<-exactAusdf%>%
  pivot_longer(
    cols = 1:12,
    names_to = "Month",
    values_to = "Temp"
  )
twomonths <- squishdata %>%
  # | = OR
  filter(., Month=="Jan" | Month=="Jun")
meantwomonths <- twomonths %>%
  group_by(Month) %>%
  summarise(mean=mean(Temp, na.rm=TRUE))
meantwomonths
ggplot(twomonths, aes(x=Temp, color=Month, fill=Month)) +
  geom_histogram(position="identity", alpha=0.5)+
  geom_vline(data=meantwomonths, 
             aes(xintercept=mean, 
                 color=Month),
             linetype="dashed")+
  labs(title="Ggplot2 histogram of Australian Jan and Jun
       temperatures",
       x="Temperature",
       y="Frequency")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))
#ajust for binwidth and na
data_complete_cases <- squishdata %>%
  drop_na()%>% 
  mutate(Month = factor(Month, levels = c("Jan","Feb","Mar",
                                          "Apr","May","Jun",
                                          "Jul","Aug","Sep",
                                          "Oct","Nov","Dec")))
# Plot faceted histogram
ggplot(data_complete_cases, aes(x=Temp, na.rm=TRUE))+
  geom_histogram(color="black", binwidth = 5)+
  labs(title="Ggplot2 faceted histogram of Australian temperatures", 
       x="Temperature",
       y="Frequency")+
  facet_grid(Month ~ .)+
  theme(plot.title = element_text(hjust = 0.5))
# interactive histogram using plotly
library(plotly)
# split the data for plotly based on month
jan <- squishdata %>%
  drop_na() %>%
  filter(., Month=="Jan")
jun <- squishdata %>%
  drop_na() %>%
  filter(., Month=="Jun")
# give axis titles
x <- list (title = "Temperature")
y <- list (title = "Frequency")
# set the bin width
xbinsno<-list(start=0, end=40, size = 2.5)
# plot the histogram calling all the variables we just set
ihist<-plot_ly(alpha = 0.6) %>%
  add_histogram(x = jan$Temp,
                xbins=xbinsno, name="January") %>%
  add_histogram(x = jun$Temp,
                xbins=xbinsno, name="June") %>% 
  layout(barmode = "overlay", xaxis=x, yaxis=y)
ihist
# better display data both effectively and efficiently
# mean per month
meanofall <- squishdata %>%
  group_by(Month) %>%
  summarise(mean = mean(Temp, na.rm=TRUE))
# print the top 1
head(meanofall, n=1)
# standard deviation per month
sdofall <- squishdata %>%
  group_by(Month) %>%
  summarize(sd = sd(Temp, na.rm=TRUE))
# maximum per month
maxofall <- squishdata %>%
  group_by(Month) %>%
  summarize(max = max(Temp, na.rm=TRUE))
# minimum per month
minofall <- squishdata %>%
  group_by(Month) %>%
  summarize(min = min(Temp, na.rm=TRUE))
# Interquartlie range per month
IQRofall <- squishdata %>%
  group_by(Month) %>%
  summarize(IQR = IQR(Temp, na.rm=TRUE))
# perhaps you want to store multiple outputs in one list..
lotsofstats <- squishdata %>%
  group_by(Month) %>%
  summarize(IQR = IQR(Temp, na.rm=TRUE), 
            max=max(Temp, na.rm=T),
            min = min(Temp, na.rm=TRUE),
            sd = sd(Temp, na.rm=TRUE),
            mean = mean(Temp, na.rm=TRUE))
view(lotsofstats)
# or you want to know the mean (or some other stat) 
#for the whole year as opposed to each month...
meanwholeyear=squishdata %>%
  summarize(meanyear = mean(Temp, na.rm=TRUE))
