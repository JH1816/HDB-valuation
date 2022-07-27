library(jsonlite)
library(tidyverse)
library(geosphere)
library(geojsonR)
library(ggplot2)
#Setting seed
set.seed(43)
# Reading data
hdb2000 = read.csv("resale2000.csv")
hdb2012 = read.csv("resale2012.csv")
hdb2015 = read.csv("resale2015.csv")
hdb2017 = read.csv("resale2017.csv")
hdb2017.2 = read.csv("resale2017_2.csv")
s2 = hdb2017.2 %>%
  anti_join(hdb2017)
unique(hdb2015$storey_range)
resalepriceindex = read.csv("resalepriceindex.csv")
preschools = FROM_GeoJson(url_file_string="preschool.geojson")
kindergartens = FROM_GeoJson(url_file_string="kindergartens.geojson")
gyms = FROM_GeoJson(url_file_string="gyms.geojson")
parks = FROM_GeoJson(url_file_string="parks.geojson")
hawkers = FROM_GeoJson(url_file_string="hawker.geojson")
pharmacies = FROM_GeoJson(url_file_string="pharmacy.geojson")
mrts = FROM_GeoJson(url_file_string="mrt.geojson")

# Defining Functions
find_distance = function(point1, point2) {
  return(distm(point1, point2, fun = distHaversine))
}
#point is a vector c(lon,lat)
find_shortest_distance = function(lat, lon, list_of_points) {
  smallest =  4717 
  maxlat = lat+0.030
  minlat = lat-0.030
  maxlon = lon+0.030
  minlon = lon-0.030
  point=c(lon,lat)
  for (point2 in list_of_points) {
    pointlat = point2[2]
    pointlon = point2[1]
    if (pointlat<maxlat&pointlat>minlat|pointlon<maxlon&pointlon>minlon) {
      temp = distm(point, point2, fun = distHaversine)
      if (temp<smallest) {
        smallest=temp
      }
    }
  }
  return(smallest)
}
find_dist_gym = function(lon, lat) {
  return(find_shortest_distance(lat,lon,gymsdf))
}
find_dist_presch = function(lon, lat) {
  return(find_shortest_distance(lat,lon,preschoolsdf))
}
find_dist_kinder = function(lon, lat) {
  return(find_shortest_distance(lat,lon,kindergartensdf))
}
find_dist_park = function(lon, lat) {
  return(find_shortest_distance(lat,lon,parksdf))
}
find_dist_hawker = function(lon, lat) {
  return(find_shortest_distance(lat,lon,hawkersdf))
}
find_dist_pharm = function(lon, lat) {
  return(find_shortest_distance(lat,lon,pharmaciesdf))
}
find_dist_mrt = function(lon, lat) {
  return(find_shortest_distance(lat,lon,mrtsdf))
}
extract = function(x) {
  file_list = x$features
  answer = list()
  len = length(file_list)
  for (i in 1:len) {
    temp = file_list[[i]]$geometry$coordinates
    answer[[i]] <- temp
  }
  return (answer)
}
prepare = function(x) {
  temp = x %>%
    mutate(year=as.numeric(substr(month,1,4)), month=as.numeric(substr(month,6,7)), lease_commence_date=as.numeric(lease_commence_date)) %>%
    mutate(remaining_lease=99-year+lease_commence_date, quarter=((month-1)%/%3)+1, address=paste(block, street_name, sep = " ")) %>%
    mutate(years = year + month/12-1/24) %>%
    select(-street_name,-block)
  return(temp)
}
three_month_avg = function(x) {
  past3 = all_data %>%
  filter(years<x+1/24 & years>=x-5/24) %>%
  summarize(sum(resale_price)/n())
  return(past3[[1]])
}
convert = function(x) {
  correct <- c("01 TO 03","04 TO 06","07 TO 09","10 TO 12",
               "13 TO 15","16 TO 18","19 TO 21","22 TO 24",
               "25 TO 27","28 TO 30","31 TO 33","34 TO 36",
               "37 TO 39","40 TO 42")
  return(correct[x])
}
split_m <- function(m, range, split1, split2, a, b) {
  n = nrow(m)
  n1 = round(n*split1/5)
  n2 = round(n*split2/5)
  if (split1+split2==5){
    x <- c(n1,n-n1)
  } else {
    x<-c(n1,n2,n-n1-n2)
  }
  change = sample(rep(a:b, x))
  m$new_storey <- sapply(change, convert)
  return(m)
}
# Preparing each dataset
hdb2000_s = prepare(hdb2000)
hdb2012_s = prepare(hdb2012)
hdb2015_s = prepare(hdb2015)
hdb2017_s = prepare(hdb2017)
preschoolsdf = extract(preschools)
kindergartensdf = extract(kindergartens)
gymsdf = extract(gyms)
parksdf = extract(parks)
hawkersdf = extract(hawkers)
pharmaciesdf = extract(pharmacies)
mrtsdf = extract(mrts)
# Combine hdb data
all_data = bind_rows(hdb2000_s,hdb2012_s,hdb2015_s,hdb2017_s)
selected = all_data %>%
  filter(town %in% c("SENGKANG","JURONG WEST","TAMPINES", "WOODLANDS", "BUKIT BATOK", "BUKIT MERAH", "TOA PAYOH", "CLEMENTI"))
full_convert = function(range){
  m <- selected %>%
    filter(storey_range==range)
  x = as.numeric(substr(range, 1,2))
  y = as.numeric(substr(range, 7,8))
  if (y-x==2) {
    m = m %>%
      mutate(new_storey=storey_range)
    return(m)
  }
  a = (x-1)%/%3+1
  b = (y-1)%/%3+1
  split1 = ((3-x)%%3)+1
  split2 = min(3,5-split1)
  return(split_m(m,range,split1,split2,a,b))
}
all_range <- unique(selected$storey_range)
len = length(all_range)
df4 <- full_convert(all_range[1])
for (i in 2:len) {
  df4<-bind_rows(df4,full_convert(all_range[i]))
}
# Check that conversion is done correctly
df5 <- df4 %>%
  group_by(storey_range, new_storey) %>%
  summarize(n=n())
# Replace storey_range data with new_storey data
selected <- df4 %>%
  mutate(storey_range=new_storey) %>%
  select(-new_storey)
# Clear environment
#rm(hdb2000, hdb2000_s,hdb2012_s, hdb2012, hdb2015_s, hdb2015, hdb2017_s, hdb2017, preschools, kindergartens, gyms, parks, hawkers,pharmacies,mrts)
#Compare our metric three-month average to HDP Resale Price Index provided by Data.gov
df3 = selected %>%
  select(year, month, years)
df4 = unique(df3)
monthly_avg = df4 %>%
  group_by(years) %>%
  mutate(avg_price = three_month_avg(years)) %>%
  ungroup()
base = monthly_avg %>%
  filter(year==2009 & month==2)
monthly_index = monthly_avg %>%
  group_by(years) %>%
  mutate(index = avg_price/base$avg_price*100, type="three-month") %>%
  select(-avg_price) %>%
  ungroup()
price_index = resalepriceindex %>%
  mutate(year=as.numeric(substr(quarter,1,4)), quarter=as.numeric(substr(quarter, 7,7))) %>%
  filter(year>=2000) %>%
  mutate(years = year + quarter/4-1/8,type="HDB RPI") %>%
  select(years, index, type)
monthly_index_sub = monthly_index %>%
  select(years, index, type)
all_index = bind_rows(monthly_index_sub, price_index)
ggplot(data=all_index, aes(x=years, y=index)) + geom_line(aes(color=type))
ggsave("three-month-incl.png")
# Adding the three-month average to our dataframe
df1 = selected %>%
  inner_join(monthly_index, by=c("year","month")) %>%
  mutate(years=years.x)
  select(-years.x, -years.y)
#Check we did not miss out on any data
df6 = selected %>%
  anti_join(monthly_index, by="year","month")
nrow(df6)
# The 8 towns we selected
unique(df1$town)

# Exporting addresses to python
addresses = unique(df1$address)
# write.csv(addresses, file="addresses.csv")
# use a script in python to scrap coords into updated_addresses_coord
address_coord = read.csv("updated_addresses_coord.csv")
# Script produces some erroneous coords. Extracting them
address_correct_coord = address_coord %>%
  filter(Address %in% addresses) 
missing = address_correct_coord %>%
  filter(Lat=="Nil"|Lon=="Nil") %>%
  mutate(Lat=NULL,Lon=NULL)
error = address_correct_coord %>%
  filter(Lat!="Nil"& Lon!="Nil") %>%
  mutate(Lat=as.numeric(Lat),Lon=as.numeric(Lon)) %>%
  filter(Lat>1.5|Lat<1.1|Lon>104.1|Lon<103.5)
correct = address_coord %>%
  filter(Lat!="Nil"& Lon!="Nil") %>%
  mutate(Lat=as.numeric(Lat),Lon=as.numeric(Lon)) %>%
  filter(!(Lat>1.5|Lat<1.1|Lon>104.1|Lon<103.5))
wrong = bind_rows(missing, error)
# write.csv(wrong, file="wrong_coords.csv")
# manually update wrong coords
updated_wrong_coords = read.csv("updated_wrong_coords.csv")
updated_wrong_coords = updated_wrong_coords %>%
  select(-X) %>%
  filter(Lat!="Nil"& Lon!="Nil") %>%
  mutate(Lat=as.numeric(Lat),Lon=as.numeric(Lon))
all_coords = bind_rows(correct, updated_wrong_coords)
df2 = df1 %>%
  left_join(all_coords,by=c("address"="Address"))
temp = df2 %>%
  filter(is.na(Lat) | is.na(Lon)) %>%
  select(address)
# No more null entries
unique(temp)


df3 = distinct(df2, Lat, Lon)
#rm(address_coord, address_correct_coord, all_coords, all_index, base, correct, df, df1, error, updated_wrong_coords, wrong, price_index, monthly_avg, monthly_index, selected, resalepriceindex)
df3$dist_presch = mapply(find_dist_presch, df3$Lon, df3$Lat)
df3$dist_gym = mapply(find_dist_gym, df3$Lon, df3$Lat)
df3$dist_kinder = mapply(find_dist_kinder, df3$Lon, df3$Lat)
df3$dist_park = mapply(find_dist_park, df3$Lon, df3$Lat)
df3$dist_pharm = mapply(find_dist_pharm, df3$Lon, df3$Lat)
df3$dist_mrt = mapply(find_dist_mrt, df3$Lon, df3$Lat)
df3$dist_hawker = mapply(find_dist_hawker, df3$Lon, df3$Lat)
df2 = df2 %>%
  select(-type)
df = df2 %>%
  inner_join(df3, by=c("Lat","Lon"))
#write.csv(df, file="final.csv")
