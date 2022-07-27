library(jsonlite)
library(tidyverse)
library(geosphere)
library(geojsonR)
library(ggplot2)
library(readr)
#Setting seed
set.seed(43)
# Reading data
hdb2000 = read.csv("data/resale2000.csv")
hdb2012 = read.csv("data/resale2012.csv")
hdb2015 = read.csv("data/resale2015.csv")
hdb2017 = read.csv("data/resale2017.csv")
hdb2017.2 = read.csv("data/resale2017_2.csv")
hdb2017.3 = read.csv("data/hdb2017_Apr11.csv")
s2_data = hdb2017.2 %>%
  anti_join(hdb2017)
s2_data.2 = hdb2017.3 %>%
  anti_join(hdb2017)
new = s2_data.2 %>%
  anti_join(s2_data)
preschools = FROM_GeoJson(url_file_string="data/geojson/preschool.geojson")
gyms = FROM_GeoJson(url_file_string="data/geojson/gyms.geojson")
parks = FROM_GeoJson(url_file_string="data/geojson/parks.geojson")
hawkers = FROM_GeoJson(url_file_string="data/geojson/hawker.geojson")
pharmacies = FROM_GeoJson(url_file_string="data/geojson/pharmacy.geojson")
mrts = FROM_GeoJson(url_file_string="data/geojson/mrt.geojson")

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
    mutate(remaining_lease=99-year+lease_commence_date, quarter=((month-1)%/%3)+1, address=paste(str_to_title(block), str_to_title(street_name), sep = " ")) %>%
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
hdb2000_s = prepare(hdb2000)
hdb2012_s = prepare(hdb2012)
hdb2015_s = prepare(hdb2015)
hdb2017_s = prepare(hdb2017.2)
all_data = bind_rows(hdb2000_s,hdb2012_s,hdb2015_s,hdb2017_s)
s2 = prepare(s2_data)
new_s = prepare(new)
s2.n = bind_rows(s2, new_s)
s2.n$ind <- seq.int(nrow(s2.n))
s2_address = unique(s2$address)
s2.n_address = unique(s2.n$address)
new_address = s2.n_address %>%
  setdiff(s2_address)
#write.csv(new_address, "data/new_address.csv")
#write.csv(s2_address, "s2_address.csv")
#write.csv(s2, "s2_all.csv")
preschoolsdf = extract(preschools)
gymsdf = extract(gyms)
parksdf = extract(parks)
hawkersdf = extract(hawkers)
pharmaciesdf = extract(pharmacies)
mrtsdf = extract(mrts)

s2_coords = read.csv("data/s2_addresses_coord.csv")
s2_n_coords = read.csv("data/new_address_u.csv")
s2_coords_1 = bind_rows(s2_coords, s2_n_coords)
missing = s2_coords_1 %>%
  filter(Lat=="Nil"|Lon=="Nil") %>%
  mutate(Lat=NULL,Lon=NULL)
error = s2_coords_1 %>%
  filter(Lat!="Nil"& Lon!="Nil") %>%
  mutate(Lat=as.numeric(Lat),Lon=as.numeric(Lon)) %>%
  filter(Lat>1.5|Lat<1.1|Lon>104.1|Lon<103.5)
correct = s2_coords_1 %>%
  filter(Lat!="Nil"& Lon!="Nil") %>%
  mutate(Lat=as.numeric(Lat),Lon=as.numeric(Lon)) %>%
  filter(!(Lat>1.5|Lat<1.1|Lon>104.1|Lon<103.5))
wrong = bind_rows(missing, error)
# write.csv(wrong, "data/s2_wrong.csv")
updated_s2 = read.csv("data/s2_wrong_corrected.csv")
s2_coords = bind_rows(correct, updated_s2) 
s2_addresses = s2_coords %>%
  select(x, Lat, Lon) %>%
  mutate(address=str_to_title(x)) %>%
  select(-x)

#rm(address_coord, address_correct_coord, all_coords, all_index, base, correct, df, df1, error, updated_wrong_coords, wrong, price_index, monthly_avg, monthly_index, selected, resalepriceindex)
s2_addresses$dist_presch = mapply(find_dist_presch, s2_addresses$Lon, s2_addresses$Lat)
s2_addresses$dist_gym = mapply(find_dist_gym, s2_addresses$Lon, s2_addresses$Lat)
s2_addresses$dist_park = mapply(find_dist_park, s2_addresses$Lon, s2_addresses$Lat)
s2_addresses$dist_pharm = mapply(find_dist_pharm, s2_addresses$Lon, s2_addresses$Lat)
s2_addresses$dist_mrt = mapply(find_dist_mrt, s2_addresses$Lon, s2_addresses$Lat)
s2_addresses$dist_hawker = mapply(find_dist_hawker, s2_addresses$Lon, s2_addresses$Lat)
df = s2.n %>%
  inner_join(s2_addresses,by="address")
df2 = all_data %>%
  select(year, month, years)
df3 = unique(df2)
monthly_avg = df3 %>%
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
df4 = df %>%
  inner_join(monthly_index, by=c("year","month")) %>%
  mutate(years=years.x, storey_index = as.numeric(substr(storey_range, 7, 8))/3) %>%
  select(-years.x, -years.y, -type, -year, -month, -years, -quarter, -lease_commence_date, -storey_range)
df5 = df4 %>%
  filter(town %in% c("SENGKANG","JURONG WEST","TAMPINES", "WOODLANDS", "BUKIT BATOK", "BUKIT MERAH", "TOA PAYOH", "CLEMENTI"))
# write.csv(df4,"data/s2alltowns.csv")
# write.csv(df5, "data/s2sometowns.csv")
s2_srx = df5 %>%
  select(address, flat_type, floor_area_sqm, ind, storey_index) %>%
  mutate(floor=storey_index*3-1, unit=10) 
write.csv(s2_srx, "data/s2_srx.csv", row.names=FALSE)

temp = read.csv("data/S2_with_SRXprice.csv")
temp2 = read.csv("data/s2_srx_n_SRXprice.csv")
temp3 = bind_rows(temp, temp2)
temp = temp3
missing_srx = temp %>%
  #mutate(pred = substr(SRX_price,2,9)) %>%
  filter(substr(SRX_price,1,1) != '$') %>%
  mutate(address = paste(block, str_to_title(street_name)), street_name=str_to_title(street_name)) %>%
  select(block, street_name, address, flat_type, floor_area_sqm, floor, unit, Postal_code, SRX_price)
correct = temp %>%
  mutate(address = paste(block, str_to_title(street_name)), street_name=str_to_title(street_name)) %>%
  filter(substr(SRX_price,1,1) == '$') %>%
  mutate(SRX_pred = parse_number(SRX_price)) %>%
  mutate(psf = SRX_pred/floor_area_sqm) %>%
  select(-Postal_code, -X, -Unnamed..0)
m1 = missing_srx %>%
  filter(str_length(Postal_code)==6)
m2 = missing_srx %>%
  filter(str_length(Postal_code)!=6) %>%
  select(-Postal_code)

temp1 = s2_data %>% 
  mutate(psf = resale_price/floor_area_sqm) 
summary(temp1$psf)

#write.csv(m1, "data/hpostal.csv", row.names=FALSE)
#write.csv(m2, "data/npostal.csv", row.names=FALSE)
temp = read.csv("data/s2_unknown_price_updated.csv")
correct2 = temp %>%
  filter(substr(SRX_price,1,1) == '$') %>%
  mutate(SRX_pred = parse_number(SRX_price)) %>%
  mutate(psf = SRX_pred/floor_area_sqm) %>%
  select(-X, -Unnamed..0)
c = bind_rows(correct, correct2)
temp1 = c %>%
  filter(psf <= 3348 | psf >=12979) %>%
  select(-SRX_pred, -psf)
wrong2 = temp %>%
  filter(substr(SRX_price,1,1) != '$') %>%
  select(-X, -Unnamed..0)
wrong3 = bind_rows(wrong2, temp1)
# write.csv(wrong3, "data/npostal2.csv", row.names=FALSE)
temp = read.csv("data/npostal2_updated.csv")
correct3 = temp %>%
  filter(substr(SRX_price,1,1) == '$') %>%
  mutate(SRX_pred = parse_number(SRX_price)) %>%
  mutate(psf = SRX_pred/floor_area_sqm) 
c = bind_rows(correct, correct2, correct3)
s2_alltowns <- read.csv("data/s2alltowns.csv")

list_towns <- c("BUKIT BATOK", "BUKIT MERAH", "CLEMENTI","JURONG WEST","SENGKANG","TAMPINES", "TOA PAYOH","WOODLANDS")

List_centroids <- s2_alltowns %>%
  group_by(town) %>%
  summarise(mean_lat = mean(Lat), mean_lon = mean(Lon)) %>%
  filter(town %in% list_towns) %>%
  ungroup()

List_mean_lat <- List_centroids$mean_lat
List_mean_lon <- List_centroids$mean_lon


calc_near_town <- function(lat,lon){
  vec_dist <- c()
  for (i in seq(1,8)){
    ref_town_lat <- List_mean_lat[i]
    ref_town_lon <- List_mean_lon[i] 
    distance <- sqrt((ref_town_lat-lat)**2 + (ref_town_lon-lon)**2)
    vec_dist <- append(vec_dist,distance)
  }
  nearest_town <- list_towns[which.min(vec_dist)]
  return(nearest_town)
}
s2_othertowns <- s2_alltowns %>%
  filter(!(town %in% list_towns)) %>%
  rowwise() %>% 
  mutate(nearest_town = calc_near_town(Lat,Lon))

s2_8towns <- s2_alltowns %>%
  filter(town %in% list_towns) %>%
  rowwise() %>%
  mutate(nearest_town = town)
s2 = bind_rows(s2_othertowns, s2_8towns) %>%
  mutate(town = nearest_town) %>%
  select(-X, -nearest_town)
# write.csv(s2,"data/s2alltowns.csv", row.names=FALSE)
# Calculate % of units in alltowns with flat_model not in our training model
s2 = read.csv("data/s2alltowns.csv")
unique(s2$flat_model)
unique(hdb2017.3$flat_model)
temp2 = read.csv("data/df2017.csv")
unique(temp2$flat_model)
temp = hdb2017.3 %>%
  filter(!(flat_model %in% temp2$flat_model))
temp3 = hdb2017.3 %>%
  group_by(town) %>%
  summarise(n=n()) 
nrow(temp) / nrow(hdb2017.3) *100
some_pred = read.csv("data/sometownpred.csv")
s2 =  some_pred %>%
  inner_join(df, by="ind")
summary(s2$perc_error)
ggplot(some_pred, aes(x=flat_model, y=perc_error)) + 
  geom_boxplot() +
  guides(x=guide_axis(n.dodge=2))
ggsave("images/error_by_flat_model.png", width=10, height=10)
ggplot(some_pred, aes(x=town, y=perc_error)) + 
  geom_boxplot() +
  guides(x=guide_axis(n.dodge=2))
ggsave("images/error_by_town.png", width=10, height=10)
ggplot(some_pred, aes(perc_error)) +
  geom_density()
ggsave("images/error_density.png", width=10, height=10)
table(temp2$flat_model)
