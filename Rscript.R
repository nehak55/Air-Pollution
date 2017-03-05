library(readr)
library(dplyr)
library(data.table)
library(sqldf)
library(ggplot2)
library(ggmap)
library(maps)


setwd("/Users/neha/Documents/Capstone/data")
temp = list.files(pattern="*.csv")
tbl = lapply(temp, read_csv) %>% bind_rows()
View(tbl)
 
x1 = sqldf("select longitude, latitude, avg(ozone) as avg_ozone, avg(particullate_matter) as particulate from tbl group by longitude,latitude")
head(x1)

#Here we apply K-means algorithm for k = 4 through 10. The objective is to group 
#the data into clusters and identify the cleanest areas in the city based on ozone 
#level concentration.

# k = 4
result4<-kmeans(x1,4)
result4$centers

# k = 5
result5<-kmeans(x1,5)
result4$centers

# k = 6
result6<-kmeans(x1,6)
result6$centers

# k = 7
result7<-kmeans(x1,7)
result7$centers

# k = 8
result8<-kmeans(x1,8)
result8$centers

# k = 9
result9<-kmeans(x1,9)
result9$centers

# k = 10
result10<-kmeans(x1,10)
result10$centers
#plot the cluster
plot(x1[c("avg_ozone","particulate")], col = result10$cluster )

# Map Visualizations
df = result10$centers
df<-as.data.frame(df)
min_ozone_level = min(df[,3]) # get min level of ozone
lon <- as.vector(df['longitude'])
lat <- as.vector(df['latitude'])
df1 <- as.data.frame(cbind(lon,lat))
newdata <- subset(df, avg_ozone== min_ozone_level, 
                  select=c(longitude, latitude))
long <-as.vector(newdata['longitude'])
lati <-as.vector(newdata['latitude'])

mapgilbert <- get_map(location = c(lon = mean(df1$lon), lat = mean(df1$lat)), zoom = 12, maptype = "satellite", scale = 1)
# plotting the map with some points on it
ggmap(mapgilbert) + geom_point(data = df1, aes(x = lon, y = lat, fill = "red", alpha = 0.1), size = 3, shape = 21) + guides(fill=FALSE, alpha=FALSE, size=FALSE)
