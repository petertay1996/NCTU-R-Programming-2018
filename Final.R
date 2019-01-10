Sys.setlocale(locale = "en_us.UTF-8")
library(dplyr)
library(stringr)
library(data.table)
library(ggplot2)
library(maptools)
library(knitr)
library(mapproj)
library(RColorBrewer)
library(gridExtra)

# Reading Dataframe
df104 <- fread("opendata104m010.csv", h = T)
df104 <- df104[-1]
df104$site_id <- substr(df104$site_id,1,3)
df104 <- as.data.frame(unclass(df104))
df104$marry_count <- as.numeric(df104$marry_count)
df105 <- fread("opendata105m010.csv", h = T)
df105 <- df105[-1]
df105$site_id <- substr(df105$site_id,1,3)
df105 <- as.data.frame(unclass(df105))
df105$marry_count <- as.numeric(df105$marry_count)
df106 <- fread("opendata106m110.csv", h = T)
df106 <- df106[-1]
df106$site_id <- substr(df106$site_id,1,3)
df106 <- as.data.frame(unclass(df106))
df106$marry_count <- as.numeric(df106$marry_count)

# Reading Shapefile
tw_shp <- readShapeSpatial("gadm36_TWN_2.shp")
tw_map <- fortify(tw_shp)
ggplot(tw_map, aes(x = long, y = lat, group=group)) +
  geom_path() + 
  coord_map()

chinese_name <- c("金門縣", "連江縣", "高雄市", "新北市", "臺中市",
                  "臺南市", "臺北市", "彰化縣", "嘉義市", "嘉義縣",
                  "新竹市", "新竹縣", "花蓮縣", "基隆市", "苗栗縣",
                  "南投縣", "澎湖縣", "屏東縣", "臺東縣", "桃園市",
                  "宜蘭縣", "雲林縣")

df104 <- aggregate(df104$marry_count,by=list(df104$site_id),sum)
colnames(df104) <- c("city","marry_count")
count <- sum(df104$marry_count)
df104 <- df104[match(chinese_name,df104$city),]
df104$marry_count <- round(100*df104$marry_count/sum(df104$marry_count),2)
rownames(df104) <- seq(length=nrow(df104))

df105 <- aggregate(df105$marry_count,by=list(df105$site_id),sum)
colnames(df105) <- c("city","marry_count")
count <- c(count,sum(df105$marry_count))
df105 <- df105[match(chinese_name,df105$city),]
df105$marry_count <- round(100*df105$marry_count/sum(df105$marry_count),2)
rownames(df105) <- seq(length=nrow(df105))

df106 <- aggregate(df106$marry_count,by=list(df106$site_id),sum)
colnames(df106) <- c("city","marry_count")
count <- c(count,sum(df106$marry_count))
df106 <- df106[match(chinese_name,df106$city),]
df106$marry_count <- round(100*df106$marry_count/sum(df106$marry_count),2)
rownames(df106) <- seq(length=nrow(df106))

mydata_104 <- data.frame(Year=104,
                     Chi.name=chinese_name,
                     Eng.name=tw_shp$NAME_2,
                     marry_percentage=df104$marry_count)
mydata_105 <- data.frame(Year=105,
                     Chi.name=chinese_name,
                     Eng.name=tw_shp$NAME_2,
                     marry_percentage=df105$marry_count)
mydata_106 <- data.frame(Year=106,
                     Chi.name=chinese_name,
                     Eng.name=tw_shp$NAME_2,
                     marry_percentage=df106$marry_count)
mydata_104$id <- 0
mydata_105$id <- 0
mydata_106$id <- 0
for(i in 0:22){
  mydata_104$id[i] <- i
  mydata_105$id[i] <- i
  mydata_106$id[i] <- i
}
tw_map$id <- as.character(as.integer(tw_map$id)+1)
tw.plot_104<-merge(tw_map,mydata_104,by="id",all.x=T)
tw.plot_105<-merge(tw_map,mydata_105,by="id",all.x=T)
tw.plot_106<-merge(tw_map,mydata_106,by="id",all.x=T)

temp104 <- fread("opendata104m010.csv", h = T)
temp104 <- temp104[-1]
temp104$site_id <- substr(temp104$site_id,1,3)
temp104 <- as.data.frame(unclass(temp104))
temp104$marry_count <- as.numeric(temp104$marry_count)
temp104 <- aggregate(temp104$marry_count,by=list(temp104$site_id,temp104$age),sum)
colnames(temp104) <- c("city","age","marry_count")
temp104 <- split(temp104,temp104$age)
count104 <- c("")
age <- levels(temp104[[1]]$age)
for(i in 1:12){
  count104 <- c(count104,sum(temp104[[i]]$marry_count))
}
count104 <- count104[-1]

temp105 <- fread("opendata105m010.csv", h = T)
temp105 <- temp105[-1]
temp105$site_id <- substr(temp105$site_id,1,3)
temp105 <- as.data.frame(unclass(temp105))
temp105$marry_count <- as.numeric(temp105$marry_count)
temp105 <- aggregate(temp105$marry_count,by=list(temp105$site_id,temp105$age),sum)
colnames(temp105) <- c("city","age","marry_count")
temp105 <- split(temp105,temp105$age)
count105 <- c("")
age <- levels(temp105[[1]]$age)
for(i in 1:12){
  count105 <- c(count105,sum(temp105[[i]]$marry_count))
}
count105 <- count105[-1]

temp106 <- fread("opendata106m110.csv", h = T)
temp106 <- temp106[-1]
temp106$site_id <- substr(temp106$site_id,1,3)
temp106 <- as.data.frame(unclass(temp106))
temp106$marry_count <- as.numeric(temp106$marry_count)
temp106 <- aggregate(temp106$marry_count,by=list(temp106$site_id,temp106$age),sum)
colnames(temp106) <- c("city","age","marry_count")
temp106 <- split(temp106,temp106$age)
count106 <- c("")
age <- levels(temp106[[1]]$age)
for(i in 1:12){
  count106 <- c(count106,sum(temp106[[i]]$marry_count))
}
count106 <- count106[-1]

sumofmarriage <- c(sum(as.numeric(count104)),sum(as.numeric(count105)),sum(as.numeric(count106)))

# Plotting
tw_ppl_dist_104 <- ggplot() +
  geom_polygon(data = tw.plot_104, 
               aes(x = long, y = lat, group = group, 
                   fill = marry_percentage), 
               color = "black", size = 0.25) + 
  coord_map()+
  scale_fill_gradientn(colours = brewer.pal(9,"Reds"),
                       name = "Percentage(%)") +
  #theme_void()+
  labs(title=paste("104 Percentage distribute marriage in Taiwan: ",toString(sumofmarriage[1])), 
       x ="Latitude", y = "Longitude")
ggsave(filename="104_map.png", plot=tw_ppl_dist_104)

tw_ppl_dist_105 <- ggplot() +
  geom_polygon(data = tw.plot_105, 
               aes(x = long, y = lat, group = group, 
                   fill = marry_percentage), 
               color = "black", size = 0.25) + 
  coord_map()+
  scale_fill_gradientn(colours = brewer.pal(9,"Blues"),
                       name = "Percentage(%)") +
  #theme_void()+
  labs(title=paste("105 Percentage distribute marriage in Taiwan: ",toString(sumofmarriage[2])), 
       x ="Latitude", y = "Longitude")
ggsave(filename="105_map.png", plot=tw_ppl_dist_105)

tw_ppl_dist_106 <- ggplot() +
  geom_polygon(data = tw.plot_106, 
               aes(x = long, y = lat, group = group, 
                   fill = marry_percentage), 
               color = "black", size = 0.25) + 
  coord_map()+
  scale_fill_gradientn(colours = brewer.pal(9,"Greens"),
                       name = "Percentage(%)") +
  #theme_void()+
  labs(title=paste("106 Percentage distribute marriage in Taiwan: ",toString(sumofmarriage[3])), 
       x ="Latitude", y = "Longitude")
ggsave(filename="106_map.png", plot=tw_ppl_dist_106)

theme <- theme_get()
theme$text$family <- "STFangsong"
theme_set(theme)

city.plot = rbind(mydata_104,mydata_105,mydata_106)
city.plot$Year <- as.factor(city.plot$Year)
figure = ggplot(city.plot,aes(x=Chi.name,y=marry_percentage,group=Year,colour=Year)) + geom_line() + geom_point() +
  labs(x = "City", y = "Percentage(%)", 
       title = "104 ~ 106 Marriage Percentage of Taiwan")
ggsave(filename="3years_city.png", plot=figure)
figure

for(i in 1:12){
  p <- data.frame(NULL)
  p <- rbind(p,data.frame(Year=104,Total_Marriage=count104[i],Age=age[i]))
  p <- rbind(p,data.frame(Year=105,Total_Marriage=count105[i],Age=age[i]))
  p <- rbind(p,data.frame(Year=106,Total_Marriage=count106[i],Age=age[i]))
  ggplot(p,aes(x=Year,y=Total_Marriage,group=1)) + geom_line()
}
p <- split(p,p$Age)
ggplot(p[[1]],aes(x=Year,y=Total_Marriage,group=1)) + geom_line()

       