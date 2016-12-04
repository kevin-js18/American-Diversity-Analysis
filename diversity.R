library(ggplot2)
library(maptools)
library(rgeos)
library(Cairo)
library(ggmap)
library(scales)
library(RColorBrewer)
library(readr)
library(dplyr)
library(tidyr)
set.seed(8000)

url1 <- "C:/Users/aet/Desktop/School/R projects/Diversity data/us-counties-diversity-index/diversityindex.csv"
url2 <- "C:/Users/aet/Desktop/School/R projects/Diversity data/cb_2015_us_county_20m/cb_2015_us_county_20m.shp"

diversity_data <- read_csv(url1)

head(diversity_data)
glimpse(diversity_data)

diversity_data <- separate(diversity_data, Location, c("County", "State"), sep = ", ", verbose = TRUE)

for (i in 1:length(diversity_data$State)) {
  if (is.na(diversity_data$State[i]) == TRUE) {
    diversity_data <- diversity_data[-i, ]
  }
}

usa.shp <- readShapeSpatial(url2)

usa.shp <- subset(usa.shp, usa.shp$STATEFP != 72)

diversity_data$County[1] <- "Aleutians West"
diversity_data$County[5] <- "Aleutians East"
diversity_data$County[2299] <- "Oglala Lakota"
diversity_data$County[grep("Ana", diversity_data$County)] <- "Doña Ana"
diversity_data$County[grep("Wade Hampton", diversity_data$County)] <- "Kusilvak"

diversity_data$County <- sapply(strsplit(diversity_data$County, " Count"), "[", 1)
diversity_data$County <- sapply(strsplit(diversity_data$County, " Census Are"), "[", 1)
diversity_data$County <- sapply(strsplit(diversity_data$County, " Paris"), "[", 1)
diversity_data$County <- sapply(strsplit(diversity_data$County, " cit"), "[", 1)
diversity_data$County <- sapply(strsplit(diversity_data$County, " City and Boroug"), "[", 1)
diversity_data$County <- sapply(strsplit(diversity_data$County, " Boroug"), "[", 1)
diversity_data$County <- sapply(strsplit(diversity_data$County, " Municipalit"), "[", 1)

names(diversity_data)[1] <- "id"

usa.shp.f <- fortify(usa.shp, region = "NAME")

merge.shp <- merge(usa.shp.f, diversity_data, by = "id", all.x=TRUE)
for (i in 3032:length(merge.shp$long)) {
  if (merge.shp$long[i] > 100) {
    merge.shp$long[i] <- -merge.shp$long[i]
  }
}

alaska.shp <- subset(merge.shp, merge.shp$long < -130 & merge.shp$lat > 38)
hawaii.shp <- subset(merge.shp, merge.shp$long < -130 & merge.shp$lat < 38)
merge.shp <- subset(merge.shp, merge.shp$long > - 130)

final.plot <- merge.shp[order(merge.shp$order), ]
alaska.plot <- alaska.shp[order(alaska.shp$order), ]
hawaii.plot <- hawaii.shp[order(hawaii.shp$order), ]

# Alaskan plots
ggplot() +
  geom_polygon(data = alaska.plot,
               aes(x = long, y = lat, group = group, fill = `Asian alone, percent, 2013`),
               color = "grey", size = 0.25) +
  coord_map() +
  scale_fill_gradient(low = "white", high = "purple", limits = c(0, 50)) +
  theme_nothing(legend = TRUE) +
  ggtitle("Asian")

ggplot() +
  geom_polygon(data = alaska.plot,
               aes(x = long, y = lat, group = group, fill = `Black or African American alone, percent, 2013`),
               color = "grey", size = 0.25) +
  coord_map() +
  scale_fill_gradient(low = "white", high = "purple", limits = c(0, 100)) +
  theme_nothing(legend = TRUE) +
  ggtitle("Black")

ggplot() +
  geom_polygon(data = alaska.plot,
               aes(x = long, y = lat, group = group, fill = `Hispanic or Latino, percent, 2013`),
               color = "grey", size = 0.25) +
  coord_map() +
  scale_fill_gradient(low = "white", high = "purple", limits = c(0, 100)) +
  theme_nothing(legend = TRUE) +
  ggtitle("Hispanic")

ggplot() +
  geom_polygon(data = alaska.plot,
               aes(x = long, y = lat, group = group, fill = `American Indian and Alaska Native alone, percent, 2013`),
               color = "grey", size = 0.25) +
  coord_map() +
  scale_fill_gradient(low = "white", high = "purple", limits = c(0, 100)) +
  theme_nothing(legend = TRUE) +
  ggtitle("Indian")

ggplot() +
  geom_polygon(data = alaska.plot,
               aes(x = long, y = lat, group = group, fill = `Native Hawaiian and Other Pacific Islander alone, percent,`),
               color = "grey", size = 0.25) +
  coord_map() +
  scale_fill_gradient(low = "white", high = "purple", limits = c(0, 20)) +
  theme_nothing(legend = TRUE) +
  ggtitle("Pacific")

ggplot() +
  geom_polygon(data = alaska.plot,
               aes(x = long, y = lat, group = group, fill = `White alone, not Hispanic or Latino, percent, 2013`),
               color = "grey", size = 0.25) +
  coord_map() +
  scale_fill_gradient(low = "white", high = "purple", limits = c(0, 100)) +
  theme_nothing(legend = TRUE) +
  ggtitle("White")

ggplot() +
  geom_polygon(data = alaska.plot,
               aes(x = long, y = lat, group = group, fill = `Two or More Races, percent, 2013`),
               color = "grey", size = 0.25) +
  coord_map() +
  scale_fill_gradient(low = "white", high = "purple", limits = c(0, 30)) +
  theme_nothing(legend = TRUE) +
  ggtitle("Biracial")

# Hawaiian Plots

ggplot() +
  geom_polygon(data = hawaii.plot,
               aes(x = long, y = lat, group = group, fill = `Asian alone, percent, 2013`),
               color = "grey", size = 0.25) +
  coord_map() +
  scale_fill_gradient(low = "white", high = "purple", limits = c(0, 50)) +
  theme_nothing(legend = TRUE) +
  ggtitle("Asian")

ggplot() +
  geom_polygon(data = hawaii.plot,
               aes(x = long, y = lat, group = group, fill = `Black or African American alone, percent, 2013`),
               color = "grey", size = 0.25) +
  coord_map() +
  scale_fill_gradient(low = "white", high = "purple", limits = c(0, 100)) +
  theme_nothing(legend = TRUE) +
  ggtitle("Black")

ggplot() +
  geom_polygon(data = hawaii.plot,
               aes(x = long, y = lat, group = group, fill = `Hispanic or Latino, percent, 2013`),
               color = "grey", size = 0.25) +
  coord_map() +
  scale_fill_gradient(low = "white", high = "purple", limits = c(0, 100)) +
  theme_nothing(legend = TRUE) +
  ggtitle("Hispanic")

ggplot() +
  geom_polygon(data = hawaii.plot,
               aes(x = long, y = lat, group = group, fill = `American Indian and Alaska Native alone, percent, 2013`),
               color = "grey", size = 0.25) +
  coord_map() +
  scale_fill_gradient(low = "white", high = "purple", limits = c(0, 100)) +
  theme_nothing(legend = TRUE) +
  ggtitle("Indian")

ggplot() +
  geom_polygon(data = hawaii.plot,
               aes(x = long, y = lat, group = group, fill = `Native Hawaiian and Other Pacific Islander alone, percent,`),
               color = "grey", size = 0.25) +
  coord_map() +
  scale_fill_gradient(low = "white", high = "purple", limits = c(0, 20)) +
  theme_nothing(legend = TRUE) +
  ggtitle("Pacific")

ggplot() +
  geom_polygon(data = hawaii.plot,
               aes(x = long, y = lat, group = group, fill = `White alone, not Hispanic or Latino, percent, 2013`),
               color = "grey", size = 0.25) +
  coord_map() +
  scale_fill_gradient(low = "white", high = "purple", limits = c(0, 100)) +
  theme_nothing(legend = TRUE) +
  ggtitle("White")

ggplot() +
  geom_polygon(data = hawaii.plot,
               aes(x = long, y = lat, group = group, fill = `Two or More Races, percent, 2013`),
               color = "grey", size = 0.25) +
  coord_map() +
  scale_fill_gradient(low = "white", high = "purple", limits = c(0, 30)) +
  theme_nothing(legend = TRUE) +
  ggtitle("Biracial")

# Continental plots
ggplot() +
  geom_polygon(data = final.plot,
               aes(x = long, y = lat, group = group, fill = `Asian alone, percent, 2013`),
               color = "grey", size = 0.25) +
  coord_map() +
  scale_fill_gradient(name = "Percent", low = "white", high = "purple", limits = c(0, 50)) +
  theme_nothing(legend = TRUE) +
  ggtitle("Percent of Asian-American Alone by County, United States, 2013")

ggplot() +
  geom_polygon(data = final.plot,
               aes(x = long, y = lat, group = group, fill = `Black or African American alone, percent, 2013`),
               color = "grey", size = 0.25) +
  coord_map() +
  scale_fill_gradient(name = "Percent", low = "white", high = "purple", limits = c(0, 100)) +
  theme_nothing(legend = TRUE) +
  ggtitle("Percent of Black or African-American Alone by County, United States, 2013")

ggplot() +
  geom_polygon(data = final.plot,
               aes(x = long, y = lat, group = group, fill = `Hispanic or Latino, percent, 2013`),
               color = "grey", size = 0.25) +
  coord_map() +
  scale_fill_gradient(name = "Percent", low = "white", high = "purple", limits = c(0, 100)) +
  theme_nothing(legend = TRUE) +
  ggtitle("Percent of Hispanic of Latino Alone by County, United States, 2013")

ggplot() +
  geom_polygon(data = final.plot,
               aes(x = long, y = lat, group = group, fill = `American Indian and Alaska Native alone, percent, 2013`),
               color = "grey", size = 0.25) +
  coord_map() +
  scale_fill_gradient(name = "Percent", low = "white", high = "purple", limits = c(0, 100)) +
  theme_nothing(legend = TRUE) +
  ggtitle("Percent of American Indian and Alaska Native Alone by County, United States, 2013")

ggplot() +
  geom_polygon(data = final.plot,
               aes(x = long, y = lat, group = group, fill = `Native Hawaiian and Other Pacific Islander alone, percent,`),
               color = "grey", size = 0.25) +
  coord_map() +
  scale_fill_gradient(name = "Percent", low = "white", high = "purple", limits = c(0, 20)) +
  theme_nothing(legend = TRUE) +
  ggtitle("Percent of Native Hawaiian and Other Pacific Islander Alone by County, United States, 2013")

ggplot() +
  geom_polygon(data = final.plot,
               aes(x = long, y = lat, group = group, fill = `White alone, not Hispanic or Latino, percent, 2013`),
               color = "grey", size = 0.25) +
  coord_map() +
  scale_fill_gradient(name = "Percent", low = "white", high = "purple", limits = c(0, 100)) +
  theme_nothing(legend = TRUE) +
  ggtitle("Percent of White alone, non-Hispanic or Latino by County, United States, 2013")

ggplot() +
  geom_polygon(data = final.plot,
               aes(x = long, y = lat, group = group, fill = `Two or More Races, percent, 2013`),
               color = "grey", size = 0.25) +
  coord_map() +
  scale_fill_gradient(name = "Percent", low = "white", high = "purple", limits = c(0, 30)) +
  theme_nothing(legend = TRUE) +
  ggtitle("Percent of Two or More Races by County, United States, 2013")

# Plot controlled for Hawaii
ggplot() +
  geom_polygon(data = final.plot,
               aes(x = long, y = lat, group = group, fill = `Native Hawaiian and Other Pacific Islander alone, percent,`),
               color = "grey", size = 0.25) +
  coord_map() +
  scale_fill_gradient(name = "Percent", low = "white", high = "purple") +
  theme_nothing(legend = TRUE) +
  ggtitle("Percent of Native Hawaiian and Other Pacific Islander Alone by County, United States, 2013, excluding Hawaii")

ggplot() +
  geom_polygon(data = alaska.plot,
               aes(x = long, y = lat, group = group, fill = `Native Hawaiian and Other Pacific Islander alone, percent,`),
               color = "grey", size = 0.25) +
  coord_map() +
  scale_fill_gradient(low = "white", high = "purple") +
  theme_nothing(legend = TRUE) +
  ggtitle("Pacific")
