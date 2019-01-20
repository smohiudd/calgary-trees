library(dplyr)
library(readr)
library(ggplot2)
library(sf)
library(gridExtra)


# read the public trees data set (https://data.calgary.ca/Environment/Public-Trees/tfs4-3wwa)
if(!exists("df1")) { 
  df1 <- read_csv("Public_Trees.csv")
  names(df1) <- tolower(names(df1))
  return(df1)
}

# read the community boundaries and calculate the area for each in sq km
if(!exists("df2")) {
  df2 <- st_read("comm_boundary/geo_export_8f035447-d05d-4524-b9d7-4a6562a760b7.shp")
  df2$area <- as.numeric(st_area(df2) / 1000000 )
  return(df2)
}

# read the boundary of Calgary
if(!exists("df3")) {
  df3 <- st_read("city_boundary/geo_export_1b1ca5c0-7590-47ad-a0f6-c401eaf28b25.shp")
  return(df3)
}

# read the hydrology boundary of Calgary (not used in this version)
if(!exists("df4")) {
  df4 <- st_read("hydrology/geo_export_04ec2eaa-7576-474b-a7e3-4b08f524a504.shp")
  return(df4)
}

tree_species <- df1 %>% select(common_name, genus, species) %>% distinct()

missing_common_name <- df1 %>% filter(is.na(common_name)) %>% count()

common_name_count <- df1 %>% group_by(common_name) %>% summarize(count=n()) %>% arrange(desc(count))

map_theme1 <- theme(axis.text.x = element_text(size = 9),
                    axis.text.y = element_text(size = 9),  
                    axis.title.x = element_text(size = 10),
                    axis.title.y = element_text(size = 10),
                    legend.title = element_text(size = 10))

# bar graph of common name counts
g <- ggplot(common_name_count %>% filter(count>5000 & !is.na(common_name)), aes(x = reorder(common_name, count), y = count)) + geom_bar(stat = "identity",fill="#00bc7d")+geom_text(aes(label=count), hjust=-0.1, size=3)+coord_flip()+labs(x ="Tree Common Name", y = "Count")+map_theme1+ expand_limits(y = 70000)+theme(axis.title.y=element_blank())


# histogram of dbh (trunk diamter)
g1 <- ggplot(df1 %>% group_by(common_name) %>% filter(dbh_cm <100 & n()>20000& !is.na(common_name)), aes(dbh_cm)) + geom_histogram(binwidth=3,aes(fill=common_name))+scale_fill_brewer(palette="Greens")+labs(x ="Diameter at Breast Height (cm)", y = "Count",fill = "Tree Common Name")+map_theme1

map_theme <- theme_void()+
  theme(strip.text = element_text(family = "Helvetica Light", colour='#444444', size=8,margin=margin(b=5),face="bold"))+
  theme(plot.title = element_text(family = "Helvetica Light", hjust = 0.5,size = 12, margin=margin(0,0,10,0)))+
  theme(legend.direction="horizontal",legend.position="top", legend.box = "vertical")+
  theme(legend.text= element_text(family = "Helvetica Light", size = 8, margin=margin(0,0,0,0)),legend.title=element_text(family = "Helvetica Light", size = 9))+
  theme(panel.spacing.x=unit(-1, "lines"))

# select tree counts less than 18,000
tree_limit <- 18000

# plot of tree locations
g2 <- ggplot()+
  geom_sf(data=df3,fill='#444444',linetype = "blank")+
  geom_point(data=df1 %>% group_by(common_name) %>% filter(n()>tree_limit & !is.na(common_name)), aes(longitude, latitude), size = 0.6, stroke = 0, shape = 20, color="green",alpha = 0.04)+
  facet_wrap(~ common_name, ncol=3,strip.position = "bottom")+
  coord_sf(datum=NA)+
  map_theme

# plot of tree density by community 
g3 <- ggplot()+
  geom_sf(data=df3,fill='black',linetype = "blank")+
  geom_sf(data=df1 %>% group_by(common_name) %>% filter(n()>tree_limit & !is.na(common_name)) %>% ungroup() %>% group_by(comm_code, common_name) %>% count() %>% inner_join(df2) %>% mutate(density=n/area), aes(fill=density),linetype = "blank")+
  facet_wrap(~ common_name, ncol=3,strip.position = "bottom")+
  coord_sf(datum=NA)+
  map_theme+ scale_fill_viridis_c(option = "inferno",begin = 0.15)+
  labs(fill = "Density (trees/km^2)")+
  guides(fill = guide_colourbar(barwidth = 8, barheight = 0.25,ticks = FALSE))

# plot of tree density by community 
g4 <- ggplot()+
  geom_sf(data=df3,fill='black',linetype = "blank")+
  geom_sf(data=df1 %>% group_by(comm_code) %>% filter(!is.na(common_name)) %>% count() %>% inner_join(df2) %>% mutate(density=n/area), aes(fill=density),linetype = "blank")+
  coord_sf(datum=NA)+
  map_theme+ scale_fill_viridis_c(option = "inferno",begin = 0.15)+
  labs(fill = "Density (trees/km^2)")+
  guides(fill = guide_colourbar(barwidth = 8, barheight = 0.25,ticks = FALSE))


# plot of tree density by community 
g5 <- ggplot(data=df1 %>% group_by(comm_code) %>% filter(!is.na(common_name)) %>% count() %>% inner_join(df2) %>% mutate(density=n/area) %>% filter(density>1800), aes(x = reorder(name, density), y = density)) + geom_bar(stat = "identity",fill="#57007a")+coord_flip()+labs(y = "Density (trees/km^2)")+map_theme1+theme(axis.title.y=element_blank())

# ggsave("points.png", dpi=180, width=5, height=8)