###################################################### trade #################################################################


# Load required packages
library(tidyverse)
library(igraph)
library(ggraph)
library(countrycode)
library(openxlsx)
#rm(list = ls())
data = read.xlsx('dataset_GBAD_00 (1).xlsx',sheet = 1)

data <- data[data$agreement_target == 'people',]

##################################################### prepare map  #################################################################

world_map <- map_data("world")
## removing Antarctica
world_map<- world_map %>% filter(region != "Antarctica")

world_map$cowc <- countrycode(world_map$region,  'country.name','cowc')

# Manually add missing countries' coordinates
missing_coords <- tibble(
  cowc = c("HKG", "EU", "YUG", "Israel"),
  long = c(114.1095, 10.4515, 19.2622, 35.2137),
  lat = c(22.3964, 51.1657, 44.1536, 31.7683)
)

# Merge world_map and missing_coords
world_map <- world_map %>%
  bind_rows(missing_coords)

# Function to create a blank world map
create_blank_map <- function() {
  world_data <- map_data("world") %>%
    filter(region != "Antarctica")
  
  ggplot() +
    geom_polygon(data = world_data, aes(x = long, y = lat, group = group),
                 colour = "black", fill = NA, size = 0.03) +
    theme_void() +
    coord_fixed(ratio = 1) +
    theme(plot.background = element_rect(fill = "white"),
          panel.background = element_rect(fill = "white"))
}

##################################################### prepare link 2000###########################################################
data2000 <- data %>%
  filter(signYear <= 2000 & endYear >= 2000
  ) 
# some values including blank 
data2000$cowName1 = gsub('\ ','',data2000$cowName1)
data2000$cowName2 = gsub('\ ','',data2000$cowName2)

# Define nodes and links
nodes <- tibble(
  name = unique(c(data2000$cowName1, data2000$cowName2)),
  region = countrycode(unique(c(data2000$cowName1, data2000$cowName2)), 'cowc', 'un.region.name')
) %>% arrange(region)
# Some values were not matched unambiguously: CZE, EU, HKG, ISRAEL, KOS, MEC, TAW, YUG
nodes = nodes[!duplicated(nodes$name),]
# Manually update the region information for unmatched countries
nodes$region[nodes$name == "ARM"] <- "Asia" # Armenia
nodes$region[nodes$name == "AZE"] <- "Asia" # Azerbaijan
nodes$region[nodes$name == "BEL"] <- "Europe" # Belgium
nodes$region[nodes$name == "CAP"] <- "Africa" # Assuming CAP refers to Cape Verde
nodes$region[nodes$name == "CZE"] <- "Europe" # Czech Republic
nodes$region[nodes$name == "EU"] <- "Europe" # If you want to treat the European Union as a separate entity
nodes$region[nodes$name == "HKG"] <- "Asia" # Hong Kong
nodes$region[nodes$name == "ISR"] <- "Asia" # Israel
nodes$region[nodes$name == "ISRAEL"] <- "Asia" # Israel
nodes$region[nodes$name == "KOS"] <- "Europe" # Kosovo
nodes$region[nodes$name == "KYR"] <- "Asia" # Kyrgyzstan
nodes$region[nodes$name == "MAL"] <- "Asia" # Malaysia
nodes$region[nodes$name == "MEC"] <- "Asia" # Assuming MEC refers to Macau
nodes$region[nodes$name == "NIG"] <- "Africa" # Niger
nodes$region[nodes$name == "TAJ"] <- "Asia" # Tajikistan
nodes$region[nodes$name == "TAW"] <- "Asia" # Assuming TAW refers to Taiwan
nodes$region[nodes$name == "TUR"] <- "Asia" # Turkey
nodes$region[nodes$name == "UKR"] <- "Europe" # Ukraine
nodes$region[nodes$name == "UZB"] <- "Asia" # Uzbekistan
nodes$region[nodes$name == "YUG"] <- "Europe" # Assuming YUG refers to the former Yugoslavia
nodes$region[nodes$name == "Israel"] <- "Asia" # Israel

nodes <- nodes %>% arrange(region)
links2000 <- data2000 %>%
  select(from = cowName1, to = cowName2, agreement_target) %>%
  mutate(agreement_target = factor(agreement_target, levels = c("trade", "people")))

world_map2000 = world_map
# Check if all country codes are present
all(links2000$from %in% world_map2000$cowc)
all(links2000$to %in% world_map2000$cowc)


country_count<- data.frame(cowc = c(links2000$from, links2000$to)) %>% count(cowc)
world_map2000 <- left_join(world_map2000,country_count)

# Calculate country centroids
centroids <- world_map2000 %>%
  group_by(cowc) %>%
  summarise(center_long = mean(long, na.rm = TRUE),
            center_lat = mean(lat, na.rm = TRUE))



# Add link data to the map data
mapdata <- world_map2000  %>% 
  left_join(centroids)

center_map2000 = unique(mapdata[,c('cowc','center_long','center_lat','n')])

links2000 <- links2000 %>%
  left_join(center_map2000, by = c("from" = "cowc")) %>%
  left_join(center_map2000, by = c("to" = "cowc"), suffix = c("_from", "_to"))

# Create connection maps for trade and people
agreement_type = c("trade", "people")[2]

filtered_links2000 <- links2000 %>% filter(agreement_target == agreement_type)


##################################################### prepare link 2022###########################################################
data2022 <- data %>%
  filter(signYear <= 2022 & endYear >= 2022
  ) 
# some values including blank 
data2022$cowName1 = gsub('\ ','',data2022$cowName1)
data2022$cowName2 = gsub('\ ','',data2022$cowName2)

# Define nodes and links
nodes <- tibble(
  name = unique(c(data2022$cowName1, data2022$cowName2)),
  region = countrycode(unique(c(data2022$cowName1, data2022$cowName2)), 'cowc', 'un.region.name')
) %>% arrange(region)
# Some values were not matched unambiguously: CZE, EU, HKG, ISRAEL, KOS, MEC, TAW, YUG
nodes = nodes[!duplicated(nodes$name),]
# Manually update the region information for unmatched countries
nodes$region[nodes$name == "ARM"] <- "Asia" # Armenia
nodes$region[nodes$name == "AZE"] <- "Asia" # Azerbaijan
nodes$region[nodes$name == "BEL"] <- "Europe" # Belgium
nodes$region[nodes$name == "CAP"] <- "Africa" # Assuming CAP refers to Cape Verde
nodes$region[nodes$name == "CZE"] <- "Europe" # Czech Republic
nodes$region[nodes$name == "EU"] <- "Europe" # If you want to treat the European Union as a separate entity
nodes$region[nodes$name == "HKG"] <- "Asia" # Hong Kong
nodes$region[nodes$name == "ISR"] <- "Asia" # Israel
nodes$region[nodes$name == "ISRAEL"] <- "Asia" # Israel
nodes$region[nodes$name == "KOS"] <- "Europe" # Kosovo
nodes$region[nodes$name == "KYR"] <- "Asia" # Kyrgyzstan
nodes$region[nodes$name == "MAL"] <- "Asia" # Malaysia
nodes$region[nodes$name == "MEC"] <- "Asia" # Assuming MEC refers to Macau
nodes$region[nodes$name == "NIG"] <- "Africa" # Niger
nodes$region[nodes$name == "TAJ"] <- "Asia" # Tajikistan
nodes$region[nodes$name == "TAW"] <- "Asia" # Assuming TAW refers to Taiwan
nodes$region[nodes$name == "TUR"] <- "Asia" # Turkey
nodes$region[nodes$name == "UKR"] <- "Europe" # Ukraine
nodes$region[nodes$name == "UZB"] <- "Asia" # Uzbekistan
nodes$region[nodes$name == "YUG"] <- "Europe" # Assuming YUG refers to the former Yugoslavia
nodes$region[nodes$name == "Israel"] <- "Asia" # Israel

nodes <- nodes %>% arrange(region)
links2022 <- data2022 %>%
  select(from = cowName1, to = cowName2, agreement_target) %>%
  mutate(agreement_target = factor(agreement_target, levels = c("trade", "people")))

world_map2022 = world_map
# Check if all country codes are present
all(links2022$from %in% world_map2022$cowc)
all(links2022$to %in% world_map2022$cowc)


country_count<- data.frame(cowc = c(links2022$from, links2022$to)) %>% count(cowc)
world_map2022 <- left_join(world_map2022,country_count)

# Calculate country centroids
centroids <- world_map2022 %>%
  group_by(cowc) %>%
  summarise(center_long = mean(long, na.rm = TRUE),
            center_lat = mean(lat, na.rm = TRUE))



# Add link data to the map data
mapdata <- world_map2022  %>% 
  left_join(centroids)

center_map2022 = unique(mapdata[,c('cowc','center_long','center_lat','n')])

links2022 <- links2022 %>%
  left_join(center_map2022, by = c("from" = "cowc")) %>%
  left_join(center_map2022, by = c("to" = "cowc"), suffix = c("_from", "_to"))

# Create connection maps for trade and people
agreement_type = c("trade", "people")[2]

filtered_links2022 <- links2022 %>% filter(agreement_target == agreement_type)



##################################################### combine 2000 and 2022 ###################################################################


colorScaleLimits <-  c(1,max(c(na.omit(center_map2022$n),na.omit(center_map2000$n))))
#color_agreement  = ''
g2000 <- create_blank_map() +
  geom_point(data = center_map2000,
             aes(x = center_long, y = center_lat, size = n, color = n),
             alpha = 0.7) +
  scale_size_continuous(range = c(1, 4), limits = colorScaleLimits) +
  scale_color_gradientn(colours = RColorBrewer::brewer.pal(9, "YlOrRd")[4:9], limits = colorScaleLimits) +
  geom_curve(data = filtered_links2000, 
             aes(x = center_long_from , y = center_lat_from, xend = center_long_to, yend = center_lat_to), 
             color = 'red', 
             alpha = 0.3, 
             size = 0.15) +
  theme_void() +
  theme(legend.position = "none",
        panel.border = element_blank(),
        plot.title = element_text(color = "Black", size = 16, face = "bold"))+
  coord_fixed(ratio = 1)+
  xlim(c(-180, 180)) +  # Replace with your actual min and max
  ylim(c(-80, 100))

g2022 <- create_blank_map() +
  geom_point(data = center_map2022,
             aes(x = center_long, y = center_lat, size = n, color = n),
             alpha = 0.7)  +
  scale_size_continuous(name = NULL, range = c(1, 4), limits = colorScaleLimits) +
  scale_color_gradientn(name = "Number of agreements", colours = RColorBrewer::brewer.pal(9, "YlOrRd")[4:9], limits = colorScaleLimits) +
  geom_curve(data = filtered_links2022, 
             aes(x = center_long_from , y = center_lat_from, xend = center_long_to, yend = center_lat_to), 
             color = 'red', 
             alpha = 0.3, 
             size = 0.15) +
  theme_void() +
  guides(size = guide_legend(title = "      Number of agreements", title.position = "top", title.hjust = 0.1),
         color = guide_legend(title = "      Number of agreements", title.position = "top", title.hjust = 0.1)) +
  theme(legend.position = "none",
        legend.text = element_text(size = 6,  face = "bold"),
        legend.title = element_text(size = 6,  face = "bold"),
        plot.title = element_text(color = "Black", size = 16, face = "bold"))+
  coord_fixed(ratio = 1)+
  xlim(c(-180, 180)) +  # Replace with your actual min and max
  ylim(c(-80, 100))
  

# Combine the two plots without legends
combined_plots <- cowplot::plot_grid(g2000, g2022, ncol = 1, align = "h", rel_widths = c(1, 1), label_size = 10,label_x = -0.1,
                                     labels = "Agreements to secure travel, 2000 (left) and 2022 (right)")

# Extract the legend
plot_legend <-  cowplot::get_legend(g2022+theme(legend.position = "top"))



# Arrange the combined plot and the legend
final_plot <- gridExtra::grid.arrange(combined_plots, plot_legend, ncol = 1, heights = c(7.2, 0.5))

# Save the final plot
ggsave("combined_plot_2000_2022_travel_with_legend_below.png", final_plot,width = 6,height = 6.5)



       
       
