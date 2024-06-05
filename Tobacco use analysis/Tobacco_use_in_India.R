#This is an analysis of tobacco usage in India across the states and union territories.

#loading the library
library("tidyverse")
library(sf)
library(tmap)
library(plotly)
library(reshape2)
library(corrplot)

#importing the data
tobacco_data <- read.csv("C:\\Users\\Dell\\Desktop\\Tobacco_use_in_India.csv")

view(tobacco_data)

#1.What is the prevalence of tobacco use across different states and UTs in India?

# Bar chart for Ever Tobacco Users
ggplot(tobacco_data, aes(x = reorder(State_UT, Ever_tobacco_users_percentage), y = Ever_tobacco_users_percentage)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  coord_flip() +
  labs(title = "Prevalence of Ever Tobacco Users across States/UTs in India",
       x = "State/UT",
       y = "Ever Tobacco Users % ") +
  theme_minimal()

# Bar chart for Current Tobacco Users
ggplot(tobacco_data, aes(x = reorder(State_UT,Current_tobacco_users_percentage), y = Current_tobacco_users_percentage)) +
  geom_bar(stat = "identity", fill = "coral") +
  coord_flip() +
  labs(title = "Prevalence of Current Tobacco Users across States/UTs in India",
       x = "State/UT",
       y = "Current Tobacco Users %") +
  theme_minimal()

#creating a map Visualization(chloropleth)

# Load the shapefile
india_shapefile <- st_read("C:\\Users\\Dell\\Downloads\\maps-master\\maps-master\\States\\Admin2.shp")

# Ensure that the state names match between the shapefile and your data

# If necessary, rename states in either the shapefile or the tobacco_data for matching

# Merge the shapefile with the tobacco data
#tobacco_data <- tobacco_data %>%
  #rename(State = State_UT) #rename for merging
  india_data <- merge(india_shapefile, tobacco_data, by.x = "ST_NM", by.y = "State_UT")

# Choropleth map for Ever Tobacco Users
tm_shape(india_data) +
  tm_polygons("Ever_tobacco_users_percentage",
              title = "Ever Tobacco Users %",
              palette = "Blues",
              style = "quantile") +
  tm_layout(title = "Prevalence of Ever Tobacco Users in India")

# Choropleth map for Current Tobacco Users
tm_shape(india_data) +
  tm_polygons("Current_tobacco_users_percentage",
              title = "Current Tobacco Users (%)",
              palette = "Reds",
              style = "quantile") +
  tm_layout(title = "Prevalence of Current Tobacco Users in India")

# 2.How does tobacco use differ between urban and rural areas within each state/UT?

# Prepare data for stacked bar chart
stacked_data <- tobacco_data %>%
  select(State_UT, Area,Ever_tobacco_users_percentage, Current_tobacco_users_percentage) %>%
  gather(key = "Tobacco_Use_Type", value = "Percentage", -State_UT, -Area)

# Stacked bar chart for Ever and Current Tobacco Users in Urban and Rural areas
ggplot(stacked_data, aes(x = reorder(State_UT, Percentage), y = Percentage, fill = Area)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~ Tobacco_Use_Type) +
  coord_flip() +
  labs(title = "Tobacco Use in Urban and Rural Areas across States/UTs in India",
       x = "State_UT",
       y = "Percentage",
       fill = "Area") +
  theme_minimal()

# Grouped bar chart for Ever Tobacco Users in Urban and Rural areas
ggplot(tobacco_data, aes(x = reorder(`State/UT`, `Ever tobacco users (%)`), y = `Ever tobacco users (%)`, fill = Area)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Ever Tobacco Users in Urban and Rural Areas across States/UTs in India",
       x = "State/UT",
       y = "Ever Tobacco Users (%)",
       fill = "Area") +
  coord_flip() +
  theme_minimal()

# Grouped bar chart for Current Tobacco Users in Urban and Rural areas
ggplot(tobacco_data, aes(x = reorder(State_UT, Current_tobacco_users_percentage), y = Current_tobacco_users_percentage, fill = Area)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Current Tobacco Users in Urban and Rural Areas across States/UTs in India",
       x = "State/UT",
       y = "Current Tobacco Users (%)",
       fill = "Area") +
  coord_flip() +
  theme_minimal()

# 3. What are the smoking patterns among current tobacco users (cigarette vs. bidi) across different regions?

# Prepare data for side-by-side bar chart
side_by_side_data <- tobacco_data %>%
  select(State_UT, Current_cigarette_users_percentage, Current_bidi_users_percentage) %>%
  gather(key = "Tobacco_Type", value = "Percentage", -State_UT)

# Side-by-side bar chart for Current Cigarette and Bidi Users
ggplot(side_by_side_data, aes(x = reorder(State_UT, Percentage), y = Percentage, fill = Tobacco_Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Current Cigarette vs. Bidi Users across States/UTs in India",
       x = "State/UT",
       y = "Percentage",
       fill = "Tobacco Type") +
  coord_flip() +
  theme_minimal()

#Pie Chart for all the region combined
# Aggregate data for pie chart
aggregate_data <- tobacco_data %>%
  summarise(Cigarette_Users = sum(Current_cigarette_users_percentage, na.rm = TRUE),
            Bidi_Users = sum(Current_bidi_users_percentage, na.rm = TRUE))

# Convert to long format for plotting
pie_data <- gather(aggregate_data, key = "Tobacco_Type", value = "Percentage")

# Pie chart for overall distribution of Current Cigarette and Bidi Users
pie_chart <- plot_ly(pie_data, labels = ~Tobacco_Type, values = ~Percentage, type = 'pie',
                     textinfo = 'label+percent',
                     insidetextorientation = 'radial') %>%
  layout(title = 'Overall Distribution of Current Cigarette vs. Bidi Users in India')

# Display pie chart
pie_chart

# 4. What is the extent of exposure to tobacco advertisements among students in different states/UTs?

# Bar chart for exposure to tobacco advertisements
filtered_data <- subset(tobacco_data, Area %in% c("Rural", "Urban"))

ggplot(filtered_data, aes(x = reorder(State_UT,Students_who_noticed_cigarette_advertisements_promotions_at_point_of_sale_in_past_30_days_percentage), y = Students_who_noticed_cigarette_advertisements_promotions_at_point_of_sale_in_past_30_days_percentage)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  coord_flip() +
  labs(title = "Students Noticing Cigarette Advertisements at Point of Sale",
       x = "State/UT",
       y = "Percentage") +
  theme_minimal()

ggplot(filtered_data, aes(x = reorder(State_UT,Students_who_saw_tobacco_advertisements_anywhere_in_past_30_days_percentage), y = Students_who_saw_tobacco_advertisements_anywhere_in_past_30_days_percentage)) +
  geom_bar(stat = "identity", fill = "lightgreen") +
  coord_flip() +
  labs(title = "Students Seeing Tobacco Advertisements Anywhere",
       x = "State/UT",
       y = "Percentage") +
  theme_minimal()

# Heatmap to show the exposure of tobacco/cigarette advertisements

# Prepare data for heatmap
heatmap_data <- filtered_data %>%
  select(State_UT,Students_who_noticed_cigarette_advertisements_promotions_at_point_of_sale_in_past_30_days_percentage, Students_who_saw_tobacco_advertisements_anywhere_in_past_30_days_percentage) %>%
  melt(id.vars = 'State_UT')

# Heatmap for exposure to tobacco advertisements
ggplot(heatmap_data, aes(x = variable, y = reorder(State_UT, value), fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  labs(title = "Heatmap of Exposure to Tobacco Advertisements",
       x = "Advertisement Type",
       y = "State/UT") +
  theme_minimal()

# 5. How aware are students and school heads of anti-tobacco policies and their harmful effects in different regions?

# Bar chart for awareness of anti-tobacco policies
ggplot(filtered_data, aes(x = reorder(State_UT,Students_who_were_taught_in_class_about_harmful_effects_of_tobacco_use_during_past_12_months_percentage), y = Students_who_were_taught_in_class_about_harmful_effects_of_tobacco_use_during_past_12_months_percentage)) +
  geom_bar(stat = "identity", fill = "purple") +
  coord_flip() +
  labs(title = "Students Taught about Harmful Effects of Tobacco Use",
       x = "State/UT",
       y = "Percentage") +
  theme_minimal()

ggplot(filtered_data, aes(x = reorder(State_UT, School_heads_aware_of_COTPA_2003_percentage), y = School_heads_aware_of_COTPA_2003_percentage)) +
  geom_bar(stat = "identity", fill = "orange") +
  coord_flip() +
  labs(title = "School Heads Aware of COTPA, 2003",
       x = "State/UT",
       y = "Percentage") +
  theme_minimal()

# Line chart for awareness of anti-tobacco policies
awareness_data <- filtered_data %>%
  select(State_UT, Students_who_were_taught_in_class_about_harmful_effects_of_tobacco_use_during_past_12_months_percentage, School_heads_aware_of_COTPA_2003_percentage) %>%
  melt(id.vars = 'State_UT')

ggplot(awareness_data, aes(x = reorder(State_UT, value), y = value, color = variable, group = variable)) +
  geom_line() +
  geom_point() +
  coord_flip() +
  labs(title = "Awareness of Anti-Tobacco Policies",
       x = "State/UT",
       y = "Percentage") +
  theme_minimal()

# 6. What are the trends in anti-tobacco message exposure among students in different states/UTs?

# Bar chart for exposure to anti-tobacco messages
ggplot(filtered_data, aes(x = reorder(State_UT,Students_who_noticed_anti_tobacco_messages_in_mass_media_in_past_30_days_percentage), y = Students_who_noticed_anti_tobacco_messages_in_mass_media_in_past_30_days_percentage)) +
  geom_bar(stat = "identity", fill = "cyan") +
  coord_flip() +
  labs(title = "Students Noticing Anti-Tobacco Messages in Mass Media",
       x = "State/UT",
       y = "Percentage") +
  theme_minimal()

ggplot(filtered_data, aes(x = reorder(State_UT,Students_who_noticed_anti_tobacco_messages_at_sporting_fairs_concerts_community_events_or_social_gatherings_in_past_30_days_percentage), y = Students_who_noticed_anti_tobacco_messages_at_sporting_fairs_concerts_community_events_or_social_gatherings_in_past_30_days_percentage)) +
  geom_bar(stat = "identity", fill = "magenta") +
  coord_flip() +
  labs(title = "Students Noticing Anti-Tobacco Messages at Events",
       x = "State/UT",
       y = "Percentage") +
  theme_minimal()

# Line chart for exposure to anti-tobacco messages
message_data <- filtered_data %>%
  select(State_UT,State_UT,Students_who_noticed_anti_tobacco_messages_in_mass_media_in_past_30_days_percentage, State_UT,Students_who_noticed_anti_tobacco_messages_at_sporting_fairs_concerts_community_events_or_social_gatherings_in_past_30_days_percentage) %>%
  melt(id.vars = 'State_UT')

ggplot(message_data, aes(x = reorder(State_UT, value), y = value, color = variable, group = variable)) +
  geom_line() +
  geom_point() +
  coord_flip() +
  labs(title = "Exposure to Anti-Tobacco Messages",
       x = "State/UT",
       y = "Percentage") +
  theme_minimal()

# 7. What is the relationship between perceived difficulty in quitting tobacco and the prevalence of tobacco use?

# Scatter plot for perceived difficulty in quitting vs. tobacco use prevalence
ggplot(filtered_data, aes(x = Students_who_thought_it_is_difficult_to_quit_once_someone_starts_smoking_tobacco_percentage, y = Ever_tobacco_users_percentage)) +
  geom_point(color = "blue") +
  labs(title = "Perceived Difficulty in Quitting vs. Ever Tobacco Users",
       x = "Perceived Difficulty in Quitting (%)",
       y = "Ever Tobacco Users (%)") +
  theme_minimal()

ggplot(filtered_data, aes(x = Students_who_thought_it_is_difficult_to_quit_once_someone_starts_smoking_tobacco_percentage, y = Current_tobacco_users_percentage)) +
  geom_point(color = "red") +
  labs(title = "Perceived Difficulty in Quitting vs. Current Tobacco Users",
       x = "Perceived Difficulty in Quitting (%)",
       y = "Current Tobacco Users (%)") +
  theme_minimal()

# 8. What are the common sources for purchasing tobacco products among current smokers?

# Bar chart for sources of purchasing tobacco products
ggplot(filtered_data, aes(x = reorder(State_UT,Current_cigarette_smokers_who_bought_cigarettes_from_a_store_paan_shop_street_vendor_or_vending_machine_percentage), y = Current_cigarette_smokers_who_bought_cigarettes_from_a_store_paan_shop_street_vendor_or_vending_machine_percentage)) +
  geom_bar(stat = "identity", fill = "gold") +
  coord_flip() +
  labs(title = "Sources of Purchasing Cigarettes",
       x = "State/UT",
       y = "Percentage") +
  theme_minimal()

ggplot(filtered_data, aes(x = reorder(State_UT,Current_bidi_smokers_who_bought_bidi_from_a_store_paan_shop_or_street_vendor_percentage), y = Current_bidi_smokers_who_bought_bidi_from_a_store_paan_shop_or_street_vendor_percentage)) +
  geom_bar(stat = "identity", fill = "lightcoral") +
  coord_flip() +
  labs(title = "Sources of Purchasing Bidi",
       x = "State/UT",
       y = "Percentage") +
  theme_minimal()

#Pie chart to show overall distribution of tobacco purchasing sources

# Aggregate data for pie chart
aggregate_purchase_data <- filtered_data %>%
  summarise(Cigarette_Sources = sum(Current_cigarette_smokers_who_bought_cigarettes_from_a_store_paan_shop_street_vendor_or_vending_machine_percentage, na.rm = TRUE),
            Bidi_Sources = sum(Current_bidi_smokers_who_bought_bidi_from_a_store_paan_shop_or_street_vendor_percentage, na.rm = TRUE))

# Convert to long format for plotting
purchase_pie_data <- gather(aggregate_purchase_data, key = "Source", value = "Percentage")

# Pie chart for overall distribution of sources for purchasing tobacco products
purchase_pie_chart <- plot_ly(purchase_pie_data, labels = ~Source, values = ~Percentage, type = 'pie',
                              textinfo = 'label+percent',
                              insidetextorientation = 'radial') %>%
  layout(title = 'Overall Distribution of Sources for Purchasing Tobacco Products')

# Display pie chart
purchase_pie_chart