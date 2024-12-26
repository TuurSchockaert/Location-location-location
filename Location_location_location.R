###############################################################################

## Old Prints: Location ########################################################

###############################################################################

# loading packages #-----------------------------------------------------------

library(effects)
library(dplyr)
library(tidyverse)
library(mapview)
library(ggplot2)
library(igraph)
library(ggraph)

rm(list = ls())
options(scipen = 10)

# read data set #--------------------------------------------------------------

df <- read.csv("Location_location_location.csv", sep=",", header=TRUE,
               stringsAsFactors = TRUE)

str(df)

# filter data set #------------------------------------------------------------

# Filter out all rows that got wrongly duplicated in OpenRefine
df_filtered <- df %>%
  filter(!is.na(unique_id)) %>%
  droplevels()

# Due to the fact that RStudio has difficulty of working with NAs, several
# dataframes will be created

# Filter out rows with missing values for the Place of Publication
df_filtered1 <- df_filtered %>%
  filter(!is.na(Place_of_Publication_1)) %>%
  droplevels()

# Filter out rows with missing values for the Head Author's Place of Birth
df_filtered2 <- df_filtered %>%
  filter(!is.na(Head_Author_Place_of_Birth)) %>%
  distinct(Head_Author, .keep_all = TRUE) %>%
  droplevels()

# Filter out rows with missing coordinates for the Place of Birth
df_filtered3 <- df_filtered2 %>%
  filter(!is.na(Place_of_Birth_Coordinate)) %>%
  droplevels()

# Filter out rows with missing values for Owning Library (and its city's
# coordinates)
df_filtered4 <- df_filtered %>%
  filter(!is.na(Owning_Library_1_City_Coordinate)) %>%
  droplevels()

# Filter out all rows where no specific Date of Publication is specified
# that includes NAs, timeframes("-"), and estimates ("c.")
df_filtered5 <- df_filtered %>%
  filter(!grepl("c\\.|-", Date_of_Publication_1)) %>%
  droplevels()
df_filtered5 <- df_filtered5 %>%
  filter(!is.na(Date_of_Publication_1)) %>%
  droplevels()

# Convert Factor to Number
df_filtered5$Date_of_Publication <- as.numeric(as.character
                                    (df_filtered5$Date_of_Publication_1))

# create new variables #-------------------------------------------------------

# Create variable decade based on the Date of Publication
df_filtered5$decade <- ifelse(df_filtered5$Date_of_Publication < 1561, "1551-1560",
                       ifelse(df_filtered5$Date_of_Publication < 1571, "1561-1570",
                       ifelse(df_filtered5$Date_of_Publication < 1581, "1571-1580",
                       ifelse(df_filtered5$Date_of_Publication < 1591, "1581-1590",
                       ifelse(df_filtered5$Date_of_Publication < 1601, "1591-1600",
                       "1601-1700")))))

# Create a more abstract classification for the Place of Publication
df_filtered1$Place_of_Publication_Abstract <- ifelse(df_filtered1$Place_of_Publication_1 == "Antwerp", "Antwerp",
                                              ifelse(df_filtered1$Place_of_Publication_1 == "Leuven", "Leuven",
                                              "Rest"))
df_filtered1$Place_of_Publication_Abstract <- as.factor(df_filtered1$Place_of_Publication_Abstract)

# dataset exploration: descriptive statistics #--------------------------------

# PLACE OF PUBLICATION
nrow(df_filtered1)
nlevels(df_filtered1$Place_of_Publication_1)
levels(df_filtered1$Place_of_Publication_1)

# table
t <- xtabs(~ Place_of_Publication_1, data = df_filtered1) %>%
  print()

# sorted tables
t %>%
  as.data.frame() %>%
  arrange(desc(Freq)) %>%
  print()

result <- t %>%
  prop.table() %>%
  round(4) %>%
  as.data.frame() %>%
  arrange(desc(Freq))

sum <- result$Freq[1] + result$Freq[2]
sum

# plot
ggplot(df_filtered1) +
  geom_bar(aes(x = Place_of_Publication_1, fill = Place_of_Publication_1)) +
  geom_text(stat = "count", aes(x = Place_of_Publication_1, y = ..count.., label = ..count..), 
            vjust = -0.5) +
  labs(title = "",
         x = "Place of Publication",
         y = "Number of Publications") +
  theme(legend.position = "none")

# map
mapview(df_filtered1, xcol = "Place_of_Publication_1_Longitude",
        ycol = "Place_of_Publication_1_Latitude", crs = 4269, grid = FALSE)

# heat map
df_filtered_size <- df_filtered1 %>%
  group_by(Place_of_Publication_1_Longitude, Place_of_Publication_1_Latitude) %>%
  summarise(frequency = n()) %>%
  ungroup()

mapview(df_filtered_size, 
        xcol = "Place_of_Publication_1_Longitude", 
        ycol = "Place_of_Publication_1_Latitude", 
        crs = 4269, 
        grid = FALSE, 
        legend = TRUE, 
        zcol = "frequency")

# abstraction
ggplot(df_filtered1) +
  geom_bar(aes(x = Place_of_Publication_Abstract, fill = Place_of_Publication_Abstract))

# PLACE/COUNTRY OF BIRTH
nrow(df_filtered2)
nlevels(df_filtered2$Head_Author_Place_of_Birth)
levels(df_filtered2$Head_Author_Place_of_Birth)
nlevels(df_filtered2$Head_Author_Country_of_Birth)

# tables
t <- xtabs(~ Head_Author_Place_of_Birth, data = df_filtered2) %>%
  print()
u <- xtabs(~ Head_Author_Country_of_Birth, data = df_filtered2) %>%
  print()

# sorted tables
t %>%
  as.data.frame() %>%
  arrange(desc(Freq)) %>%
  print()
u %>%
  as.data.frame() %>%
  arrange(desc(Freq)) %>%
  print()

t %>%
  prop.table() %>%
  round(4) %>%
  as.data.frame() %>%
  arrange(desc(Freq))
u %>%
  prop.table() %>%
  round(4) %>%
  as.data.frame() %>%
  arrange(desc(Freq))

# map
mapview(df_filtered3, xcol = "Place_of_Birth_Longitude", ycol = "Place_of_Birth_Latitude", crs = 4269, grid = FALSE)

# OWNING LIBRARY
nrow(df_filtered4)
nlevels(df_filtered4$Owning_Library_1)
nlevels(df_filtered4$Owning_Library_1_City)
nlevels(df_filtered4$Country_Owning_Library_1)
levels(df_filtered4$Country_Owning_Library_1)

# tables
t <- xtabs(~ Owning_Library_1, data = df_filtered4) %>%
  print()
u <- xtabs(~ Owning_Library_1_City, data = df_filtered4) %>%
  print()
v <- xtabs(~ Country_Owning_Library_1, data = df_filtered4) %>%
  print()

# sorted tables
t %>%
  as.data.frame() %>%
  arrange(desc(Freq)) %>%
  print()
u %>%
  as.data.frame() %>%
  arrange(desc(Freq)) %>%
  print()
v %>%
  as.data.frame() %>%
  arrange(desc(Freq)) %>%
  print()

t %>%
  prop.table() %>%
  round(2) %>%
  as.data.frame() %>%
  arrange(desc(Freq))
u %>%
  prop.table() %>%
  round(2) %>%
  as.data.frame() %>%
  arrange(desc(Freq))
v %>%
  prop.table() %>%
  round(2) %>%
  as.data.frame() %>%
  arrange(desc(Freq))

# more complex table
t_df <- df_filtered4 %>%
  group_by(Owning_Library_1, Country_Owning_Library_1) %>%
  summarise(Number = n(), .groups = 'drop') %>%
  mutate(Frequency = 100*round(Number / sum(Number), 4)) %>%
  arrange(desc(Frequency))

# plot
ggplot(df_filtered4) +
  geom_bar(aes(x = Country_Owning_Library_1, fill = Country_Owning_Library_1)) +
  geom_text(stat = "count", aes(x = Country_Owning_Library_1, y = ..count.., label = ..count..), 
            vjust = -0.5) +
  labs(title = "",
       x = "Country of Owning Library",
       y = "Number of Holdings") +
  theme(legend.position = "none")

# map
mapview(df_filtered4, xcol = "Owning_Library_1_City_Longitude", ycol = "Owning_Library_1_City_Latitude", crs = 4269, grid = FALSE)

# DATE OF PUBLICATION
nrow(df_filtered5)

# table
t <- xtabs(~ decade, data = df_filtered5) %>%
  print()

t %>%
  prop.table() %>%
  round(4)

# plot
ggplot(df_filtered5) +
  geom_bar(aes(x = Date_of_Publication_1, fill = Date_of_Publication_1))

ggplot(df_filtered5) +
  geom_bar(aes(x = decade, fill = decade)) +
  geom_text(stat = "count", aes(x = decade, y = ..count.., label = ..count..), 
            vjust = -0.5) +
  labs(title = "",
       x = "Decade",
       y = "Number of Publications") +
  theme(legend.position = "none")

# plot including Place of Publication
df_filtered6 <- df_filtered5 %>%
  filter(!is.na(Place_of_Publication_1)) %>%
  droplevels()

df_filtered6$Place_of_Publication_Abstract <- ifelse(df_filtered6$Place_of_Publication_1 == "Antwerp", "Antwerp",
                                              ifelse(df_filtered6$Place_of_Publication_1 == "Leuven", "Leuven",
                                              "Rest"))
df_filtered6$Place_of_Publication_Abstract <- as.factor(df_filtered6$Place_of_Publication_Abstract)

ggplot(df_filtered6) +
  geom_bar(aes(x = decade, fill = Place_of_Publication_Abstract), position = "fill") +
  labs(title = "",
       x = "Decade",
       y = "Percentage",
       fill = "Place of Publication")

# statistical analysis: linear regression #------------------------------------
df_filtered7 <- droplevels(filter(df_filtered6, Place_of_Publication_Abstract != "Rest"))
df_filtered7 <- droplevels(filter(df_filtered7, decade != "1601-1700"))

# general plot
df_count <- df_filtered7 %>%
  group_by(Date_of_Publication, Place_of_Publication_Abstract) %>%
  summarise(count = n()) %>%
  ungroup()

ggplot(df_count, aes(x = Date_of_Publication, y = count, color = Place_of_Publication_Abstract, group = Place_of_Publication_Abstract)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "",
       x = "Date of Publication",
       y = "Number of Publications",
       color = "Place of Publication") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# linear regression
df_filtered7$Place_of_Publication_Abstract <- relevel(df_filtered7$Place_of_Publication_Abstract, ref = "Antwerp")

summary(model <- glm(Place_of_Publication_Abstract ~ Date_of_Publication,
                     data = df_filtered7, family = binomial))
plot(allEffects(model), ylim=c(0,1), rescale.axis=FALSE, xlab="Date of Publication",
     ylab="Chance of Publishing in Leuven", main="")

# network analysis #-----------------------------------------------------------

# Create a new data frame with Head_Author_Place_of_Birth and
# Place_of_Publication_1 as clean columns
network <- df_filtered %>%
  filter(!is.na(Head_Author_Place_of_Birth) & !is.na(Place_of_Publication_1)) %>%
  dplyr::select(Head_Author_Place_of_Birth, Place_of_Publication_1) %>%
  distinct()

head(network)

# igraph
g <- graph_from_data_frame(network, directed = TRUE)
summary(g)

ggraph(g, layout = 'fr') +
  geom_edge_link(alpha = 0.5, color = "gray") +
  geom_node_point(color = "#619CFF", size = 3) +
  geom_node_text(aes(label = name), repel = TRUE, size = 3) +
  theme_void() +
  theme(legend.position = "none") +
  labs(title = "")

# Create a new data frame with Head_Author_Country_of_Birth and
# Place_of_Publication_1 as clean columns
network2 <- df %>%
  filter(!is.na(Head_Author_Country_of_Birth) & !is.na(Place_of_Publication_1)) %>%
  dplyr::select(Head_Author_Country_of_Birth, Place_of_Publication_1) %>%
  distinct()

head(network2)

# igraph
g <- graph_from_data_frame(network2, directed = TRUE)
summary(g)

V(g)$category <- ifelse(V(g)$name %in% network2$Head_Author_Country_of_Birth, 
                        "Country of Birth", "Place of Publication")

ggraph(g, layout = 'fr') +
  geom_edge_link(alpha = 0.5, color = "gray") +
  geom_node_point(aes(color = category, shape = category), size = 4) +
  geom_node_text(aes(label = name), repel = TRUE, size = 3) +
  scale_color_manual(values = c("Country of Birth" = "#619CFF", "Place of Publication" = "#F8766D")) +
  scale_shape_manual(values = c("Country of Birth" = 16, "Place of Publication" = 17)) +
  theme_void() +
  theme(legend.position = "none") +
  labs(title = "")

# Weighted edges
network2 <- df %>%
  filter(!is.na(Head_Author_Country_of_Birth) & !is.na(Place_of_Publication_1)) %>%
  dplyr::select(Head_Author_Country_of_Birth, Place_of_Publication_1) %>%
  count(Head_Author_Country_of_Birth, Place_of_Publication_1)

g <- graph_from_data_frame(network2, directed = TRUE)

E(g)$weight <- network2$n

V(g)$category <- ifelse(V(g)$name %in% network2$Head_Author_Country_of_Birth, 
                        "Country of Birth", "Place of Publication")

ggraph(g, layout = 'fr') +
  geom_edge_link(aes(edge_alpha = weight, edge_width = weight), color = "gray") +  # Weight affects edge transparency and width
  geom_node_point(aes(color = category, shape = category), size = 4) +
  geom_node_text(aes(label = name), repel = TRUE, size = 3) +
  scale_color_manual(values = c("Country of Birth" = "#619CFF", "Place of Publication" = "#F8766D")) +
  scale_shape_manual(values = c("Country of Birth" = 16, "Place of Publication" = 17)) +
  theme_void() +
  theme(legend.position = "none") +
  labs(title = "")
