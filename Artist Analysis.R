
  
  ## 1. Introduction {.tabset .tabset-fade}
  
# South Indian music is a captivating cultural tradition that has gained
# global popularity, thanks to streaming platforms like Spotify. In this
# analysis, we use the Spotify API and R programming language to explore
# the world of South Indian music artists. By analyzing data on
# popularity, trends, and track features, we aim to gain insights and
# create visualizations that showcase the vibrant and diverse nature of
# this genre. Let's embark on this musical journey where data meets
# artistry and discover the fascinating world of South Indian music
# artists.

### 1.1 Loading Packages

##[spotifyr](https://www.rcharlie.com/spotifyr/) is an R wrapper for
##pulling track audio features and other information from Spotify's Web
##API in bulk.


library(spotifyr)
library(tidyverse)
library(ggjoy)
library(ggcorrplot)
library(plotly)


### 1.2 API

Sys.setenv(SPOTIFY_CLIENT_ID = '65ee0ef4c92e4117bf1874366f08f58a')
Sys.setenv(SPOTIFY_CLIENT_SECRET = '02c627246625434384fcbbeab4a7c191')
access_token <- get_spotify_access_token()


# Setting up the necessary environment variables for the **Spotify API**
#   authentication in R. 'Client ID' and 'Secret No' can be acquired from
# [Spotify for Developers](https://developer.spotify.com/)


Sys.setenv(SPOTIFY_CLIENT_ID = 'ID')
Sys.setenv(SPOTIFY_CLIENT_SECRET = 'SECRET_NO')
access_token <- get_spotify_access_token()



##  {.unnumbered}

## 2. Shaan Rahman {.tabset .tabset-fade}
# 
# A prominent composer known for his catchy compositions in the Malayalam film industry.


# ███████████████████████████████████████
# █                                     █
# █            SHAAN RAHMAN  ♫          █
# █                                     █
# ███████████████████████████████████████


### 2.1 Getting Data


#Getting the Data and storing it to a variable
Shaan <- get_artist_audio_features('Shaan Rahman')




### 2.2 Value Conversion


Shaan$album_release_year <- as.integer(Shaan$album_release_year)
Shaan$duration_ms <- Shaan$duration_ms/60000
Shaan$duration_ms <- round(Shaan$duration_ms, digits = 1)

#Renaming Column
Shaan <- Shaan %>% 
  rename(duration_min = duration_ms)


### 2.3 Data Cleaning


#Cleaning of Data️

patterns_to_remove <- c("\\(", "\\)", "\\{", "\\}", "BGM", "Desi Mix", "Treadmill Mix", "Theme Music", "Instrumental","Reprise","theme","Unplugged")

# Use dplyr and stringr to remove rows containing the patterns
Shaan <- Shaan %>% 
  filter(!str_detect(track_name, str_c(patterns_to_remove, collapse = "|"))) %>% 
  arrange(track_name)


#<====Removing Duplicate Rows=====>

# Identify duplicate track names
duplicates <- duplicated(Shaan$track_name)

# Remove duplicate rows
Shaan<- Shaan[!duplicates, ]




### 2.4 Data Exploration


#Shaan Rahman Most Keys Used
keys <- Shaan %>% 
  count(key_mode, sort = T)
head(keys)



#Shaan Rahman Valence Analysis
joyness <- Shaan %>% 
  arrange(-valence) %>% 
  select(track_name, valence) 
head(joyness)



### 2.5 Regression Model


#Linear Regression model to predict valence using energy
#valence(dependentvariable), energy (independent variable)

model <- lm(formula = valence ~ energy, data = Shaan)
summary(model)
plot(model)


#predicting valence
energy <- c(0.785, 0.462, 0.921)  
valence <- NA   

new_data <- data.frame(energy, valence)

predicted_valence <- predict(model, newdata = new_data)

new_data <- data.frame(energy, predicted_valence)

new_data


### 2.6 Visualizations


#▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲
#▲▲▲▲▲▲▲                  ▲▲▲▲▲
#▲▲▲▲▲▲▲   PLOTTING       ▲▲▲▲▲
#▲▲▲▲▲▲▲                  ▲▲▲▲▲
#▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲





#To find correlation between differen attributes like valence, loudness,....
corr <- round(cor(Shaan[,9:19]),8)
ggcorrplot(corr)



Shaan %>% 
  group_by(album_name) %>%
  ggplot(aes(valence, album_name, fill = after_stat(x)))+
  geom_density_ridges_gradient()



#BAR PLOT to show key modes
# Generate a color palette based on the number of unique values in 'key_mode'
palette <- scales::hue_pal()(length(unique(Shaan$key_mode)))




# Create the bar plot to 
gg <- ggplot(Shaan, aes(x = key_mode, fill = key_mode)) +
  geom_bar() +
  labs(title = "Key Mode Distribution",
       x = "Key Mode",
       y = "Count") +
  scale_fill_manual(values = palette) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1))

ggplotly(gg)



op <- ggplot(Shaan, aes(x = album_release_year)) +
  geom_bar(fill = "steelblue", color = "black", alpha = 0.8) +
  labs(title = "Release Count by Year",
       x = "Year of Release",
       y = "Release Count") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.title = element_text(size = 12, face = "bold"),
        axis.text = element_text(size = 10),
        axis.text.x = element_text(angle = 45, hjust = 1))
ggplotly(op)


##  {.unnumbered}


#❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃
#❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃


## 3. Gopi Sundar Analysis {.tabset .tabset-fade}

A versatile Indian music director, singer, songwriter, and actor known
for his award-winning compositions in Malayalam, Telugu, and Tamil
films, as well as his extensive work in advertising and his
contributions to the music industry.

{r, , echo=FALSE}
# ███████████████████████████████████████
# █                                     █
# █            Gopi SUNDAR  ♫           █
# █                                     █
# ███████████████████████████████████████



### 3.1 Getting Data


#Getting Artist Data
Gopi <- get_artist_audio_features('Gopi Sundar')



### 3.2 Value Conversion


#Value Conversion

Gopi$album_release_year <- as.integer(Gopi$album_release_year)
Gopi$duration_ms <- Gopi$duration_ms/60000
Gopi$duration_ms <- round(Gopi$duration_ms, digits = 1)

#Renaming Column
Gopi <- Gopi %>% 
  rename(duration_min = duration_ms)



### 3.3 Data Cleaning




#Cleaning of Data️🧹️🧹️🧹️
patterns_to_remove <- c("\\(", "\\)", "\\{", "\\}", "BGM", "Desi Mix", "Treadmill Mix", "Theme Music", "Instrumental","Reprise","theme","Unplugged","-", ",")

# Use dplyr and stringr to remove rows containing the patterns
Gopi <- Gopi %>% 
  filter(!str_detect(track_name, str_c(patterns_to_remove, collapse = "|"))) %>% 
  arrange(track_name)


#<====Removing Duplicate Rows=====>
# Identify duplicate track names
duplicates <- duplicated(Gopi$track_name)

# Remove duplicate rows
Gopi<- Gopi[!duplicates, ]



### 3.4 Data Exploration


#Counting the most scale used
keys <- Gopi %>%  
  count(key_mode, sort = T)
head(keys)



#Gopi Sundar Valence Analysis

joyness <- Gopi %>% 
  arrange(-valence) %>% 
  select(track_name, valence) 
head(joyness)



### 3.5 Visualizations

{r,  echo=FALSE}
#▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲
#▲▲▲▲▲▲▲                  ▲▲▲▲▲
#▲▲▲▲▲▲▲   PLOTTING       ▲▲▲▲▲
#▲▲▲▲▲▲▲                  ▲▲▲▲▲
#▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲




Gopi %>% 
  group_by(album_name) %>%
  ggplot(aes(valence, album_name, fill = after_stat(x)))+
  geom_density_ridges_gradient()




#BAR PLOT to show key modes
# Generate a color palette based on the number of unique values in 'key_mode'
palette <- scales::hue_pal()(length(unique(Gopi$key_mode)))


# Create the bar plot to 
gg <- ggplot(Gopi, aes(x = key_mode, fill = key_mode)) +
  geom_bar() +
  labs(title = "Key Mode Distribution",
       x = "Key Mode",
       y = "Count") +
  scale_fill_manual(values = palette) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1))

ggplotly(gg)



#BAR CHART for showing number of tracks each year  by Gopi Sundar
gp <- ggplot(Gopi, aes(x = album_release_year)) +
  geom_bar(fill = "steelblue", color = "black", alpha = 0.8) +
  labs(title = "Release Count by Year",
       x = "Year of Release",
       y = "Release Count") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.title = element_text(size = 12, face = "bold"),
        axis.text = element_text(size = 10),
        axis.text.x = element_text(angle = 45, hjust = 1))

ggplotly(gp)


##Important For example, if you have two data points with different
energy levels: one with an energy level of 10 and another with an energy
level of 11. Using the coefficient of 0.5630, we can estimate the
expected difference in valence levels between these two data points.

For the first data point (energy level = 10), we can estimate the
valence level using the formula: Valence = Intercept + Coefficient \*
  Energy

Valence = 0.2733 + 0.5630 \* 10 = 5.8633 For the second data point
(energy level = 11), we can estimate the valence level:
  
  Valence = 0.2733 + 0.5630 \* 11 = 6.4263 Therefore, the expected
difference in valence levels between the two data points is
approximately 6.4263 - 5.8633 = 0.5630 units.

### 3.6 Regression Model


#Linear Regression model to predict valence using energy
#valence(dependentvariable), energy (independent variable)

# Intercept = mean(y) - slope * mean(x)
#Valence = Intercept + Coefficient * Energy

model <- lm(formula = valence ~ energy, data = Gopi)
summary(model)
plot(model)


#predicting valence
energy <- c(0.785, 0.462, 0.921)  
valence <- NA   

new_data_g <- data.frame(energy, valence)

predicted_valence <- predict(model, newdata = new_data_g)

new_data2_g <- data.frame(energy, predicted_valence)
new_data2_g


{r, , echo=FALSE}
#❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃
#❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃


## 4. Sushin Shyam Analysis {.tabset .tabset-fade}

A multi-talented Indian music composer, singer, instrumentalist, and
actor known for his notable contributions to Malayalam cinema, including
award-winning compositions and performances as part of the folk metal
band The Down Troddence.

{r, , echo=FALSE}
# ███████████████████████████████████████
# █                                     █
# █            Sushin Shyam  ♫          █
# █                                     █
# ███████████████████████████████████████



### 4.1 Getting Data


#Getting Artist Data
Sushin <- get_artist_audio_features('Sushin Shyam')



### 4.2 Value Conversion


#Value Conversion

Sushin$album_release_year <- as.integer(Sushin$album_release_year)
Sushin$duration_ms <- Sushin$duration_ms/60000
Sushin$duration_ms <- round(Sushin$duration_ms, digits = 1)

#Renaming Column
Sushin <- Sushin %>% 
  rename(duration_min = duration_ms)



### 4.3 Data Cleaning




#Cleaning of Data️🧹️🧹️🧹️
patterns_to_remove <- c("\\(", "\\)", "\\{", "\\}", "BGM", "Desi Mix", "Treadmill Mix", "Theme Music", "Instrumental","Reprise","theme","Unplugged","-", ",")

# Use dplyr and stringr to remove rows containing the patterns
Sushin <- Sushin %>% 
  filter(!str_detect(track_name, str_c(patterns_to_remove, collapse = "|"))) %>% 
  arrange(track_name)


#<====Removing Duplicate Rows=====>
# Identify duplicate track names
duplicates <- duplicated(Sushin$track_name)

# Remove duplicate rows
Sushin<- Sushin[!duplicates, ]



### 4.4 Data Exploration


#Counting the most scale used
keys <- Sushin %>%  
  count(key_mode, sort = T)
head(keys)



#Shaan Rahman Valence Analysis

joyness <- Sushin %>% 
  arrange(-valence) %>% 
  select(track_name, valence) 
head(joyness)



### 4.5 Visualizations

{r,  echo=FALSE}
#▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲
#▲▲▲▲▲▲▲                  ▲▲▲▲▲
#▲▲▲▲▲▲▲   PLOTTING       ▲▲▲▲▲
#▲▲▲▲▲▲▲                  ▲▲▲▲▲
#▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲




Sushin %>% 
  group_by(album_name) %>%
  ggplot(aes(valence, album_name, fill = after_stat(x)))+
  geom_density_ridges_gradient()




#BAR PLOT to show key modes
# Generate a color palette based on the number of unique values in 'key_mode'
palette <- scales::hue_pal()(length(unique(Sushin$key_mode)))


# Create the bar plot to 
gg <- ggplot(Sushin, aes(x = key_mode, fill = key_mode)) +
  geom_bar() +
  labs(title = "Key Mode Distribution",
       x = "Key Mode",
       y = "Count") +
  scale_fill_manual(values = palette) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1))

ggplotly(gg)



#BAR CHART for showing number of tracks each year  by Sushin Sundar
gp <- ggplot(Sushin, aes(x = album_release_year)) +
  geom_bar(fill = "steelblue", color = "black", alpha = 0.8) +
  labs(title = "Release Count by Year",
       x = "Year of Release",
       y = "Release Count") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.title = element_text(size = 12, face = "bold"),
        axis.text = element_text(size = 10),
        axis.text.x = element_text(angle = 45, hjust = 1))

ggplotly(gp)


##Important For example, if you have two data points with different
energy levels: one with an energy level of 10 and another with an energy
level of 11. Using the coefficient of 0.5630, we can estimate the
expected difference in valence levels between these two data points.

For the first data point (energy level = 10), we can estimate the
valence level using the formula: Valence = Intercept + Coefficient \*
  Energy

Valence = 0.2733 + 0.5630 \* 10 = 5.8633 For the second data point
(energy level = 11), we can estimate the valence level:
  
  Valence = 0.2733 + 0.5630 \* 11 = 6.4263 Therefore, the expected
difference in valence levels between the two data points is
approximately 6.4263 - 5.8633 = 0.5630 units.

### 4.6 Regression Model


#Linear Regression model to predict valence using energy
#valence(dependentvariable), energy (independent variable)

# Intercept = mean(y) - slope * mean(x)
#Valence = Intercept + Coefficient * Energy

model <- lm(formula = valence ~ energy, data = Sushin)
summary(model)
plot(model)


#predicting valence
energy <- c(0.785, 0.462, 0.921)  
valence <- NA   

new_data_g <- data.frame(energy, valence)

predicted_valence <- predict(model, newdata = new_data_g)

new_data2_g <- data.frame(energy, predicted_valence)
new_data2_g


{r, echo=FALSE}
#❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃
#❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃


## 5. Justin Varghese Analysis {.tabset .tabset-fade}

An Indian music composer and producer recognized for his notable
contributions to the Malayalam cinema industry, including acclaimed
works in films like Thanneer Mathan Dinangal, Joji, and Ajagajantharam.

{r,  echo=FALSE}
# ███████████████████████████████████████
# █                                     █
# █           Justin Varghese  ♫        █
# █                                     █
# ███████████████████████████████████████



### 5.1 Getting Data


#Getting Artist Data
Justin <- get_artist_audio_features('Justin Varghese')



### 5.2 Value Conversion


#Value Conversion

Justin$album_release_year <- as.integer(Justin$album_release_year)
Justin$duration_ms <- Justin$duration_ms/60000
Justin$duration_ms <- round(Justin$duration_ms, digits = 1)

#Renaming Column
Justin <- Justin %>% 
  rename(duration_min = duration_ms)



### 5.3 Data Cleaning




#Cleaning of Data️🧹️🧹️🧹️
patterns_to_remove <- c("\\(", "\\)", "\\{", "\\}", "BGM", "Desi Mix", "Treadmill Mix", "Theme Music", "Instrumental","Reprise","theme","Unplugged","-", ",")

# Use dplyr and stringr to remove rows containing the patterns
Justin <- Justin %>% 
  filter(!str_detect(track_name, str_c(patterns_to_remove, collapse = "|"))) %>% 
  arrange(track_name)


#<====Removing Duplicate Rows=====>
# Identify duplicate track names
duplicates <- duplicated(Justin$track_name)

# Remove duplicate rows
Justin<- Justin[!duplicates, ]



### 5.4 Data Exploration


#Counting the most scale used
keys <- Justin %>%  
  count(key_mode, sort = T)
head(keys)



#Shaan Rahman Valence Analysis

joyness <- Justin %>% 
  arrange(-valence) %>% 
  select(track_name, valence) 
head(joyness)



### 5.5 Visualizations


#▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲
#▲▲▲▲▲▲▲                  ▲▲▲▲▲
#▲▲▲▲▲▲▲   PLOTTING       ▲▲▲▲▲
#▲▲▲▲▲▲▲                  ▲▲▲▲▲
#▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲




Justin %>% 
  group_by(album_name) %>%
  ggplot(aes(valence, album_name, fill = after_stat(x)))+
  geom_density_ridges_gradient()




#BAR PLOT to show key modes
# Generate a color palette based on the number of unique values in 'key_mode'
palette <- scales::hue_pal()(length(unique(Justin$key_mode)))


# Create the bar plot to 
gg <- ggplot(Justin, aes(x = key_mode, fill = key_mode)) +
  geom_bar() +
  labs(title = "Key Mode Distribution",
       x = "Key Mode",
       y = "Count") +
  scale_fill_manual(values = palette) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1))

ggplotly(gg)



#BAR CHART for showing number of tracks each year  by Justin Sundar
gp <- ggplot(Justin, aes(x = album_release_year)) +
  geom_bar(fill = "steelblue", color = "black", alpha = 0.8) +
  labs(title = "Release Count by Year",
       x = "Year of Release",
       y = "Release Count") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.title = element_text(size = 12, face = "bold"),
        axis.text = element_text(size = 10),
        axis.text.x = element_text(angle = 45, hjust = 1))

ggplotly(gp)



#❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃
#❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃❃


##Important For example, if you have two data points with different
# energy levels: one with an energy level of 10 and another with an energy
# level of 11. Using the coefficient of 0.5630, we can estimate the
# expected difference in valence levels between these two data points.
# 
# For the first data point (energy level = 10), we can estimate the
# valence level using the formula: Valence = Intercept + Coefficient \*
#   Energy
# 
# Valence = 0.2733 + 0.5630 \* 10 = 5.8633 For the second data point
# (energy level = 11), we can estimate the valence level:
#   
#   Valence = 0.2733 + 0.5630 \* 11 = 6.4263 Therefore, the expected
# difference in valence levels between the two data points is
# approximately 6.4263 - 5.8633 = 0.5630 units.


### 5.6 Regression Model


#Linear Regression model to predict valence using energy
#valence(dependentvariable), energy (independent variable)

# Intercept = mean(y) - slope * mean(x)
#Valence = Intercept + Coefficient * Energy

model <- lm(formula = valence ~ energy, data = Justin)
summary(model)
plot(model)


#predicting valence
energy <- c(0.785, 0.462, 0.921)  
valence <- NA   

new_data_g <- data.frame(energy, valence)

predicted_valence <- predict(model, newdata = new_data_g)

new_data2_g <- data.frame(energy, predicted_valence)
new_data2_g


## 6. Combined Artist Analysis {.tabset .tabset-fade}

Analysis an on all four artists


# ███████████████████████████████████████
# █                                     █
# █            COMPARISON               █
# █             between                 █
# █      ♫      Artists    ♫            █
# █                                     █
# ███████████████████████████████████████



# Create a new data frame combining the data from both artists
topbestartist <- rbind(Shaan, Sushin,Gopi,Justin)



### 6.1 Visualization on all artists data


#▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲
#▲▲▲▲▲▲▲                  ▲▲▲▲▲
#▲▲▲▲▲▲▲   PLOTTING       ▲▲▲▲▲
#▲▲▲▲▲▲▲                  ▲▲▲▲▲
#▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲




# Create the bar plot comparing release counts of all artists
comb <- ggplot(topbestartist, aes(x = album_release_year, fill = artist_name)) +
  geom_bar(position = "stack", color = "black", alpha = 0.8) +
  labs(title = "Release Count by Year",
       x = "Year of Release",
       y = "Release Count") +
  scale_fill_manual(values = c("steelblue", "orange", "green","red")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.title = element_text(size = 12, face = "bold"),
        axis.text = element_text(size = 10),
        axis.text.x = element_text(angle = 45, hjust = 1))

ggplotly(comb)



#Plotting an emoitional quadrant

emotionalQuadrant <- ggplot(data = topbestartist, aes(x = valence, y = energy, color = artist_name, text = paste("Song:", track_name, "<br>Energy:", energy, "<br>Valence:", valence))) +
  geom_jitter() +
  geom_vline(xintercept = 0.5) +
  geom_hline(yintercept = 0.5) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 1)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1)) +
  annotate('text', 0.25 / 2, 0.95, label = "Angry / Turbulent") +
  annotate('text', 1.75 / 2, 0.95, label = "Joyful / Happy") +
  annotate('text', 1.75 / 2, 0.05, label = "Peace / Chill") +
  annotate('text', 0.25 / 2, 0.05, label = "Depressing / Sad") +
  labs(x = "Valence", y = "Energy") +
  ggtitle("Emotional quadrant of Shaan, Gopi, Justin and  Sushin", "Based on energy y valence")

ggplotly(emotionalQuadrant, tooltip = "text")





#Creating a Duration plot
duration_data <- topbestartist %>% 
  select(artist_name, duration_min)


duration_plot <- ggplot(duration_data, aes(x = duration_min,  fill = artist_name))+
  geom_histogram(binwidth = 0.5, color = "white")+
  labs(title = "Distribution of Track Duration", x = "Track Duration(Minutes)", y = "Count")+
  theme_minimal()+
  facet_wrap(~artist_name)
ggplotly(duration_plot)

# Calculate summary statistics of track durations
summary_stats <- summary(duration_data$duration_min)
summary_stats



