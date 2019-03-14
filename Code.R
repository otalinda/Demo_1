library(tidyverse)
library(dslabs)
data("gapminder")
filter(gapminder, year == "1962") %>% ggplot(aes(fertility, life_expectancy, color = continent)) +
  geom_point()

filter(gapminder, year %in% c(1962, 2012)) %>%
         ggplot(aes(fertility, life_expectancy, color = continent)) +
         geom_point() +
         facet_grid(continent~year)
       
filter(gapminder, year %in% c(1962, 2012)) %>%
  ggplot(aes(fertility, life_expectancy, color = continent)) +
  geom_point() +
  facet_grid(.~year)

years <- c(1962, 1980, 1990, 2000, 2012)
continents <- c("Europe", "Asia")

gapminder %>%
  filter(year %in% years & continent %in% continents) %>%
  ggplot(aes(fertility, life_expectancy, color = continent)) +
  geom_point() +
  facet_wrap(~year)

countries <- c("South Korea", "Germany")
gapminder %>%
  filter(country %in% countries) %>%
  ggplot(aes(year, fertility, color = country)) + 
  geom_line() 

labels <- data.frame(country = countries, x = c(1975, 1965), y = c(60, 72))
gapminder %>%
  filter(country %in% countries) %>%
  ggplot(aes(year, life_expectancy, color = country)) +  
  geom_line() +
  geom_text(data = labels, aes(x, y, label = country, size = 5)) +
  theme(legend.position = "none")

gapminder <- gapminder %>%
  mutate(dollars_per_day = gdp/population/365)
gapminder %>% 
  filter (!is.na(gdp)) %>%
  ggplot(aes(log2(dollars_per_day))) +
  geom_histogram(binwidth = 1,  color = "black")



gapminder %>% 
  filter(!is.na(gdp)) %>%
  ggplot(aes(region, dollars_per_day)) +
           geom_boxplot() +
           theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
        
gapminder %>% 
      filter(!is.na(gdp)) %>%
      mutate(region = reorder(region, dollars_per_day, FUN = median)) %>%
      ggplot(aes(region, dollars_per_day, fill = continent)) +
      geom_boxplot() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      xlab("") +
      scale_y_continuous(trans = "log2") +
      geom_point(show.legend = FALSE)

country_list1 <- gapminder %>% filter(year == 1970 & !is.na(dollars_per_day)) %>% .$country 
country_list2 <- gapminder %>% filter(year == 2010 & !is.na(dollars_per_day)) %>% .$country 
country_list <- intersect(country_list1, country_list2) 


gapminder %>% 
  filter (year %in% c(1970, 2010) & country %in% country_list & !is.na(gdp)) %>%
  mutate(region = reorder(region, dollars_per_day, FUN = median)) %>%
  ggplot(aes(region, dollars_per_day, fill = factor(year))) +
  geom_boxplot()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("") +
  scale_y_continuous(trans = "log2") +
  geom_point((aes(region, dollars_per_day, fill = factor(year))))


levels(gapminder$region)

west <- c("Northern Europe", "Australia and New Zealand", "Eastern Europe", "Southern Europe", "Western Europe", "Northern America")

gapminder %>% 
  filter (!is.na(gdp)) %>%
  mutate(group = ifelse(region %in% west, 'West', 'Developing')) %>% 
  ggplot(aes(dollars_per_day)) +
  geom_histogram(binwidth = 1,  color = "black") +
  scale_x_continuous(trans = 'log2') +
  facet_grid(.~group) 
       

gapminder %>% 
  filter (year %in% c(1970, 2010) & country %in% country_list) %>%
  mutate(group = ifelse(region %in% west, 'West', 'Developing')) %>% 
  ggplot(aes(dollars_per_day)) +
  geom_histogram(binwidth = 1,  color = "black") +
  scale_x_continuous(trans = 'log2') +
  facet_grid(year~group) 


gapminder %>% 
  filter (year %in% c(1970, 2010) & country %in% country_list) %>%
  mutate(group = ifelse(region %in% west, 'West', 'Developing')) %>%  
  ggplot(aes(x = dollars_per_day, y = ..count.., fill = group)) +
  geom_density(alpha = 0.2, bw = 0.75) + 
  scale_x_continuous(trans = 'log2') +
  facet_grid(year~.) 



gapminder <- gapminder %>% 
  mutate(group = case_when(.$region %in% west ~ "West", .$region %in% c("Eastern Asia", "South-Eastern Asia") ~ "Asia", .$region %in% c("Carribean", "Central America", "South America") ~ "Latin America", .$continent == "Africa" & .$region != "Northern  Africa" ~ "Sub Saharan Africa",
  TRUE ~ "Others"))

gapminder <- gapminder %>% mutate(group = factor(group, levels = c("Others", "Latin America", "East Asia", "Sub-Saharan Africa", "West")))


gapminder %>% filter (year %in% c(1970, 2010) & country %in% country_list) %>%
  ggplot(aes(x = dollars_per_day, y = ..count.., fill = group)) +
           geom_density(alpha = 0.2, bw = 0.75, position = "stack") +
           scale_x_continuous(trans = 'log2') +
           facet_grid(year~.) 

gapminder %>% filter (year %in% c(1970, 2010) & country %in% country_list) %>%
  group_by(year) %>%
  mutate(weight = population / sum(population)*2) %>%
  ungroup() %>%
  ggplot(aes(x = dollars_per_day, fill = group, weight = weight)) +
  scale_x_continuous(trans = 'log2') +
  geom_density(alpha = 0.2, bw = 0.75, position = "stack") +
  facet_grid(year~.) 

gapminder <- gapminder %>% 
  mutate(group = case_when(
    .$region %in% west ~ "West",
    .$region %in% "Northern Africa" ~ "Northern Africa",
    .$region %in% c("Eastern Asia", "South-Eastern Asia") ~ "East Asia",
    .$region == "Southern Asia" ~ "Southern Asia",
    .$region %in% c("Carribean", "Central America", "South America") ~ "Latin America",
    .$region %in% c("Melanesia", "Micronesia", "Polynesia") ~ "Pacific Islands",
    TRUE ~ "Others"))

survey_income <- gapminder %>%
  filter (year %in% c(1970, 2010) & !is.na(gdp) & !is.na(infant_mortality & !is.na(group))) %>%
  group_by(group) %>%
  summarize(income = sum(gdp)/sum(population)/365, infant_survival_rate = 1 - sum(infant_mortality/1000*population)/sum(population)) %>%
  arrange(income)
  
survey_income %>%
  ggplot(aes(income, infant_survival_rate, label = group, color = group)) +
  scale_x_continuous(trans = 'log2', limit = c(.25, 150)) +
  scale_y_continuous(trans = 'log2', limit = c(.875, 9981), breaks = c(.85, .9, .95, .99, .995, .998)) + 
  geom_label(size = 3, show.legend = FALSE)


head(gapminder)
