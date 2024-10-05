# Import Libraries needed
library(tidyverse)
library(tidyr)
library(ggplot2)
library(lubridate)
library(plyr)
library(dplyr)
library(ggthemes)
library(scales)
library(RColorBrewer)


# Import the Dataset
df <- read.csv("data/202404_CombinedData.csv")

head(select(df, 1:8), 10)
glimpse(df)
str(df)
summary(df)


# Check for Duplicates
withDuplicates <- df[duplicated(df$Province) | duplicated(df$Province, fromLast = FALSE), ]
withDuplicates

# Function to extract Year, Month, Day, Day Name, and Hour
extract_info <- function(date_str) {
  date_obj <- as_datetime(date_str, tz = "UTC")  # Convert to datetime object
  year <- year(date_obj)
  month <- month(date_obj)
  day <- day(date_obj)
  day_name <-weekdays(date_obj)
  interval <- hour(date_obj)

  return(data.frame(Year = year, Month = month, Day = day, `Day Name` = day_name, `Hour Interval` = interval))
}

df <- bind_cols(df, do.call(rbind, lapply(df$datetime, extract_info)))




# ================ Boxplot for Day ===================
df$Day = as.factor(df$Day)
df$Day

colourCount = length(unique(df$Day))
colourCount
getPalette = colorRampPalette(brewer.pal(9, "YlOrRd"))

df %>% ggplot(aes(x=Day, y=main.temp, fill=Day)) +
  geom_boxplot() +
  scale_fill_manual(values = getPalette(colourCount)) +
  theme_bw() +
  theme(legend.position="none",
        axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank()) +
  labs(title = "Distribution of Daily Temperature for the Month of April",
       x = "Day",
       y = "Temperature") 


# ================ Boxplot for Hour Interval ===================

df$Hour.Interval = as.factor(df$Hour.Interval)
df$Hour.Interval

colourCount = length(unique(df$Hour.Interval))
colourCount
getPalette = colorRampPalette(brewer.pal(9, "YlOrRd"))

df %>% ggplot(aes(x=Hour.Interval, y=main.temp, fill=Hour.Interval)) +
  geom_boxplot() +
  scale_fill_manual(values = getPalette(colourCount)) +
  theme_bw() +
  theme(legend.position="none",
        axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank()) +
  labs(title = "Distribution of Temperature per 3 Hour Interval",
       x = "3 Hour Interval",
       y = "Temperature") 


 
# ================ Boxplot for Day Name ===================

df$Day.Name = as.factor(df$Day.Name)
df$Day.Name

colourCount = length(unique(df$Day.Name))
colourCount
getPalette = colorRampPalette(brewer.pal(9, "YlOrRd"))

df %>% ggplot(aes(x=Day.Name, y=main.temp, fill=Day.Name)) +
  geom_boxplot() +
  scale_fill_manual(values = getPalette(colourCount)) +
  theme_bw() +
  theme(legend.position="none",
        axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank()) +
  labs(title = "Distrubution of Temperature per Day Name",
       x = "Day Name",
       y = "Temperature") 





# ================== Gridges for Density of Temperature by 3 Hour Interval ============================
library(ggridges)

# Gridges Plot
df %>% ggplot(aes(x = main.temp, y = Hour.Interval, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_viridis(name = "Temp. [F]", option = "B") +
  labs(title = "Density of Temperature by 3 Hour Interval", x = "Mean Temperature (°C)",
       y = "") +
  theme_ipsum() +
  theme(
    plot.title = element_text(size = 12),
    legend.position="none",
    plot.title.position = "plot",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  )


# Plot
df %>% ggplot(aes(x = main.temp_min, y = Day.Name, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_viridis(name = "Temp. [C]", option = "F") +
  labs(title = 'Temperatures in Lincoln NE in 2016') +
  theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  )


# ========= Bar Plot for Top 15 Hottest City ========

mean_temp_city <- df%>%
  group_by(city_name) %>%
  summarise_at(vars(main.temp_max), list(name=max)) %>%
  arrange(desc(name))
  
mean_temp_city

top5_highest_temp <- head(mean_temp_city, 15)

top10_cities <- list(top5_highest_temp$city_name)

top10_cities

top5_highest_temp %>% ggplot(aes(x=reorder(city_name, +name), y=name, fill=name)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient(low="#FEB624",high="#B42424") +
  coord_flip() +
  geom_text(aes(label=name), 
            position = position_dodge(width = 0), hjust=1.3,) +
  labs(title = "Top 15 Cities with Highest Recorded Temperature",
       x = NULL,
       y = "Max Temperature (°C)") +
  theme_bw() +
  theme(axis.text.x = element_text(hjust = 1),
        legend.position="none",
        axis.line = element_line(color='black'),
        plot.background = element_blank(),
        plot.title.position = "plot",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank())


# ========= Bar Plot for Top 15 Coldest City ========

max_temp_city <- df %>%
  group_by(city_name) %>%
  summarise_at(vars(main.temp_min), list(temp=mean)) %>%
  mutate_if(is.numeric, round, digits = 1) %>%
  arrange(desc(temp)) 

max_temp_city
  

top10_highest_temp <- tail(max_temp_city, 15)
top10_highest_temp$city_name

top10_highest_temp %>% ggplot(aes(x=reorder(city_name, +temp), y=temp, fill=temp)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient(low="yellow1",high="#FEB624") +
  coord_flip() +
  geom_text(aes(label=temp), 
            position = position_dodge(width = 0), hjust=1.3) +
  labs(title = "Top 15 Cities with Lowest Recorded Temperature",
       x = NULL,
       y = "Mean Temperature (°C)") +
  theme_bw() +
  theme(axis.text.x = element_text(hjust = 1),
        legend.position="none",
        axis.line = element_line(color='black'),
        plot.title.position = "plot",
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank())



# ========= Line Plot for Top 15 with Hottest and Coldest City over Hour ========
library(gghighlight)
mean_temp_city
top15_cities_df <- df %>% 
  filter(city_name %in% c("Santa Rosa",
                          "Cabanatuan City",
                          "Muñoz",
                          "San Carlos",
                          "Urdaneta",
                          "Ilagan",
                          "Cauayan",
                          "Tuguegarao",
                          "San Pedro",
                          "Gapan",
                          "Tarlac City",
                          "Palayan City",
                          "Mabalacat City",
                          "Santo Tomas",
                          "Dagupan",
                          
                          "Tacloban City",
                          "Legazpi City",
                          "Bislig",
                          "Lamitan","City of Sorsogon","Borongan","Tandag","Iligan City","Tayabas","Canlaon","Lipa City","Tagaytay City","Marawi","Malaybalay","Baguio" 
                          ))

top15_cities_df

city <- top15_cities_df %>% pivot_wider(names_from = city_name, values_from=main.temp, values_fill=0) 
city


mean_temp_city <- city %>%
  group_by(Day) %>%
  summarise_at(c("Santa Rosa",
                 "Cabanatuan City",
                 "Muñoz",
                 "San Carlos",
                 "Urdaneta",
                 "Ilagan",
                 "Cauayan",
                 "Tuguegarao",
                 "San Pedro",
                 "Gapan",
                 "Tarlac City",
                 "Palayan City",
                 "Mabalacat City",
                 "Santo Tomas",
                 "Dagupan",
                 
                 "Tacloban City",
                 "Legazpi City",
                 "Bislig",
                 "Lamitan","City of Sorsogon","Borongan","Tandag","Iligan City","Tayabas","Canlaon","Lipa City","Tagaytay City","Marawi","Malaybalay","Baguio"), max, na.rm = TRUE)

mean_temp_city


names(mean_temp_city)[2] <- "SantaRosa"
names(mean_temp_city)[3] <- "CabanatuanCity"
names(mean_temp_city)[5] <- "SanCarlos"
names(mean_temp_city)[10] <- "SanPedro"
names(mean_temp_city)[12] <- "TarlacCity"
names(mean_temp_city)[13] <- "PalayanCity"
names(mean_temp_city)[14] <- "MabalacatCity"
names(mean_temp_city)[15] <- "SantoTomas"

names(mean_temp_city)[17] <- "TaclobanCity"
names(mean_temp_city)[18] <- "LegazpiCity"
names(mean_temp_city)[21] <- "CityofSorsogon"
names(mean_temp_city)[24] <- "IliganCity"
names(mean_temp_city)[27] <- "LipaCity"
names(mean_temp_city)[28] <- "TagaytayCity"

mean_temp_city %>% ggplot() +
  geom_line(aes(x = Day, y = Malaybalay, color=Day)) + geom_line(aes(x = Day, y = Malaybalay,  group=1), size=1, color="gray") +
  geom_line(aes(x = Day, y = Marawi , color=Day)) +   geom_line(aes(x = Day, y = Marawi, group=1), size=1, color="gray") +
  geom_line(aes(x = Day, y = TagaytayCity, color=Day)) +  geom_line(aes(x = Day, y = TagaytayCity, group=1), size=1, color="gray") +
  geom_line(aes(x = Day, y = LipaCity , color=Day)) + geom_line(aes(x = Day, y = LipaCity, group=1), size=1, color="gray") +
  geom_line(aes(x = Day, y = IliganCity , color=Day)) + geom_line(aes(x = Day, y = IliganCity, group=1), size=1, color="gray") +
  geom_line(aes(x = Day, y = Tandag , color=Day)) + geom_line(aes(x = Day, y = Tandag, group=1), size=1, color="gray") +

  geom_line(aes(x = Day, y = Borongan, color=Day)) + geom_line(aes(x = Day, y = Borongan,  group=1), size=1, color="gray") +
  geom_line(aes(x = Day, y = CityofSorsogon , color=Day)) +   geom_line(aes(x = Day, y = CityofSorsogon, group=1), size=1, color="gray") +
  geom_line(aes(x = Day, y = Lamitan , color=Day)) + geom_line(aes(x = Day, y = Lamitan, group=1), size=1, color="gray") +
  geom_line(aes(x = Day, y = Bislig , color=Day)) + geom_line(aes(x = Day, y = Bislig, group=1), size=1, color="gray") +
  geom_line(aes(x = Day, y = LegazpiCity , color=Day)) + geom_line(aes(x = Day, y = LegazpiCity, group=1), size=1, color="gray") +
  geom_line(aes(x = Day, y = TaclobanCity, color=Day)) +  geom_line(aes(x = Day, y = TaclobanCity, group=1), size=1, color="gray") +

  geom_line(aes(x = Day, y = Dagupan, color=Day)) + geom_line(aes(x = Day, y = Dagupan,  group=1), size=1, color="gray") +
  geom_line(aes(x = Day, y = SantoTomas , color=Day)) +   geom_line(aes(x = Day, y = SantoTomas, group=1), size=1, color="gray") +
  geom_line(aes(x = Day, y = MabalacatCity, color=Day)) +  geom_line(aes(x = Day, y = MabalacatCity, group=1), size=1, color="gray") +
  geom_line(aes(x = Day, y = PalayanCity , color=Day)) + geom_line(aes(x = Day, y = PalayanCity, group=1), size=1, color="gray") +
  geom_line(aes(x = Day, y = TarlacCity , color=Day)) + geom_line(aes(x = Day, y = TarlacCity, group=1), size=1, color="gray") +
  geom_line(aes(x = Day, y = Gapan , color=Day)) + geom_line(aes(x = Day, y = Gapan, group=1), size=1, color="gray") +
  geom_line(aes(x = Day, y = SanPedro , color=Day)) + geom_line(aes(x = Day, y = SanPedro, group=1), size=1, color="gray") +

  geom_line(aes(x = Day, y = Tuguegarao, color=Day)) + geom_line(aes(x = Day, y = Tuguegarao,  group=1), size=1, color="gray") +
  geom_line(aes(x = Day, y = Cauayan , color=Day)) +   geom_line(aes(x = Day, y = Cauayan, group=1), size=1, color="gray") +
  geom_line(aes(x = Day, y = Ilagan , color=Day)) + geom_line(aes(x = Day, y = Ilagan, group=1), size=1, color="gray") +
  geom_line(aes(x = Day, y = Urdaneta , color=Day)) + geom_line(aes(x = Day, y = Urdaneta, group=1), size=1, color="gray") +
  geom_line(aes(x = Day, y = SanCarlos , color=Day)) + geom_line(aes(x = Day, y = SanCarlos, group=1), size=1, color="gray") +
  geom_line(aes(x = Day, y = Muñoz , color=Day)) + geom_line(aes(x = Day, y = Muñoz, group=1), size=1, color="gray") +
  geom_line(aes(x = Day, y = CabanatuanCity, color=Day)) +  geom_line(aes(x = Day, y = CabanatuanCity, group=1), size=1, color="gray") +
  geom_line(aes(x = Day, y = SantaRosa , color=Day)) + geom_line(aes(x = Day, y = SantaRosa, group=1), size=2, color="#B42424") +
  geom_line(aes(x = Day, y = Baguio , color=Day)) + geom_line(aes(x = Day, y = Baguio, group=1), size=2, color="#24EAB5") +
  labs(title = "Cities with Highest and Lowest Recorded Daily Temperature in April",
       x = "Day",
       y = "Temperature (°C)") +
  theme_bw() +
  theme(axis.text.x = element_text(hjust = 1),
        legend.position="none",
        axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank())

  # geom_line(aes(color=Hour.Interval)) + geom_point(aes(color=Hour.Interval))

top10_cities_df1 <- subset(df, city_name %in% c("Santa Rosa",
                          "Cabanatuan City",
                          "Muñoz",
                          "San Carlos",
                          "Urdaneta",
                          "Ilagan",
                          "Cauayan",
                          "Tuguegarao",
                          "San Pedro",
                          "Gapan",
                          "Tarlac City",
                          "Palayan City",
                          "Lamitan",
                          "Tagaytay City",
                          "City of Sorsogon",
                          "Borongan",
                          "Tandag",
                          "Iligan City",     
                          "Tayabas",
                          "Canlaon",
                          "Lipa City",
                          "Marawi",
                          "Malaybalay",
                          "Baguio" ))
str(top10_cities_df1)

top10_cities_df2 <- subset(df, city_name %in% c("Santa Rosa",
                                                "Cabanatuan City",
                                                "Muñoz",
                                                "San Carlos",
                                                "Urdaneta",
                                                "Ilagan",
                                                "Cauayan",
                                                "Tuguegarao",
                                                "San Pedro",
                                                "Gapan",
                                                "Tarlac City",
                                                "Palayan City"))

city_aggregated1 <- aggregate(main.temp_max ~ city_name + Hour.Interval, data=top10_cities_df1, FUN = mean)
  aggregate(main.temp ~ city_name, data = top10_cities_df1, mean)
  
str(city_aggregated)

city_aggregated2 <- aggregate(main.temp_max ~ city_name + Hour.Interval, data=top10_cities_df2, FUN = mean)
aggregate(main.temp ~ city_name, data = top10_cities_df2, mean)

city_aggregated2
city_aggregated2 %>%
  ggplot(aes(x=Hour.Interval, y=main.temp_max, group=city_name, colour=city_name)) +
  geom_line(size=1)
  theme_bw() +
  theme(axis.text.x = element_text(hjust = 1),
        axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank())



library(ggforce)
library(viridis)
library(hrbrthemes)

city_aggregated_df2 <- city_aggregated2 %>%
  mutate(name2=city_name)

city_aggregated_df2
city_aggregated_df2 %>%
  ggplot(aes(x=Hour.Interval, y=main.temp_max)) +
  geom_line(data=city_aggregated_df2 %>% dplyr::select(-city_name), aes(group=name2), color="grey", size=1, alpha=0.8) +
  geom_line(aes(color=city_name), color="red2", size=1.5, group=1)+
  scale_color_viridis(discrete = TRUE) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=14),
    # panel.grid = element_blank(),
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank()
  ) +
  ggtitle("A spaghetti chart of baby names popularity") +
  facet_wrap(~city_name)        

top10_cities_df1

  
top10_line


df_starosa <- df %>% 
  filter(city_name == "Santa Rosa")

mean_temp <- df_starosa %>%
  group_by(Hour.Interval) %>%
  summarise(mean_temp = mean(main.temp))

mean_temp

 
mean_temp

ggplot(mean_temp, aes(x = Hour.Interval, y = mean_temp)) +
  geom_line() +
  labs(title = "Mean Temperature Variation in Cabanatuan City",
       x = "Hour Interval",
       y = "Mean Temperature") +
  theme_minimal()






df_santarosa %>% ggplot(aes(x = Hour.Interval)) +
  geom_line(aes(y = main.temp, color = "Temperature")) +
  theme_minimal()


df %>% ggplot(aes(x = ))

df %>% ggplot(aes(x=main.temp)) +
  geom_histogram()
ggplot() + 
  geom_boxplot(data = df, aes(x = main.temp, fill = "r"), alpha = 0.3) +
  geom_boxplot(data = df, aes(x = main.temp_max, fill = "b"), alpha = 0.3) +
  scale_colour_manual(name ="etapa", values = c("r" = "red", "b" = "blue"), labels=c("b" = "blue values", "r" = "red values")) +
  scale_fill_manual(name ="etapa", values = c("r" = "red", "b" = "blue"), labels=c("b" = "blue values", "r" = "red values"))



df$ma




ggplot(data = df, aes(x = Day, y = main.temp, fill = Day)) +
  geom_boxplot() +
  scale_fill_gradient(low = "yellow", high = "red") +
  labs(title = "Boxplot for Day",
       x = "Day",
       y = "Values")

df %>% ggplot(aes(x=factor(Hour.Interval), y=main.temp)) +
  geom_line()


st_rosa <- df %>% filter(city_name == "Santa Rosa")

dayHour <- ddply(df, c( "Hour.Interval", "Day.Name"), summarise,
                 N = main.temp)
dayHour

col1 = "#d8e1cf" 
col2 = "#438484" 
ggplot(dayHour, aes(Hour.Interval, Day.Name)) + geom_tile(aes(fill = N),colour = "white", na.rm = TRUE) +
  scale_fill_gradient(low = col1, high = col2) +  
  guides(fill=guide_legend(title="Total Incidents")) +
  theme_bw() + theme_minimal() + 
  labs(title = "Histogram of Seattle Incidents by Day of Week and Hour",
       x = "Incidents Per Hour", y = "Day of Week") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())


st_rosa %>% ggplot(aes(x = Day, y = main.feels_like)) + 
  geom_line(color="blue") +
  xlab("") +
  theme(axis.text.x=element_text(angle=60, hjust=1)) 
