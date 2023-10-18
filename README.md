# Bike Share Case Study

I made this case study during the [Google Analytics Professional Certificate](https://www.coursera.org/professional-certificates/google-data-analytics) course. It is based on the scenario "How Does a Bike-Share Navigate Speedy Success?".

To complete the study from cleaning to the analysis and visualizations I used **R** in **RStudio**.

## Business Task

Cyclistic is a fictional bike-share program in Chicago, US. As part of changing the marketing strategy, it was proposed to attemp to turn casual drivers (who only rent bikes for a short duration) into members (the other group of customers with annual membership). The question for the analysis was: *How do annual members and casual riders use Cyclistic bikes differently?*

## Data

The data were provided by an actual bicycle sharing service, the Lyft Bikes and Scooters, LLC (“Bikeshare”). The City of Chicago permits Bikeshare to make certain Divvy system data owned by the City available to the public, subject to the terms and conditions of a [license agreement](https://ride.divvybikes.com/data-license-agreement). The last 13 months (January 2022 - January 2023) of data were downloaded [here](https://divvy-tripdata.s3.amazonaws.com/index.html).

A separate file was obtained for each month. Each files contained thirteen variables - the ride ID, name and ID of start and end station and their coordinates, start and end time, the type of customer (member, casual) and type of bike (classic, electric, docked). After merging the files together (see below), the structure can be observed for example by using the `skim_without_charts()` function from the `skimr` library:

![skim_structure](https://user-images.githubusercontent.com/130282838/230786358-f5e11bef-b56e-40cb-9d1e-636e15ce9b5b.png)

## Data Cleaning and Transformations

Create and save a concatenate of data frames (`all_trips`; `ride_id` is unique identificator). Transform date and time to `POSIXlt` format and calculate the length of trips:

```
library(skimr)
library(dplyr)
library(lubridate)

#create a list of names of files in subfolder 'trips'
list_trips <- list.files(path="./trips", pattern=".csv")

# create a list of data frames
ls <- list()
for (i in 1:13){
  ls[[i]] <- read.csv(paste0("./trips/",list_trips[i]))
}

# calculate new columns (transform date and time of start and end from type char to POSIXlt)
for (i in 1:13){
  ls[[i]]$t_start <- as.POSIXlt(ls[[i]]$started_at)   # date and time of start from type char to POSIXlt
  ls[[i]]$t_end <- as.POSIXlt(ls[[i]]$ended_at)   # date and time of end from type char to POSIXlt
  ls[[i]]$trip_duration <- as.numeric(ls[[i]]$t_end - ls[[i]]$t_start)  # duration of the trip in seconds
  ls[[i]]$week_day <- wday(ls[[i]]$t_start, label = TRUE)  # day of the week of start
  # this is here to check that POSIXlt worked properly for the specific file:
  cat("\n\nsoubor cislo:", i, " ", list_trips[i], "\n")
  print(head(ls[[i]]))
}

# concatenate data frames and save as RData
all_trips <- data.frame()
for (i in 1:13){
  all_trips <- bind_rows(all_trips,ls[i])
}
skim_without_charts(all_trips)
save(all_trips, file = "all_trips.RData")
```

The following code was used to find IDs of stations which were not unique for any possible geographical analysis (such detailed analysis was not performed during the project):

```
# makes df of station IDs, number of multiple start station names and their names
starts <- all_trips %>%
  group_by(start_station_id) %>%    # grouping
  reframe(n_names_start = length(unique(start_station_name)), start_station_name) %>%  # counting unique names (n_names_start)
  select(start_station_id, n_names_start, start_station_name) %>%   # relevant columns for further analysis
  distinct(.keep_all = TRUE) %>%    # removing duplicit rows
  dplyr::rename(station_id = start_station_id) %>%   # renaming column with station IDs to join with end stations later
  arrange(-n_names_start)   # sorting from the ID with highest number of related names
starts <- starts[starts$n_names_start == 1 & starts$station_id != '', ]    # only unique IDs, no empty IDs

# makes df of station IDs, number of multiple end station names and their names, like above (starts)
ends <- all_trips %>%
  group_by(end_station_id) %>%
  reframe(n_names_end = length(unique(end_station_name)), end_station_name) %>%
  select(end_station_id, n_names_end, end_station_name) %>%
  distinct(.keep_all = TRUE) %>% 
  dplyr::rename(station_id = end_station_id) %>%
  arrange(-n_names_end)
ends <- ends[ends$n_names_end == 1 & ends$station_id != '', ]

stations <- merge(starts, ends, by = 'station_id', all=TRUE)  # outer join start and end stations podle station ID
stations$comparison <- ifelse(stations$start_station_name == stations$end_station_name , TRUE, FALSE)  # comparison of names at the same ID
table(stations['comparison'])   # number of trues and falses

show_the_id <- stations[stations$comparison == FALSE, ]   # show the ID which is not unique
```

## Analysis

Some more transformations were performed for the purposes of the analysis - the trips shorter than 1 minute were omitted, important state holidays were identified and the trips sorted into one-way and round trips.

```
# leaves only rows with trip_duration at least one minute
all_trips <- all_trips[all_trips$trip_duration >= 60, ]

# important state holidays
important_holidays <- list()
important_holidays <- c(as_date("2022-01-01"), as_date("2022-07-04"), as_date("2022-11-24"), as_date("2022-11-25"), as_date("2022-12-25"), as_date("2022-12-26"), as_date("2023-01-02"))
all_trips$imp_holidays <- ifelse(as_date(all_trips$t_start) %in% important_holidays, 'imp_holiday', 'regular')

# round trips vs. A to B trips
all_trips$direction <- ifelse(all_trips$start_station_id == all_trips$end_station_id, 'round', 'a_to_b')
```

After that, most of the analysis was represented by comparing the casual riders and members. Some differences were confirmed using the Student's t-test. Mainly durations of trips and number of trips during specific time periods were compared between the two groups, as well as the ratio of one-way and round trips and ratio of different bike types.

```
# mean trip duration
all_trips %>% group_by(member_casual) %>% summarize(mean_trip_duration = mean(trip_duration))

# Student's t-test
t.test(trip_duration ~ member_casual, all_trips, var.equal=TRUE)

# now for median
all_trips %>% group_by(member_casual) %>% summarize(median_trip_duration = median(trip_duration))
t.test(trip_duration ~ member_casual, all_trips, var.equal=TRUE)

# duration and mean for days in the week, months, imp_holidays/regular
all_trips %>% group_by(member_casual) %>% count(week_day) %>% print(n=24)
all_trips %>% group_by(member_casual, week_day) %>% summarize(mean_trip_duration = mean(trip_duration)) %>% print(n=24)
all_trips %>% group_by(member_casual) %>% count(month) %>% print(n=24)
all_trips %>% group_by(member_casual, month) %>% summarize(mean_trip_duration = mean(trip_duration)) %>% print(n=24)
all_trips %>% group_by(member_casual) %>% count(imp_holidays) %>% print(n=24)
all_trips %>% group_by(member_casual, imp_holidays) %>% summarize(mean_trip_duration = mean(trip_duration)) %>% print(n=24)

# road vs other trips, type of bike
all_trips %>% group_by(member_casual) %>% count(direction) %>% print(n=24)
all_trips %>% group_by(member_casual) %>% count(rideable_type) %>% print(n=24)
```

For the visualizations, the trips were binned with respect to their length:
```
# binning of trip duration for both groups
binned <- all_trips %>% mutate(dur_bin = cut(trip_duration, breaks=c(300, 600, 1200, 1800, 3600, 43200)))
```

The most used stations were found (both start and end stations):
```
frequencies_start_station <- all_trips %>% group_by(start_station_id) %>% count(start_station_id)
frequencies_end_station <- all_trips %>% group_by(end_station_id) %>% count(end_station_id)
frequencies_start_station <- rename(frequencies_start_station,c("station_id"="start_station_id"))
frequencies_start_station <- rename(frequencies_start_station,c("n_start"="n"))
frequencies_end_station <- rename(frequencies_end_station,c("station_id"="end_station_id"))
frequencies_end_station <- rename(frequencies_end_station,c("n_end"="n"))
frequencies_stations <- merge(frequencies_start_station, frequencies_end_station, by='station_id', all=TRUE)   # outer join
frequencies_stations$n_start[is.na(frequencies_stations$n_start)] <- 0   # 0 instead NULL
frequencies_stations$n_end[is.na(frequencies_stations$n_end)] <- 0
frequencies_stations$n_sum <- frequencies_stations$n_start + frequencies_stations$n_end   # starts and ends together
arrange(frequencies_stations, -n_sum)
```


## Visualizations

A number of graphs to compare the two groups was created (see the attached presentation for complete overview). Some examples and the respective codes are listed here.

```
library(tidyverse)
library(ggplot2)
library(scales)

# trip duration binns
xlabels <- c("1-5 min", "5-10 min", "10-20 min", "20-30 min", "0.5-1 h", "1-12 h", "more")
ggplot(data=binned) + geom_bar(mapping=aes(x=dur_bin, fill=member_casual), position="dodge") +
  labs(x="trip duration", y="number of trips") + scale_fill_discrete(name = "type of customer") + theme_bw() +
  scale_y_continuous(labels = label_comma(), breaks = seq(0, 1100000, by = 200000)) + scale_x_discrete(labels= xlabels) +
  labs(title="Number of Trips of Different Durations") +
  annotate("text",x=5.5,y=450000,label="Casual riders often", size=4, color="#F8766D", fontface="bold") +
  annotate("text",x=5.5,y=400000,label="bike longer", size=4, color="#F8766D", fontface="bold") +
  annotate("text",x=4.0,y=1100000,label="Members make more", size=4, color="#00BFC4", fontface="bold") +
  annotate("text",x=4.0,y=1050000,label="short trips", size=4, color="#00BFC4", fontface="bold")
```

![no_trips_diff_durations](https://user-images.githubusercontent.com/130282838/230790034-8448604b-fc20-44aa-b10e-9744514acb1a.png)

```
# number of trips for hours
ggplot(data=all_trips) + geom_bar(mapping=aes(x=hour, fill=member_casual), position="dodge") +
  labs(x="hour", y="number of trips") + scale_fill_discrete(name = "type of customer") + theme_bw() +
  scale_x_continuous(breaks = seq(0, 23, by = 1), minor_breaks=NULL) +
  scale_y_continuous(labels = label_comma(), breaks = seq(0, 550000, by = 100000)) +
  labs(title="Number of Trips", subtitle="During the Day") +
  annotate("text",x=4.8,y=250000,label="Members: mornig spike", size=4, color="#00BFC4", fontface="bold")
```

![no_trips_hours](https://user-images.githubusercontent.com/130282838/230790650-f75e6a4f-78c5-4944-8441-7aa4c218bec1.png)

A problem with two Januaries solved:

```
# number of trips in months
all_trips_distinctyears <- all_trips
all_trips_distinctyears$month <-                      # rename Jan 2022, 2023
  ifelse((all_trips_distinctyears$month == "Jan" & all_trips_distinctyears$year == 2022), "Jan 22",
         ifelse(all_trips_distinctyears$month == "Feb", "Feb",
                ifelse(all_trips_distinctyears$month == "Mar", "Mar",
                       ifelse(all_trips_distinctyears$month == "Apr", "Apr",
                              ifelse(all_trips_distinctyears$month == "May", "May",
                                     ifelse(all_trips_distinctyears$month == "Jun", "Jun",
                                            ifelse(all_trips_distinctyears$month == "Jul", "Jul",
                                                   ifelse(all_trips_distinctyears$month == "Aug", "Aug",
                                                          ifelse(all_trips_distinctyears$month == "Sep", "Sep",
                                                                 ifelse(all_trips_distinctyears$month == "Oct", "Oct",
                                                                        ifelse(all_trips_distinctyears$month == "Nov", "Nov",
                                                                               ifelse(all_trips_distinctyears$month == "Dec", "Dec", "Jan 23")
                                                                               )
                                                                        )
                                                                 )
                                                          )
                                                   )
                                            )
                                     )
                              )
                       )
                )
         )
all_trips_distinctyears$month <- factor(all_trips_distinctyears$month,       # Change ordering manually (because of the plot - months are characters now)
                  levels = c("Jan 22", "Feb", "Mar", "Apr", "May", "Jun", "Jul","Aug", "Sep", "Oct", "Nov",
                             "Dec", "Jan 23"))
ggplot(data=all_trips_distinctyears) + geom_bar(mapping=aes(x=month, fill=member_casual), position="dodge") +
  labs(x="month", y="number of trips") + scale_fill_discrete(name = "type of customer") + theme_bw() +
  scale_y_continuous(labels = label_comma()) +
  labs(title="Number of Trips", subtitle="Individual Months")
```

![no_trips_months](https://user-images.githubusercontent.com/130282838/230790331-379158a8-2a12-42d6-a088-13cc9db457f2.png)

```
# mean trip duration for each week day
plot_table <- all_trips %>% group_by(member_casual, week_day) %>% summarize(mean_trip_duration = mean(trip_duration)) %>% print(n=24)
ggplot(data=plot_table) + geom_line(mapping=aes(x=week_day, y=mean_trip_duration/60, color=member_casual, group=factor(member_casual)),size=2) +
  labs(x="day of the week", y="mean trip duration (min)") + expand_limits(y = 0) + scale_color_discrete(name = "type of customer") +
  theme_bw() + labs(title="Mean Trip Duration", subtitle="Days of the Week")
```

![mean_trip_duration_wdays](https://user-images.githubusercontent.com/130282838/230790185-31b9269c-6363-44f3-b7dc-5c918f213c89.png)

```
# pie charts for directions
plot_table <- all_trips %>% group_by(member_casual) %>% count(direction) %>% print(n=24)
plot_table <- plot_table %>% mutate(share=n/sum(n)*100.0)
ggplot(plot_table, aes(x=1,y=share,fill=direction)) + 
  geom_bar(stat="identity",width=2, position = 'stack') + 
  coord_polar(theta='y') +
  theme(axis.ticks=element_blank(), axis.title=element_blank(),axis.text.y = element_blank(),axis.text.x = element_blank(), panel.grid  = element_blank())+
  scale_x_continuous(limits=c(-1,2.5)) +
  scale_fill_manual(values=c("#009900", "#CCCC00"), labels=c("one-way", "round"))+
  geom_text(aes(label = paste0(round(share), "%")),
             position = position_stack(vjust = 0.5),
             show.legend = FALSE) +
  facet_wrap(~ member_casual) +
  labs(title="Proportion of one-way and round trips")
```

![direction](https://user-images.githubusercontent.com/130282838/230790483-956b842d-c1a4-417c-b94f-c22c364d4703.png)


## Delivering Business Task

A [presentation](https://github.com/radek-kricek/Bike-Share-Case-Study/blob/main/presentation_brief.pdf) is attached, containing all charts relevant for the research question. TOP 3 recommendations and proposed next steps are included. The main findings are:
  - Some casual riders use bikes for shorter trips like members, likely for commuting.
  - Many casual riders use bikes for leisure activities.
  - Many casual riders prefer electric bikes.
