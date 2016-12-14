################################

## Date: 12/13/2016
## Author: Amy White

## Purpose: Samples of interesting graphs I created on various projects to use for
#           future reference. 

# Data Used: facebook_time_series.R
#             -- This dataset came from the time series data loaded onto Redshift with
#                additional formatting done in an R program "Time_series_analysis.R" 

# Output: Code creates the graphs whose screenshots are included in this GitHub:

#         Graph1.png
#         Graph2.png


#############################

setwd("/Users/amywhite/Dropbox/MoveOn/FacebookTesting/")

# Clearing workspace
# rm(list = ls())

# Loading packages
library(Hmisc)
library(doBy)
library(lubridate)
library(dplyr)
library(sqldf)
library(DBI)
library(RPostgreSQL)
library(data.table)
library(xlsx)
library(stringr)
library(sqldf)
library(DataCombine)
library(ggplot2)
detach("package:RPostgreSQL", unload=TRUE)

# Reassigns the select fucntion to be the dplyr version. 
select <- dplyr::select 

# Importing facebook time series formatted data
fb <- readRDS("./facebook_time_series.Rda")


### (1) Graph1 - scatter plot function of total views over time ###

# Graphing percent of total views over time.  Splitting into 13 plots so we have only 7 videos per plot
splot <- function(videogroup, df) {
  end = videogroup*7
  start = end - 6
  plot <- ggplot(df[df$vid_id %in% vidids[start:end] & df$hours_since_post <=500,], aes(x = hours_since_post, y = pct_of_total)) +
    geom_point(aes(color=factor(substr(title,1,20)))) +
    geom_hline(yintercept = 0.90) +
    geom_vline(xintercept = 48) +
    scale_color_discrete(name = "Video Title") +
    ggtitle("Video Performance Over Time") +
    labs(x="Hours Since Post",y="Percent of Total Views")
  return(plot) }
splot(1,fb)
#splot(2,fb)
#splot(3,fb)
# ... etc 


## (2) Graph2 - Similar scatter plot with additional formatting ###

ggplot(fb[fb$category == "plateau",], aes(x = hours_since_post, y = pct_of_total)) +
  geom_point(aes(color=factor(substr(title,1,20)))) +
  geom_hline(yintercept = 0.90) +
  geom_vline(xintercept = 48) + 
  scale_color_discrete(name = "Video Title") +
  ggtitle("'Plateau' Video Performance Over Time") +
  labs(x="Hours Since Post",y="Percent of Total Views")


## (3) Creating demonstratives to share with VidLab in an email about my findings so far ### 

# Exemplary lots for email to vidlab team. 
ggplot(fb10[fb10$vid_id %in% vidids[21:25],], aes(x = hours_since_post, y = pct_of_total)) +
  geom_point(aes(color=factor(substr(title,1,20)))) +
  geom_hline(yintercept = 0.90) +
  geom_vline(xintercept = 48) + 
  scale_color_discrete(name = "Video Title") +
  ggtitle("'Typical' Video Performance Over Time") +
  labs(x="Hours Since Post",y="Percent of Total Views") 
# Saved on my desktop and pasted into my email. 

 
# Saved on my desktop and pasted into my email. 

ggplot(fb10[fb10$category == "steady grower" ,], aes(x = hours_since_post, y = pct_of_total)) +
  geom_point(aes(color=factor(substr(title,1,20)))) +
  geom_hline(yintercept = 0.90) +
  geom_vline(xintercept = 48) + 
  scale_color_discrete(name = "Video Title") +
  ggtitle("'Steady Growth' Video Performance Over Time") +
  labs(x="Hours Since Post",y="Percent of Total Views") 
# Saved on my desktop and pasted into my email. 

vidlist <- fb10 %>% select(vid_id, title, created_time, total_views_to_date, category) %>% distinct(.keep_all=TRUE) %>% arrange(category, total_views_to_date)
# 69 videos
write.csv(vidlist, file = "./Video Category List.csv")
# Attached to my email and then deleted from my work folder. 


## (4)  Looking at how growth rate changes over time. 

ggplot(fb10[fb10$category == "plateau" & fb10$vid_id > 45 & fb10$hours_since_post <= 500 & !(fb10$total_views_pct_growth %in% c(Inf,NA)) & fb10$hours_since_post > 5,], 
       aes(x = hours_since_post, y = total_views_pct_growth)) +
  geom_point(aes(color=factor(vid_id))) +
  geom_hline(yintercept = 0.90) +
  geom_vline(xintercept = 48)

ggplot(fb10[fb10$category == "90 pct 48 hours" & fb10$vid_id %in% c(21) & fb10$hours_since_post <= 500 & !(fb10$total_views_pct_growth %in% c(Inf,NA)) & fb10$total_views_pct_growth < 10,], 
       aes(x = hours_since_post, y = total_views_pct_growth)) +
  geom_point(aes(color=factor(vid_id))) +
  geom_hline(yintercept = 0.90) +
  geom_vline(xintercept = 48)


ggplot(fb10[fb10$category == "steady grower" & fb10$vid_id < 30 & fb10$hours_since_post <= 500 & !(fb10$total_views_pct_growth %in% c(Inf,NA)) & fb10$total_views_pct_growth < 50,], 
       aes(x = hours_since_post, y = total_views_pct_growth)) +
  geom_point(aes(color=factor(vid_id))) +
  geom_hline(yintercept = 0.90) +
  geom_vline(xintercept = 48)


### (5) Looking at plots of different characteristics by type of video ###

# (a) Total views by category 
totalviews <- fb10 %>% distinct(vid_id, category, total_views_to_date)

ggplot(totalviews, aes(x = category, y = total_views_to_date)) +
  geom_point(aes(color=factor(category))) +
  stat_summary(fun.y = mean, 
               fun.ymin = function(x) mean(x) - sd(x),
               fun.ymax = function(x) mean(x) + sd(x),
               geom = "pointrange") +
  geom_jitter(aes(color = factor(category))) +
  theme(legend.position = "none") +
  ggtitle("Total Views per Video") +
  labs(x = "Video Category", y = "Number of Total Views")

# Limited to exclude huge performers. 
ggplot(totalviews[totalviews$total_views_to_date < 500000,], aes(x = category, y = total_views_to_date)) +
  geom_point(aes(color=factor(category))) +
  stat_summary(fun.y = mean, 
               fun.ymin = function(x) mean(x) - sd(x),
               fun.ymax = function(x) mean(x) + sd(x),
               geom = "pointrange") +
  geom_jitter(aes(color = factor(category))) +
  theme(legend.position = "none") +
  ggtitle("Total Views per Video") +
  labs(x = "Video Category", y = "Number of Total Views")

## (b) Variables as of 40/45/50/55 minutes, by category

# Keeping record when first hit 40 hours, 45 hours, etc. 
at40 <- fb10 %>% filter(hours_since_post >= 40 & hours_since_post < 45) %>% 
  group_by(vid_id) %>% 
  mutate(ct = order(snapshot_time)) %>%
  filter(ct == 1) %>%
  mutate(at_hour = 40)
# Only 67 so two videos had no obs between 40 and 45 hours

at45 <- fb10 %>% filter(hours_since_post >= 45 & hours_since_post < 50) %>% 
  group_by(vid_id) %>% 
  mutate(ct = order(snapshot_time)) %>%
  filter(ct == 1) %>%
  mutate(at_hour = 45)
# 66 obs 

at50 <- fb10 %>% filter(hours_since_post >= 50 & hours_since_post < 55) %>% 
  group_by(vid_id) %>% 
  mutate(ct = order(snapshot_time)) %>%
  filter(ct == 1) %>%
  mutate(at_hour = 50)
# 69 obs

at55 <- fb10 %>% filter(hours_since_post >= 55 & hours_since_post < 60) %>% 
  group_by(vid_id) %>% 
  mutate(ct = order(snapshot_time)) %>%
  filter(ct == 1) %>%
  mutate(at_hour = 55)
# 66 obs

# Stacking them together
athours <- rbind(at40,at45,at50,at55)


## (b.1) Total Views

# From http://stackoverflow.com/questions/20060949/ggplot2-multiple-sub-groups-of-a-bar-chart
install.packages("grid")
library(grid)

g1 <- ggplot(athours, aes(x = interaction(at_hour, category), y = total_views)) +
  geom_point(aes(color=factor(category))) +
  coord_cartesian(ylim = c(0, 2300000)) +
  annotate("text", x = 1:12, y = - 200000,
           label = rep(c(40,45,50,55), 3)) +
  annotate("text", x = c(2.5,6.5,10.5), y = - 350000,
           label = c("Traditional","Plateau","Steady Grower")) +
  annotate("text", x = 6, y = - 500000,
           label = "Hours Since Post") +
  theme_classic() +
  theme(plot.margin = unit(c(1, 1, 4, 1), "lines"),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "none") +
  geom_jitter(aes(color = factor(category))) +
  ggtitle("Total Views per Video") +
  labs(y = "Number of Total Views")

g2 <- ggplot_gtable(ggplot_build(g1))
g2$layout$clip[g2$layout$name == "panel"] <- "off"
grid.newpage()
grid.draw(g2)


# Limited to under 330k
g1 <- ggplot(athours, aes(x = interaction(at_hour, category), y = total_views)) +
  geom_point(aes(color=factor(category))) +
  coord_cartesian(ylim = c(0, 330000)) +
  annotate("text", x = 1:12, y = - 30000,
           label = rep(c(40,45,50,55), 3)) +
  annotate("text", x = c(2.5,6.5,10.5), y = - 50000,
           label = c("Traditional","Plateau","Steady Grower")) +
  annotate("text", x = 6, y = - 70000,
           label = "Hours Since Post") +
  theme_classic() +
  theme(plot.margin = unit(c(1, 1, 4, 1), "lines"),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "none") +
  geom_jitter(aes(color = factor(category))) +
  ggtitle("Total Views per Video") +
  labs(y = "Number of Total Views")

g2 <- ggplot_gtable(ggplot_build(g1))
g2$layout$clip[g2$layout$name == "panel"] <- "off"
grid.newpage()
grid.draw(g2)


## (b.2) Pct growth rate in views over last hour

#(currently % change over last 10 minutes - want to change to over last hour)

g1 <- ggplot(athours, aes(x = interaction(at_hour, category), y = total_views_pct_growth)) +
  geom_point(aes(color=factor(category))) +
  coord_cartesian(ylim = c(0, 3)) +
  annotate("text", x = 1:12, y = - 10,
           label = rep(c(40,45,50,55), 3)) +
  annotate("text", x = c(2.5,6.5,10.5), y = - 20,
           label = c("Traditional","Plateau","Steady Grower")) +
  annotate("text", x = 6, y = - 30,
           label = "Hours Since Post") +
  theme_classic() +
  theme(plot.margin = unit(c(1, 1, 4, 1), "lines"),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "none") +
  geom_jitter(aes(color = factor(category))) +
  ggtitle("Total Views Percent Growth per Video") +
  labs(y = "Number of Total Views")

g2 <- ggplot_gtable(ggplot_build(g1))
g2$layout$clip[g2$layout$name == "panel"] <- "off"
grid.newpage()
grid.draw(g2)


## (b.3) Number of new views over last hour


### WEDNESDAY - LOOK AT ITEMS IN NOTEBOOK ### 






## LOOK AT VIEWS OVERTIME - DO WE SEE JUMPS IN VIEWS AT ANY POINT?  WHAT WOULD CAUSE THESE JUMPS?
## FAMOUS PEOPLE?  NEWS POSTINGS? PAID BOOSTS? OTHER? 

## DOES VIEW GROWTH SLOW RIGHT BEFORE THE PLATEAU SUGGESTING THAT VIEWERSHIP WAS FALLING OFF ANYWAY? 
## HOW DOES THIS COMPARE FOR THE DIFFERENT VIDEO CATEGORIES?

## ARE THERE SIMILAR PATTERNS IN SHARES AND COMMENTS? (WOULD NEED TO PULL THESE DATA.)






## (4) Looking at total views over time ##

# Are all steady growers also 
ggplot(fb10[fb10$category == "90 pct 48 hours" & fb10$vid_id <= 20 & fb10$hours_since_post <= 60,], aes(x = hours_since_post, y = total_views)) +
  geom_point(aes(color=factor(vid_id))) +
  labs(x="Hours Since Post",y="Total Views") +
  theme(legend.position="none") +
  ggtitle("90 Pct w/in 48 Hours Videos, Pt 1")  # Most under 20,000, as high as 60,000
ggplot(fb10[fb10$category == "90 pct 48 hours" & fb10$vid_id %in% c(21:40) & fb10$hours_since_post <= 60,], aes(x = hours_since_post, y = total_views)) +
  geom_point(aes(color=factor(vid_id))) +
  labs(x="Hours Since Post",y="Total Views") +
  theme(legend.position="none") +
  ggtitle("90 Pct w/in 48 Hours Videos, Pt 2")  # Most under 50,000, as high as 150,000
ggplot(fb10[fb10$category == "90 pct 48 hours" & fb10$vid_id > 40 & fb10$hours_since_post <= 60,], aes(x = hours_since_post, y = total_views)) +
  geom_point(aes(color=factor(vid_id))) +
  labs(x="Hours Since Post",y="Total Views") +
  theme(legend.position="none") +
  ggtitle("90 Pct w/in 48 Hours Videos, Pt 3")  # Most under 50,000, as high as 150,000
ggplot(fb10[fb10$category == "steady grower" & fb10$hours_since_post <= 200,], aes(x = hours_since_post, y = total_views)) +
  geom_point(aes(color=factor(vid_id))) +
  labs(x="Hours Since Post",y="Total Views") +
  theme(legend.position="none") +
  ggtitle("Steady Grower Videos")# All over the place up to 3 million

# Excluding huge outlier video
ggplot(fb10[fb10$category == "steady grower" & fb10$hours_since_post <= 150 & fb10$vid_id != 11,], aes(x = hours_since_post, y = total_views)) +
  geom_point(aes(color=factor(vid_id))) +
  labs(x="Hours Since Post",y="Total Views") +
  theme(legend.position="none") +
  ggtitle("Steady Grower Videos")
ggplot(fb10[fb10$category == "plateau",], aes(x = hours_since_post, y = total_views)) +
  geom_point(aes(color=factor(vid_id))) +
  labs(x="Hours Since Post",y="Total Views") +
  theme(legend.position="none") +
  ggtitle("Plateau Videos")  # Most under 50,000, one as high as 150,000 and growing.



multiplot(plot1,plot2,cols=2)



### THURSDAY - GRAPH AT WHAT TIME EACH VIDEO REACHES 95% OF ITS TOTAL VIEWS?  80% OF TOTAL VIEWS? 
## WHY DO SOME VIDEOS HAVE SUCH DIFFERENT TRAJECTORIES THAN OTHERS?  MOST HAVE ALL VIEWS HAPPEN IMMEDIATELY,
## OTHERS SPAN MULTIPLE DAYS.  DOES THIS HAVE TO DO WITH WHICH VIDEOS HAVE THE MOST VIEWS?
## CAN WE PREDICT WHETHER A VIDEO WILL BE A BIG HIT BASED ON THE BEHAVIOR OF THE VIDEO IN THE FIRST FEW HOURS? 
## HOW CAN WE USE THIS TO INFORM SELF-CROWDING OF VIDEOS?  


## CREATE BAR CHARTS WITH ERROR BARS OF TIME TILL 95%, DO SAME FOR # OF VIEWS W/IN 2 HOURS, 6 HOURS, ETC ###                 

## (5) Looking at video growth over time ##

# Plotting histogram of percent growth by hour. 

ggplot(fb8, aes(x = hours_since_post, y = total_views_pct_growth)) +
  geom_point(aes(color=factor(video_id))) +
  geom_hline(yintercept = 0.90) +
  geom_vline(xintercept = 48)






# Adding incrementing variable to limited data so we can find first time when reached 90%.  
fb90 <- fb10 %>%
  filter(pct_of_total >= 0.90) %>% 
  group_by(video_id, created_time) %>%
  mutate(snapshot_ct = order(snapshot_time)) %>%
  filter(snapshot_ct == 1)
# 58 videos  

table(fb90$pct_of_total)
# Good, 54 are 90% on the dot, 3 at 91% and 1 at 94%. 
# The one with 94% would have hit 90% right when we have a period of 
# time when no data was collected for a few hours between 2016-11-05 14:50:04
# and 2016-11-05 21:10:05

hist(fb90$hours_since_post)

# Creating cumulative frequency table 
freq <- transform(transform(fb90 %>% 
                              group_by(vid_id,hours_since_post, total_views) %>% 
                              summarise (n = n()) %>%
                              arrange(hours_since_post), 
                            cum_n = cumsum(n), pct = prop.table(n)), 
                  cum_pct = cumsum(pct))
freq
# 50% of videos hit 90% within 33 hours,
# 75% of videos hit 90% within 52 hours, 
# 90% of videos hit 95% within 143 hours (6 days)

# Finding counts and averages for time to 90 by category
test <- fb90 %>% 
  group_by(category) %>% 
  summarise(n = n(),
            mean_total_views = mean(total_views), sd_total_views = sd(total_views),
            mean_time_til_90 = mean(hours_since_post), sd_time_til_90 = sd(hours_since_post))

### MONDAY - GRAPH BOX PLOTS OF TOTAL VIEWS W/ ERROR BARS AND TIME TILL 90 W/ ERROR BARS FOR THESE
### THREE CATEGORIES OF VIDEOS. 

### LOOK AT SOME MORE OF THE PLATEAU VIDEOS - WERE THEY REPOSTED?  WERE THEY SHARED BY SOMEONE FAMOUS?  

### FOLLOW UP WITH TECH SUPPORT ABOUT SCRAPING SHARE DATA FROM FACEBOOK

### LOOK AT VIDEOS THAT WERE BOOSTED - CAN WE SEE THE BOOST IMPACT?

### COORDINATE W/ ANNE ABOUT GETTING ANOTHER VIDEO TO POST FOR MY TESTING OF SELF-CROWDING. 

## CAN WE PREDICT VIDEOS THAT WILL GET > 1 MILLION VIEWS?  LOOKING AT TIME TILL 1,000 VIEWS?  VIEW COUNTS AS OF 
## 1 HOUR, 6 HOURS, ETC? 
## LOOK AT TRAJECTORIES OF VIDEOS BASED ON TOTAL VIEWS - ARE THERE SIMILARITIES? 

## DO WE SEE SPIKES IN VIEWS OF ANY VIDEOS SUGGESTING VALIDATORS?  LIKE TRUMPED AT 4:49PM OCT 30TH WAS SHARED BY
## HILLARY CLINTON DEMOCRATS AND AT 2:00PM ON OCT 30TH BY DEMOCRATIC COALITION AGAINST TRUMP. 

### 

# Looking at Trumped video which I know was shared by others - specifically Hillary Clinton Democrats
# at 4:49PM Oct 30th (not sure what timezone this is, but around 10:50pm 10/30 in UTC) and 2:00pm 10/2
# (or 8pm 10/2 UTC) by Democratic Coalition Against Trump
ggplot(fb10[fb10$vid_id == 1 & fb10$snapshot_time > '2016-10-29 0:0:0' & fb10$snapshot_time < '2016-10-31 23:59:59',], aes(x = snapshot_time, y = total_views)) +
  labs(x="Snapshot Time",y="Total Views") +
  ggtitle("Trumped (vid_id 1) Video Views Over Time") +
  geom_point(aes(color=factor(vid_id))) +
  geom_vline(xintercept = c(1477879200,1477889400))
# We can't really see much impact from these share, at least not immediately.  Something caused
# the video to come out of its plateau but the timing of these shares does not line up with that. 
# I checked and there were no shares near the time when this video came out if its plateau. 
# (Unless there is something off about the timezones..... 


geom_hline(yintercept = c(1477879200,1477889400))

unclass(as.POSIXct('2016-10-30 22:50:00', format ="%m/%d/%Y %H:%M:%S", tz = Sys.timezone()))


unclass(as.POSIXct("10/30/16 20:00:00", format = "%m/%d/%y %H:%M:%S")) # 1477879200
unclass(as.POSIXct("10/30/16 22:50:00", format = "%m/%d/%y %H:%M:%S")) # 1477889400












