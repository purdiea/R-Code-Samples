################################

## Date: 12/13/2016
## Author: Amy White

## Purpose: Samples of interesting graphs I created on various projects to use for
#           future reference. 

# Data Used: facebook1.R

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



## (1) Pulling over data and data integrity work ## 

library(RPostgreSQL)

drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, user = 'analytics', password = 'wgBjnlFSqsqfD7QYL0aHBASd', dbname = 'dev', 
                 host = 'moveon.csqr1hggac2r.us-west-2.redshift.amazonaws.com', port=5439)

que <- "SELECT count(*) as rec_ct
FROM facebook.video_time_series"
rs <- dbSendQuery(con, que)
fbcount <- fetch(rs, n=-1)
# 315195 obs - not too big, I should be able to pull it all over. 

que <- "SELECT *
FROM facebook.video_time_series
ORDER BY video_id, snapshot_time"
rs <- dbSendQuery(con, que)
fb <- fetch(rs, n=-1)
# Takes about 3 minutes. 
detach("package:RPostgreSQL", unload=TRUE)
# 497,477 obs as of ~ 9:30am 12/12

# Looking at data
summary(fb)

# There are about 72k records with missing performance information.  Which videos have missing total_views? 
test <- as.vector(unique(filter(fb, is.na(total_views))$video_id)) # 18 videos

# Do any of them ever have non-missing total_views? 
test2 <- filter(fb, video_id %in% test & !is.na(total_views))
# 0 obs - so these videos that have missing total_views never end up having views recorded. 

test3 <- filter(fb, video_id == '10153836068220493')
# I went to the permanlink for this video at https://business.facebook.com/moveon/videos/10153836068220493/ and it
# shows it as having 159,000 views and having been published.  I'm not sure why it doesn't show up with data in our
# table though. Are these all videos that were published right around when Sandra started collecting the data?

test4 <- fb %>% filter(video_id %in% test) %>% group_by(video_id, created_time) %>% distinct(.keep_all=TRUE)
# No, they were created at all different times. 

# Getting table of all the observed times to make sure there are no gaps
test <- mutate(as.data.frame(table(fb$snapshot_time)), snapshot_time = as.POSIXct(Var1))
test2 <- slide(test, Var = "snapshot_time", slideBy = -1) # Gets lagged snapshot_time
names(test2)
names(test2)[4] <- "snapshot_timelag"
test2 <- mutate(test2, snapshot_timelag = as.POSIXct(snapshot_timelag, origin = "1970-01-01"),
                timediff = snapshot_time - snapshot_timelag)
test3 <- filter(test2, timediff > 12)
# 20 obs
# There are odd breaks in the times where data was not pulled.  None of them happen in the 
# newly implemented script starting on 12/1 so I'm not going to worry about them. 

# There is also an issue with a period of time when the query wasn't working and instead of populating
# the table with new data every 10 minutes, it kept populating with the same data over and over.  This
# happened on 2016-11-07 19:20:06 and wasn't identified and fixed until 2016-12-01 21:10:05.  So 
# we have lots of duplicate data at the 11/7 time and no data between then and 12/1.  Therefore I will
# keep only 1 record on 2016-11-07 19:20:06 and 
# will be careful to exclude any videos that were created recently and may not have reached full viewership. 

# Excluding records after the last 11/7 timestamp and records missing no total_views.
test <- fb %>% filter(!is.na(total_views) & snapshot_time != '2016-11-07 19:20:06') # 193541
fb1 <- fb %>% filter(!is.na(total_views)) %>% distinct() # 193609 obs
test <- filter(fb1, snapshot_time == '2016-11-07 19:20:06') # 68 obs - these are all distinct videos so all the records at 
# this time are in fact perfect duplicates. Plus 193541 + 68 = 193609 

# 193609 obs. 

# Bringing over total video info which will have the current total stats.  Will use this to 
# compare total_views from our time series with total_views to date to see when the video 
# reaches 90% of total views in the time series. 
library(RPostgreSQL)

drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, user = 'analytics', password = 'wgBjnlFSqsqfD7QYL0aHBASd', dbname = 'dev', 
                 host = 'moveon.csqr1hggac2r.us-west-2.redshift.amazonaws.com', port=5439)

que <- "SELECT video_id, description, total_views as total_views_to_date
FROM facebook.videos"
rs <- dbSendQuery(con, que)
videos <- fetch(rs, n=-1)
# 293 videos 

detach("package:RPostgreSQL", unload=TRUE)

# Joining in total views to our time series data.  Adding percent of total views and hours since post variables 
fb2 <- merge(x=fb1, y=videos, by.x = "video_id", by.y = "video_id", all.x=TRUE)
fb2$pct_of_total <- with(fb2, round(total_views/total_views_to_date,2))
fb2$hours_since_post <- with(fb2, round(as.numeric(difftime(snapshot_time, created_time, units = "hours")),3))

# Did all of the time series videos have a record in the total video file? 
summary(fb2)
# total_views_to_date is never missing but it does equal 0 for at least one video

test <- filter(fb2, total_views_to_date == 0)
# This is for an Alicia Keys video that had two time series records with about 1,000 views. 
# I wonder if the video was posted and then removed?  I will excluding this video from my analysis.

fb3 <- filter(fb2, total_views_to_date != 0)
# 193607 obs. 

# How many distinct videos are there? 
as.data.frame(table(fb3$video_id))
# There are 98 distinct videos in our database. Most have 500-3,000 obs, some have as few as 5-10

# Identifying videos that hit 90% during our gap in data. 
test <- fb3 %>% filter((pct_of_total >= 0.90 & snapshot_time == '2016-12-01 21:10:05') | (pct_of_total < 0.90 & snapshot_time == '2016-11-07 19:20:06')) %>%
  arrange(video_id, snapshot_time)
# Now any videos that meet both criteria - hadn't hit 90% as of the break in the data, but had hit as of the restart - should have
# two records in this df and should be excluded from my analysis.

test2 <- as.data.frame(table(test$video_id))
gapvids <- as.vector(test2$Var1[test2$Freq == 2])
# 11 videos. I will keep them in for now. 

# Adding in lag information between each consecutive record 
fb3 <- arrange(fb3, video_id, snapshot_time)
fb4 <- slide(fb3, Var = "total_views", slideBy = -1)
fb4 <- slide(fb4, Var = "views_10sec", slideBy = -1)
fb4 <- slide(fb4, Var = "hours_since_post", slideBy = -1)

names(fb4)
names(fb4)[c(12:14)] <- c("total_views_lag","views_10sec_lag","hours_since_post_lag")

# Adding variable of change in total views per hour (slope) and % growth per hour
fb5 <- fb4 %>% mutate(total_views_slope = round((total_views - total_views_lag)/(hours_since_post - hours_since_post_lag),4),
                      total_views_pct_growth = round((total_views - total_views_lag)*100/(total_views_lag)/(hours_since_post - hours_since_post_lag),4))

# Checking to make sure each video_id has only one created_time. 
test <- fb5 %>% select(video_id, created_time) %>% distinct(.keep_all = TRUE)
# 98 records so that means each video id must have only one created_time. 

# Does each video have only one title?
test <- fb5 %>% select(video_id, title) %>% distinct(.keep_all = TRUE)
# 104 obs - so no, there are some videos with multiple titles. 

# The duplicated function returns a logical vector indicating whether or not
# there was a prior element (if given a vector) or a prior entire row in a 
# dataframe (if given a dataframe) that matched perfectly.  Thus, it
# returns TRUE for the second and all subsequent occurrences of a value. 
# If you want to find both the first occurrence and all subsequent occurrances
# you can do duplicated(df) | duplicated(df, fromLast=TRUE) because the fromLast
# option marks all initial observations of a value as TRUE and the final occurance as 
# FALSE. 
test2 <- filter(test, duplicated(test$video_id) | duplicated(test$video_id, fromLast = TRUE))
# 11 obs, 5 videos each w/ 2-3 titles. 
# Looking at the time series data for these videos
test3 <- arrange(fb5[fb5$video_id %in% test2$video_id,], video_id, snapshot_time)
# These all have continuous, non-overlapping snapshot times as though the title
# was edited some time after the initial posting.  I will keep the last version of the 
# title for these videos and use that going foward. 

# Add incrementing by-video variable, descending by snapshot time
lasttitle <- fb5 %>%
  group_by(video_id, created_time) %>%
  mutate(snapshot_ct = order(snapshot_time, decreasing = TRUE)) %>%
  filter(snapshot_ct == 1) %>%
  select(video_id, created_time, title)

describe(lasttitle$title)
# Merging these last titles back into our dataframe
fb6 <- merge(x=select(fb5, -title), y=lasttitle, all.x = TRUE)
describe(fb6$title)

# Are there duplicated titles that are assigned to more than one video?
test <- fb6 %>% distinct(video_id, title)
test2 <- filter(test, duplicated(test$title) | duplicated(test$title, fromLast = TRUE))
# Yes - Stuck On Stupid is used twice and there are 10 other videos with missing titles. 
# These must have been reposts of the same videos.
# Looking at the data for these videos
test2$title
test3 <- arrange(fb6[fb6$title %in% test2$title,], video_id, snapshot_time)
# It looks like the first posting of stuck on stupid was stopped after an hour of running
# and then the second was posted.  I will delete the first version.

# I will look at the videos with missing titles on Facebook and assign titles to them.
fb7 <- fb6 %>% filter(video_id != '10153845758145493') %>%
  mutate(title = ifelse(video_id == '10153845896335493', "Anna GOTV",
                        ifelse(video_id == '10153889721805493', "Obama Trump Hardball",
                               ifelse(video_id == '10153926207230493', "Karine re: Trump Staff Picks",
                                      ifelse(video_id == '10153927072670493', "Karine re: Recounts",
                                             ifelse(video_id == '10153927082170493', "Karine re: Margin of Victory",
                                                    ifelse(video_id == '10153936552155493',"Karine re: Trump Breaking Promises",
                                                           ifelse(video_id == '10153936562725493',"Karine re: Cooper Resistance",
                                                                  ifelse(video_id == '10153936577680493',"Karine re:Carrier",
                                                                         ifelse(video_id == '10153951639275493',"Karine re: Boeing",
                                                                                ifelse(video_id == '10153954899150493',"Live Stop Bannon Cap Hill",
                                                                                       title)))))))))))

# 193600 obs. 
as.data.frame(table(fb7$title))

# Flagging videos that may not have stopped growing yet. 
# Looking at growth rate in last 24 hours for each video.
test1 <- fb7 %>% group_by(video_id) %>% filter(snapshot_time == max(snapshot_time)) %>% select(video_id, snapshot_time, hours_since_post, total_views, created_time)
test2 <- fb7 %>% group_by(video_id) %>% filter(snapshot_time %in% c((max(snapshot_time) - (24*3600) - 60):(max(snapshot_time) - (24*3600) + 60))) %>%
  rename(snapshot_time_24 = snapshot_time, hours_since_post_24 = hours_since_post, total_views_24 = total_views) %>%
  select(video_id, snapshot_time_24, hours_since_post_24, total_views_24, created_time)
# Only 97 obs so one video didn't exist 24 hours ago.

# Joining together 
test3 <- merge(test1,test2,by.x = c("video_id","created_time"), by.y=c("video_id","created_time"),all=TRUE)

test4 <- test3 %>% mutate(pct_growth_24 = round((total_views - total_views_24)*100/(total_views_24),2)) %>% arrange(pct_growth_24)
# Getting Unknown Column 'Var 1' errors sporadically.  Doesn't seem to affect the result but I can't figure out why I'm getting them.
# There are 7 videos that grew by at least 1% over the last 24 hours.  2 of them look like steady growers that were posted awhile ago
# but are still going strong and have 3M+ views.  The other 5 were posted very recently on 12/7, 8, or 9 (today is the 12th).  I will flage
# these all as videos that are still potentially growing.

stillgrowing <- as.vector(test4$video_id[test4$pct_growth_24 >= 1])
# Note, it is possible that other videos have currently plateaud and thus have 24 hour growth rates of < 1% but will grow again
# in the future.  There is no way to know this. 



## (2) Analyzing total view trends over time ##



# Add incrementing by-group variable to use as new, shorter, video id
fb7 <- arrange(fb7, video_id) # Not sure if this matters but trying to make sure videos are labeled in same
# order in future iterations. 
fb8 <- transform(fb7, vid_id = match(video_id, unique(video_id))) 

# Getting vector of all video ids
vidids <- unique(fb8$vid_id)
# 97 videos 

# Graphing percent of total views over time.  Splitting into 13 plots so we have only 97 videos per plot
splot <- function(videogroup, df) {
  end = videogroup*7
  start = end - 6
  plot <- ggplot(df[df$vid_id %in% vidids[start:end] & df$hours_since_post <=500,], aes(x = hours_since_post, y = pct_of_total)) +
    geom_point(aes(color=factor(vid_id))) +
    geom_hline(yintercept = 0.90) +
    geom_vline(xintercept = 48)
  return(plot) }
splot(1,fb8)
splot(2,fb8)
splot(3,fb8)
splot(4,fb8)
splot(5,fb8)
splot(6,fb8)
splot(7,fb8)
splot(8,fb8)
splot(9,fb8)
splot(10,fb8)
splot(11,fb8)
splot(12,fb8)
splot(13,fb8)
splot(14,fb8)
# This highlights that most videos have very similar projections where 90% of the views occur within the first 48 hours and 
# they occur in a fairly straight pattern and then immediately stop getting more views.  
# A number of videos (~69-76) fall out because they ONLY had time series records that
# were at least 500 hours from the posted time.  

# It also shows that some videos are distinctly different though - some have a more gradual but still consistent
# slope upward and don't make the 90% mark until much later.  Others have a plateau for a while and then get a boost
# and grow some more.  

# I will exclude the videos that don't seem to have enough information to really see the projection because
# we are either missing data from when the video was first posted or we don't have enough data since
# the video was posted. 
fb9 <- fb8 %>% filter(!(vid_id %in% c(54,59:85)))
# 148,585 obs

# I will group the videos into these three categories for now.  
fb10 <- fb9 %>% mutate(category = ifelse(vid_id %in% c(1,5,6,8,12,13,16,27,38,52,53,96), "plateau",
                                         ifelse(vid_id %in% c(3,4,11,33,36,39,42,51,86,90,91,95), "steady grower",
                                                "90 pct 48 hours")))

# Now plotting again by group
ggplot(fb10[fb10$category == "90 pct 48 hours" & fb10$vid_id <= 20 & fb10$hours_since_post <= 500,], aes(x = hours_since_post, y = pct_of_total)) +
  geom_point(aes(color=factor(vid_id))) +
  geom_hline(yintercept = 0.90) +
  geom_vline(xintercept = 48)
ggplot(fb10[fb10$category == "90 pct 48 hours" & fb10$vid_id %in% c(21:40) & fb10$hours_since_post <= 500,], aes(x = hours_since_post, y = pct_of_total)) +
  geom_point(aes(color=factor(vid_id))) +
  geom_hline(yintercept = 0.90) +
  geom_vline(xintercept = 48)
ggplot(fb10[fb10$category == "90 pct 48 hours" & fb10$vid_id %in% c(41:60) & fb10$hours_since_post <= 500,], aes(x = hours_since_post, y = pct_of_total)) +
  geom_point(aes(color=factor(vid_id))) +
  geom_hline(yintercept = 0.90) +
  geom_vline(xintercept = 48)
# ggplot(fb10[fb10$category == "90 pct 48 hours" & fb10$vid_id %in% c(61:80) & fb10$hours_since_post <= 500,], aes(x = hours_since_post, y = pct_of_total)) +
#   geom_point(aes(color=factor(vid_id))) +
#   geom_hline(yintercept = 0.90) +
#   geom_vline(xintercept = 48)
ggplot(fb10[fb10$category == "90 pct 48 hours" & fb10$vid_id > 80 & fb10$hours_since_post <= 500,], aes(x = hours_since_post, y = pct_of_total)) +
  geom_point(aes(color=factor(vid_id))) +
  geom_hline(yintercept = 0.90) +
  geom_vline(xintercept = 48)
ggplot(fb10[fb10$category == "steady grower" & fb10$vid_id <= 45 & fb10$hours_since_post <= 500,], aes(x = hours_since_post, y = pct_of_total)) +
  geom_point(aes(color=factor(vid_id))) +
  geom_hline(yintercept = 0.90) +
  geom_vline(xintercept = 48)
ggplot(fb10[fb10$category == "steady grower" & fb10$vid_id > 45 & fb10$hours_since_post <= 500,], aes(x = hours_since_post, y = pct_of_total)) +
  geom_point(aes(color=factor(vid_id))) +
  geom_hline(yintercept = 0.90) +
  geom_vline(xintercept = 48)
ggplot(fb10[fb10$category == "plateau" & fb10$vid_id <= 45 & fb10$hours_since_post <= 500,], aes(x = hours_since_post, y = pct_of_total)) +
  geom_point(aes(color=factor(vid_id))) +
  geom_hline(yintercept = 0.90) +
  geom_vline(xintercept = 48)
ggplot(fb10[fb10$category == "plateau" & fb10$vid_id > 45 & fb10$hours_since_post <= 500,], aes(x = hours_since_post, y = pct_of_total)) +
  geom_point(aes(color=factor(vid_id))) +
  geom_hline(yintercept = 0.90) +
  geom_vline(xintercept = 48)

# For the plateau videos, it looks like they mostly plateaued right around 48 hours and then stayed
# at that state for anywhere from 10 hours to 200+ hours until they continued growing again for another 48
# hours and then plateaued again.  A question is why did they start growing again?
# - Were they shared by someone famous which caused a burst in their views?
# - Did we repost them?
# - Did we pay to boost them?
# - Is it related to the day/time they were posted?
# - Is it related to total views? 
# - Something else? 


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

ggplot(fb10[fb10$category == "plateau" & fb10$vid_id %in% c(12,15,7,4,11),], aes(x = hours_since_post, y = pct_of_total)) +
  geom_point(aes(color=factor(substr(title,1,20)))) +
  geom_hline(yintercept = 0.90) +
  geom_vline(xintercept = 48) + 
  scale_color_discrete(name = "Video Title") +
  ggtitle("'Plateau' Video Performance Over Time") +
  labs(x="Hours Since Post",y="Percent of Total Views") 
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












