
############################################## Data Import ########################################################
install.packages('tidyverse')
install.packages("corrplot")
install.packages("dplyr")
install.packages("randomForest")
install.packages("caret")
install.packages("ggplot2")
install.packages("png")
install.packages("grid")
install.packages("pROC")
install.packages("ROCR")
library(ROCR)
library(tidyverse)
library(plyr)
library(caret)
library(corrplot)
library(randomForest)
library(mice)
library(grid)
library(ggplot2)
library(pROC)
library(png)
kobe <- read_csv("C:/Users/Anujan Mobile/Desktop/kobe.csv", 
                 col_types = cols(combined_shot_type = col_factor(levels = c("Bank Shot", "Dunk", "Hook Shot", "Jump Shot", "Layup", "Tip Shot")), 
                                  period = col_factor(levels = c("1", "2", "3", "4", "5", "6", "7")),
                                  playoffs = col_factor(levels = c("0","1")), 
                                  shot_type = col_factor(levels = c("2PT Field Goal","3PT Field Goal")), 
                                  shot_zone_area = col_factor(levels = c("Back Court(BC)","Center(C)", "Left Side Center(LC)","Left Side(L)", "Right Side Center(RC)","Right Side(R)")), 
                                  shot_zone_basic = col_factor(levels = c("Mid Range","Above the Break 3", "Backcourt","In The Paint (Non-RA)", "Left Corner 3", "Mid-Range", "Restricted Area", "Right Corner 3")), 
                                  shot_zone_range = col_factor(levels = c("16-24 ft.", "24+ ft.", "8-16 ft.", "Back Court Shot", "Less Than 8 ft.")), 
                                  team_id = col_skip(), 
                                  team_name = col_skip()))
View(kobe)
# imported data through readr
#     Changes Made Initially:
#       - combined_shot_type changed from character to factor
#       - period changed from numeric to factor
#       - Playoff changed from numeric to factor (0 for regular, 1 for playoffs)
#       - shot_type, Shot_zone basic & shot_zone area turned from characters to factor
#       - Team_id & team_name have been removed 

###################################################### Summary of Data #################################################################

summary(kobe)
# using Machine learning tool for NA values
md.pattern(kobe)
# 5000 NA Values in Shot Flag 
# Unsupervised machine learning will need to be employed to use this data. 5000/30697 = 0.1628 
# 5000 entries will be removed as they are outside of the scope of this project
kobe<-na.omit(kobe)
md.pattern(kobe)
# no NA values in the dataset
kobe_original<-kobe
########################################################## Class Changes #################################################################

class(kobe$action_type)
# action_type from character to factor
kobe$action_type<-as.factor(kobe$action_type)
summary(kobe$action_type)

#shot_made_flag from numerical to factor
kobe$shot_made_flag<-as.factor(kobe$shot_made_flag)

#game date in correct format but only year and month required
class(kobe$game_date)
#split the attribute into just months and years
#eliminate the game_date
kobe$game_year<-str_sub(kobe$game_date, start =1, end=4)
kobe$game_month<-str_sub(kobe$game_date, start =6, end=7)
kobe$game_month<-as.factor(kobe$game_month)
class(kobe$game_month)
kobe$game_month = str_replace_all(kobe$game_month,"10", "OCT")
kobe$game_month = str_replace_all(kobe$game_month,"11", "NOV")
kobe$game_month = str_replace_all(kobe$game_month,"12", "DEC")
kobe$game_month = str_replace_all(kobe$game_month,"01", "JAN")
kobe$game_month = str_replace_all(kobe$game_month,"02", "FEB")
kobe$game_month = str_replace_all(kobe$game_month,"03", "MAR")
kobe$game_month = str_replace_all(kobe$game_month,"04", "APR")
kobe$game_month = str_replace_all(kobe$game_month,"05", "MAY")
kobe$game_month = str_replace_all(kobe$game_month,"06", "JUN")
kobe$game_month = str_replace_all(kobe$game_month,"07", "JUL")
kobe$game_month = str_replace_all(kobe$game_month,"08", "AUG")
kobe$game_month = str_replace_all(kobe$game_month,"09", "SEP")

# Season

# Clean and formalize season designation
class(kobe$season)
kobe$season = str_replace_all(kobe$season,"1996-97", "1")
kobe$season = str_replace_all(kobe$season,"1997-98", "2")
kobe$season = str_replace_all(kobe$season,"1998-99", "3")
kobe$season = str_replace_all(kobe$season,"1999-00", "4")
kobe$season = str_replace_all(kobe$season,"Jan-00", "5")
kobe$season = str_replace_all(kobe$season,"Feb-01", "6")
kobe$season = str_replace_all(kobe$season,"Mar-02", "7")
kobe$season = str_replace_all(kobe$season,"Apr-03", "8")
kobe$season = str_replace_all(kobe$season,"May-04", "9")
kobe$season = str_replace_all(kobe$season,"Jun-05", "10")
kobe$season = str_replace_all(kobe$season,"Jul-06", "11")
kobe$season = str_replace_all(kobe$season,"Aug-07", "12")
kobe$season = str_replace_all(kobe$season,"Sep-08", "13")
kobe$season = str_replace_all(kobe$season,"Oct-09", "14")
kobe$season = str_replace_all(kobe$season,"Nov-10", "15")
kobe$season = str_replace_all(kobe$season,"Dec-11", "16")
kobe$season = str_replace_all(kobe$season,"2012-13", "17")
kobe$season = str_replace_all(kobe$season,"2013-14", "18")
kobe$season = str_replace_all(kobe$season,"2014-15", "19")
kobe$season = str_replace_all(kobe$season,"2015-16", "20")
kobe$season<-as.factor(kobe$season)
summary(kobe$season)

#Matchup clean up 

#change LAL @ into home games -> courts are regulation, game location does not effect
#playing away may effect the shot accuracy
kobe$matchup<-gsub("LAL @ ", "AWAY", kobe$matchup)
kobe$matchup<-str_sub(kobe$matchup, start =1, end=4)
kobe$matchup<-gsub("LAL", "HOME", kobe$matchup)
# convert from character to factor 
kobe$matchup<-as.factor(kobe$matchup)
class(kobe$matchup)

#Time Attributes

#time is being measured in game_date,minutes and seconds. 
#convert game time into a single unit
kobe$minutes_remaining<-kobe$minutes_remaining*60

# add both minutes_remaining and seconds_remaining into a single attribute
time_remaining<-kobe$minutes_remaining+kobe$seconds_remaining
kobe$time_remaining<-time_remaining

# loc_x 

# original values are a factor of 10 of feet 
kobe$loc_x<-kobe$loc_x/10

# loc_y

# original values are a factor of 10 of feet 
kobe$loc_y<-kobe$loc_y/10

# Explanation of why loc_x and loc_y reduced by a factor of 10

# loc_x -250 to 250 (25 feet on each side of origin or basketball net)
# loc_y -44 to 791 (shots taken 4 feet area behind net and almost at opposite end)
# the dimensions of a regulation NBA court as per NBA.com = 94 feet by 50 feet
# assuming location of net to be origin (0,0) 
# 1 foot in distance = 10 points in the cartesian plane

# Opponent
# team changes due to relocation and name changes. Dataset has all of these instances as individual teams

kobe$opponent = str_replace_all(kobe$opponent,"NJN", "BKN")
kobe$opponent = str_replace_all(kobe$opponent,"VAN", "MEM")
kobe$opponent = str_replace_all(kobe$opponent,"SEA", "OKC")
kobe$opponent = str_replace_all(kobe$opponent,"NOH", "NOP")

###################################################### checking outliers #################################################################

#loc_x

boxplot(kobe$loc_x, main=" Boxplot of loc_x", xlab="loc_x variable", ylab="x axis distances")
min(kobe$loc_x)
max(kobe$loc_x)

#no outliers
#The range is between -25 and 25


#loc_y 

boxplot(kobe$loc_y, main=" Boxplot of loc_y", xlab="loc_y variable", ylab="y axis distances")

min(kobe$loc_y)
#-4.4 (not within the court, assuming 1 foot = y=10 - this is 4.4 feet behind the net)
max(kobe$loc_y)
#79.1 (within the scope of the court but near the opposite end of the court)

scatter.smooth(x=kobe$loc_x, y=kobe$loc_y, main="shot location", xlab="width of court", ylab="length of court")

#percentile test
lower_bound <- quantile(kobe$loc_y, 0.01)
#the lowest the lower bound can be is -18
upper_bound<-quantile(kobe$loc_y,0.99)
#the highest the higher bound can be is 271

#counting outliers in loc_y
out <- boxplot.stats(kobe$loc_y)$out
out
# this boxplot stat states which loc_y values are outliers
out_ind <- which(kobe$loc_y %in% c(out))
# this out_ind tells us in which row they are found in
out_ind
# there are 76 potential outliers (76/25697 = 0.002 of dataset)
# cross verification with minutes_remaining and seconds_remaining show these shots were taken at the ends of quarters or ends of game
# These were shot attempts made but unsuccessful. 
# Predictive analysis will be done with data containing the outliers and then once again with the outliers omitted to see the changes
hist(kobe$loc_y, main="Loc_y distribution", xlab="y distance of shot", ylab="Frequency where most shot locations are taken")

#lon

boxplot(kobe$lon,main=" Lon Boxplot", xlab="longitude variable (lon) ", ylab="longitudinal range")
#max is -118.5 and min is -118

#lat

boxplot(kobe$lat, main= "Lat Boxplot", xlab="latitude variable (lat) ", ylab="latitudinal range")
#max is 34.09 and min is 33.25

#time_remaining

boxplot(kobe$time_remaining, main=" Boxplot of time remaining", xlab="time remaining variable", ylab="Range of Time available")
max(kobe$time_remaining)
#714 (basically 11 minutes 59 seconds)
min(kobe$time_remaining)
#0

#shot_distance 

boxplot(kobe$shot_distance, main=" Boxplot of Shot Distance", xlab="Shot_distance variable", ylab="Range of Shot Distances")

min(kobe$shot_distance)
#0 feet from net (this would be a layup)
max(kobe$shot_distance)
# 79 feet from the net (half way would be 47 feet so this is in opposite side of court)

out2 <- boxplot.stats(kobe$shot_distance)$out
#out2 tells us which shot_distances in feet are outliers
out2
out_ind2 <- which(kobe$shot_distance %in% c(out2))
#out_ind2 explains the outlier shot distance row location
out_ind2
# there are 77 potential outliers (57/25697 = 0.002 of dataset)


# Insights on shot_distance

#shot_distance is the diagonal distance from the net (0,0)
# therefore the relationship between loc_x, loc_y and shot distance can be explained using the Pythagorean Theorem

################################################# Not Normalized #########################################

hist(kobe$game_event_id,xlab="game_event_id", ylab="count", main="(Not Normalized - game_event_id)")
hist(kobe$game_id, xlab="game_id", ylab="count", main="(Not Normalized - game id)")
hist(kobe$lat,xlab="Latitude", ylab="count", main="(Not Normalized - Latitude)")
hist(kobe$lon,xlab="Longitude", ylab="count", main="(Not Normalized - Longitude)")
hist(kobe$loc_x,xlab="loc_x", ylab="count", main="(Not Normalized - loc x)")
hist(kobe$loc_y,xlab="loc_y", ylab="count", main="(Not Normalized - loc y)")
hist(kobe$minutes_remaining,xlab="minutes remaining", ylab="count", main="(Not Normalized - minutes remaining)")
hist(kobe$seconds_remaining,xlab="seconds remaining", ylab="count", main="(Not Normalized - seconds remaining)")
hist(kobe$shot_distance,xlab="shot distance", ylab="count", main="(Not Normalized - shot distance)")
hist(kobe$shot_made_flag,xlab="shot made flag", ylab="count", main="(Not Normalized - shot made flag)")
hist(kobe$shot_id,xlab="shot_id", ylab="count", main="(Not Normalized - shot id)")


############################################################## Correlation Matrix ###################################################################

num_kobe<-kobe[,sapply(kobe, is.numeric)]
corrA<-cor(num_kobe, use="complete.obs", method="spearman")
corrplot(corrA)

############################################# Data Analysis ################################################################

################# determine which features should be included in the model #################################################

###### Relationship between loc_x, loc_y #######


#1. all shots plotted

shot_loc_plot<-ggplot(kobe, aes(x=loc_x, y=loc_y))+geom_point(aes(colour =shot_made_flag))
shot_loc_plot + ggtitle("Shot Locations") + theme(plot.title=element_text(hjust = 0.5)) + xlab("Width of Court (ft)") + ylab("Length of Court (ft)")

#doesnt show much information

shot_loc_plot2<-ggplot(kobe, aes(x=loc_x, y=loc_y))+geom_point(aes(colour = shot_zone_area, shape=shot_made_flag))
shot_loc_plot2 + ggtitle("Shot Locations") + theme(plot.title=element_text(hjust = 0.5)) + xlab("Width of Court (ft)") + ylab("Length of Court (ft)")
# shot location based on shot zone area

#2. Hexbin Shot Chart
library(hexbin)
shot_loc_plot3<-ggplot(kobe, aes(x=loc_x, y=loc_y))+stat_binhex(bins=10, colour="gray", alpha=0.7)+ scale_fill_gradientn(colours=c( "aliceblue", "gold", "purple")) + guides(alpha=F, size=F)
shot_loc_plot3 + ggtitle("Shot Locations") + theme(plot.title=element_text(hjust = 0.5)) + xlab("Width of Court (ft)") + ylab("Length of Court (ft)")

#3. Shot Accuracy percentage plot
shot<-ddply(kobe,.(shot_zone_area),summarize,shot_attempted=length(shot_made_flag), shots_made=sum(as.numeric(as.character(shot_made_flag))), mloc_x=mean(loc_x), mloc_y=mean(loc_y))
made<-shot$shots_made
totalshots<-shot$shot_attempted
percentage<-made/totalshots
percentage_label<-paste(as.character(round(100*percentage, 1)))

court<-rasterGrob(readPNG("C:/Users/Anujan Mobile/Desktop/Capstone Project/visualizations/court_all.png"))
ggplot(shot, aes(x=mloc_x, y=mloc_y)) + 
  annotation_custom(court)+
  geom_point(aes(colour = shot_zone_area, size = percentage, alpha = 0.8), size = 8) +
  geom_text(aes(colour = shot_zone_area, label = percentage_label), vjust = -1.2, size = 8) +
  guides(alpha = FALSE, size = FALSE) +
  xlim(25, -25) +
  ylim(-5,90 ) +
  xlab("Width of Court (ft)") + ylab("Length of Court (ft)")+ggtitle("Shot Locations Percentages")+
  coord_fixed() 
###### combined shot type occurrences #######

#4. Action Type
action_freq<-table(kobe$action_type)
action_freq<-as.data.frame(action_freq)
names(action_freq)[1] = "shot_type"
AT<-ggplot(action_freq, aes(x= reorder(shot_type, Freq), y=Freq)) + geom_bar(stat = "identity") + coord_flip() + xlab("Frequency of shot") + ylab("Action_type") + ggtitle("Frequency of Action Types")
# the graph range is from 1 to 15836. That's why certain numbers do not even register
# shot types that have a frequency with less than 100 attempts made will be combined to create "other"
#action_freq$shot_type<-as.factor(action_freq$shot_type)
less_than_100<-filter(action_freq, Freq < 100)
other<-sum(less_than_100$Freq)
action_freq2 <- action_freq[-c(1,2,3,4,6,7,8,9,10,11,13,14,16,18,19,20,21,22,23,25,28,30,31,32,33,35,36,37,38,39,40,42,43,44,45,46,49,51,53,54),] 
other = data.frame(shot_type = "other", Freq = 1082)
action_freq2 = rbind(action_freq2,other)
AT_clean<-ggplot(action_freq2, aes(x= reorder(shot_type, Freq), y=Freq)) + geom_bar(stat = "identity") + coord_flip() +xlab("Action Type") + ylab("Frequency") + ggtitle("Frequency of Action Types")
# Jump shot is still the most frequent type of shot that kobe made but there is more information to describe the shot
kobe$action_type <- recode_factor(kobe$action_type,  
                                  "alley oop dunk shot" = "other", "finger roll shot" = "other", "cutting layup shot" = "other", "driving bank shot" = "other","driving finger roll layup shot" = "other",
                                  "driving finger roll shot" = "other","driving floating bank jump shot" = "other","driving floating jump shot"= "other",
                                  "driving hook shot"= "other","driving jump shot"= "other","driving reverse layup shot"= "other","driving slam dunk shot"= "other",
                                  "fadeaway bank shot"= "other","finger roll layup shot"= "other","floating jump shot"= "other","follow up dunk shot"= "other",
                                  "hook bank shot"= "other","hook shot"= "other","jump hook shot"= "other","pullup bank shot"= "other","putback dunk shot"= "other",
                                  "putback layup shot"= "other","putback slam dunk shot"= "other","reverse dunk shot"= "other","reverse slam dunk shot"= "other",
                                  "running bank shot"= "other","running dunk shot"= "other","running finger roll layup shot"= "other","running finger roll shot"= "other",
                                  "running hook shot"= "other","running layup shot"= "other","running pull-up jump shot"= "other","running reverse layup shot"= "other",
                                  "running slam dunk shot"= "other","running tip shot"= "other","tip layup shot"= "other","turnaround bank shot"= "other",
                                  "turnaround finger roll shot"= "other","turnaround hook shot"= "other", "alley oop layup shot" = "other", "turnaround fadeaway bank jump shot"="other",
                                  " finger roll shot "="other", "cutting finger roll layup shot" ="other" )

#5. combined shot type
combined_freq<-table(kobe$combined_shot_type)
combined_freq<-as.data.frame(combined_freq)
names(combined_freq)[1] = "shot_type"
CST<-ggplot(combined_freq, aes(x= reorder(shot_type, Freq), y=Freq)) + geom_bar(stat = "identity") + coord_flip() +xlab("Frequency of shot") + ylab("combined_shot_type") + ggtitle("Frequency of Combined_shot_types")

#6. comparisons between 2 pt and 3 pt made 
z<-table(kobe$shot_type)
z<-as.data.frame(z)
names(z)[1] = "point_type"
Pt<-ggplot(z, aes(x=point_type, y=Freq)) + geom_bar(position="dodge", stat="identity")
# This shows us that kobe takes more 2PT field Goal attempts over 3PT attempts. 
# This makes sense as when Kobe started playing in the 90s, 3 point shots were not as frequently used. 
# 3 point shot attempts grew in frequency later in Kobe's career
a<-table(kobe$shot_type,kobe$shot_made_flag)
a<-as.data.frame(a)
names(a)[1] = "shot_type"
names(a)[2] = "shot_outcome"
Pt_flag<-ggplot(a, aes( fill= shot_outcome, x=shot_type, y=Freq)) + geom_bar(position="dodge", stat="identity") + xlab("Shot Type") + ylab("Frequency")+ggtitle("Shot Type vs Shot Outcome")

#7. comparisons between shots_made_flag with respect to playoff or regular season
y<-table(kobe$playoffs)
y<-as.data.frame(y)
names(y)[1] = "Regular_or_playoff"
RP<-ggplot(y, aes(x=Regular_or_playoff, y=Freq)) + geom_bar(position="dodge", stat="identity") +xlab("Frequency of Shot Type") + ylab("Regular Season or Post Season")+ggtitle("Shot Type vs Shot Outcome")

#8. This is obvious as there are more NBA regular season games vs playoff games
b<-table(kobe$playoffs,kobe$shot_made_flag)
b<-as.data.frame(b)
names(b)[1] = "Playoff_or_post_season"
names(b)[2] = "shot_outcome"
RP_flag<-ggplot(b, aes( fill= shot_outcome, x=Playoff_or_post_season, y=Freq)) + geom_bar(position="dodge", stat="identity")+xlab("Regular Season / Post Season") + ylab("Frequency")+ggtitle(" Regular Vs Post Season")

#9. Opponent

# shots missed and made against each team 
c<-table(kobe$opponent,kobe$shot_made_flag)
c<-as.data.frame(c)
names(c)[1] = "Opponent"
names(c)[2] = "shot_outcome"
opponent<-ggplot(c, aes( fill= shot_outcome, x=Opponent, y=Freq)) + geom_bar(position="dodge", stat="identity")

# shots made against each team
e<-table(kobe$opponent)
e<-as.data.frame(e)
names(e)[1] = "Opponent"
names(e)[2] = "Freq"
g<-ggplot(e, aes(fill=Opponent, x=Opponent, y=Freq))+ geom_bar( width=0.5, stat="identity")

#pie chart to express percentage of shots made against each team
games<-c(e$Freq)
opp<-c("ATL", "BKN", "BOS", "CHA", "CHI", "CLE", "DAL", "DEN", "DET", "GSW", "HOU", "IND", "LAC", "MEM", "MIA", "MIL", "MIN", "NOP", "NYK", "OKC", "ORL", "PHI", "PHX", "POR", "SAC", "SAS", "TOR", "UTA", "WAS")
pct<-((games/25697)*100)
pct<-round(pct, digits=2)
opp<-paste(opp,pct)
opp<-paste(opp,"%", sep="")
opp_pie<-pie(games, labels=opp, col=rainbow(length(opp)), main="Pie Chart of Opponents")


#10. Home or Away
d<-table(kobe$matchup,kobe$shot_made_flag)
d<-as.data.frame(d)
names(d)[1] = "Home_or_Away"
names(d)[2] = "shot_outcome"
H_A<-ggplot(d, aes( fill= shot_outcome, x=Home_or_Away, y=Freq)) + geom_bar( position="dodge", stat="identity") +ggtitle("Shot Outcome vs Home/Away")


#11. month

j<-table(kobe$game_month)
j<-as.data.frame(j)
names(j)[1] = "month"
names(j)[2] = "shots_taken"
ggplot(j, aes(x=month, y=shots_taken, main="Shots taken per month")) + geom_bar( position="dodge", stat="identity")

#shot months Pie chart

month<-c(j$shots_taken)
mon<-c("APR", "DEC", "FEB", "JAN", "JUN", "MAR", "MAY", "NOV", "OCT")
pct2<-((month/25697)*100)
pct2<-round(pct2, digits=2)
shot_month<-paste(mon,pct2)
shot_month<-paste(shot_month,"%", sep="")
shot_month_pie<-pie(month, labels=shot_month, col=rainbow(length(shot_month)), main="Pie Chart of shots taken in each month")

#12. Year
k<-table(kobe$game_year)
k<-as.data.frame(k)
names(k)[1] = "Year"
names(k)[2] = "shots_taken"
p<-ggplot(k, aes(x=Year, y=shots_taken, fill=kobe$shot_made_flag)) + geom_bar( position="dodge", stat="identity") + xlab("year") + ylab("Frequency") + ggtitle("Shots Attempts made per Year")

l<-table(kobe$game_year,kobe$shot_made_flag)
l<-as.data.frame(l)
names(l)[1] = "year"
names(l)[2] = "shot_outcome"
Y<-ggplot(l, aes( x=year, y=Freq, fill=shot_outcome)) + geom_bar( position="dodge", stat="identity") +ggtitle("Shot Outcome per Year")


#shot per year Pie chart

year<-c(k$shots_taken)
mon<-c("1996", "1997", "1998", "1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016")
pct3<-((year/25697)*100)
pct3<-round(pct2, digits=2)
shot_year<-paste(mon,pct3)
shot_year<-paste(shot_year,"%", sep="")
shot_year_pie<-pie(year, labels=shot_year, col=rainbow(length(shot_year)), main="Pie Chart of shots taken per year")

############################################# Eliminated Variables #######################################################


# game_id and game_event_id do not have an effect on kobe's shot attempt going through. 
# both variables are related to internal NBA information related to all teams, not just LA Lakers
# shot_id is just a designation in the dataset that specifies all 25697 entries. This has no effect on kobe's shot 
kobe = select(kobe, -c(game_id,game_event_id,shot_id))

# lon and lat are redundant as loc_x and loc_y provide more details and only specifies the court regardless
# of the physical location of the court with respect to the world. Lat and Lon specify the locations of the 
# arenas in which the game was played. 
kobe = select(kobe, -c(lon,lat))

#shot_distance will be eliminated as this attribute only specifies a straight distance from the net(0,0)
#shot_distance does not specify location on the court. shot_distance can be specifed as sqrt(loc_x^2 + loc_y^2)
# this becomes redundant as loc_X and loc_y provide details necessary to specify location on the court
kobe = select(kobe, -c(shot_distance))

#the dataset denotes 3 different attributes that split the court into portions to show shot zones which are 
# 1. shot_zone_area - Splits the court into general zones taking into more detail 
# 2. shot_zone_basic - names the area of the shot (close to the net = restricted area) --> does not specify the width of a court, a mid range shot could be 
# both on the left side or right side. 
# 3. shot_zone_range - shows ranges in ft away from the net such as less than 8 feet, 8 to 16 feet etc.(length of court, not width)
kobe = select(kobe, -c(shot_zone_basic, shot_zone_range))

# minutes_remaining and seconds_remaining are parts of the same thing , the time at which the shot was taken
# minutes_remaining can be converted to seconds and added to second_remaining. 
# A new attribute created as Time Remaining which is a sum of minutes and seconds remaining as seconds
kobe = select(kobe, -c(minutes_remaining, seconds_remaining))

# action_type provides more details with the description for the shot when compared to combined_shot_type
# as such combined shot type is redundant 
kobe = select(kobe, -c(combined_shot_type))

# game_date is eliminated as the attribute was split into just month and year. This was done with the hope to see
# changes in kobe's abilities over each year of his career and month
kobe = select(kobe,-c(game_date))

#################################### function to normalize data ########################################################

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
#let
kobe_norm<-kobe

################################### Normalized ############################################################################

kobe_norm$loc_x<-normalize(kobe_norm$loc_x)
kobe_norm$loc_y<-normalize(kobe_norm$loc_y)
kobe_norm$time_remaining<-normalize(kobe_norm$time_remaining)

hist(kobe_norm$loc_x,xlab="loc_x", ylab="count", main="(Normalized - loc x)")
hist(kobe_norm$loc_y,xlab="loc_y", ylab="count", main="(Normalized - loc y)")
hist(kobe_norm$time_remaining,xlab="Time remaining", ylab="count", main="(Normalized - time remaining)")


#################################### Split data into Training and Test Set ############################################################

samp <- sample(nrow(kobe_norm), .8*nrow(kobe_norm), replace = F)
training <- kobe_norm[samp, ]
testing <- kobe_norm[-samp, ]

####################################### Run a Random Forest ###########################################################################

rf_model <- randomForest(shot_made_flag ~ ., ntree=500, data= training, importance= T )
#Call:
#  randomForest(formula = shot_made_flag ~ ., data = training, ntree = 500,      importance = T) 
#Type of random forest: classification
#Number of trees: 500
#No. of variables tried at each split: 3

#OOB estimate of  error rate: 33.07%
#Confusion matrix:
#  0    1 class.error
#0 9226 2224   0.1942358
#1 4575 4532   0.5023608

rf_model1000 <- randomForest(training$shot_made_flag ~ ., ntree=1000, mtry= 7, data= training, importance= T )
library(pROC)
rf_prediction<-predict(rf_model1000,testing,type="prob")
roc_rf <- roc(testing$shot_made_flag, rf_prediction[,2])
roc_rf_auc<-auc(roc_rf)
par(pty="s")
plot(roc_rf, legacy.axes=TRUE,percent=TRUE, lwd=4, main="ROC Curve for Random Forest Model", xlab="False Positive Percentage", ylab="True Positive Percentage", print.auc=T)
#Call:
#  randomForest(formula = shot_made_flag ~ ., data = training, ntree = 1000,      importance = T) 
#Type of random forest: classification
#Number of trees: 1000
#No. of variables tried at each split: 3

#OOB estimate of  error rate: 32.98%
#Confusion matrix:
#  0    1 class.error
#0 9287 2163   0.1889083
#1 4616 4491   0.5068629

# the graph shows us that without cross validation, 1000 trees, mtrys with 3 variables per split
# we get an out of bag error rate of 32.98%
# as seen in the rf_model plot, the black line which represents oob error rate starts to taper around 32%
# Precision --> 64.3% Model accuracy (the prediction is correct around 64.3% of the time)
# Recall --> 50.8% (the model is able to predict true positives 50.8% of the time)
# Accuracy --> 64.5%
# F1 score --> 0.2345

rf_model1000_test <- randomForest(shot_made_flag ~ ., ntree=1000, mtry= 3, data= testing, importance= T )
############ ROC AUC and MSE ###############

### ROC
install.packages("ROCR")
library(ROCR)

