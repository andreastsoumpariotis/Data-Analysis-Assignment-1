library(dplyr)
library(tidyr)

##### The Ultimate Halloween Candy Power Ranking #####

candy = read.csv("https://raw.githubusercontent.com/fivethirtyeight/data/master/candy-power-ranking/candy-data.csv")
candy

candy$chocolate = factor(candy$chocolate)
candy$fruity = factor(candy$fruity)
candy$caramel = factor(candy$caramel)
candy$peanutyalmondy = factor(candy$peanutyalmondy)
candy$nougat = factor(candy$nougat)
candy$crispedricewafer = factor(candy$crispedricewafer)
candy$hard = factor(candy$hard)
candy$bar = factor(candy$bar)
candy$pluribus = factor(candy$pluribus)
candy

levels(candy$chocolate) = c("no", "yes")
levels(candy$fruity) = c("no", "yes")
levels(candy$caramel) = c("no", "yes")
levels(candy$peanutyalmondy) = c("no", "yes")
levels(candy$nougat) = c("no", "yes")
levels(candy$crispedricewafer) = c("no", "yes")
levels(candy$hard) = c("no", "yes")
levels(candy$bar) = c("no", "yes")
levels(candy$pluribus) = c("no", "yes")
candy

# 1. Find the top 5 best rated and top 5 worst rated candy

# for top 5 best:
candy %>% group_by(competitorname) %>% 
  mutate(best = desc(winpercent)) %>%
  arrange(best)

# for top 5 worst:
candy %>% group_by(competitorname) %>% 
  mutate(worst = winpercent) %>%
  arrange(worst)

# Top 5 Best: (1) Reeses Peanut Butter Cup (2) Reeses Miniatures 
# (3) Twix (4) Kit Kat (5) Snickers

# Top 5 Worst: (1) Nik L Nip (2) Boston Baked Beans (3) Chiclets
# (4) Super Bubble (5) Jawbusters

# 2. Plot winpercent against sugarpercent. Do you see any association?
# Now, plot winpercent against pricepercent. Do you see any association?

library(ggplot2)

# winpercent against sugarpercent:
qplot(candy$winpercent, candy$sugarpercent) + geom_line()
# There is no association

# winpercent against pricepercent:
qplot(candy$winpercent, candy$pricepercent) + geom_line()
# There is no association

# 3. Consider all the logical-type variables in the dataset. For each logical variable,
# find the average difference in winpercent between the treats that satisfy the
# condition and the treats that don’t satisfy it. Which logical variable seems to have
# the strongest effect on winpercent?

library(dplyr)

# Chocolate (yes)
data_chocolate_yes = candy %>% select(chocolate, winpercent) %>%
  filter(chocolate == "yes") %>%
  arrange(desc(winpercent))
data_chocolate_yes
mean_chocolate_yes = mean(data_chocolate_yes$winpercent)
mean_chocolate_yes #Ans = 60.92153
# Chocolate (no)
data_chocolate_no = candy %>% select(chocolate, winpercent) %>%
  filter(chocolate == "no") %>%
  arrange(desc(winpercent))
data_chocolate_no
mean_chocolate_no = mean(data_chocolate_no$winpercent)
mean_chocolate_no #Ans = 42.14226
(mean_chocolate_yes) - (mean_chocolate_no)
# Ans = 18.77927

# Fruity (yes)
data_fruity_yes = candy %>% select(fruity, winpercent) %>%
  filter(fruity == "yes") %>%
  arrange(desc(winpercent))
data_fruity_yes
mean_fruity_yes = mean(data_fruity_yes$winpercent)
mean_fruity_yes #Ans = 44.11974
# Fruity (no)
data_fruity_no = candy %>% select(fruity, winpercent) %>%
  filter(fruity == "no") %>%
  arrange(desc(winpercent))
data_fruity_no
mean_fruity_no = mean(data_fruity_no$winpercent)
mean_fruity_no #Ans = 55.32712
(mean_fruity_yes) - (mean_fruity_no)
# Ans = -11.20738

# Caramel (yes)
data_caramel_yes = candy %>% select(caramel, winpercent) %>%
  filter(caramel == "yes") %>%
  arrange(desc(winpercent))
data_caramel_yes
mean_caramel_yes = mean(data_caramel_yes$winpercent)
mean_caramel_yes #Ans = 57.34691
# Caramel (no)
data_caramel_no = candy %>% select(caramel, winpercent) %>%
  filter(caramel == "no") %>%
  arrange(desc(winpercent))
data_caramel_no
mean_caramel_no = mean(data_caramel_no$winpercent)
mean_caramel_no #Ans = 48.93054
(mean_caramel_yes) - (mean_caramel_no)
# Ans = 8.416369

# Peanuty/Almondy (yes)
data_peanutyalmondy_yes = candy %>% select(peanutyalmondy, winpercent) %>%
  filter(peanutyalmondy == "yes") %>%
  arrange(desc(winpercent))
data_peanutyalmondy_yes
mean_peanutyalmondy_yes = mean(data_peanutyalmondy_yes$winpercent)
mean_peanutyalmondy_yes #Ans = 63.69714
# Peanuty/Almondy (no)
data_peanutyalmondy_no = candy %>% select(peanutyalmondy, winpercent) %>%
  filter(peanutyalmondy == "no") %>%
  arrange(desc(winpercent))
data_peanutyalmondy_no
mean_peanutyalmondy_no = mean(data_peanutyalmondy_no$winpercent)
mean_peanutyalmondy_no #Ans = 47.67838
(mean_peanutyalmondy_yes) - (mean_peanutyalmondy_no)
# Ans = 16.01876

# Nougat (yes)
data_nougat_yes = candy %>% select(nougat, winpercent) %>%
  filter(nougat == "yes") %>%
  arrange(desc(winpercent))
data_nougat_yes
mean_nougat_yes = mean(data_nougat_yes$winpercent)
mean_nougat_yes #Ans = 60.05188
# Nougat (no)
data_nougat_no = candy %>% select(nougat, winpercent) %>%
  filter(nougat == "no") %>%
  arrange(desc(winpercent))
data_nougat_no
mean_nougat_no = mean(data_nougat_no$winpercent)
mean_nougat_no #Ans = 49.4431
(mean_nougat_yes) - (mean_nougat_no)
# Ans = 10.60878

# Crisped Rice Wafer (yes)
data_crispedricewafer_yes = candy %>% select(crispedricewafer, winpercent) %>%
  filter(crispedricewafer == "yes") %>%
  arrange(desc(winpercent))
data_crispedricewafer_yes
mean_crispedricewafer_yes = mean(data_crispedricewafer_yes$winpercent)
mean_crispedricewafer_yes #Ans = 66.17025
# Crisped Rice Wafer (no)
data_crispedricewafer_no = candy %>% select(crispedricewafer, winpercent) %>%
  filter(crispedricewafer == "no") %>%
  arrange(desc(winpercent))
data_crispedricewafer_no
mean_crispedricewafer_no = mean(data_crispedricewafer_no$winpercent)
mean_crispedricewafer_no #Ans = 48.89401
(mean_crispedricewafer_yes) - (mean_crispedricewafer_no)
# Ans = 17.27624

# Hard (yes)
data_hard_yes = candy %>% select(hard, winpercent) %>%
  filter(hard == "yes") %>%
  arrange(desc(winpercent))
data_hard_yes
mean_hard_yes = mean(data_hard_yes$winpercent)
mean_hard_yes #Ans = 40.50898
# Hard (no)
data_hard_no = candy %>% select(hard, winpercent) %>%
  filter(hard == "no") %>%
  arrange(desc(winpercent))
data_hard_no
mean_hard_no = mean(data_hard_no$winpercent)
mean_hard_no #Ans = 52.41843
(mean_hard_yes) - (mean_hard_no)
# Ans = -11.90945

# Bar (yes)
data_bar_yes = candy %>% select(bar, winpercent) %>%
  filter(bar == "yes") %>%
  arrange(desc(winpercent))
data_bar_yes
mean_bar_yes = mean(data_bar_yes$winpercent)
mean_bar_yes #Ans = 61.29541
# Bar (no)
data_bar_no = candy %>% select(bar, winpercent) %>%
  filter(bar == "no") %>%
  arrange(desc(winpercent))
data_bar_no
mean_bar_no = mean(data_bar_no$winpercent)
mean_bar_no #Ans = 46.71439
(mean_bar_yes) - (mean_bar_no)
# Ans = 14.58102

# Pluribus (yes)
data_pluribus_yes = candy %>% select(pluribus, winpercent) %>%
  filter(pluribus == "yes") %>%
  arrange(desc(winpercent))
data_pluribus_yes
mean_pluribus_yes = mean(data_pluribus_yes$winpercent)
mean_pluribus_yes #Ans = 46.82278
# Pluribus (no)
data_pluribus_no = candy %>% select(pluribus, winpercent) %>%
  filter(pluribus == "no") %>%
  arrange(desc(winpercent))
data_pluribus_no
mean_pluribus_no = mean(data_pluribus_no$winpercent)
mean_pluribus_no #Ans = 54.0664
(mean_pluribus_yes) - (mean_pluribus_no)
# Ans = -7.243623

# Strongest effect on winpercent: chocolate with "18.77927"

##### College admissions dataset #####

college = read.csv("https://vicpena.github.io/admin.csv")
college

# 1. Find the percentage of men who applied and got in and the percentage
# of women who applied and got in. What do you see?

cond = sum(college$Freq)
cond

# percent of men who applied:
cond1 = college$Gender == "Male"
male = college[cond1,]
100*sum(male$Freq)/cond
# Ans = 59.45647%

x = college %>% select(Gender, Admit, Freq) %>%
  filter(Gender == "Male", Admit == "Admitted")
sum(x$Freq)

# percent of men who applied and got accepted:
cond2 = college$Admit == "Admitted"
admitmale = na.omit(male[cond2,])
100*sum(admitmale$Freq)/cond
# Ans = 26.46929%

# percent of women who applied:
cond3 = college$Gender == "Female"
female = college[cond3,]
100*sum(female$Freq)/cond
# Ans = 40.54353%

# percent of women who applied and got accepted:
cond4 = college$Admit == "Admitted"
admitfemale = na.omit(female[cond4,])
100*sum(admitfemale$Freq)/cond
# Ans = 26.46929%

# 2. Now, find the percentage of men who applied and got in by department. 
# Do the same with women. Compare the results with what you found in part 1.

# Department A:

# Total Males in Department A
depta = college %>% filter(Gender == "Male", Dept == "A")
totaldeptamale = depta$Freq
100*sum(totaldeptamale/cond)
# Ans = 18.22802%

# Total Admitted Males in Department A
admitteddepta = college %>% filter(Admit == "Admitted", Gender == "Male", Dept == "A")
admitteddeptamale = admitteddepta$Freq
100*sum(admitteddeptamale/cond)
# Ans = 11.31242%

# Total Females in Department A
depta = college %>% filter(Gender == "Female", Dept == "A")
totaldeptafemale = depta$Freq
100*sum(totaldeptafemale/cond)
# Ans = 2.386213%

# Total Admitted Females in Department A
admitteddepta = college %>% filter(Admit == "Admitted", Gender == "Female", Dept == "A")
admitteddeptafemale = admitteddepta$Freq
100*sum(admitteddeptafemale/cond)
# Ans = 1.966416%

# Department B:

# Total Males in Department B 
deptb = college %>% filter(Gender == "Male", Dept == "B")
totaldeptbmale = deptb$Freq
100*sum(totaldeptbmale/cond)
# Ans = 12.37296%

# Total Admitted Males in Department B
admitteddeptb = college %>% filter(Admit == "Admitted", Gender == "Male", Dept == "B")
admitteddeptbmale = admitteddeptb$Freq
100*sum(admitteddeptbmale/cond)
# Ans = 7.799381%

# Total Females in Department B
deptb = college %>% filter(Gender == "Female", Dept == "B")
totaldeptbfemale = deptb$Freq
100*sum(totaldeptbfemale/cond)
# Ans = 0.5523641%

# Total Admitted Females in Department B
admitteddeptb = college %>% filter(Admit == "Admitted", Gender == "Female", Dept == "B")
admitteddeptbfemale = admitteddeptb$Freq
100*sum(admitteddeptbfemale/cond)
# Ans = 0.3756076%

# Department C:

# Total Males in Department C 
deptc = college %>% filter(Gender == "Male", Dept == "C")
totaldeptcmale = deptc$Freq
100*sum(totaldeptcmale/cond)
# Ans = 7.180734%

# Total Admitted Males in Department C
admitteddeptc = college %>% filter(Admit == "Admitted", Gender == "Male", Dept == "C")
admitteddeptcmale = admitteddeptc$Freq
100*sum(admitteddeptcmale/cond)
# Ans = 2.651348%

# Total Females in Department C
deptc = college %>% filter(Gender == "Female", Dept == "C")
totaldeptcfemale = deptc$Freq
100*sum(totaldeptcfemale/cond)
# Ans = 13.10208%

# Total Admitted Females in Department C
admitteddeptc = college %>% filter(Admit == "Admitted", Gender == "Female", Dept == "C")
admitteddeptcfemale = admitteddeptc$Freq
100*sum(admitteddeptcfemale/cond)
# Ans = 4.463102%

# Department D:

# Total Males in Department D
deptd = college %>% filter(Gender == "Male", Dept == "D")
totaldeptdmale = deptd$Freq
100*sum(totaldeptdmale/cond)
# Ans = 9.213433%

# Total Admitted Males in Department D
admitteddeptd = college %>% filter(Admit == "Admitted", Gender == "Male", Dept == "D")
admitteddeptdmale = admitteddeptd$Freq
100*sum(admitteddeptdmale/cond)
# Ans = 3.04905%

# Total Females in Department D
deptd = college %>% filter(Gender == "Female", Dept == "D")
totaldeptdfemale = deptd$Freq
100*sum(totaldeptdfemale/cond)
# Ans = 8.285462%

# Total Admitted Females in Department D
admitteddeptd = college %>% filter(Admit == "Admitted", Gender == "Female", Dept == "D")
admitteddeptdfemale = admitteddeptd$Freq
100*sum(admitteddeptdfemale/cond)
# Ans = 2.894388%

# Department E:

# Total Males in Department E
depte = college %>% filter(Gender == "Male", Dept == "E")
totaldeptemale = depte$Freq
100*sum(totaldeptemale/cond)
# Ans = 4.220062%

# Total Admitted Males in Department E
admitteddepte = college %>% filter(Admit == "Admitted", Gender == "Male", Dept == "E")
admitteddeptemale = admitteddepte$Freq
100*sum(admitteddeptemale/cond)
# Ans = 1.171012%

# Total Females in Department E
depte = college %>% filter(Gender == "Female", Dept == "E")
totaldeptefemale = depte$Freq
100*sum(totaldeptefemale/cond)
# Ans = 8.683164%

# Total Admitted Females in Department E
admitteddepte = college %>% filter(Admit == "Admitted", Gender == "Female", Dept == "E")
admitteddeptefemale = admitteddepte$Freq
100*sum(admitteddeptefemale/cond)
# Ans = 2.076889%

# Department F:

# Total Males in Department F
deptf = college %>% filter(Gender == "Male", Dept == "F")
totaldeptfmale = deptf$Freq
100*sum(totaldeptfmale/cond)
# Ans = 8.241273%

# Total Admitted Males in Department F
admitteddeptf = college %>% filter(Admit == "Admitted", Gender == "Male", Dept == "F")
admitteddeptfmale = admitteddeptf$Freq
100*sum(admitteddeptfmale/cond)
# Ans = 0.4860804%

# Total Females in Department F
deptf = college %>% filter(Gender == "Female", Dept == "F")
totaldeptffemale = deptf$Freq
100*sum(totaldeptffemale/cond)
# Ans = 7.534247%

# Total Admitted Females in Department F
admitteddeptf = college %>% filter(Admit == "Admitted", Gender == "Female", Dept == "F")
admitteddeptffemale = admitteddeptf$Freq
100*sum(admitteddeptffemale/cond)
# Ans = 0.5302696%

##### Fandango Movie Ratings #####

library(fivethirtyeight)
data(fandango)
?fandango
head(fandango)
glimpse(fandango)
fandango

# 1. Identify the Top 5 best rated and Top 5 worst rated movies in the dataset.
# Average over different platforms.
# (only use norm values!)

fandango$avg = rowMeans(fandango[,10:14])
fandango %>% arrange(desc(avg)) %>% select(film, avg) #best
fandango %>% arrange(avg) %>% select(film, avg)  #worst

# Top 5 Best-Rated Movies

# (1) "Inside Out" (2) "About Elly" (3) "Mad Max: Fury Road" (4) "Song of the Sea" (5) "Amy" 

# Top 5 Worst-Rated Movies

# (1) "Fantastic Four" (2) "Paul Blart: Mall Cop 2" (3) "Hot Tub Time Machine 2" (4) "Mortdecai" (5) "The Loft"

# 3. Some movies are loved by the critics, 
# but hated by the audience (and sometimes, it’s the other way around). 
# Given the data you have, create a metric
# to measure discrepancies between user and critic ratings. 
# Create a table that contains the Top 5 movies that seem to appeal 
# to critics but not the audience, and another table with the 
# Top 5 movies that users seem to like more than critics do.
# (only use norm values!)

# Metric between User & Critic Ratings
new_fandango = fandango %>%
  select(film, rt_norm, metacritic_norm, rt_user_norm, metacritic_user_nom)
new_fandango

new_fandango$difference_rt_norms = (new_fandango$rt_norm - new_fandango$rt_user_norm)
new_fandango$difference_metacritic_norms = (new_fandango$metacritic_norm - new_fandango$metacritic_user_nom)

new_fandango %>% select(film, difference_rt_norms, difference_metacritic_norms)

# Critic

fandango$criticavg = rowMeans(fandango[,c(10, 12)])
fandango$criticavg
fandango %>% arrange(desc(criticavg)) %>% select(film, criticavg) #best

# (1) Mr. Turner (2) Inside Out (3) Leviathan (4) Timbuktu (5) Phoenix

critictable <- matrix(c("Mr. Turner", "Inside Out", "Leviathan", "Timbuktu", "Phoenix"),ncol=1,byrow=TRUE)
colnames(critictable) <- c("Best Critic Movies")
rownames(critictable) <- c("#1", "#2", "#3", "#4", "#5")
critictable

# User

fandango$useravg = rowMeans(fandango[,c(11, 13, 14)])
fandango$useravg
fandango %>% arrange(desc(useravg)) %>% select(film, useravg) #best

# (1) Inside Out (2) About Elly (3) Wild Tales (4) Amy (5) Mad max: Fury Road

usertable <- matrix(c("Inside Out", "About Elly", "Wild Tales", "Amy", "Mad max: Fury Road"),ncol=1,byrow=TRUE)
colnames(usertable) <- c("Best User Movies")
rownames(usertable) <- c("#1", "#2", "#3", "#4", "#5")
usertable

##### Lahman Baseball Dataset (Part 1) #####

teams = read.csv("Documents/2019-20/Fall/STA 9750 Basic Software Tools/Homework/HW1/Teams1719.csv")
teams

# 1. Create a statistic that quantifies “home advantage”. You’ll use this statistic for the
# next few questions. There is more than one reasonable choice here. Propose 2
# different statistics and justify why you picked the one you’ll use from now on.

# Home advtanage = (num home games won - num home games lost) / total home games

# Statistic 1:
total_home_games = (teams$HomeW + teams$HomeL)
teams$homeadvantage = (teams$HomeW - teams$HomeL)/total_home_games
teams

# 2. Find home advantage statistics for the American League (AL) 
# and National League (NL) in the 2017-2019 period. Comment on 
# the results. Do you see any differences between leagues? 
# Do you see any evidence of home advantage at all? What are the years 
# where there seems to be more of a home advantage, and those where 
# the effect might not be as strong (or doesn’t seem to be there)?

# American League
AL = teams %>% select(Team, Year, League, homeadvantage) %>%
  filter(League == "AL")
AL
sum(AL$homeadvantage) #2.34537

# National League
NL = teams %>% select(Team, Year, League, homeadvantage) %>%
  filter(League == "NL")
NL
sum(NL$homeadvantage) #3.477822

# Ans = Home advantage seems to play a slightly bigger role within
# the National League for the 2017-19 seasons

# Home advantage between years:

# American League 2017
AL_2017 = teams %>% select(Team, Year, League, homeadvantage) %>%
  filter(League == "AL", Year == "2017")
AL_2017
sum(AL_2017$homeadvantage) #1.123457

# National League 2017
NL_2017 = teams %>% select(Team, Year, League, homeadvantage) %>%
  filter(League == "NL", Year == "2017")
NL_2017
sum(NL_2017$homeadvantage) #1.270927

# American League 2018
AL_2018 = teams %>% select(Team, Year, League, homeadvantage) %>%
  filter(League == "AL", Year == "2018")
AL_2018
sum(AL_2018$homeadvantage) #0.9012346

# National League 2018
NL_2018 = teams %>% select(Team, Year, League, homeadvantage) %>%
  filter(League == "NL", Year == "2018")
NL_2018
sum(NL_2018$homeadvantage) #0.7624511

# American League 2019
AL_2019 = teams %>% select(Team, Year, League, homeadvantage) %>%
  filter(League == "AL", Year == "2019")
AL_2019
sum(AL_2019$homeadvantage) #0.320679

# National League 2019
NL_2019 = teams %>% select(Team, Year, League, homeadvantage) %>%
  filter(League == "NL", Year == "2019")
NL_2019
sum(NL_2019$homeadvantage) #1.444444

# Ans = For the 2017 and 2018 seasons, home advantage didn't seem
# to play that big of a role between the two leagues. However,
# for the 2019 season, home advantage seemed to play a huge overall 
# role for national league teams.

# 3. Find the teams that had the highest and lowest home 
# advantage effect by league in 2017, 2018, and 2019 
# separately. Comment on the results.

# Teams American League 2017
AL_2017 = teams %>% select(Team, Year, League, homeadvantage) %>%
  filter(League == "AL", Year == "2017") %>%
  arrange(desc(homeadvantage)) #highest
AL_2017
AL_2017 = teams %>% select(Team, Year, League, homeadvantage) %>%
  filter(League == "AL", Year == "2017") %>%
  arrange(homeadvantage) #lowest
AL_2017

# Ans = Yankees (highest) ; Tigers (lowest)

# Teams National League 2017
NL_2017 = teams %>% select(Team, Year, League, homeadvantage) %>%
  filter(League == "NL", Year == "2017") %>%
  arrange(desc(homeadvantage)) #highest
NL_2017
NL_2017 = teams %>% select(Team, Year, League, homeadvantage) %>%
  filter(League == "NL", Year == "2017") %>%
  arrange(homeadvantage) #lowest
NL_2017

# Ans = Dodgers (highest) ; Braves & Mets (lowest)

# Teams American League 2018
AL_2018 = teams %>% select(Team, Year, League, homeadvantage) %>%
  filter(League == "AL", Year == "2018") %>%
  arrange(desc(homeadvantage)) #highest
AL_2018
AL_2018 = teams %>% select(Team, Year, League, homeadvantage) %>%
  filter(League == "AL", Year == "2018") %>%
  arrange(homeadvantage) #lowest
AL_2018

# Ans = Red Sox (highest) ; Orioles (lowest)

# Teams National League 2018
NL_2018 = teams %>% select(Team, Year, League, homeadvantage) %>%
  filter(League == "NL", Year == "2018") %>%
  arrange(desc(homeadvantage)) #highest
NL_2018
NL_2018 = teams %>% select(Team, Year, League, homeadvantage) %>%
  filter(League == "NL", Year == "2018") %>%
  arrange(homeadvantage) #lowest
NL_2018

# Ans = Brewers (highest) ; Padres (lowest)

# Teams American League 2019
AL_2019 = teams %>% select(Team, Year, League, homeadvantage) %>%
  filter(League == "AL", Year == "2019") %>%
  arrange(desc(homeadvantage)) #highest
AL_2019
AL_2019 = teams %>% select(Team, Year, League, homeadvantage) %>%
  filter(League == "AL", Year == "2019") %>%
  arrange(homeadvantage) #lowest
AL_2019

# Ans = Astros (highest) ; Tigers (lowest)

# Teams National League 2019
NL_2019 = teams %>% select(Team, Year, League, homeadvantage) %>%
  filter(League == "NL", Year == "2019") %>%
  arrange(desc(homeadvantage)) #highest
NL_2019
NL_2019 = teams %>% select(Team, Year, League, homeadvantage) %>%
  filter(League == "NL", Year == "2019") %>%
  arrange(homeadvantage) #lowest
NL_2019

# Ans = Dodgers (highest) ; Marlins (lowest)

# 4. Which franchise had the highest average home advantage 
# in the 2017-2019 period? Which one had the lowest 
# average home advantage effect?

twenty_17 = teams %>% select(Team, Year, League, homeadvantage) %>%
  filter(Year == "2017") %>%
  arrange(Team)
twenty_17

twenty_18 = teams %>% select(Team, Year, League, homeadvantage) %>%
  filter(Year == "2018") %>%
  arrange(Team)
twenty_18

twenty_19 = teams %>% select(Team, Year, League, homeadvantage) %>%
  filter(Year == "2019") %>%
  arrange(Team)
twenty_19

h = cbind(twenty_17, homeadvantage_2018 = twenty_18$homeadvantage, homeadvantage_2019 = twenty_19$homeadvantage)
h$average_homeadvantage = rowMeans(h[,4:6])
h
h %>% arrange(desc(average_homeadvantage)) %>% 
  select(Team, average_homeadvantage)
# Ans = Yankees (highest) & Tigers (lowest)

##### Lahman Baseball Dataset (Part 2) #####

library(Lahman)
?Lahman
LahmanData
?Pitching

# 1. Let’s consider data from 2018 only and look at the subset of pitchers who pitched
# more than 250 outs. Plot the earned run average (ERA; small values are good
# and big ones are bad) of the pitchers against their age. Do you see any patterns?
# Now, find a table with the average ERAs by age. Do you see any patterns?

players = People %>% select(playerID, birthYear)
players

Pitching_2018 = Pitching %>% select(playerID, yearID, IPouts, ERA) %>%
  filter(yearID == "2018", IPouts >= 250) %>%
  left_join(players)

# Plot
plot(x = Pitching_2018$birthYear, y = Pitching_2018$ERA, main="MLB Pitchers and their ERA's", 
     xlab="Pitcher", ylab="ERA", pch=1)

# Table
pitchingTable = Pitching_2018 %>% select(birthYear, ERA)
pitchingTable
table(pitchingTable$ERA, pitchingTable$birthYear)

# 2. Again, let’s look at pitchers who pitched more than 250 outs 
# in 2018. Identify the top 5 best and worst pitchers, 
# in terms of ERA.

Pitching_2018 = Pitching %>% select(playerID, yearID, IPouts, ERA) %>%
  filter(yearID == "2018", IPouts >= 250) %>% 
  arrange(ERA) #best
Pitching_2018

Pitching_2018 = Pitching %>% select(playerID, yearID, IPouts, ERA) %>%
  filter(yearID == "2018", IPouts >= 250) %>% 
  arrange(desc(ERA)) #worst
Pitching_2018

# The top five best pitchers by ERA are: 
# (1) Jacob DeGrom (degroja01) (2) Blake Snell (snellbl01) 
# (3) Clay Buchholz (buchhcl01) (4) Chris Sale (salech01) 
# (5) Trevor Bauer (bauertr01)

# The top five worst pitchers by ERA are:
# (1) Matt Moore (moorema02) (2) Martín Pérez (perezma02) 
# (3) Lucas Giolito (giolilu01) (4) Homer Bailey (baileho02)
# (5) Jason Hammel (hammeja01)

# 3. Consider the best pitcher (in terms of ERA) that you found 
# in part 2. Find his ERA by season throughout his career. 
# Based on this alone, do you think he’s already “peaked”? 
# If you like baseball, you’re welcome to share your opinion here 
# as well.

pitcher_with_best_ERA = Pitching %>% select(playerID, yearID, ERA) %>%
  filter(playerID == "degroja01")
pitcher_with_best_ERA

# Based on this data alone, it appears that he has already peaked.
# However, as a baseball and Mets fan, he is bound to have more great
# seasons (he had a great season this year as well).

# 4. Let’s do a similar exercise, but now with batting average (BA; more is better).
# Use the battingStats function in Lahman to find BAs. Consider data from 2018
# only and look at players that have more than 200 at bats (AB). Plot BA against
# age. Do you see any patterns? Find a table with average BAs by age. Explain
# what you see.

?battingStats
bstats = battingStats()
batting = bstats %>% select(playerID, yearID, AB, BA) %>%
  filter(yearID == "2018", AB > 200) %>%
  left_join(players)
batting

# Plot
plot(x = batting$birthYear, y = batting$BA, main="MLB Players and their BA's", 
     xlab="Player", ylab="BA", pch=1)

# Table
battingTable = batting %>% select(birthYear, BA)
battingTable
table(battingTable$BA, battingTable$birthYear)

# 5. Again, let’s look at players with more than 200 ABs in 2018. 
# Find the top 5 best and worst players in terms of BA.

batting_2018 = bstats %>% select(playerID, yearID, AB, BA) %>%
  filter(yearID == "2018", AB >= 200) %>% 
  arrange(desc(BA)) #best
batting_2018

batting_2018 = bstats %>% select(playerID, yearID, AB, BA) %>%
  filter(yearID == "2018", AB >= 200) %>% 
  arrange(BA) #worst
batting_2018

# The top five best players by BA are: 
# (1) Mookie Betts (bettsmo01) (2) J.D. Martinez (martijd02) 
# (3) Jeff McNeil (mcneije01) (4) Christian Yelich (yelicch01) 
# (5) José Altuve (altuvjo01)

# The top five worst players by BA are:
# (1)  Chris Davis (davisch02) (2) Sandy León (leonsa01) 
# (3) Dexter Fowler (fowlede01) (4) Aaron Altherr (altheaa01)
# (5) Logan Morrison (morrilo01)

# 6. Consider the best player (in terms of BA) that you found 
# in part 5. Find his BA by season throughout his career. Based on 
# this alone, do you think he’s already “peaked”? If you like 
# baseball, you’re welcome to share your opinion here as well.

player_with_best_BA = bstats %>% select(playerID, yearID, BA) %>%
  filter(playerID == "bettsmo01")
player_with_best_BA

# Based on this data alone, it appears that he has already peaked.
# However, as a baseball fan, he is bound to have more great
# seasons as he had previously  peaked in 2016 
# (before having peaked in 2018) and had a great season this 
# year as well.
