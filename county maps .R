# Feel free to use the code below, but please mention me in any production. 
# - Elliott

# get updated county results
source("scrape_county_election_results_2016.R")

# libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(choroplethr)
library(choroplethrMaps)
library(grid)
library(gridExtra)

##############
#### data ####
##############

data(county.regions)

us <- county.regions

#2016 results
results16 <- read.csv("county_election_results_2016.csv")
names(results16) <- c("abbr_state",
                    "county.name",
                    "candidate",
                    "votes",
                    "percent_won",
                    "percent_complete",
                    "region",
                    "year")

#2012 

results12 <- read.csv("county_election_results_2012.csv")
names(results12) <- c("abbr_state",
                    "county.name",
                    "candidate",
                    "votes",
                    "percent_won",
                    "percent_complete",
                    "region",
                    "year")

# get clinton - Obama
Clinton <-  results16 %>% filter(candidate == "Hillary Clinton") %>% select("Clinton" =percent_won, region) 
Obama <- results12 %>% filter(candidate == "Barack Obama") %>% select("Obama" = percent_won, region)

# get difference 

Difference <- left_join(Clinton,Obama, by = "region")
Difference$value <- as.numeric(Difference$Clinton) - as.numeric(Difference$Obama)

Difference <- Difference %>% filter(!is.na(region))

# make it 2-party
# get numbers

Clinton <-  results16 %>% filter(candidate == "Hillary Clinton") %>% select("Clinton" =percent_won, region,county.name,abbr_state) 
Trump <-  results16 %>% filter(candidate == "Donald Trump") %>% select("Trump" =percent_won, region) 
Obama <- results12 %>% filter(candidate == "Barack Obama") %>% select("Obama" = percent_won, region)
Romney <- results12 %>% filter(candidate == "Mitt Romney") %>% select("Romney" = percent_won, region)

Difference12 <- left_join(Clinton,Trump, by = "region")
Difference16 <- left_join(Obama,Romney, by = "region")

Difference <- left_join(Difference12,Difference16, by = "region")

Difference$Clinton2Party <- (Difference$Clinton/(Difference$Clinton + Difference$Trump)) * 100
Difference$Obama2Party <-  (Difference$Obama/(Difference$Obama + Difference$Romney)) * 100
Difference$value <- Difference$Clinton2Party - Difference$Obama2Party

Difference <- Difference %>% filter(!is.na(region))

# get demog data
data(df_county_demographics)
demog <- df_county_demographics

# add spreadsheets

edu <- read.csv("Education.csv")
edu <- data.frame(edu[1], edu[length(edu)]) 
names(edu) <- c("region","BachelorsPlus")

unem <- read.csv("Unemployment.csv")
unem <- data.frame(unem[1], unem[length(unem)-2],unem[length(unem)-1]) 
names(unem) <- c("region","Pct.Unemployed.2015","MHI.2014")

# join

Extra <- left_join(edu, unem, "region")
# final demog join
demog <- left_join(demog, Extra, "region")


## COMBINE WITH ELEC RESULTS ## 
data <- left_join(us,results16,by = "region")

data_formatted <- data %>% select(region,county.name.x,state.name,state.abb,candidate,"value" = percent_won,
                                  county.fips.character,state.fips.character) %>% filter(candidate == "Hillary Clinton")
data_formatted <- left_join(data_formatted, demog, by = "region")

## COMBINE WITH DIFFERENCE RESULTS ## 
Difference <- left_join(Difference,demog, "region")

##############
# data (end) #
##############

##############
#### Maps ####
##############

# 2016 map (vote share) #####



gg <- county_choropleth(data_formatted, num_colors = 1, title = "Clinton Vote Share by County") +
scale_fill_gradient2(high = "blue", 
                     low = "white", 
                     na.value = "#EAECEE", 
                     breaks = pretty(data_formatted$value, n = 10),name = "Clinton %") + 
  theme(plot.title = element_text(face = "bold",hjust = .5, size = 20),
        legend.position = "bottom")


grid.newpage()
footnote <- "By @gelliottmorris | thecrosstab.com | elliott@thecrosstab.com"
g <- arrangeGrob(gg, 
                 right = textGrob(footnote, x = 0, rot = 90, hjust = .8, vjust=1.3, gp = gpar(fontface = "italic", fontsize = 12)))
grid.draw(g)
print(g)

dev.copy(png,"ClintonShare2016.png",width = 10, height = 8, unit = "in", res = 200)
dev.off()
# 2016 map (Clinton Margin) #####

data2 <- data %>% select(region,candidate,"Trump" = percent_won,
                                  county.fips.character,state.fips.character) %>% filter(candidate == "Donald Trump")

data_formatted2 <- left_join(data_formatted,data2,by = "region")
data_formatted2$value <- data_formatted2$value - data_formatted2$Trump

gg <- county_choropleth(data_formatted2, num_colors = 1, title = "Clinton Margin of Victory") +
  scale_fill_gradient2(high = "blue", 
                       low = "red", 
                       na.value = "#EAECEE", 
                       breaks = pretty(data_formatted2$value, n = 10),name = "Clinton %") + 
  theme(plot.title = element_text(face = "bold",hjust = .5, size = 20),
        legend.position = "bottom")


grid.newpage()
footnote <- "By @gelliottmorris | thecrosstab.com | elliott@thecrosstab.com"
g <- arrangeGrob(gg, 
                 right = textGrob(footnote, x = 0, rot = 90, hjust = .8, vjust=1.3, gp = gpar(fontface = "italic", fontsize = 12)))
grid.draw(g)
print(g)

dev.copy(png,"ClintonMargin2016.png",width = 10, height = 8, unit = "in", res = 200)
dev.off()
# 2016 map (Clinton Margin - Midwest)#####

gg <- county_choropleth(data_formatted2, num_colors = 1, title = "Clinton Margin of Victory",state_zoom = c("ohio","iowa","illinois","indiana","michigan","wisconsin","pennsylvania","missouri","new york","minnesota"))+
  scale_fill_gradient2(high = "blue", 
                       low = "red", 
                       na.value = "#EAECEE", 
                       breaks = pretty(data_formatted2$value, n = 10),name = "Clinton %") + 
  theme(plot.title = element_text(face = "bold",hjust = .5, size = 20),
        legend.position = "bottom")


grid.newpage()
footnote <- "By @gelliottmorris | thecrosstab.com | elliott@thecrosstab.com"
g <- arrangeGrob(gg, 
                 right = textGrob(footnote, x = 0, rot = 90, hjust = .8, vjust=1.3, gp = gpar(fontface = "italic", fontsize = 12)))
grid.draw(g)
print(g)

dev.copy(png,"ClintonMargin2016MIDWEST.png",width = 10, height = 8, unit = "in", res = 200)
dev.off()

# 2016 map (Clinton Margin - biggest wins/losses)#####

data_formattedBIG <- data_formatted2 %>% filter(abs(value) > 60)

gg <- county_choropleth(data_formattedBIG, num_colors = 1, title = "Clinton Margin of Victory (Bigges Wins / Loses)") +
  scale_fill_gradient2(high = "blue", 
                       low = "red", 
                       na.value = "#EAECEE", 
                       breaks = pretty(data_formattedBIG$value, n = 10),name = "Clinton %") + 
  theme(plot.title = element_text(face = "bold",hjust = .5, size = 20),
        legend.position = "bottom")


grid.newpage()
footnote <- "By @gelliottmorris | thecrosstab.com | elliott@thecrosstab.com"
g <- arrangeGrob(gg, 
                 right = textGrob(footnote, x = 0, rot = 90, hjust = .8, vjust=1.3, gp = gpar(fontface = "italic", fontsize = 12)))
grid.draw(g)
print(g)

dev.copy(png,"ClintonMargin2016BIGSHIFT.png",width = 10, height = 8, unit = "in", res = 200)
dev.off()

# 2016 map (Clinton Margin - cities (>250k))#####

data_formattedcities <- data_formatted2 %>% filter(total_population > 250000)

gg <- county_choropleth(data_formattedcities, num_colors = 1, title = "Clinton Margin of Victory \n(County Population > 250,000)") +
  scale_fill_gradient2(high = "blue", 
                       low = "red", 
                       na.value = "#EAECEE", 
                       breaks = pretty(data_formattedcities$value, n = 10),name = "Clinton Margin %") + 
  theme(plot.title = element_text(face = "bold",hjust = .5, size = 20),
        legend.position = "bottom")


grid.newpage()
footnote <- "By @gelliottmorris | thecrosstab.com | elliott@thecrosstab.com"
g <- arrangeGrob(gg, 
                 right = textGrob(footnote, x = 0, rot = 90, hjust = .8, vjust=1.3, gp = gpar(fontface = "italic", fontsize = 12)))
grid.draw(g)
print(g)

dev.copy(png,"ClintonMargin2016cities.png",width = 10, height = 8, unit = "in", res = 200)
dev.off()
# 2016 map (Clinton Margin - cities (rural))#####

data_formattedrural <- data_formatted2 %>% filter(total_population < 100000)

gg <- county_choropleth(data_formattedrural, num_colors = 1, title = "Clinton Margin of Victory \n(County Population < 100,000)") +
  scale_fill_gradient2(high = "blue", 
                       low = "red", 
                       na.value = "#EAECEE", 
                       breaks = pretty(data_formattedrural$value, n = 10),name = "Clinton Margin %") + 
  theme(plot.title = element_text(face = "bold",hjust = .5, size = 20),
        legend.position = "bottom")


grid.newpage()
footnote <- "By @gelliottmorris | thecrosstab.com | elliott@thecrosstab.com"
g <- arrangeGrob(gg, 
                 right = textGrob(footnote, x = 0, rot = 90, hjust = .8, vjust=1.3, gp = gpar(fontface = "italic", fontsize = 12)))
grid.draw(g)
print(g)

dev.copy(png,"ClintonMargin2016rural.png",width = 10, height = 8, unit = "in", res = 200)
dev.off()
# 2016 map (Clinton Margin - (black))#####

data_formattedblack <- data_formatted2 %>% filter(percent_black > 10)

gg <- county_choropleth(data_formattedblack, num_colors = 1, title = "Clinton Margin of Victory \n(Counties with > 10% Black Voters)") +
  scale_fill_gradient2(high = "blue", 
                       low = "red", 
                       na.value = "#EAECEE", 
                       breaks = pretty(data_formattedblack$value, n = 10),name = "Clinton Margin %") + 
  theme(plot.title = element_text(face = "bold",hjust = .5, size = 20),
        legend.position = "bottom")


grid.newpage()
footnote <- "By @gelliottmorris | thecrosstab.com | elliott@thecrosstab.com"
g <- arrangeGrob(gg, 
                 right = textGrob(footnote, x = 0, rot = 90, hjust = .8, vjust=1.3, gp = gpar(fontface = "italic", fontsize = 12)))
grid.draw(g)
print(g)

dev.copy(png,"ClintonMargin2016Black.png",width = 10, height = 8, unit = "in", res = 200)
dev.off()
# Difference Map #####

gg<- county_choropleth(Difference,num_colors = 1, title = "Where Clinton Outperformed Obama") +
scale_fill_gradient2(high = "blue", 
                     low = "red", 
                     na.value = "#EAECEE", 
                     breaks = pretty(Difference$value, n = 10),name = "Clinton - Obama %") + 
  theme(plot.title = element_text(face = "bold",hjust = .5, size = 20),
        legend.position = "bottom")



grid.newpage()
footnote <- "By @gelliottmorris | thecrosstab.com | elliott@thecrosstab.com"
g <- arrangeGrob(gg, 
                 right = textGrob(footnote, x = 0, rot = 90, hjust = .8, vjust=1.3, gp = gpar(fontface = "italic", fontsize = 12)))
grid.draw(g)
print(g)

dev.copy(png,"ClintonOverperfom.png",width = 10, height = 8, unit = "in", res = 200)
dev.off()
# Difference Map (biggest shifts) #####
DifferenceBIG <- Difference %>% filter(abs(value)>10)

gg<- county_choropleth(DifferenceBIG,num_colors = 1, title = "Where Clinton Outperformed Obama\n(Shifts > 10%)") +
  scale_fill_gradient2(high = "blue", 
                       low = "red", 
                       na.value = "#EAECEE", 
                       breaks = pretty(DifferenceBIG$value, n = 10),name = "Clinton - Obama %") + 
  theme(plot.title = element_text(face = "bold",hjust = .5, size = 20),
        legend.position = "bottom")



grid.newpage()
footnote <- "By @gelliottmorris | thecrosstab.com | elliott@thecrosstab.com"
g <- arrangeGrob(gg, 
                 right = textGrob(footnote, x = 0, rot = 90, hjust = .8, vjust=1.3, gp = gpar(fontface = "italic", fontsize = 12)))
grid.draw(g)
print(g)

dev.copy(png,"ClintonOverperfomBIGGEST.png",width = 10, height = 8, unit = "in", res = 200)
dev.off()

# Difference Map (midewst) #####

gg<- county_choropleth(Difference,num_colors = 1, title = "Where Clinton Outperformed Obama",state_zoom = c("ohio","iowa","illinois","indiana","michigan","wisconsin","pennsylvania","missouri","new york","minnesota"))+
  scale_fill_gradient2(high = "blue", 
                       low = "red", 
                       na.value = "#EAECEE", 
                       breaks = pretty(Difference$value, n = 10),name = "Clinton - Obama %") + 
  theme(plot.title = element_text(face = "bold",hjust = .5, size = 20),
        legend.position = "bottom")



grid.newpage()
footnote <- "By @gelliottmorris | thecrosstab.com | elliott@thecrosstab.com"
g <- arrangeGrob(gg, 
                 right = textGrob(footnote, x = 0, rot = 90, hjust = .8, vjust=1.3, gp = gpar(fontface = "italic", fontsize = 12)))
grid.draw(g)
print(g)

dev.copy(png,"ClintonOverperfomMidwest.png",width = 10, height = 8, unit = "in", res = 200)
dev.off()
# difference (hispanic counties)#####

diff_hispanic <- Difference %>% filter(percent_hispanic > 25)

gg <- county_choropleth(diff_hispanic,num_colors = 1, title = "Where Clinton Outperformed Obama \n(Counties >25% Hispanic)")+
  scale_fill_gradient2(high = "blue", 
                       low = "red", 
                       na.value = "#EAECEE", 
                       breaks = pretty(Difference$value, n = 10),name = "Clinton - Obama %") + 
  theme(plot.title = element_text(face = "bold",hjust = .5, size = 20),
        legend.position = "bottom")



grid.newpage()
note <- "**Note: Clinton may have done poorly in New Mexico
due to a very strong Gary Johnson vote."
footnote <- "By @gelliottmorris | thecrosstab.com | elliott@thecrosstab.com"
g <- arrangeGrob(gg, 
                 bottom = textGrob(note, x = 0, hjust = -0.1, vjust=0.1, gp = gpar(fontface = "italic", fontsize = 12)),
                 right = textGrob(footnote, x = 0, rot = 90, hjust = .8, vjust=1.3, gp = gpar(fontface = "italic", fontsize = 12)))
grid.draw(g)
print(g)

dev.copy(png,"ClintonOverperfomHispanic.png",width = 10, height = 8, unit = "in", res = 200)
dev.off()
# difference poor counties #####

diff_poor <- Difference %>% filter(per_capita_income >25000)

gg <- county_choropleth(diff_poor,num_colors = 1, title = "Where Clinton Overperformed Obama\n (Counties < $25,000 Per Capita Income)")+
  scale_fill_gradient2(high = "blue", 
                       low = "red", 
                       na.value = "#EAECEE", 
                       breaks = pretty(Difference$value, n = 10),name = "Clinton - Obama %") + 
  theme(plot.title = element_text(face = "bold",hjust = .5, size = 20),
        legend.position = "bottom")



grid.newpage()
note <- "**Note: Clinton may have done poorly in New Mexico
due to a very strong Gary Johnson vote."
footnote <- "By @gelliottmorris | thecrosstab.com | elliott@thecrosstab.com"
g <- arrangeGrob(gg, 
                 bottom = textGrob(note, x = 0, hjust = -0.1, vjust=0.1, gp = gpar(fontface = "italic", fontsize = 12)),
                 right = textGrob(footnote, x = 0, rot = 90, hjust = .8, vjust=1.3, gp = gpar(fontface = "italic", fontsize = 12)))
grid.draw(g)
print(g)

dev.copy(png,"ClintonOverperformIncome.png",width = 10, height = 8, unit = "in", res = 200)
dev.off()
# difference white counties #####

diff_poor <- Difference %>% filter(percent_white >75)

gg <- county_choropleth(diff_poor,num_colors = 1, title = "Where Clinton Overperformed Obama\n (Counties > 75% White)")+
  scale_fill_gradient2(high = "blue", 
                       low = "red", 
                       na.value = "#EAECEE", 
                       breaks = pretty(Difference$value, n = 10),name = "Clinton - Obama %") + 
  theme(plot.title = element_text(face = "bold",hjust = .5, size = 20),
        legend.position = "bottom")



grid.newpage()
footnote <- "By @gelliottmorris | thecrosstab.com | elliott@thecrosstab.com"
g <- arrangeGrob(gg, 
                 right = textGrob(footnote, x = 0, rot = 90, hjust = .8, vjust=1.3, gp = gpar(fontface = "italic", fontsize = 12)))
grid.draw(g)
print(g)

dev.copy(png,"ClintonOverperformWhite.png",width = 10, height = 8, unit = "in", res = 200)
dev.off()
# difference poor & white counties #####

diff_poor <- Difference %>% filter(percent_white >75, per_capita_income < 25000)

gg <- county_choropleth(diff_poor,num_colors = 1, title = "Where Clinton Overperformed Obama\n (Counties >75% White, <$25K Per Capita Income)")+
  scale_fill_gradient2(high = "blue", 
                       low = "red", 
                       na.value = "#EAECEE", 
                       breaks = pretty(Difference$value, n = 10),name = "Clinton - Obama %") + 
  theme(plot.title = element_text(face = "bold",hjust = .5, size = 20),
        legend.position = "bottom")



grid.newpage()
footnote <- "By @gelliottmorris | thecrosstab.com | elliott@thecrosstab.com"
g <- arrangeGrob(gg, 
                 right = textGrob(footnote, x = 0, rot = 90, hjust = .8, vjust=1.3, gp = gpar(fontface = "italic", fontsize = 12)))
grid.draw(g)
print(g)

dev.copy(png,"ClintonOverperformPoorWhite.png",width = 10, height = 8, unit = "in", res = 200)
dev.off()
# difference large counties #####

diff_large <- Difference %>% filter(total_population > 500000)

gg <- county_choropleth(diff_large,num_colors = 1, title = "Where Clinton Overperformed Obama\n (Counties > 500,000 People)")+
  scale_fill_gradient2(high = "blue", 
                       low = "red", 
                       na.value = "#EAECEE", 
                       breaks = pretty(diff_large$value, n = 10),name = "Clinton - Obama %") + 
  theme(plot.title = element_text(face = "bold",hjust = .5, size = 20),
        legend.position = "bottom")



grid.newpage()
footnote <- "By @gelliottmorris | thecrosstab.com | elliott@thecrosstab.com"
g <- arrangeGrob(gg, 
                 right = textGrob(footnote, x = 0, rot = 90, hjust = .8, vjust=1.3, gp = gpar(fontface = "italic", fontsize = 12)))
grid.draw(g)
print(g)

dev.copy(png,"ClintonOverperformLargeCounties.png",width = 10, height = 8, unit = "in", res = 200)
dev.off()
# difference (hispanic counties > 90%, two party)#####
diff_hispanic <- Difference %>% filter(percent_hispanic > 90)

# map
gg <- county_choropleth(diff_hispanic,num_colors = 1, title = "Where Clinton Outperformed Obama \n(Counties >90% Hispanic)",state_zoom = "texas")+
  scale_fill_gradient2(high = "blue", 
                       low = "red", 
                       na.value = "#EAECEE", 
                       breaks = pretty(Difference$value, n = 10),name = "Clinton - Obama %") + 
  theme(plot.title = element_text(face = "bold",hjust = .5, size = 20),
        legend.position = "bottom")



grid.newpage()

footnote <- "By @gelliottmorris | thecrosstab.com | elliott@thecrosstab.com"
g <- arrangeGrob(gg, right = textGrob(footnote, x = 0, rot = 90, hjust = .8, vjust=1.3, gp = gpar(fontface = "italic", fontsize = 12)))
grid.draw(g)
print(g)

dev.copy(png,"ClintonOverperfomHispanicTWOPARTY.png",width = 10, height = 8, unit = "in", res = 200)
dev.off()


library(knitr)

# make table
outputTable <- diff_hispanic %>% select("County" =county.name, "% Hispanic" = percent_hispanic, Obama2Party, Clinton2Party, "Shift" = value) %>% filter(County %in% c("starr","maverick","webb","zavala","zapata","jim hogg"))
outputTable$Clinton2Party <- round(as.numeric(outputTable$Clinton2Party) , 2)
outputTable$Obama2Party<-  round(as.numeric(outputTable$Obama2Party) , 2)
outputTable$Shift <- round(as.numeric(outputTable$Shift) , 2)

#outputTable$Clinton2Party <- outputTable$Clinton2Party - (100 - outputTable$Clinton2Party)
#outputTable$Obama2Party <- outputTable$Obama2Party - (100 - outputTable$Obama2Party)
#outputTable$Shift <- round(outputTable$Clinton2Party - outputTable$Obama2Party)


outputTable <- outputTable[order(outputTable$`% Hispanic`,decreasing = TRUE),]
outputTable <- rbind(outputTable, c("","","","Average",round(mean(outputTable$Shift),2)))
kable(outputTable,row.names = FALSE)
# difference (hispanic counties > 50%, two party)#####

#get numbers

diff_hispanic <- Difference %>% filter(percent_hispanic > 50)

# map
gg <- county_choropleth(diff_hispanic,num_colors = 1, title = "Where Clinton Outperformed Obama \n(Counties >50% Hispanic)")+
  scale_fill_gradient2(high = "blue", 
                       low = "red", 
                       na.value = "#EAECEE",  
                       breaks = pretty(Difference$value, n = 10),name = "Clinton - Obama %") + 
  theme(plot.title = element_text(face = "bold",hjust = .5, size = 20),
        legend.position = "bottom")



grid.newpage()

footnote <- "By @gelliottmorris | thecrosstab.com | elliott@thecrosstab.com"
g <- arrangeGrob(gg, right = textGrob(footnote, x = 0, rot = 90, hjust = .8, vjust=1.3, gp = gpar(fontface = "italic", fontsize = 12)))
grid.draw(g)
print(g)

dev.copy(png,"ClintonOverperfomHispanicTWOPARTY50.png",width = 10, height = 8, unit = "in", res = 200)
dev.off()
# Difference Map IN IOWA #####

gg<- county_choropleth(Difference,num_colors = 1, title = "Where Clinton Outperformed Obama\nIn Iowa",state_zoom = "iowa") +
  scale_fill_gradient2(high = "blue", 
                       low = "red", 
                       na.value = "#EAECEE", 
                       breaks = pretty(Difference$value, n = 10),name = "Clinton - Obama %") + 
  theme(plot.title = element_text(face = "bold",hjust = .5, size = 20),
        legend.position = "bottom")



grid.newpage()
footnote <- "By @gelliottmorris | thecrosstab.com | elliott@thecrosstab.com"
g <- arrangeGrob(gg, 
                 right = textGrob(footnote, x = 0, rot = 90, hjust = .8, vjust=1.3, gp = gpar(fontface = "italic", fontsize = 12)))
grid.draw(g)
print(g)

dev.copy(png,"DifferenceMapIowa.png",width = 10, height = 8, unit = "in", res = 200)
dev.off()
# Difference Map IN OREGON #####

gg<- county_choropleth(Difference,num_colors = 1, title = "Where Clinton Outperformed Obama\nIn Oregon",state_zoom = "oregon") +
  scale_fill_gradient2(high = "blue", 
                       low = "red", 
                       na.value = "#EAECEE", 
                       breaks = pretty(Difference$value, n = 10),name = "Clinton - Obama %") + 
  theme(plot.title = element_text(face = "bold",hjust = .5, size = 20),
        legend.position = "bottom")



grid.newpage()
footnote <- "By @gelliottmorris | thecrosstab.com | elliott@thecrosstab.com"
g <- arrangeGrob(gg, 
                 right = textGrob(footnote, x = 0, rot = 90, hjust = .8, vjust=1.3, gp = gpar(fontface = "italic", fontsize = 12)))
grid.draw(g)
print(g)

dev.copy(png,"DifferenceMapOregon.png",width = 10, height = 8, unit = "in", res = 200)
dev.off()

# Difference Map IN CALIFORNIA #####

gg<- county_choropleth(Difference,num_colors = 1, title = "Where Clinton Outperformed Obama\nIn California",state_zoom = "california") +
  scale_fill_gradient2(high = "blue", 
                       low = "red", 
                       na.value = "#EAECEE", 
                       breaks = pretty(Difference$value, n = 10),name = "Clinton - Obama %") + 
  theme(plot.title = element_text(face = "bold",hjust = .5, size = 20),
        legend.position = "bottom")



grid.newpage()
footnote <- "By @gelliottmorris | thecrosstab.com | elliott@thecrosstab.com"
g <- arrangeGrob(gg, 
                 right = textGrob(footnote, x = 0, rot = 90, hjust = .8, vjust=1.3, gp = gpar(fontface = "italic", fontsize = 12)))
grid.draw(g)
print(g)

dev.copy(png,"DifferenceMapCalifornia.png",width = 10, height = 8, unit = "in", res = 200)
dev.off()



#########################
## Scale County by Pop ##
#########################

# 2016 map (Clinton Margin) ###

data2 <- data %>% select(region,candidate,"Trump" = percent_won,
                         county.fips.character,state.fips.character) %>% filter(candidate == "Donald Trump")

data_formatted2 <- left_join(data_formatted,data2,by = "region")
data_formatted2$value <- data_formatted2$value - data_formatted2$Trump

gg <- county_choropleth(data_formatted2, num_colors = 1, title = "Clinton Margin of Victory") +
  scale_fill_gradient2(high = "blue", 
                       low = "red", 
                       na.value = "#EAECEE", 
                       breaks = pretty(data_formatted2$value, n = 10),name = "Clinton %") + 
  theme(plot.title = element_text(face = "bold",hjust = .5, size = 20),
        legend.position = "bottom")


grid.newpage()
footnote <- "By @gelliottmorris | thecrosstab.com | elliott@thecrosstab.com"
g <- arrangeGrob(gg, 
                 right = textGrob(footnote, x = 0, rot = 90, hjust = .8, vjust=1.3, gp = gpar(fontface = "italic", fontsize = 12)))
grid.draw(g)
print(g)

dev.copy(png,"SCALEBYSIZE.png",width = 10, height = 8, unit = "in", res = 200)
dev.off()
