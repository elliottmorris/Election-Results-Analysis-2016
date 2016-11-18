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
##############
# data (end) #
##############

# 2016 map (vote share) #####

data <- left_join(us,results16,by = "region")

data_formatted <- data %>% select(region,county.name.x,state.name,state.abb,candidate,"value" = percent_won,
                                  county.fips.character,state.fips.character) %>% filter(candidate == "Hillary Clinton")

gg <- county_choropleth(data_formatted, num_colors = 1, title = "Clinton Vote Share by County") +
scale_fill_gradient2(high = "blue", 
                     low = "white", 
                     na.value = "gray", 
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

# 2016 map (Clinton Margin)#####

data2 <- data %>% select(region,candidate,"Trump" = percent_won,
                                  county.fips.character,state.fips.character) %>% filter(candidate == "Donald Trump")

data_formatted2 <- left_join(data_formatted,data2,by = "region")
data_formatted2$value <- data_formatted2$value - data_formatted2$Trump

gg <- county_choropleth(data_formatted2, num_colors = 1, title = "Clinton Margin of Victory") +
  scale_fill_gradient2(high = "blue", 
                       low = "red", 
                       na.value = "gray", 
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
                       na.value = "gray", 
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

# Difference Map #####

gg<- county_choropleth(Difference,num_colors = 1, title = "Where Clinton Outperformed Obama") +
scale_fill_gradient2(high = "blue", 
                     low = "red", 
                     na.value = "gray", 
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

# Difference Map (midewst) #####

gg<- county_choropleth(Difference,num_colors = 1, title = "Where Clinton Outperformed Obama",state_zoom = c("ohio","iowa","illinois","indiana","michigan","wisconsin","pennsylvania","missouri","new york","minnesota"))+
  scale_fill_gradient2(high = "blue", 
                       low = "red", 
                       na.value = "gray", 
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

data(df_county_demographics)
demog <- df_county_demographics
diff_hispanic <- left_join(Difference,demog,by = "region")
diff_hispanic <- diff_hispanic %>% filter(percent_hispanic > 25)

gg <- county_choropleth(diff_hispanic,num_colors = 1, title = "Where Clinton Outperformed Obama \n(Counties >25% Hispanic)")+
  scale_fill_gradient2(high = "blue", 
                       low = "red", 
                       na.value = "gray", 
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

data(df_county_demographics)
demog <- df_county_demographics
diff_poor <- left_join(Difference,demog,by = "region")
diff_poor <- diff_poor %>% filter(per_capita_income >25000)

gg <- county_choropleth(diff_poor,num_colors = 1, title = "Where Clinton Overperformed Obama\n (Counties < $25,000 Per Capita Income)")+
  scale_fill_gradient2(high = "blue", 
                       low = "red", 
                       na.value = "gray", 
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

data(df_county_demographics)
demog <- df_county_demographics
diff_poor <- left_join(Difference,demog,by = "region")
diff_poor <- diff_poor %>% filter(percent_white >75)

gg <- county_choropleth(diff_poor,num_colors = 1, title = "Where Clinton Overperformed Obama\n (Counties > 75% White)")+
  scale_fill_gradient2(high = "blue", 
                       low = "red", 
                       na.value = "gray", 
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

data(df_county_demographics)
demog <- df_county_demographics
diff_poor <- left_join(Difference,demog,by = "region")
diff_poor <- diff_poor %>% filter(percent_white >75, per_capita_income < 25000)

gg <- county_choropleth(diff_poor,num_colors = 1, title = "Where Clinton Overperformed Obama\n (Counties >75% White, <$25K Per Capita Income)")+
  scale_fill_gradient2(high = "blue", 
                       low = "red", 
                       na.value = "gray", 
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

nrow(diff_poor[diff_poor$Clinton < diff_poor$Obama,])

