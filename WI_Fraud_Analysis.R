# Wisconsin 2016 Fraud Analysis
# George Elliott Mmrris | TheCrosstab.com 
# Please give credit where credit is due.

# *NOTE: You should run the "County Maps.R" file in the root directory to gather all the data and dependencies.

# libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(choroplethr)
library(choroplethrMaps)
library(grid)
library(gridExtra)

###############
# Regressions #
###############

sample_data <- Difference %>% filter(abbr_state %in% c("wi"))
sample_data$paperballot <- 0
paper_ballot_counties <- c("ashland", "bayfield","brown" ,"columbia" ,"dane" ,"douglas","door" ,"fond du lac","green" ,"kenosha",  
                           "la crosse","lincoln","milwaukee","ozaukee" ,"portage","rock" ,"sauk","st. croix" ,"washington"  ,"waukesha" ,"winnebago" ,"wood")
sample_data[sample_data$county.name %in% paper_ballot_counties,]$paperballot <- 1

model <- lm(sample_data$value ~ sample_data$percent_white + sample_data$BachelorsPlus + sample_data$paperballot)
summary(model)

#############
# Table
#############

library(knitr)

# format

outputTable <- data.frame("County" =sample_data$county.name, "Clinton" =round(sample_data$Clinton2Party, 2),
                          "Obama" =round(sample_data$Obama2Party,2),
                          "Change" = round(sample_data$value, 2),
                          "Paper Ballot" =sample_data$paperballot,
                          "Percent White" = sample_data$percent_white,
                          "Percent College+" = sample_data$BachelorsPlus)

mean(outputTable[outputTable$Paper.Ballot == 1,]$Change)
mean(outputTable[outputTable$Paper.Ballot == 0,]$Change)

mean(outputTable[outputTable$Paper.Ballot == 1,]$Percent.White)
mean(outputTable[outputTable$Paper.Ballot == 0,]$Percent.White)

mean(outputTable[outputTable$Paper.Ballot == 1,]$Percent.College.)
mean(outputTable[outputTable$Paper.Ballot == 0,]$Percent.College.)

kable(outputTable, row.names = FALSE)

#############
#Graphs
#############
#paper 
sample_data1 <- sample_data %>% filter(paperballot == 1)
gg1<- county_choropleth(sample_data1,num_colors = 1, title = "Clinton's Vote Share vs Obama's - \nPaper Ballot Counties", state_zoom = "wisconsin")+
  scale_fill_gradient2(high = "blue", 
                       low = "red", 
                       na.value = "#EAECEE", 
                       breaks = pretty(sample_data1$value, n = 10),name = "Clinton - Obama %") + 
  theme(plot.title = element_text(face = "bold",hjust = .5, size = 20),
        legend.position = "bottom")



grid.newpage()
footnote <- "By @gelliottmorris | thecrosstab.com | elliott@thecrosstab.com"
g <- arrangeGrob(gg1, 
                 right = textGrob(footnote, x = 0, rot = 90, hjust = .8, vjust=1.3, gp = gpar(fontface = "italic", fontsize = 12)))
grid.draw(g)
print(g)

dev.copy(png,"ClintonWIPaper.png",width = 10, height = 8, unit = "in", res = 200)
dev.off()

#machine
sample_data1 <- sample_data %>% filter(paperballot == 0)
gg2<- county_choropleth(sample_data1,num_colors = 1, title = "Clinton's Vote Share vs Obama's - \nMachine Ballot Counties", state_zoom = "wisconsin")+
  scale_fill_gradient2(high = "blue", 
                       low = "red", 
                       na.value = "#EAECEE", 
                       breaks = pretty(sample_data1$value, n = 10),name = "Clinton - Obama %") + 
  theme(plot.title = element_text(face = "bold",hjust = .5, size = 20),
        legend.position = "bottom")



grid.newpage()
footnote <- "By @gelliottmorris | thecrosstab.com | elliott@thecrosstab.com"
g <- arrangeGrob(gg2, 
                 right = textGrob(footnote, x = 0, rot = 90, hjust = .8, vjust=1.3, gp = gpar(fontface = "italic", fontsize = 12)))
grid.draw(g)
print(g)

dev.copy(png,"ClintonWIMachine.png",width = 10, height = 8, unit = "in", res = 200)
dev.off()

#uneducated
sample_data2 <- sample_data %>% filter(BachelorsPlus > mean(BachelorsPlus))
gg3<- county_choropleth(sample_data2,num_colors = 1, title = "Clinton's Vote Share vs Obama's - \nCounties With Above\nAverage Educational Attainment", state_zoom = "wisconsin")+
  scale_fill_gradient2(high = "blue", 
                       low = "red", 
                       na.value = "#EAECEE", 
                       breaks = pretty(sample_data2$value, n = 10),name = "Clinton - Obama %") + 
  theme(plot.title = element_text(face = "bold",hjust = .5, size = 20),
        legend.position = "bottom")



grid.newpage()
footnote <- "By @gelliottmorris | thecrosstab.com | elliott@thecrosstab.com"
g <- arrangeGrob(gg3, 
                 right = textGrob(footnote, x = 0, rot = 90, hjust = .8, vjust=1.3, gp = gpar(fontface = "italic", fontsize = 12)))
grid.draw(g)
print(g)

dev.copy(png,"ClintonWIUrban.png",width = 10, height = 8, unit = "in", res = 200)
dev.off()


## printy 
grid.newpage()
g <- arrangeGrob(gg1, gg3, ncol = 2)
grid.draw(g)

dev.copy(png,"ClintonWinCorr.png",width = 14, height = 8, unit = "in", res = 200)
dev.off()


### Graph #####

gg1 <- ggplot(data = sample_data, aes(x=BachelorsPlus, y = value,col = paperballot)) + geom_point(size=2) + theme_minimal() +
  ggtitle("Clinton - Obama Shift in Wisconsin\n(Predicted by Education Level\nColored by Ballot Type)") +
  theme(plot.title = element_text(face ="bold",size =20, hjust = .5)) + ylab("Clinton %") + xlab("Percent with Bachelors Degree or Higher") +
  geom_smooth(method = "lm", col = "red", se = FALSE) + geom_hline(yintercept = 0, linetype = 2) + theme(legend.position = "none")


gg2 <- ggplot(data = sample_data, aes(x=BachelorsPlus, y = Clinton,col = paperballot)) + geom_point(size=2) + theme_minimal() +
  ggtitle("Clinton's Vote Share in Wisconsin\n(Predicted by Education Level\nColored by Ballot Type)") +
  theme(plot.title = element_text(face ="bold",size =20, hjust = .5)) + ylab("Clinton %") + xlab("Percent with Bachelors Degree or Higher") +
  geom_smooth(method = "lm", col = "red", se = FALSE) + geom_hline(yintercept = 50, linetype = 2)


footnote <- "By @gelliottmorris | thecrosstab.com | elliott@thecrosstab.com"
grid.newpage()
g <- arrangeGrob(gg1, gg2, ncol = 2,bottom = textGrob(footnote, x = 0, rot = 0, hjust = 0, vjust= 0, gp = gpar(fontface = "italic", fontsize = 12)))
grid.draw(g)

dev.copy(png,"ClintonWinStats.png",width = 16, height = 8, unit = "in", res = 200)
dev.off()
