#######################################################
### script for re-making dta frames for CS329E Final ##
#######################################################

# libraries 

library(dplyr)
library(tidyr)

# get data 

#######################################################
# 2016 results
#######################################################

results16 <- read.csv("county_election_results_2016.csv")
names(results16) <- c("abbr_state",
                      "county.name",
                      "candidate",
                      "votes",
                      "percent_won",
                      "percent_complete",
                      "region",
                      "year")

# make new columns for Clinton and Trump
Clinton <- results16 %>% filter(candidate == "Hillary Clinton") %>% select(region,abbr_state,county.name,"Clinton Pct" = percent_won)
Trump <- results16 %>% filter(candidate == "Donald Trump") %>% select(region, "Trump Pct" = percent_won)

# put into new DF
results16NEW <- left_join(Clinton,Trump,by="region")

# save as new CSV
write.csv(results16NEW, "Election_Results_2016.csv",row.names = FALSE)
#######################################################
# 2012 results
#######################################################

results12 <- read.csv("county_election_results_2012.csv")
names(results12) <- c("abbr_state",
                      "county.name",
                      "candidate",
                      "votes",
                      "percent_won",
                      "percent_complete",
                      "region",
                      "year")


# make new columns for Clinton and Trump
Obama <- results12 %>% filter(candidate == "Barack Obama") %>% select(region,abbr_state,county.name,"Obama Pct" = percent_won)
Romney <- results12 %>% filter(candidate == "Mitt Romney") %>% select(region, "Romney Pct" = percent_won)

# put into new DF
results12NEW <- left_join(Obama,Romney,by="region")

# save as new CSV
write.csv(results12NEW, "Election_Results_2012.csv",row.names = FALSE)

#######################################################
# Demographic Data
#######################################################
# get data

data(df_county_demographics)

# write data

write.csv(df_county_demographics, "County_Demographics.csv",row.names = FALSE)

