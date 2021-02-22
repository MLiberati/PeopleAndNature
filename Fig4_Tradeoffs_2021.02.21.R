
##### PURPOSE: Explore two indicator-use scenarios applied to all three teams
#####           to compare relative rankings between teams and between scenarios
##### AUTHOR:  Marjorie R. Liberati

#### INPUT files:
# "IndividualScores_URBAN_2019.11.05.xlsx"
# "IndividualScores_AGRICULTURE_2019.11.05.xlsx"
# "IndividualScores_COASTAL_2019.11.05.xlsx"
# "Indicator Scoring - Data-richness & Realism - 2020.04.16.xlsx"
# "SCORING LIST - METRIC LIST - with Shorthand.xlsx"

#### OUTPUT:
# Figure 4 in "Planning for people and nature"


###------------ SETUP ------------### 

rm(list = ls())

library(readxl) # read excel files directly without converting to csv first
library(writexl) # write directly to excel sheets
library(dplyr) # create summary tables and information
library(reshape2) # use to restructure (i.e., "melt" and "cast") data frames
library(ggplot2) # create figures
library(stringr) # format character objects
library(tibble) # changing dplyr tibbles
library(ggpubr) # arrange ggplots into a grid

###------------ DATA & INFORMATION ------------### 

teams <- c("Urban", "Coastal", "Agriculture") # team names

criteria <- c("Relevant", "Resonant", "Responsive", "Confidence", "Realism", "Data-availability")

Criteria <- rep(rep(criteria[1:3], each=78), 2)
Statistic <- c(rep("Mean",78*3),rep("SD", 78*3))

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# Relevant, Resonant, Responsive, & Confidence

filename <- "IndividualScores_URBAN_2019.11.05.xlsx"
df_urban <- as.data.frame(read_excel(filename, sheet = "URBAN"))
df_urban <- select(df_urban, -contains("Rank"))
df_urban <- melt(df_urban)
df_urban$Criteria <- Criteria
df_urban$Statistic <- Statistic
df_urban$Team <- "Urban"
df_urban$variable <- NULL

filename <- "IndividualScores_COASTAL_2019.11.05.xlsx"
df_coastal <- as.data.frame(read_excel(filename, sheet = "COASTAL"))
df_coastal <- select(df_coastal, -contains("Rank"))
df_coastal <- melt(df_coastal)
df_coastal$Criteria <- Criteria
df_coastal$Statistic <- Statistic
df_coastal$Team <- "Coastal"
df_coastal$variable <- NULL

filename <- "IndividualScores_AGRICULTURE_2019.11.05.xlsx"
df_ag <- as.data.frame(read_excel(filename, sheet = "AGRICULTURE"))
df_ag <- select(df_ag, -contains("Rank"))
df_ag <- melt(df_ag)
df_ag$Criteria <- Criteria
df_ag$Statistic <- Statistic
df_ag$Team <- "Agriculture"
df_ag$variable <- NULL

df_all <- rbind(df_urban, df_coastal, df_ag)

# Realism & DataAvailability

filename <- "Indicator Scoring - Data-richness & Realism - 2020.04.16.xlsx"

df_data_ag <- as.data.frame(read_excel(filename, sheet = "AGRICULTURE"))
df_data_ag <- select(df_data_ag, Shorthand, Realism, 'Data-availability')
df_data_ag <- melt(df_data_ag)
colnames(df_data_ag) <- c("Shorthand", "Criteria", "value")
df_data_ag$Team <- "Agriculture"
df_data_ag$Statistic <- "Mean"

df_data_coastal <- as.data.frame(read_excel(filename, sheet = "COASTAL"))
df_data_coastal <- select(df_data_coastal, Shorthand, Realism, 'Data-availability')
df_data_coastal <- melt(df_data_coastal)
colnames(df_data_coastal) <- c("Shorthand", "Criteria", "value")
df_data_coastal$Team <- "Coastal"
df_data_coastal$Statistic <- "Mean"

df_data_urban <- as.data.frame(read_excel(filename, sheet = "URBAN"))
df_data_urban <- select(df_data_urban, Shorthand, Realism, 'Data-availability')
df_data_urban <- melt(df_data_urban)
colnames(df_data_urban) <- c("Shorthand", "Criteria", "value")
df_data_urban$Team <- "Urban"
df_data_urban$Statistic <- "Mean"

df_data <- bind_rows(df_all, df_data_urban, df_data_coastal, df_data_ag)
df_data$value <- as.numeric(df_data$value)

# Merge all criteria together
df <- bind_rows(df_data, df_data)
df$Scenario <- c(rep("Short-term", nrow(df_data)), rep("Long-term", nrow(df_data)))
df$Criteria <- as.factor(df$Criteria)
df$Statistic <- as.factor(df$Statistic)
df$Team <- as.factor(df$Team)
df$Scenario <- as.factor(df$Scenario)

########## CASE STUDY WEIGHTS ########## 

# AGRICULTURE
AB <- c(5, 3, 3, 0, 4, 5) # baseline moving forward
AD <- c(1, 5, 1, 0, 3, 5) # community engagement

# COASTAL WETLANDS
CA <- c(3, 3, 3, 2, 5, 5) # wetland optimization
CB <- c(5, 5, 4, 3, 0, 0) # full capacity to monitor

# URBAN WATER
UA <- c(5, 4, 0, 0, 1, 1) # communicating progress
UB <- c(4, 3, 5, 1, 1, 1) # design activities and communication for SHC installation

Weight <- c(AB, AD, CA, CB, UA, UB)
Criteria <- rep(criteria, 6)
Team <- c(rep("Agriculture", 12), rep("Coastal", 12), rep("Urban", 12))
Scenario <- rep(c(rep("Short-term",length(criteria)),rep("Long-term", length(criteria))), 3)
weights <- data.frame(Weight, Criteria, Team, Scenario, stringsAsFactors = TRUE)

###########################
#### ALL WEIGHTS normalized
###########################

scaled <- weights %>% 
  group_by(Team, Scenario) %>%
  mutate(Scaled = Weight/max(Weight))

df <- left_join(df, scaled, by = c("Criteria", "Team", "Scenario"))
df$Scenario <- as.factor(df$Scenario)
df$Criteria <- as.factor(df$Criteria)
df$Team <- as.factor(df$Team)

#########################
#### WEIGHT * SCORES
#########################

df <- df %>%
  mutate(WScore = value * Scaled)

#########################
#### StDev of TEAM SCORES
#########################

df_sd <- df %>%
  filter(Statistic == "SD") %>%
  filter(Criteria == "Relevant" | Criteria == "Resonant" | Criteria == "Responsive")

df_sd <- df_sd %>%
  group_by(Scenario, Team, Shorthand) %>%
  summarise(USum = sum(WScore)) %>%
  mutate(value = 3 - (3 / (max(USum)-min(USum)) * (USum - max(USum)) + 3))

df_sd <- left_join(df_sd, filter(scaled, Criteria=='Confidence'), by = c("Team", "Scenario"))
df_sd <- df_sd %>%
  mutate(WScore = value * Scaled)

df_sd$value <- round(df_sd$value, 2)
df_sd$WScore <- round(df_sd$WScore, 2)
df_sd$USum <- NULL
df_sd$Criteria <- "Confidence"
df_sd$Statistic <- "Mean"

##############################
#### Scores for all 6 CRITERIA
##############################

df6 <- bind_rows(df, df_sd)
df6$Scenario <- as.factor(df6$Scenario)
df6$Criteria <- as.factor(df6$Criteria)
df6$Team <- as.factor(df6$Team)
df6$Statistic <- as.factor(df6$Statistic)

##############################
#### ADD DOMAINS
##############################

setwd()
filename <- "SCORING LIST - METRIC LIST - with Shorthand.xlsx"
df_domains <- as.data.frame(read_excel(filename, sheet = "SCORING_LIST"))

df6 <- left_join(df6, df_domains[,3:4], by = "Shorthand")

##############################
#### OVERALL SCORES & RANKINGS
##############################

df_WSM <- df6 %>%
  filter(Statistic == "Mean") %>%
  group_by(Scenario, Team, Shorthand) %>%
  summarise(Overall = sum(WScore), Domain = first(Domain)) %>%
  arrange(Team, desc(Overall))

df_rank <- df6 %>%
  filter(Statistic == "Mean") %>%
  group_by(Scenario, Team, Shorthand) %>%
  summarise(Overall = sum(WScore), Domain = first(Domain)) %>%
  mutate(Rank = dense_rank(desc(Overall)))

########################################
########## SUMMARY TABLES ##############
########################################

df_rank$Scenario <- factor(df_rank$Scenario, levels = c("Short-term", "Long-term"))
df_rank$Team <- factor(df_rank$Team, levels = c("Agriculture", "Coastal", "Urban"))

Tbl_rank <- dcast(df_rank, Shorthand + Domain ~ Scenario + Team, value.var = "Rank")
Tbl_overall <- dcast(df_rank, Shorthand + Domain ~ Scenario + Team, value.var = "Overall")

top5 <- df_rank %>% 
  group_by(Shorthand) %>%
  mutate(MIN = min(Rank)) %>%
  filter(MIN <= 5) %>%
  arrange(Team, Rank)

Tbl_top5 <- dcast(top5, Shorthand + Domain ~ Team + Scenario, value.var = "Rank")
Tbl_top5 <- Tbl_top5 %>% arrange(`Agriculture_Short-term`)

########################################
############### FIGURE 4 ###############
########################################

#--- AGRICULTURE ---#

csFULL <- left_join(df6, df_rank, by = c("Team", "Shorthand", "Scenario"))
csFULL$Criteria <- factor(csFULL$Criteria, levels=rev(criteria))
csFULL$Scenario <- factor(csFULL$Scenario, levels=c("Short-term","Long-term"))
levels(csFULL$Team)[levels(csFULL$Team)=="Coastal"] <- "Coastal wetlands"
levels(csFULL$Team)[levels(csFULL$Team)=="Urban"] <- "Urban water"

cs <- filter(csFULL, Team == "Agriculture", Rank < 6, Statistic == "Mean")
cs <- arrange(cs, Scenario, desc(Overall))

metric_order <- unique(cs %>%
  group_by(Shorthand) %>%
  arrange(Team, Scenario, Rank) %>%
  pull(Shorthand))

cs$Shorthand <- factor(cs$Shorthand, levels=metric_order)

pltA <- ggplot(cs, aes(x=Shorthand, y=WScore, fill=Criteria, label=Rank)) +
  geom_bar(stat = "identity", width = 0.4) +
  ylab("Overall score") +
  xlab("Indicators") +
  scale_y_continuous(limits = c(0,11), breaks = seq(0,11,1)) +
  scale_fill_manual(values=cbPalette) +
  coord_flip() +
  facet_grid(cols=vars(Scenario), rows=vars(Team)) +
  theme_light(base_family = "serif") +
  guides(fill = guide_legend(nrow = 1, reverse=TRUE)) +
  theme(plot.title = element_text(hjust = 0.5, size=10, face="bold"),
        legend.text = element_text(size=8),
        legend.title = element_text(size=8, ,face="bold"),
        legend.position = "top",
        axis.title = element_text(size=8, face="bold"),
        axis.text = element_text(size=8),
        strip.text = element_text(size=10, face = "bold"),
        strip.background = element_rect(fill="gray20"))

#--- COASTAL --#

cs <- filter(csFULL, Team == "Coastal wetlands", Rank < 6, Statistic == "Mean")
cs <- arrange(cs, Scenario, desc(Overall))

metric_order <- unique(cs %>%
                         group_by(Shorthand) %>%
                         arrange(Team, Scenario, Rank) %>%
                         pull(Shorthand))

cs$Shorthand <- factor(cs$Shorthand, levels=metric_order)

pltC <- ggplot(cs, aes(x=Shorthand, y=WScore, fill=Criteria, label=Rank)) +
  geom_bar(stat = "identity", width = 0.4) +
  ylab("Overall score") +
  xlab("Indicators") +
  scale_y_continuous(limits = c(0,11), breaks = seq(0,11,1)) +
  scale_fill_manual(values=cbPalette) +
  coord_flip() +
  facet_grid(cols=vars(Scenario), rows=vars(Team)) +
  theme_light(base_family = "serif") +
  theme(plot.title = element_text(hjust = 0.5, size=10, face="bold"),
        legend.text = element_text(size=7),
        legend.title = element_text(size=8, ,face="bold"),
        axis.title = element_text(size=8, face="bold"),
        axis.text = element_text(size=8),
        strip.text = element_text(size=10, face = "bold"),
        strip.background = element_rect(fill="gray20"))

#--- URBAN --#

cs <- filter(csFULL, Team == "Urban water", Rank < 6, Statistic == "Mean")
cs <- arrange(cs, Scenario, desc(Overall))

metric_order <- unique(cs %>%
                         group_by(Shorthand) %>%
                         arrange(Team, Scenario, Rank) %>%
                         pull(Shorthand))

cs$Shorthand <- factor(cs$Shorthand, levels=metric_order)


pltU <- ggplot(cs, aes(x=Shorthand, y=WScore, fill=Criteria, label=Rank)) +
  geom_bar(stat = "identity", width = 0.4) +
  ylab("Overall score") +
  xlab("Indicators") +
  scale_y_continuous(limits = c(0,11), breaks = seq(0,11,1)) +
  scale_fill_manual(values=cbPalette) +
  coord_flip() +
  facet_grid(cols=vars(Scenario), rows=vars(Team)) +
  theme_light(base_family = "serif") +
  theme(plot.title = element_text(hjust = 0.5, size=10, face="bold"),
        legend.text = element_text(size=7),
        legend.title = element_text(size=8, ,face="bold"),
        axis.title.x = element_text(size=12, face="bold"),
        axis.text = element_text(size=8),
        strip.text = element_text(size=10, face = "bold"),
        strip.background = element_rect(fill="gray20"))

#--- THREE TEAMS ORDERED BY OVERALL SCORE ---#

plt_ag <- pltA + 
  ylab(element_blank()) + 
  xlab(element_blank()) 
plt_cw <- pltC + 
  ylab(element_blank()) + 
  xlab(element_blank()) +
  theme(legend.position = "none")
plt_uw <- pltU + 
  xlab(element_blank()) +
  theme(legend.position = "none")

plt <- ggarrange(plt_ag, plt_cw, plt_uw, 
          ncol=1, nrow=3, align = "v",
          heights=c(1.65,1.20,1.47))

filename <-"Fig4_Planning_for_people_and_nature.tiff"
ggsave(filename, plt, dpi=300, width = 9, height = 6, unit = "in")
