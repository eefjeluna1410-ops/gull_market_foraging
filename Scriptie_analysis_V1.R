#Visualisations of the data
library(readxl)
# Load the package used to import Excel files into R

path <- "C:/Users/Gebruiker/OneDrive - UvA/Documenten/fps jaar 3/Scriptie eindproduct/21-5-2026_database_scriptie_V1.xlsx"
# Stores the file location in a variable, so the path only needs to be written once


Ethogram <- read_excel(path, sheet = "Ethogram")
Markets <- read_excel(path, sheet = "Markets")
Visits <- read_excel(path, sheet = "Visits")
Individuals <- read_excel(path, sheet = "Individuals")
Sunday_activities <- read_excel(path, sheet = "Sunday_activities")
# Imports all worksheets from the Excel file into separate dataframes



library(dplyr)
library(ggplot2)
# Loads ggplot2# Loads the package used for data cleaning and transformation

Visits <- Visits %>%
  mutate(
    Arrival_time_GMT = format(as.POSIXct(Arrival_time_GMT), "%H:%M"),
    Arrival_time_CEST = format(as.POSIXct(Arrival_time_CEST), "%H:%M"),
    Departure_time_GMT = format(as.POSIXct(Departure_time_GMT), "%H:%M"),
    Departure_time_CEST = format(as.POSIXct(Departure_time_CEST), "%H:%M")
  )

Visits$Duration_min <- format(Visits$Duration_min, "%H:%M")
# Converts all time columns into a clean hour:minute format 

library(dplyr)


Sunday_activities <- Sunday_activities %>%
  mutate(
    `Time (to Ams)` = format(
      as.POSIXct(as.numeric(`Time (to Ams)`) * 86400, origin = "1899-12-30"),
      "%H:%M"
    ),
    `Time2 (to ams` = format(
      as.POSIXct(`Time2 (to ams`),
      "%H:%M"
    )
  )
#Converts Excel time values into a clean HH:MM format for analysis and visualisation. 

colSums(is.na(Visits))

library(dplyr)
#Use to clean and filter the data. 
library(ggplot2)
#Use to make graphs.

ggplot(Visits, aes(x = Market, fill = Market_open)) +
  #puts market names on the x-as and colours the bars based on whether the market was open or closed. 
  geom_bar(position = "dodge") +
  #Creates a bar chart, 'dodge' so that the bars are shown next to each other not stacked. 
  labs(
    title = "Bird visits when markets were open vs closed",
    x = "Market",
    y = "Number of visits",
    fill = "Market open"
  ) +
  #Adds labels to the graph.
  theme_minimal() +
  #Applies a clean and simple design to the graph. 
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
#Rotates the market names on the x-axis.
#Overall visualisation of bird visits during open vs closed markets number of visits


ggplot(Visits, aes(x = Market, fill = Market_open)) +
  geom_bar(position = "fill") +
  #Unlike the previous graph each bar is scaled to 100%, so the graph shows proportions instead of total numbers. 
  labs(
    title = "Proportion of bird visits during open vs closed markets",
    x = "Market",
    y = "Proportion",
    fill = "Market open"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
#proportion of bird visits during open vs closed markets


Sunday_summary <- Sunday_activities %>%
  mutate(
    Behaviour = case_when(
      `Went to Amsterdam (close to market)?` == "yes" ~ "Went to Amsterdam",
      `Went to sea?` == "yes" ~ "Went to sea",
      TRUE ~ "Other locations"
    )
  )
#Created a new column in the dataset named 'behaviour'. If 'went to sea' is yes the behaviour is sea. And 'went to Amsterdam' is Amsterdam. If it is anything else it is categorized 'other'. 


ggplot(Sunday_summary, aes(x = Behaviour, fill = Behaviour)) +
  #Used the Sunday_summary dataset with the behaviour categories on the x-axis and different colours for each behaviour type. 
  geom_bar() +
  #Creates a bar chart. 
  labs(
    title = "Behaviour of gulls on market-closed days",
    x = "Behaviour",
    y = "Number of observations"
  ) +
  #Adds labels to the graph. 
  theme_minimal()
#Applies a clean and simple visual style. 
#Behaviour of gulls on market-closed days


ggplot(Sunday_summary, aes(x = factor(Bird_ID), fill = Behaviour)) +
  #X, puts each bird on the x-axis, the factor tells R to treat Bird IDs as categories, not numbers.
  geom_bar(position = "fill") +
  #Creates bar chart, position= 'fill' shows the count in proportions. 
  labs(
    title = "Behaviour per gull on market-closed days",
    x = "Bird ID",
    y = "Proportion"
  ) +
  #Adds labels to the graphs
  theme_minimal()
#Applies a simple and clean design to the graph. 
#Behaviour of gulls on market-closed days per individual

library(stringr)
#Used to work with text, here it helps to detect market names inside the notes column. 

Individuals_clean <- Individuals %>%
  mutate(
    Market = case_when(
      str_detect(Notes, "Plein 40-45") ~ "Plein 40-45",
      str_detect(Notes, "Albert cuyp") ~ "Albert Cuyp Market",
      str_detect(Notes, "Boerenmarkt") ~ "Boerenmarkt",
      str_detect(Notes, "Buikslotermeerplein") ~ "Buikslotermeerplein",
      str_detect(Notes, "Bos en Lommermarkt") ~ "Bos en Lommermarkt",
    )
  )
#Creates a new column in the dataset called 'market'. E.g. str_detect(Notes, "Plein 40-45") ~ "Plein 40-45", checks if the notes contain 'plein 40-45', if yes the new market value becomes 'plein 40-45'. 

ggplot(Individuals_clean, aes(x = Market, y = factor(Bird_ID))) +
  #puts market names on the x-axis. Puts bird ids on y-axis, the factor makes sure bird ids are treated as categories not as numbers. 
  geom_point(size = 4) +
  #Creates points on the graph, size is set to 4. Each point is one gull visiting one market. 
  labs(
    title = "Markets visited by individual gulls",
    x = "Market",
    y = "Bird ID"
  ) +
  #Adds labels to the graph
  theme_minimal() +
  #Applies a clean and simple design. 
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
#Rotates the market names on the x-axis. 
#Bird_iD per market


Sunday_scatter <- Sunday_activities %>%
  filter(!is.na(`Time (to Ams)`)) %>%
  #Removes rows where the column time (to ams) is empty. 
  mutate(
    Time_decimal = as.numeric(substr(`Time (to Ams)`, 1, 2)) +
      #Extract the hours from the time. 
      as.numeric(substr(`Time (to Ams)`, 4, 5))/60
  )
#converts the minute so that they become part of a decimal hour. 

ggplot(Sunday_scatter, aes(x = factor(Bird_ID), y = Time_decimal)) +
  #Puts birdId o the x-axis, factor() makes BirdId categories instead of numbers. And visit time on the y-axis. 
  geom_point(size = 4, alpha = 0.7) +
  #Creates the scatterplot points, each point is one visit, size makes the points bigger, alpha makes the points slightly transparent. 
  labs(
    title = "Amsterdam visit times on market-closed days",
    x = "Bird ID",
    y = "Time of day"
  ) +
  #Adds labels to the graph
  scale_y_continuous(
    #Costumises the y-axis to look like real clock times. 
    breaks = seq(0, 24, by = 2),
    #Create labels every 2 hours. 
    labels = paste0(seq(0, 24, by = 2), ":00")
    #Turn numbers into readable times. 2 --> 14:00
  ) +
  theme_minimal()
#Applies a clean and simple design. 
#Amsterdam visit times on market-closed days


library(dplyr)

Visits <- Visits %>%
  mutate(
    hour = as.numeric(substr(Arrival_time_CEST, 1, 2)),
    minute = as.numeric(substr(Arrival_time_CEST, 4, 5)),
    Time_decimal = hour + minute / 60
  )
Open_scatter <- Visits %>%
  filter(Market_open == "yes")
ggplot(Open_scatter, aes(x = factor(Bird_ID), y = Time_decimal)) +
  geom_point(size = 4, alpha = 0.7) +
  labs(
    title = "Amsterdam visit times on market-open days",
    x = "Bird ID",
    y = "Time of day"
  ) +
  scale_y_continuous(
    breaks = seq(0, 24, by = 2),
    labels = paste0(seq(0, 24, by = 2), ":00")
  ) +
  theme_minimal()
#Exactly the same, but for the market-open days





#The analysis 


# -------------------------
# 1. Make the analysis table
# -------------------------

# Make one row for each bird market combination (only for the birds combinations that do exist).

#
# The table should have these columns:
#
# bird_id
#   A unique name or number for the gull.
#
# market_id
#   The market used by that gull in this row.
#
# bird_market_id
#   A unique label for this bird-market combination.
#   This is useful because one bird can appear in two rows.
#   For example: B1_M1, B1_M4, B2_M6.
#
# market_days_open_per_week
#   How many days per week that market is normally open.
#
# market_type
#   A simple category:
#   "rare"         = market open 1 or 2 days per week
#   "intermediate" = market open 3 days per week
#   "frequent"     = market open 5 or 6 days per week
#
# O
#   Number of open opportunity periods for the market in this row.
#   In the simple version, one opportunity period can be one day during the
#   relevant market time window.
#   Example: if a market is open 2 days per week and you observed 10 weeks,
#   then O = 2 * 10 = 20.
#
# C
#   Number of closed opportunity periods for the market in this row.
#   Example: if a market is open 2 days per week and you observed 10 weeks,
#   then C = 5 * 10 = 50.
#
# vO
#   Number of observed visits by that bird to this market during open periods.
#
# vC
#   Number of observed visits by that bird to this market during closed periods.
#
# The final table should look like this:
#
# bird_id  market_id  bird_market_id  market_days_open_per_week  market_type   O   C  vO  vC
# B1       M1         B1_M1           1                          rare         10  60   3  12
# B1       M4         B1_M4           3                          intermediate 30  40   6   5
# B2       M6         B2_M6           6                          frequent     60  10  20   2
#
# Save the table as a CSV file, for example:
# gull_market_counts.csv
#
# Then read it into R:

dat <- read.csv2("C:/Users/Gebruiker/OneDrive - UvA/Documenten/fps jaar 3/Scriptie eindproduct/R studio visualisations/21-5-2026_database_scriptie_V1_gulls_count.csv")


# -------------------------------
# 2. Check the data before fitting
# -------------------------------

# Always look at the first few rows.
head(dat)


# Check the column names.
names(dat)

# Check how many bird-market rows are in each market type.
table(dat$Market_type)

# Check how many rows each bird has.
# Most birds should have one or two rows.
table(dat$Bird_ID)

# Check that visit counts add up sensibly for each bird-market row.
dat$n_visits <- dat$vO + dat$vC
summary(dat$n_visits)

# Calculate the expected fraction of visits during open periods.
#
# This is what we would expect if visits were random with respect to market
# opening status and only depended on how often the market is open.
dat$p_expected <- dat$O / (dat$O + dat$C)

# Calculate the observed fraction of visits during open periods.
dat$p_observed <- dat$vO / (dat$vO + dat$vC)

# Look at the key columns.
dat[, c("bird_id", "market_id", "bird_market_id", "market_type", "O", "C", "vO", "vC",
        "p_expected", "p_observed")]

# ---------------------------------
# 3. Main model: overall enrichment
# ---------------------------------

# Question:
# Across all bird-market combinations, are visits more concentrated during open periods than
# expected from market opening frequency alone?
#
# Model:
# The response is cbind(vO, vC), meaning:
#   number of visits during open periods,
#   number of visits during closed periods.
#
# The offset qlogis(p_expected) tells the model what open-visit probability is
# expected from the market schedule alone.
#
# The intercept estimates enrichment above that expectation.

model_overall <- glm(
  cbind(vO, vC) ~ 1 + offset(qlogis(p_expected)),
  family = binomial,
  data = dat
)

summary(model_overall)

# The main number is the intercept.
# If the intercept is:
#   close to 0: visits match the market schedule
#   above 0: visits are enriched during open periods
#   below 0: visits are less common during open periods than expected

coef(model_overall)

# Convert the intercept from log-odds to an odds ratio.
# An odds ratio above 1 means enrichment during open periods.
exp(coef(model_overall))


# ----------------------------------------------------
# 4. Robustness check: quasibinomial overall enrichment
# ----------------------------------------------------

# Real birds and markets may differ more than the simple binomial model expects.
# For example, some birds may be much better at timing market visits than others,
# and some markets may be easier or harder to predict.
#
# The quasibinomial model estimates this extra variation.
# The coefficient estimate is usually similar, but the standard error and
# p-value can change.

model_overall_quasi <- glm(
  cbind(vO, vC) ~ 1 + offset(qlogis(p_expected)),
  family = quasibinomial,
  data = dat
)

summary(model_overall_quasi)
exp(coef(model_overall_quasi))


# ----------------------------------------------------
# 5. Market-type model: rare versus frequent markets
# ----------------------------------------------------

# Question:
# Do bird-market combinations at frequent markets show stronger enrichment
# during open periods than bird-market combinations at rare markets?
#
# For this simple comparison, use only rare and frequent markets.
# Exclude intermediate markets.

dat_rf <- dat[dat$Market_type == "rare" | dat$Market_type == "frequent", ]

# Check how many birds are in each group.
table(dat_rf$Market_type)

# In this project we expect both rare and frequent markets to be present.

dat_rf$Market_type <- factor(dat_rf$Market_type)
dat_rf$Market_type <- relevel(dat_rf$Market_type, ref = "rare")



model_market_type <- glm(
  cbind(vO, vC) ~ Market_type + offset(qlogis(p_expected)),
  family = binomial,
  data = dat_rf
)

summary(model_market_type)
exp(coef(model_market_type))

# Interpretation:
#
# The intercept is the enrichment for rare-market bird-market combinations.
#
# The coefficient called market_typefrequent is the extra enrichment for
# frequent-market rows compared with rare-market rows.
#
# If market_typefrequent is above 0, then frequent-market rows show stronger
# enrichment than rare-market rows.
#
# exp(market_typefrequent) is the odds ratio for that difference.

# look at only the frequent

dat_f<-dat_rf[dat_rf$Market_type=='frequent',]


model_f_type <- glm(
  cbind(vO, vC) ~ offset(qlogis(p_expected)),
  family = binomial,
  data = dat_f
)

summary(model_f_type)

# -------------------------------------------------------------
# 6. Robustness check: quasibinomial rare versus frequent model
# -------------------------------------------------------------

model_market_type_quasi <- glm(
  cbind(vO, vC) ~ Market_type + offset(qlogis(p_expected)),
  family = quasibinomial,
  data = dat_rf
)

summary(model_market_type_quasi)
exp(coef(model_market_type_quasi))


# ----------------
# 7. Simple plots
# ----------------

# Plot observed and expected open fractions by market type.


boxplot(
  p_observed ~ Market_type,
  data = dat,
  ylim = c(0, 1),
  xlab = "Market type",
  ylab = "Observed fraction of visits during open periods",
  main = "Observed open-period visiting"
)

boxplot(
  p_expected ~ Market_type,
  data = dat,
  ylim = c(0, 1),
  xlab = "Market type",
  ylab = "Expected open fraction from schedule",
  main = "Expected from market schedule"
)

# A simple enrichment ratio:
# observed fraction divided by expected fraction.
# Values above 1 mean more open-period visiting than expected.

dat$enrichment_ratio <- dat$p_observed / dat$p_expected

boxplot(
  enrichment_ratio ~ Market_type,
  data = dat,
  xlab = "Market type",
  ylab = "Observed / expected open fraction",
  main = "Open-period enrichment ratio"
)
abline(h = 1, lty = 2)


summary_table <- aggregate(
  cbind(p_expected, p_observed, enrichment_ratio, n_visits) ~ Market_type,
  data = dat,
  FUN = mean
)
#Creates a summary table for each market type. Calculates the average for each market type. 

n_rows_by_type <- as.data.frame(table(dat$Market_type))
names(n_rows_by_type) <- c("Market_type", "number_of_bird_market_rows")

summary_table <- merge(n_rows_by_type, summary_table, by = "Market_type")

summary_table

write.csv(summary_table, "gull_market_summary_by_type.csv", row.names = FALSE)



#8. Individual statistics


individual_results <- dat %>%
#starts with my dataset and saves the final results under individual_results
  mutate(
    n_visits = vO + vC
#creates a new column, n_visits= vO + vC. Counts the total number of visits for each bird-market row. 
  ) %>%
  rowwise() %>%
#Treat each row separately, because we want one statistical test per row. 
  mutate(
#Creates new column called p_value
    p_value = binom.test(
#Performs a binominal test
      x = vO,
#Number of succesful visits, when the market was open
      n = n_visits,
#Total number of visits
      p = p_expected,
#Expected probability
      alternative = "greater"
#Is it higher than expected? 
    )$p.value
#I only want the p-value
  ) %>%
  ungroup()
#Removes rowwise() so the dataframe behaves normally again. 

