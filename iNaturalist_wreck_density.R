library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)

# --- NON-AUGMENTED DATA ---

data_nonaug <- read.csv("input/onland_iNaturalist_obs.csv", header = TRUE)

data_nonaug$observed_on <- as.POSIXct(data_nonaug$observed_on, format="%d/%m/%Y")

data_nonaug$Month <- format(data_nonaug$observed_on, "%B")
data_nonaug$Year <- format(data_nonaug$observed_on, "%Y")

# Summarise for all years, per month
summary_combined_nonaug <- data_nonaug %>%
  group_by(Month) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  right_join(data.frame(Month = month.name), by = "Month") %>%
  replace_na(list(Count = 0)) %>%
  mutate(Month = factor(Month, levels = month.name)) %>%
  arrange(Month)

ggplot(data=summary_combined_nonaug, aes(x=Month, y=Count)) +
  geom_bar(stat="identity") +
  geom_text(aes(label=Count), vjust=-0.3, size=3.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.background = element_rect(fill = "white"),
        panel.border = element_rect(color = "black", fill = NA)) +
  ggtitle("Observations of dead Southern Shearwaters on NSW East coast")


# Summarise for each year, and each month in that year. 
# Visualising this indicates that only 2023 and 2024 have data_aug
summary_byYear_nonaug <- data_nonaug %>%
  group_by(Year, Month) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  complete(Year, Month = month.name, fill = list(Count = 0)) %>%
  mutate(Month = factor(Month, levels = month.name)) %>%
  arrange(Year, Month) %>%
  subset(Year %in% c(2023, 2024))

ggplot(data = summary_byYear_nonaug, aes(x = Month, y = Count)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Count), vjust = -0.3, size = 3.5) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.background = element_rect(fill = "white"),
    panel.border = element_rect(color = "black", fill = NA)
  ) +
  ggtitle("Observations of Dead Southern Shearwaters on NSW East Coast") +
  facet_wrap(~ Year, ncol = 2)


# --- AUGMENTED DATA ---

data_aug <- read.csv("input/onland_iNaturalist_obs_augmented.csv", header = TRUE)

# Extract month from observed_on column

data_aug$observed_on <- as.POSIXct(data_aug$observed_on, format="%d/%m/%Y")

data_aug$Month <- format(data_aug$observed_on, "%B")
data_aug$Year <- format(data_aug$observed_on, "%Y")

# Summarise for all years, per month
summary_combined <- data_aug %>%
  group_by(Month) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  right_join(data.frame(Month = month.name), by = "Month") %>%
  replace_na(list(Count = 0)) %>%
  mutate(Month = factor(Month, levels = month.name)) %>%
  arrange(Month)
  
ggplot(data=summary_combined, aes(x=Month, y=Count)) +
  geom_bar(stat="identity") +
  geom_text(aes(label=Count), vjust=-0.3, size=3.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.background = element_rect(fill = "white"),
        panel.border = element_rect(color = "black", fill = NA)) +
  ggtitle("Observations of dead Southern Shearwaters on NSW East coast")


# Summarise for each year, and each month in that year. 
# Visualising this indicates that only 2023 and 2024 have data_aug
summary_byYear <- data_aug %>%
  group_by(Year, Month) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  complete(Year, Month = month.name, fill = list(Count = 0)) %>%
  mutate(Month = factor(Month, levels = month.name)) %>%
  arrange(Year, Month) %>%
  subset(Year %in% c(2023, 2024))

ggplot(data = summary_byYear, aes(x = Month, y = Count)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Count), vjust = -0.3, size = 3.5) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.background = element_rect(fill = "white"),
    panel.border = element_rect(color = "black", fill = NA)
  ) +
  ggtitle("Observations of Dead Southern Shearwaters on NSW East Coast") +
  facet_wrap(~ Year, ncol = 2)
  


