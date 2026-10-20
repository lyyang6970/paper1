df <- read.csv('paper_1/spatioTemporalEpisodeHistories.csv')

library(ggplot2)
library(dplyr)
library(tidyr)

print(df)

library(dplyr)

# Figure 1A
annual <- df %>%
  group_by(Year, Participant.ID) %>%
  summarise(
    episodes = sum(Episodes, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(Year) %>%
  summarise(
    children = n(),
    total_episodes = sum(episodes),
    proportion = mean(episodes > 0),
    .groups = "drop"
  )

p1a <- ggplot(data = annual, aes(x=Year, y=total_episodes))+
  geom_line(color = '#6699CC', linewidth = 2) +
  geom_line(color = '#FFD92F', aes(x=Year, y= proportion*600), linewidth = 2)+
  scale_y_continuous(
    name = "Total episodes",
    limits = c(0, 600),
    breaks = seq(0, 600, 200),
    
    sec.axis = sec_axis(
      ~ ./ 600,
      name = "Proportion",
      breaks = seq(0, 1, 0.2)
    )
  )

