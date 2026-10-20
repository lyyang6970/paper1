df <- read.csv('paper_1/spatioTemporalEpisodeHistories.csv')

library(ggplot2)
library(dplyr)
library(tidyr)

print(df)

library(dplyr)

df1 <- df %>%
  mutate(
    GotEpisode = if_else(GotEpisode == "yes", 1L, 0L)
  )

# Figure 1A
annual <- df1 %>% group_by(Year) %>% summarise(
  children = n_distinct(Participant.ID),
  total_episodes = sum(Episodes, na.rm = TRUE),
  prevalence = mean(Episodes > 0, na.rm = TRUE), .groups='drop')


proportion = sum(df1$GotEpisode)/ length(unique(df1$Participant.ID))
scale_factor <- max(annual$total_episodes) / max(annual$prevalence)

p1a <- ggplot(data = annual, aes(x=Year, y=total_episodes))+
  geom_line(color = '#6699CC', linewidth = 2) +
  geom_line(color = '#FFD92F', aes(x=Year, y= proportion*50), linewidth = 2)+
  scale_y_continuous(
    name = "Total episodes",
    limits = c(0, 600),
    breaks = seq(0, 600, 200),
    
    sec.axis = sec_axis(
      ~ ./ 50,
      name = "Proportion",
      breaks = seq(0, 1, 0.2)
    )
  )
p1a

# Figure 1B
p1b <- ggplot(data = annual, aes(x=Year, y = total_episodes)) +
  geom_bar(color = group_by(df$Episodes))