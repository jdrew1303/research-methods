## Tutorial MT, 01

# Script to bootstrap means of All Ireland title per county

library(tidyverse)
library(gridExtra)

theme_slides <- function(base_size = 12){
  theme_minimal() +
    theme(
      axis.text = element_text(colour="black"),
      strip.text = element_text(size=12),
      axis.ticks = element_line(colour = "black"),
      legend.key=element_rect(colour=NA, fill =NA),
      panel.border = element_rect(fill = NA, colour = "grey70", size = 0.7)
    )
}

ggplot2::theme_set(theme_slides())

# Load data
df <- read.csv("data/gaa_champions.csv")

df %>% 
  arrange(- titles_football) %>% 
 # head() %>% 
  select(-titles_hurling)

df %>% 
  arrange(- titles_hurling) %>% 
  select(-titles_football)


ggplot(df, aes(x = titles_hurling)) +
  geom_histogram(colour = "black", fill = "grey50") + 
  scale_x_continuous(breaks = c(seq(0, 36, 2))) +
  geom_vline(xintercept = mean(df$titles_hurling), colour = "red") +
  labs(x = "All Ireland Senior Hurling Titles", y = "Number of Counties") 
ggsave("plots/mt_01_hurling.pdf", width = 5, height = 3)

ggplot(df, aes(x = titles_football)) +
  geom_histogram() + 
  geom_histogram(colour = "black", fill = "grey50") + 
  scale_x_continuous(breaks = c(seq(0, 36, 2))) +
  geom_vline(xintercept = mean(df$titles_football), colour = "red") +
  labs(x = "All Ireland Senior Football Titles", y = "Number of Counties") +
ggsave("plots/mt_01_football.pdf", width = 5, height = 3)

boot.mean <-  function(x, B, binwidth=NULL, title) {
  n <-  length(x)
  boot.samples <-  matrix(sample(x, size = n*B, replace=TRUE), B, n)
  boot.statistics <-  apply(boot.samples, 1, mean)
  se <-  sd(boot.statistics)
  if (is.null(binwidth))
    binwidth = diff(range(boot.statistics)) / 30
  p <-  ggplot(data.frame(x=boot.statistics),aes(x = x)) +
    geom_histogram(aes(y = ..density..), binwidth = binwidth, colour = "black", fill = "grey50") + 
    geom_density(color = "red") +
    labs(x = "Sampling distribution of the sample mean") +
    ggtitle(label = title)
  plot(p)
}

hurling <- with(df, boot.mean(titles_hurling, B = 1000, binwidth = 0.2, title = "Hurling")) 

football <- with(df, boot.mean(titles_football, B = 1000, binwidth = 0.2, title = "Gaelic Football"))

pdf("plots/mt_01_sampling_means.pdf", width = 8, height = 3)
gridExtra::grid.arrange(hurling, football, nrow = 1)
dev.off()

## Estimate mean, sd and standard error

values <- c(9, 2, 5, 4)

std <- function(x) sd(x)/sqrt(length(x))

mean(values)

sd(values)

std(values)
sd(values)/sqrt(4)


## Get grades from Irish soccer team

grades <- read.csv("data/grades_player_ireland.csv")

grades <- grades %>% 
  group_by(game) %>% 
  mutate(team_mean = mean(grade))

grades %>% 
  group_by(game) %>% 
  summarise(team_mean = mean(grade),
         team_sd = sd(grade))
  
ggplot(data = grades, aes(x = surname, y = grade)) +
  geom_hline(aes(yintercept = team_mean), colour = "red", size = 1.3, alpha = 0.8) +
  geom_point(size = 2) +
  facet_wrap(~ game, scales = "free_x") +
  scale_y_continuous(limits = c(0, 10), breaks = c(seq(0, 10, 2))) +
  labs(x = NULL, y = "Grade", caption = "Grades retrieved from www.balls.ie") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
ggsave("plots/mt_01_ire_soccer.pdf", width = 6, height = 3)
