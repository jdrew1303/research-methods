## Tutorial MT, 01

# Script to bootstrap means of All Ireland title per county

library(tidyverse)
library(gridExtra)


# Load data
df <- read.csv("data/gaa_champions.csv")

df %>% 
  arrange(- titles_football) %>% 
 # head() %>% 
  select(-titles_hurling)

df %>% 
  arrange(- titles_hurling) %>% 
 # head() %>% 
  select(-titles_football)


mean(df$titles_hurling)

ggplot(df, aes(x = titles_hurling)) +
  geom_histogram(colour = "black", fill = "grey50") + 
  scale_x_continuous(breaks = c(seq(0, 36, 2))) +
  geom_vline(xintercept = mean(df$titles_hurling), colour = "red") +
  labs(x = "All Ireland Senior Hurling Titles", y = "Number of Counties") +
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

football <- with(df, boot.mean(titles_football, B = 1000, binwidth = 0.2, title = "Football"))

pdf("plots/mt_01_sampling_means.pdf", width = 10, height = 4)
gridExtra::grid.arrange(hurling, football, nrow = 1)
dev.off()


std.error(c(9, 2, 5, 4))
sqrt(26/3)

std <- function(x) sd(x)/sqrt(length(x))
std(c(9, 2, 5, 4))

sd(c(9, 2, 5, 4))/sqrt(4)
