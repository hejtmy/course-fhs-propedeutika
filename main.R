library(googlesheets4)
library(tidyverse)

df_all <- read_sheet("1SsNhDMnGU0tC_-U-tGWwZTmfif359CGuKo1RVREaLS8", "form-responses")
df_groups <- read_sheet("1SsNhDMnGU0tC_-U-tGWwZTmfif359CGuKo1RVREaLS8", "Stress-levels")

ggplot(df_all, aes(CatDog, Stress)) + 
  geom_boxplot() +
  geom_jitter(width = 0.2)

df_all %>%
  group_by(CatDog) %>%
  summarise(median = median(Stress),
            mean = mean(Stress))

df_all %>%
  filter(!grepl("Nemám", CatDog)) %>%
  ggplot(aes(Stress, fill = CatDog)) + 
  geom_density(alpha = 0.2)

df_all %>%
  filter(!grepl("Nemám", CatDog)) %>%
  ggbetweenstats(CatDog, Stress, type="nonparametric")

df_groups %>%
  filter(!grepl("Nemám", CatDog)) %>%
  group_by(CatDog, Group) %>%
  summarise(median = median(Stress),
            mean = mean(Stress))


  ggplot(aes(CatDog, Stress, color = as.factor(Group))) +
    geom_boxplot() + 
    geom_jitter(width = 0.1) +
    facet_wrap(~Group)

library(ggstatsplot)

df_groups %>%
  filter(!grepl("Nemám", CatDog)) %>%
  grouped_ggbetweenstats(CatDog, Stress, grouping.var = Group,
                         type = "nonparametric")
