
# DOC check
library(tidyverse)

doc = read_csv('/Users/hilarydugan/Dropbox/SSB_TB data/Chemistry/SSB_Chems_Full.csv')
doc2 = doc |> filter(month(Date) <= 3) |> 
  select(Date, Depth, DOC) |> 
  filter(DOC >= 0, DOC < 40) |> 
  mutate(Depth = if_else(Depth <=3, 0, Depth)) |> 
  group_by(Date, Depth) |> 
  summarise(DOC = mean(DOC)) |> 
  pivot_wider(names_from = Depth, values_from = DOC, names_prefix = 'z') |> 
  mutate(DOCgradient = z7-z0)

test = doc |> filter(month(Date) <= 3) |> 
  select(Date, Depth, DOC) |> 
  filter(DOC >= 0, DOC < 40) 
max(test$DOC)

