
library(tidyverse)
library(Hmisc)


mpg %>%
  group_by( manufacturer) %>%
  summarize( displ = mean( displ))




#solution:
library(conflicted)
