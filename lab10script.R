library(tidyverse)
library(ggplot2)

dat = tibble(rbinom(n = 10000, size = 1004, prob = .39)) 
  
colnames(dat) = "x"
hist = ggplot(data = dat) + 
  geom_histogram(aes(x = x, y = after_stat(density))) 

hist
view(dat)