library(tidyverse)
library(ggplot2)
################################################################################
# Lab 10
################################################################################

#################
# Step 1 (Size 1000)
#################
dat = tibble(x = rbinom(n = 10000, size = 1004, prob = .39)/1004)
hist = ggplot(data = dat) + 
  geom_histogram(aes(x = x, y = after_stat(density)), breaks = seq(.330,.450,.009)) +
  geom_density(aes(x = x), color = "red") +
  theme_bw() +
  ylab("Density") +
  ggtitle("Proportion Satisfaction n = 1004")
  

(lower.val = quantile(dat$x, probs = .025))
(upper.val = quantile(dat$x, probs = .975))
(range = upper.val - lower.val)
(margin.of.error = (range)/2)
hist
#################
# Simulation (size 200)
#################
dat2 = tibble(x = rbinom(n = 10000, size = 2008, prob = .39)/2004)
hist2 = ggplot(data = dat2) + 
  geom_histogram(aes(x = x, y = after_stat(density)), breaks = seq(.35,.44,.009)) +
  geom_density(aes(x = x), color = "red") +
  theme_bw() +
  ylab("Density") +
  ggtitle("Proportion Satisfaction n = 2008")

(lower.val2 = quantile(dat2$x, probs = .025))
(upper.val2 = quantile(dat2$x, probs = .975))
(range.2 = upper.val2-lower.val2)
(margin.of.error2 = (range.2)/2) 
hist2

gallup.data = rep(1, times = 392)
vec.dis = rep(0, times = 592)
vec.none = rep(-1, times = 20)
gallup.data = append(gallup.data, vec.dis)
gallup.data = append(gallup.data, vec.none)

#Do I need to include my original data in my resampling
total.gallup.data = sample(gallup.data, 1004, replace = TRUE)
summarize.prop = vector()
for(i in 1:1004){
  new.col = sample(gallup.data, 1004, replace = TRUE) 
  total.gallup.data = bind_cols(total.gallup.data, new.col)
  summarize.prop = append(summarize.prop,sum(new.col == 1)/1004)
}
summarize.prop = tibble(x = summarize.prop)
hist.resample = ggplot() +
  geom_histogram(data = summarize.prop, aes(x = x, y = after_stat(density)), 
                 breaks = seq(.32, .44, .007)) +
  geom_density(data = summarize.prop, aes(x = x))
hist.resample

(lower.val.rsample = quantile(summarize.prop$x, probs = .025))
(upper.val.rsample = quantile(summarize.prop$x, probs = .975))
(range.rsample = upper.val.rsample - lower.val.rsample)
(margin.of.error.rsample = (range.rsample)/2) 
  print(summarize.prop)
view(total.gallup.data)

####################
# Step 3 n and p
####################

probabilities.vector = seq(.01,.99,.01)
n.p.data = tibble()
for(j in 1:99)
{
  curr.vector = vector()
  #Iterates through the different sizes
  for(i in 0:290){
    size.curr = 100 + (i)*10
    curr.sim.data = rbinom(n = 10000, size = size.curr, prob = probabilities.vector[j])/size.curr
    upper.sim = quantile(curr.sim.data, probs = .975)
    lower.sim = quantile(curr.sim.data, probs = .025)
    curr.vector = append(curr.vector, (upper.sim - lower.sim)/2)
  }
  curr.vector = tibble(moe = curr.vector) |>
    bind_cols(tibble(size = seq(100,3000,10))) |>
    bind_cols(tibble(probability = rep(probabilities.vector[j], 291)))
  if(j == 1){
    n.p.data = curr.vector
  }
  else{
    n.p.data = bind_rows(n.p.data, curr.vector)
  }
}

raster.plot = ggplot() + 
  geom_raster(data = n.p.data, aes(x = size, y = probability, fill = moe)) +
  scale_fill_viridis_c() + 
  xlab("n") + 
  ylab("Probability") + 
  ggtitle("Margin Of Error With Respect To n And Probability") +
  theme_bw()
raster.plot
view(n.p.data)
print("d")
