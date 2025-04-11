library(tidyverse)
library(ggplot2)
library(patchwork)
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
  ggtitle("Simulation Data with n = 1004")+
  xlim(0.3, 0.5) +
  xlab("Proportion Satisfied")
  
hist
(lower.val = quantile(dat$x, probs = .025))
(upper.val = quantile(dat$x, probs = .975))
(range = upper.val - lower.val)
(margin.of.error = (range)/2)
dat.table = tibble() |>
  summarize(step = "Sample", moe = margin.of.error, size = 1004)
#################
# Simulation (size 2008)
#################
dat2 = tibble(x = rbinom(n = 10000, size = 2008, prob = .39)/2004)
hist2 = ggplot(data = dat2) + 
  geom_histogram(aes(x = x, y = after_stat(density)), breaks = seq(.35,.44,.009)) +
  geom_density(aes(x = x), color = "red") +
  theme_bw() +
  ylab("Density") +
  ggtitle("Simulation Data with n = 2008") +
  xlim(0.3, 0.5) +
  xlab("Proportion Satisfied")

(lower.val2 = quantile(dat2$x, probs = .025))
(upper.val2 = quantile(dat2$x, probs = .975))
(range.2 = upper.val2-lower.val2)
(margin.of.error2 = (range.2)/2) 
hist2
dat.table = dat.table %>%
  bind_rows(summarize(.,step = "Sample", moe = margin.of.error2, size = 2008))
###################
# Step 2 (Resampling)
###################
gallup.data = rep(1, times = 392)
vec.dis = rep(0, times = 592)
vec.none = rep(-1, times = 20)
gallup.data = append(gallup.data, vec.dis)
gallup.data = append(gallup.data, vec.none)

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
  geom_density(data = summarize.prop, aes(x = x)) +
  ggtitle("Resampling Data") + 
  ylab("Density") +
  xlab("Proportion Satisfied") +
  theme_bw()
hist.resample

(lower.val.rsample = quantile(summarize.prop$x, probs = .025))
(upper.val.rsample = quantile(summarize.prop$x, probs = .975))
(range.rsample = upper.val.rsample - lower.val.rsample)
(margin.of.error.rsample = (range.rsample)/2) 
  print(summarize.prop)
view(total.gallup.data)
dat.table = dat.table %>%
    bind_rows(summarize(.,step = "Resample", moe = margin.of.error.rsample, size = 1004))
dat.table
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
dat.table = dat.table %>%
  bind_rows(summarize(.,step = "Gallup", moe = 4, size = 1004)) %>%
  bind_rows(summarize(.,step = "Gallup", moe = 2, size = 2008))
  

raster.plot = ggplot() + 
  geom_raster(data = n.p.data, aes(x = size, y = probability, fill = moe)) +
  scale_fill_viridis_c() + 
  xlab("n") + 
  ylab("Probability") + 
  ggtitle("Margin Of Error With Respect To n And Probability") +
  theme_bw()
raster.plot
view(n.p.data)

#################
# Step 4
#################
wilson.data = tibble()
for(j in 1:99)
{
  curr.vector = vector()
  #Iterates through the different sizes
  for(i in 0:290){
    size.curr = 100 + (i)*10
    curr.sim.data = rbinom(n = 10000, size = size.curr, prob = probabilities.vector[j])/size.curr
    dat.mean = mean(curr.sim.data)
    z.val = 1.96
    wilson.moe = z.val*
      (sqrt(size.curr*dat.mean*(1-dat.mean) + z.val/4))/
      (size.curr + z.val^2)
    curr.vector = append(curr.vector, wilson.moe)
  }
  curr.vector = tibble(wilson.moe = curr.vector) |>
    bind_cols(tibble(size = seq(100,3000,10))) |>
    bind_cols(tibble(probability = rep(probabilities.vector[j], 291)))
  if(j == 1){
    wilson.data = curr.vector
  }
  else{
    wilson.data = bind_rows(wilson.data, curr.vector)
  }
}
view(wilson.data)

wilson.raster.plot = ggplot() + 
  geom_raster(data = wilson.data, aes(x = size, y = probability, fill = wilson.moe)) +
  scale_fill_viridis_c() + 
  xlab("n") + 
  ylab("Probability") + 
  ggtitle("Wilson Margin Of Error With Respect To n And Probability") +
  theme_bw()
(hist + hist2)/hist.resample
raster.plot + wilson.raster.plot
view(wilson.data)
view(dat.table)
