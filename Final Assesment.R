library(tidyverse)
options(digits = 3)
library(dslabs)
data(brexit_polls)

p <- 0.481    # official proportion voting "Remain"
d <- 2*p-1    # official spread

# The final proportion of voters choosing "Remain" was 𝑝=0.481. Consider a poll
# with a sample of 𝑁=1500 voters.
# What is the expected total number of voters in the sample choosing "Remain"?

N = 1500

total_N_remain = N *  p
total_N_remain

# What is the standard error of the total number of voters in the sample 
#choosing "Remain"?

se_total = sqrt(N*p*(1-p))
se_total

# What is the expected value of  𝑋̂  , the proportion of "Remain" voters?

p

# What is the standard error of  𝑋̂  , the proportion of "Remain" voters?
se_remain = sqrt(p* (1 - p)) / total_N_remain
se_remain


# What is the expected value of  𝑑 , the spread between the
# proportion of "Remain" voters and "Leave" voters?

d

#What is the standard error of  𝑑 , the spread between the
# proportion of "Remain" voters and "Leave" voters?

se_spread = 2*sqrt(p*(1-p)/N)
se_spread


# Calculate x_hat for each poll, the estimate of the proportion of
# voters choosing "Remain" on the referendum day ( 𝑝=0.481 )
# , given the observed spread and the relationship  𝑑̂ =2𝑋̂ −
# 1 . Use mutate to add a variable x_hat to the brexit_polls object by
# filling in the skeleton code below:

brexit_polls <- brexit_polls %>%
  mutate(x_hat = (spread + 1)/2)


# What is the average of the observed spreads (spread)?
mean(brexit_polls$spread)


#What is the standard deviation of the observed spreads?

sd(brexit_polls$spread)

# What is the standard deviation of the observed spreads?
mean(brexit_polls$x_hat)


# What is the average of x_hat, the estimates of the parameter  𝑝 ?

sd(brexit_polls$x_hat)

#Consider the first poll in brexit_polls, a YouGov poll run on the 
# same day as the Brexit referendum:

first_poll = brexit_polls[1,]

N = first_poll$samplesize
x_hat = first_poll$x_hat

se_hat_first_poll = sqrt(x_hat * (1  - x_hat)/ N)

ci = x_hat + c(-qnorm(0.975), qnorm(0.975)) * se_hat_first_poll

ci

!between(0.5, x_hat - qnorm(.975)*se_hat_first_poll, x_hat + qnorm(.975)*se_hat_first_poll)    # predicts winner
between(0.481, x_hat - qnorm(.975)*se_hat_first_poll, x_hat + qnorm(.975)*se_hat_first_poll)    # does not cover p

# Create the data frame june_polls containing only Brexit polls
# ending in June 2016 (enddate of "2016-06-01" and later). We will 
# calculate confidence intervals for all polls and determine how many cover 
# the true value of  𝑑 .


june_polls = brexit_polls %>% filter(enddate >= "2016-06-01")

# First, use mutate to calculate a plug-in estimate se_x_hat for the standard
# error of the estimate  SE^[𝑋]  for each poll given its sample size
# and value of  𝑋̂   (x_hat). 

june_polls = june_polls %>% mutate(se_x_hat = sqrt(x_hat * (1 - x_hat) / samplesize))

# Second, use mutate to calculate an estimate for the standard error
# of the spread for each poll given the value of se_x_hat


# Then, use mutate to calculate upper and lower bounds for
# 95% confidence intervals of the spread. 


# Last, add a column hit that indicates whether the confidence interval 
# for each poll covers the correct spread  𝑑=−0.038 .






