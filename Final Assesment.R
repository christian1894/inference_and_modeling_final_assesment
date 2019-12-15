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






