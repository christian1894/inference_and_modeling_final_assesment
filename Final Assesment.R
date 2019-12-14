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

se_total = sqrt((p* (1 - p)) / N)
se_total

# What is the expected value of  𝑋̂  , the proportion of "Remain" voters?

p

# What is the standard error of  𝑋̂  , the proportion of "Remain" voters?
se_remain = sqrt((p* (1 - p)) / total_N_remain)
se_remain


# What is the expected value of  𝑑 , the spread between the
# proportion of "Remain" voters and "Leave" voters?


# What is the standard error of  𝑑 , the spread between the proportion
# of "Remain" voters and "Leave" voters?



