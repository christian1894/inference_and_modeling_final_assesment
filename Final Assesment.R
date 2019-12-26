library(tidyverse)
options(digits = 3)
library(dslabs)
data(brexit_polls)
library(ggplot2)

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


# Second, use mutate to calculate an estimate for the standard error
# of the spread for each poll given the value of se_x_hat



# Then, use mutate to calculate upper and lower bounds for
# 95% confidence intervals of the spread. 



# Last, add a column hit that indicates whether the confidence interval 
# for each poll covers the correct spread  𝑑=−0.038 .

june_polls <- june_polls %>%
  mutate(se_x_hat = sqrt(x_hat*(1-x_hat)/samplesize),
         se_spread = 2*se_x_hat,
         ci_lower_spread = spread - qnorm(0.975)*se_spread,
         ci_upper_spread = spread + qnorm(0.975)*se_spread)

# What proportion of polls have a confidence interval that covers the value 0?

mean(june_polls$ci_lower_spread < 0 & june_polls$ci_upper_spread > 0)

# What proportion of polls predict "Remain" (confidence interval entirely above 0)?

mean(june_polls$ci_lower_spread > 0)


# What proportion of polls have a confidence interval covering the true value of d?
june_polls <- june_polls %>%
  mutate(hit = (2*p-1 > ci_lower_spread) & (2*p-1 < ci_upper_spread))
mean(june_polls$hit)


# Group and summarize the june_polls object by pollster to find
# the proportion of hits for each pollster and the number of polls 
# per pollster. Use arrange to sort by hit rate.

june_polls_group_by = june_polls %>% group_by(pollster) %>% summarize(hits_proportion = mean(hit), n = n())

# Make a boxplot of the spread in june_polls by poll type.

june_polls_group_by = june_polls %>% group_by(poll_type)

ggplot(june_polls, aes(y = spread, color = poll_type)) + geom_boxplot() 

# Calculate the confidence intervals of the spread combined across all polls in 
# june_polls, grouping by poll type. Recall that to determine the standard error of
# the spread, you will need to double the standard error of the estimate.
# 
# Use this code (which determines the total sample size per poll type,
# gives each spread estimate a weight based on the poll's sample size, and adds
# an estimate of p from the combined spread) to begin your analysis:

# combined_by_type <- june_polls %>%
#   group_by(poll_type) %>%
#   summarize(N = sum(samplesize),
#             spread = sum(spread*samplesize)/N,
#             p_hat = (spread + 1)/2)

combined_by_type <- june_polls %>%
  group_by(poll_type) %>%
  summarize(N = sum(samplesize),
            spread = sum(spread*samplesize)/N,
            p_hat = (spread + 1)/2, 
            se_spread = (sqrt(p_hat*(1-p_hat)/N)*2),
            lower = spread - qnorm(0.975) * se_spread, 
            upper = spread + qnorm(0.975) * se_spread)

# Define brexit_hit, with the following code, which computes the confidence
# intervals for all Brexit polls in 2016 and then calculates whether the confidence 
# interval covers the actual value of the spread d=???0.038:
brexit_hit <- brexit_polls %>%
  mutate(p_hat = (spread + 1)/2,
         se_spread = 2*sqrt(p_hat*(1-p_hat)/samplesize),
         spread_lower = spread - qnorm(.975)*se_spread,
         spread_upper = spread + qnorm(.975)*se_spread,
         hit = spread_lower < -0.038 & spread_upper > -0.038) %>%
  select(poll_type, hit)

brexit_chisq <- table(brexit_hit$poll_type, brexit_hit$hit)
chisq.test(brexit_chisq)$p.value

# online > telephone
hit_rate <- brexit_hit %>%
  group_by(poll_type) %>%
  summarize(avg = mean(hit))
hit_rate$avg[hit_rate$poll_type == "Online"] > hit_rate$avg[hit_rate$poll_type == "Telephone"]

# statistically significant
chisq.test(brexit_chisq)$p.value < 0.05

# Use the two-by-two table constructed in the previous exercise to calculate the odds
# ratio between the hit rate of online and telephone polls to determine the magnitude of 
# the difference in performance between the poll types.
# Calculate the odds that an online poll generates a confidence interval that
# covers the actual value of the spread.

# from previous question
brexit_chisq <- table(brexit_hit$poll_type, brexit_hit$hit)

# convert to data frame
chisq_df <- as.data.frame(brexit_chisq)

online_true <- chisq_df$Freq[chisq_df$Var1 == "Online" & chisq_df$Var2 == "TRUE"]
online_false <- chisq_df$Freq[chisq_df$Var1 == "Online" & chisq_df$Var2 == "FALSE"]

online_odds <- online_true/online_false
online_odds


# Calculate the odds that a telephone poll generates a confidence interval
# that covers the actual value of the spread.

phone_true <- chisq_df$Freq[chisq_df$Var1 == "Telephone" & chisq_df$Var2 == "TRUE"]
phone_false <- chisq_df$Freq[chisq_df$Var1 == "Telephone" & chisq_df$Var2 == "FALSE"]

phone_odds <- phone_true/phone_false
phone_odds

# Calculate the odds ratio to determine how many times larger the odds are for online polls to hit versus telephone polls.  

online_odds/phone_odds

# Use brexit_polls to make a plot of the spread (spread) over time (enddate)
# colored by poll type (poll_type). Use geom_smooth with method = "loess" to plot
# smooth curves with a span of 0.4. Include the individual data points colored by
# poll type. Add a horizontal line indicating the final value of d=???.038.

brexit_polls %>%
  ggplot(aes(enddate, spread, color = poll_type)) +
  geom_smooth(method = "loess", span = 0.4) +
  geom_point() +
  geom_hline(aes(yintercept = -.038))

# Use the following code to create the object brexit_long, which has a column 
# vote containing the three possible votes on a Brexit poll ("remain", "leave", "undecided")
# and a column proportion containing the raw proportion choosing that vote option on 
# the given poll:

brexit_long <- brexit_polls %>%
  gather(vote, proportion, "remain":"undecided") %>%
  mutate(vote = factor(vote))

brexit_long %>% ggplot(aes(enddate, proportion, color = vote)) + geom_smooth(span = 0.3, method = "loess")



