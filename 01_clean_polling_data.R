# Load libraries
library(tidyverse)
library(lubridate)
library(ggthemes)
library(gganimate)

# Load datasets
pollsters <- read_csv("Datasets/pollster-ratings.csv") %>%
    rename(pollster = Pollster,
           grade = `538 Grade`) %>%
    select(pollster, grade)
polls <- read_csv("Datasets/raw-polls.csv", guess_max = 30000) %>%
    left_join(pollsters) %>%
    mutate(polldate = mdy(polldate),
           electiondate = mdy(electiondate))

# Clean dataset
polls <- polls %>%
    mutate(time_out = electiondate - polldate,
           cand1_winner = ifelse(margin_actual < 0, 0, 1),
           rightcall = ifelse(rightcall == 1, "Poll correct", 
                              ifelse(rightcall == 0, "Poll incorrect", "Poll tied")),
           rightcall = factor(rightcall))

polls %>%
    filter(cand1_winner %in% c(0, 1)) %>%
    mutate(poll_bin = cut(cand1_pct, breaks = seq(0, 100, 5))) %>%
    group_by(poll_bin) %>%
    summarise(N = n(), win = sum(cand1_winner)/N)

# Plot
polls %>%
    ggplot(aes(x = cand1_pct/100, y = cand1_actual/100)) +
    facet_wrap(~ time_out, nrow = 5) +
    geom_point(alpha = .2, aes(color = rightcall)) +
    geom_smooth(method = "glm", 
                method.args = list(family = "binomial"), 
                se = TRUE) +
    scale_color_ptol(name = "Poll vs. outcome") +
    theme_minimal() +
    # transition_states(time_out, transition_length = 1, state_length = 1) +
    labs(x = "Candidate's polling (%)",
         y = "Candidate's result (%)")

polls
model <- lm(cand1_actual ~ cand1_pct + cand1_pct:samplesize + cand1_pct:time_out, data = polls)    
summary(model)
