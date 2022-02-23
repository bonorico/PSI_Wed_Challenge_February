# run example

#library(numDeriv)
#library(pso)
library(studydur)
library(dplyr)
library(gganimate)
library(transformr)
library(gifski)

yes_ggplot <- FALSE

# generate data
system.time(
  dur_dat <- duration_sim()

)

# filter data-set for background reference result (in order to create static layer you have to eliminate the 'transition' variable (here baseline_haz) !!) NOTE: how argument 'size' is transferred from main aes to static layer (also adding legend with no conflict)

ref_dat <- dur_dat %>% filter(baseline_haz == 0.05) %>%
  select(-baseline_haz) %>%
  rename(ref_HR = HR)

if (yes_ggplot)
{
  # plot data
  ggplot(dur_dat, aes(HR, optim_dur, size = required_events)) +
    geom_point(show.legend = FALSE, alpha = 0.8, colour = "pink") +
    geom_point(data = ref_dat, aes(ref_HR, optim_dur),
               alpha = 0.4, colour = "blue", fill = "white") +    # reference background
    guides(size = guide_legend("Required\n failures")) +
    ggtitle("Change in total study duration (versus reference in blue)\n by varying of assumed HR and active comparator hazard\n (Author: Federico Bonofiglio)",
            subtitle = "Active comparator hazard is {round(frame_time, 3)}") +
    ylab("Total study duration (months)") +
    xlab(expression(
      "increasing treatment effect" %<-% HR %->% "decreasing treatment effect" )) +
    geom_vline(xintercept = 0.775, colour = "green", alpha = 0.5) +
    geom_hline(yintercept = 30, colour = "green", alpha = 0.5) +
    transition_time(baseline_haz) +
    ease_aes('linear')

  # save gif
  anim_save("Wednesday_challenge.gif",
            path = file.path(getwd(), "out"))

}

# save data-set
write.table(dur_dat,
            file.path(getwd(), "out", "dur_dat"))

