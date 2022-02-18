#' Plot Wednesday challenge data (no animation)
#' @param df data.frame - output of 'duration_sim'
#' @param basehaz numeric - value of 'baseline_haz' variable. Range should
#'     restrict to output of 'duration_sim'
#' @details Meant to be used within Shiny app
#' @importFrom dplyr filter '%>%'
#' @import ggplot2
#' @export
#' @returns ggplot

makeplot <- function(df, basehaz){

  ref_dat <- df %>% filter(baseline_haz == 0.05) %>%
    select(-baseline_haz) %>%
    rename(ref_HR = HR)

  dur_dat <- df %>% filter(baseline_haz == basehaz)

  ggplot(dur_dat, aes(HR, optim_dur, size = required_events)) +
    geom_point(show.legend = FALSE, alpha = 0.9, colour = "pink") +
    geom_point(data = ref_dat, aes(ref_HR, optim_dur),
               alpha = 0.4, colour = "blue", fill = "white") +
    guides(size = guide_legend("Required\n failures")) +
    ylab("Total study duration (months)") +
    xlab("increasing treatment effect \U2190 HR \U2192 decreasing treatment effect") +
    geom_vline(xintercept = 0.775, colour = "green") +
    geom_hline(yintercept = 30, colour = "green") +
    scale_y_continuous(limits = c(25, 55), breaks = seq(25, 55, by = 5)) +
    theme_light() +
    theme(panel.grid.minor = element_blank(),
          panel.border = element_blank())

}
