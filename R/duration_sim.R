#' Generates data-set for Wednesday challenge February
#' @description Simulates various scenarios according to sequence of values
#'     for the hazard-rate of the active control and the hazard-ratio.
#' @param h_c_seq - numeric vector. Sequence of values for the hazard rate
#'     in control arm.
#' @param HR_seq - numeric vector. Sequence of values for the hazard ratio.
#' @param uni_dim - logical. Is optimization problem unidimansional ?
#'     Default and only current possibility is TRUE and uses 'optimize'. If
#'     FALSE the routine will still work using 'optim' and this alternative
#'     allows for development of multidimensinoal optimization (see
#'     Discussion of accompanying HTML vignette)
#' @returns data.frame
#' @export
#' @examples{
#' library(gsDesign)
#' library(optimx)
#' library(dplyr)
#' library(ggplot2)
#' library(gganimate)
#' library(transformr)
#' library(gifski)
#' library(Fallzahlchallenge)
#' # generate data
#' system.time(
#'   dur_dat <- duration_sim()
#' )
#'
#' # filter data-set for background reference result (in order to create static layer you have to eliminate the 'transition' variable (here baseline_haz) !!) NOTE: how argument 'size' is transferred from main aes to static layer (also adding legend with no conflict)
#'
#' ref_dat <- dur_dat %>% filter(baseline_haz == 0.05) %>%
#'   select(-baseline_haz) %>%
#'   rename(ref_HR = HR)
#'
#' # plot data
#' ggplot(dur_dat, aes(HR, optim_dur, size = required_events)) +
#'   geom_point(show.legend = FALSE, alpha = 0.8, colour = "pink") +
#'   geom_point(data = ref_dat, aes(ref_HR, optim_dur),
#'              alpha = 0.4, colour = "blue", fill = "white") +    #' # reference background
#'   guides(size = guide_legend("Required\n failures")) +
#'   ggtitle("Change in total study duration (versus reference in blue)\n by varying of assumed HR and active comparator hazard",
#'           subtitle = "Active comparator hazard is {round(frame_time, 3)}") +
#'   ylab("Total study duration (months)") +
#'   xlab(expression(
#'     "increasing treatment effect" %<-% HR %->% "decreasing treatment effect" )) +
#'   geom_vline(xintercept = 0.775, colour = "green", alpha = 0.5) +
#'   geom_hline(yintercept = 30, colour = "green", alpha = 0.5) +
#'   transition_time(baseline_haz) +
#'   ease_aes('linear')
#'
#' # save gif
#' anim_save("Wednesday_challenge.gif")
#'
#' # save data-set
#' write.table(dur_dat, "dur_dat")
#' }

duration_sim <- function(h_c_seq = seq(0.03, 0.07, 0.005),
                         HR_seq = seq(0.75, 0.8, 0.0025),
                         uni_dim = TRUE)
{
  do.call("rbind",
          lapply(h_c_seq, function(i)
            do.call("rbind",
                    lapply(HR_seq, function(j)
                    {
                      out <- optim_duration(25, h_c = i/12, HR = j,
                                            uni_dim = uni_dim)
                      data.frame(optim_dur = out$optim_tot_dur,
                                 HR = j, baseline_haz = i,
                                 required_events = round(out$updated_nFailures, 0))
                    }
                    )
            )
          ))
}



