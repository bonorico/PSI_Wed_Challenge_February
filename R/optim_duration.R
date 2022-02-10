#' Numerical optimization of total study duration
#' @description Optimizes total study duration for a fixed accrual time. Hence, it returns
#'     optimal follow-up time after the last patient is enrolled for a fixed sample
#'     size. Thus, the function can be used to investigate follow-up variation by varying of
#'     baseline hazard, HR, or other quantities used for sample size calculation according
#'     to the exponential model given by Lachin et al (1986) (see package gsDesign).
#' @importFrom optimx optimx
#' @importFrom gsDesign nSurvival
#' @export
#' @returns list of optimal duration and updated Nr. of required failures


optim_duration <- function(tot_dur, accr_time = 24,
                           h_c = 0.05/12, HR =0.775,
                           uni_dim = TRUE)
{
  if (uni_dim) # 09.02.2022 currently only unidim possible (but routine will work with optimx too.)
    opt <- optimize(loss_function, c(accr_time + 1, 300), h_c = h_c, HR = HR)
  else
    opt <- optimx(par = c(tot_dur = tot_dur),
                  fn = loss_function,
                  method = "L-BFGS-B",
                  lower = accr_time + 1,  # lower bound for total duration can't be less than enroll time
                  upper = 300,
                  itnmax=c(50),
                  h_c = h_c,
                  HR = HR
                  #control = list(all.methods = TRUE) # not need all methods
    )
  # redundant print-out
  # summary(opt, order = "convcode") %>%
  #   select(-value, -niter, -gevals, -fevals)

  # update nEvents given new study duration
  nFailures_new <- nSurvival(lambda1 =h_c, lambda2 =h_c*HR,
                             Ts = opt[[1]], Tr = accr_time,
                             alpha = 0.05, sided = 2)$nEvents
  return(
    list(optim_tot_dur = opt[[1]],
         updated_nFailures = nFailures_new)
  )

}


# # test
# optim_duration(30)
# optim_duration(25, h_c = 0.06/12)
