# Assess total trial duration in planning a survival study  
In a head-to-head trial about 1000 patients are needed to observe 650 failures with accrual time of 24 months, follow-up of 6 months, baseline hazard equal to 5% per year, and HR is 0.775 (error I 5%; power 90%, administrative censoring only). 
How does total trial duration change by varying the assumed baseline hazard and HR ?

This question was made at the PSI [Wednesday challenge](https://www.psiweb.org/sigs-special-interest-groups/visualisation/welcome-to-wonderful-wednesdays) of February 2022, and 
this small package gives an answer to it. 

## Installation
`devtools::install_github("bonorico/PSI_Wed_Challenge_February")`

## Additional infos
Run `output_example.R` (set `yes_ggplot <- TRUE`) in folder `out` to produce the proposed plot (see gif result), see `Bericht.pdf`, or run `Bericht.Rmd` (HTML-knit in Rstudio) to have more infos about the challenge and the proposed approach.

## Output 

![Alt Text](https://github.com/bonorico/PSI_Wed_Challenge_February/blob/main/out/Wednesday_challenge.gif)
