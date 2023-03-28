# packages 
# install 
#devtools::install_github("nicolash2/ggbrace")


pacman::p_load(dplyr,
               here,
               purrr,
               tidyr,
               readxl, 
               stringr,
               stringi, 
               lubridate, 
               magrittr, 
               openxlsx,
               tidylog,
               readr, 
               janitor, 
               tidyr,
               ggbrace)

# data
set.seed(123)
data <- tibble::tibble(
  W = runif(100, -1, 1), 
  y = 3 + 1.2 * W + 10 * (W<=0) + rnorm(100),
  dummy = (W<=0)
)

data %>%
ggplot2::ggplot(aes(x = W, y = y, group = dummy)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE, col = 'grey') +
  geom_brace(aes( c(0,0.1), c(13.2, 2.125), label = "LATE"), 
             inherit.data = F, rotate = 90, 
             labelrotate = 90, size = 1.25, labelsize = 7.5, col = '#004c93') +
  geom_brace(aes( c(-1,- 0.02), c(10, 9), label = "Treatment Group"), 
             inherit.data = F, rotate = 180, size = 1, labelsize = 3) +
  geom_brace(aes( c(0.02, 1), c(0, 1), label = "Control Group"), 
             inherit.data = F, rotate = 180, size = 1, labelsize = 3) +
  scale_x_continuous(breaks=c(0),
                   labels=c("Cutoff")) +
  geom_vline(xintercept = 0, linetype = 'longdash', col = '#1d7334') +
  labs(x ='Running Variable', y = 'Final Exam Points') +
  theme_bw() +
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

