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



lab_size <- 7.5
high <- 10

# first plot 
tikzDevice::tikz(here::here('plots/late.tex'),
                 height = high, width = high* 21/9, standAlone = TRUE)

data %>%
ggplot2::ggplot(aes(x = W, y = y, group = dummy)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE, col = 'darkgrey', size = 1.5) +
  geom_brace(aes( c(0,0.1), c(13.2, 2.125), label = "LATE", fontface = "bold"), 
             inherit.data = F, rotate = 90, 
             labelrotate = 90, size = 1.25, labelsize = lab_size*1.2, col = '#004c93') +
  geom_brace(aes( c(-1,- 0.02), c(16, 17), label = "Treatment Group", fontface = "bold"), 
             labeldistance = 0.75, inherit.data = F, size = 1, labelsize = lab_size) +
  geom_brace(aes( c(0.02, 1), c(0, 1), label = "Control Group \n", fontface = "bold"), 
             labeldistance = 0.75, inherit.data = F, rotate = 180, size = 1, labelsize = lab_size) +
  scale_x_continuous(breaks=c(0),
                   labels=c("Cutoff ($c$)")) +
  geom_vline(xintercept = 0, linetype = 'longdash', col = '#1d7334') +
  labs(x ='\nRunning Variable', y = 'Exam Points\n') +
  expand_limits(y=c(-2, 19)) +
  theme_bw() +
  theme(axis.text.y  = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x  = element_text(size = 25, face = "bold"),
        axis.title = element_text(size = 30, face = "bold"), 
        axis.ticks.length = unit(.5, "cm"),
        axis.ticks.x = element_line(size = 1))

dev.off()

ggplot2::ggsave(here::here('plots/late.png'),  height = high, width = high* 21/9,  dpi = 600)



#### bandwidth
tikzDevice::tikz(here::here('plots/non_p_late.tex'),
                 height = high, width = high* 21/9, standAlone = TRUE)

ggplot2::ggplot() +
  geom_rect(mapping = aes(xmin=-0.5, xmax = 0., ymin = 0.5, ymax = 16), fill = 'green', alpha = .5) +
  geom_rect(mapping = aes(xmin= 0, xmax = 0.5, ymin = 0.5, ymax = 16), fill = '#004c93', alpha = .5) +
  geom_brace(aes( c(-0.5,- 0.), c(16, 17), label = "Treatment Group", fontface = "bold"), 
             labeldistance = 0.75, inherit.data = F, size = 1, labelsize = lab_size) +
  geom_brace(aes( c(-0, .5), c(16, 17), label = "Control Group", fontface = "bold"),  
             labeldistance = 0.75, inherit.data = F, size = 1, labelsize = lab_size) +
  geom_point(data = data, aes(x = W, y = y, group = dummy)) + 
  geom_vline(xintercept = 0, linetype = 'longdash', col = '#1d7334') +
  scale_x_continuous(breaks = c(-0.5, 0, 0.5), 
                     labels =c('$c - \\mathrm{bandwidth}$', 'Cutoff ($c$)', '$c + \\mathrm{bandwidth}$')) +
  geom_vline(xintercept = 0, linetype = 'longdash', col = '#1d7334') +
  labs(x ='\nRunning Variable', y = 'Exam Points\n') +
  expand_limits(y=c(-2, 19)) +
  theme_bw() +
  guides(none) +
  theme(axis.text.y  = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x  = element_text(size = 25, face = "bold"),
        axis.title = element_text(size =30, face = 'bold'),
        axis.ticks.length = unit(.5, "cm"),
        axis.ticks.x = element_line(size = 1))

dev.off()

ggplot2::ggsave(here::here('plots/non_p_late.png'),  height = high, width = high* 21/9,  dpi = 600)


