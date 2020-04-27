# # library(magick)
# # tiger <-
# #   magick::image_read(path = "https://wx3.sinaimg.cn/mw690/5c690f6egy1gbtlmoa3myj20fo0n1jrk.jpg")
# #
# # class(tiger)
# #
# # print(tiger)
# #
# #
#
#
#
# setwd("D:/Test")
#
#
# dir()
#
# library(tidyverse)
# data <-
#   readxl::read_xlsx("02092020-test.xlsx", sheet = 2)
#
# data <- data[-1,]
#
# data1 <- data[,c(1, 2:4)]
# data2 <- data[,c(1, 4:6)]
#
# colnames(data1) <-
#   c("Time",paste("On", 1:3, sep = ""))
#
# colnames(data2) <-
#   c("Time",paste("Off", 1:3, sep = ""))
#
# data1 <-
# data1[,-1] %>%
#   apply(1, function(x){
#     c(mean(x), sd(x))
#   }) %>%
#   t() %>%
#   cbind(data1[,1], .) %>%
#   tibble::as_tibble() %>%
#   dplyr::rename(mean = "1", sd = "2") %>%
#   dplyr::mutate(class = "on")
#
#
# data2 <-
#   data2[,-1] %>%
#   apply(1, function(x){
#     c(mean(x), sd(x))
#   }) %>%
#   t() %>%
#   cbind(data2[,1], .) %>%
#   tibble::as_tibble() %>%
#   dplyr::rename(mean = "1", sd = "2") %>%
#   dplyr::mutate(class = "off")
#
# data <- rbind(data1, data2)
#
#
# data %>%
#   ggplot(aes(x = Time, y = mean)) +
#   # scale_y_continuous(limits = c(0.5, 1)) +
#   geom_point(aes(colour = class, shape = class), size = 3) +
#   geom_errorbar(aes(x = Time, ymin = mean - sd, ymax = mean + sd), width = 0.8) +
#   geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = FALSE, aes(colour = class)) +
#   theme_bw()
#
#
#
#
#
