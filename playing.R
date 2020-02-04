library(tidyverse)
library(imager)
library(gridExtra)
library(furrr)
library(pdist)

# load an image,
# this one is "quite" big 2.8 mb, resutls in large object 188 mb
org_image <- load.image("./Laura.vlinderslag.JPG")
#org_image

#plot(org_image)

# resize image
# there are some extra options
small_image <- imresize(im = org_image,
                        scale = 0.2)
#plot(small_image)

# convert to data frame
df_small <- as.data.frame(x = small_image)

# make wide
df_small <- df_small %>% 
  pivot_wider(names_from = cc,
              names_prefix = "c",
              values_from = value)

# calculate rgb color code
df_small <- df_small %>% 
  mutate(rgb_code = rgb(red = c1,
                        green = c2,
                        blue = c3))

# plot with ggplot2
df_small %>% 
  ggplot(aes(x = x,
             y = y)) +
  geom_raster(aes(fill = rgb_code)) +
  scale_fill_identity() +
  scale_y_continuous(trans = scales::reverse_trans())

## read file with DMC rgb color codes
dmc_codes <- read_csv(file = "./DMC_Cotton_Floss_to_RGB.csv",
                      col_types = "ccdddcc") %>%
  # rescale the rgb codes to max value 1
  mutate_at(c("Red", "Green", "Blue"),
            ~ . / 255)

##### match a color from the image to a DMC color
# calculate the euclidian distance between the colors
# the DMC color with the smallest distance is the one to keep
calc_dist <- function(dmc, red, green, blue) {
  dmc_m <- rbind(dmc, c(red, green, blue))
  # get the last row of the distance matrix
  dist_m <- as.matrix(dist(dmc_m))[nrow(dmc_m), 1:nrow(dmc_m) - 1]
  min_dist <- min(dist_m, na.rm = TRUE)
  return(min_dist)
}

calc_dist2 <- function(dmc, red, green, blue) {
  dist_m <- pdist(dmc[, c("Red", "Green", "Blue")], c(red, green, blue))@dist
  min_idx <- which.min(dist_m[1:length(dist_m)])
  rgb_code <- dmc[min_idx, "RGB code"]
  return(rgb_code)
}

# create dmc matrix
dmc_m <- as.matrix(dmc_codes[, c("Red", "Green", "Blue", "RGB code")])

plan(multiprocess)
uniq_code <- df_small %>% 
  distinct(rgb_code, .keep_all = TRUE) %>% 
  mutate(min_dist = future_pmap(.l = list(c1, c2, c3),
                                .f = ~ calc_dist2(dmc = dmc_m,
                                                  red = ..1,
                                                  green = ..2,
                                                  blue = ..3)))


##### put one plot per page in one pdf
## as an example I used mtcars dataset
## initialize list
p_list <- list()

## make the plots
for (ii in 1:3) {
  ## create plot and store in list
  p_list[[ii]] <- mtcars %>%
    ggplot(aes(x = mpg,
               y = cyl)) +
    geom_point() +
    scale_x_continuous(minor_breaks = seq(ii * 10, ii * 10 + 10, 1),
                       breaks = seq(ii * 10, ii * 10 + 10, 5),
                       limits = c(ii * 10, ii * 10 + 10))
}

## arrange the plots for saving them into a pdf
# nrow = 1 and ncol = 1, means 1 row and column per page
all_plots <- do.call(marrangeGrob, list(p_list, nrow = 1, ncol = 1))

## save
ggsave(filename = "mtcars.pdf",
       plot = all_plots,
       width = 21,
       height = 29.7,
       units = "cm")