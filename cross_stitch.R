library(magick)
library(imager)
library(tidyverse)
library(gridExtra)
library(furrr)
source("./functions.R")

# load image
my_img <- image_read("./butterfly.jpg")

## first resize image
# has to be done with magick otherwise there will to may colors again
# you can get width and height with image_info()
my_img_small <- image_resize(image = my_img,
                         geometry = "800x") # 400px wide
## reduce the colors in rgb space
max_colors <- 100
final_img <- image_quantize(my_img_small, 
                            max = max_colors, # max colors
                            colorspace = "rgb")
## write the image to disk as jpg
# image_write(image = final_img,
#             path = "./butterfly_75.jpg",
#             format = "jpg")


## convert to cimg class for further processing
c_img <- magick2cimg(final_img)

## make wide dataframe
df_img <- as.data.frame(c_img)
df_img <- df_img %>%
  pivot_wider(names_from = cc,
              names_prefix = "c",
              values_from = value)

## calculate the rgb colors
df_img <- df_img %>%
  mutate(rgb_code = rgb(red = c1,
                        green = c2,
                        blue = c3))

## check the number of colors
length(unique(df_img$rgb_code))

## here the matching of the dmc thread rgb colors can be done
# read file with DMC rgb color codes
dmc_codes <- read_csv(file = "./DMC_Cotton_Floss_to_RGB.csv",
                      col_types = "ccdddcc") %>%
  # rescale the rgb codes to max value 1
  mutate_at(c("Red", "Green", "Blue"),
            ~ . / 255) %>% 
  mutate(`RGB code` = paste("#", `RGB code`, sep = ""))
# create dmc matrix
dmc_m <- as.matrix(dmc_codes[, c("Red", "Green", "Blue", "RGB code")])

# match a rgb code of a pixel to a DMC threat code
plan(multiprocess)
uniq_code <- df_img %>% 
  distinct(rgb_code, .keep_all = TRUE) %>% 
  mutate(dmc_rgb_code = future_pmap(.l = list(c1, c2, c3),
                                    .f = ~ calc_dist2(dmc = dmc_m,
                                                      red = ..1,
                                                      green = ..2,
                                                      blue = ..3)))

df_img <- df_img %>% 
  left_join(y = uniq_code[, c("rgb_code", "dmc_rgb_code")],
            by = c("rgb_code" = "rgb_code"))

## create symbols for rgb color codes
my_symbols <- data.frame(rgb_code = unique(df_img$rgb_code),
                         symbols = unlist(strsplit(rawToChar(as.raw(33:126)), split = ""))[1:max_colors])

## create table for legend
# at the moment this is without DMC thread names
# define pdf file
pdf(file = "my_table.pdf",
    width = 8.3,
    height = 11.7,
    paper = "a4")
# create the tables
limits <- floor(seq(1, nrow(my_symbols), length.out = 4))
g1 <- tableGrob(my_symbols[limits[1]:limits[2], ])
g2 <- tableGrob(my_symbols[(limits[2] + 1):limits[3], ])
g3 <- tableGrob(my_symbols[(limits[3] + 1):limits[4], ])
# align the tables
grid.arrange(
  gtable_combine(g1, g2, g3, along = 1),
  nrow = 1
)
#save
dev.off()

df_img <- df_img %>% 
  left_join(y = my_symbols,
            by = "rgb_code")

## plot per page
# initialize plot list
p_list <- list()

# step size in x direction
num_step_w <- 40
step_w <- ceiling(width(c_img) / num_step_w)
num_step_h <- 80
step_h <- ceiling(height(c_img) / num_step_h)

# initialize page counter
page_count <- 1

for (xx in 1:step_w) {
  for (yy in 1:step_h) {
  p_list[[page_count]] <- df_img %>% 
    ggplot(aes(x = x,
               y = y)) +
    # # use this one to show the picture
    # geom_tile(aes(fill = rgb_code),
    #           colour = "grey50") +
    # scale_fill_identity() +
    # put the symbols in
    geom_text(aes(label = symbols),
              size = 2) +
    # modify y-axis
    scale_y_continuous(trans = "reverse",
                       limits = c(yy * num_step_h, (yy - 1) * num_step_h),
                       breaks = c((yy - 1) * num_step_h + 1, seq((yy - 1) * num_step_h, yy * num_step_h, 5)),
                       minor_breaks = c((yy - 1) * num_step_h + 1.5, seq((yy - 1) * num_step_h, yy * num_step_h, 1) + 0.5),
                       expand = expand_scale(add = -0.5)) +
    # modify x-axis
    scale_x_continuous(limits = c((xx - 1) * num_step_w, xx * num_step_w),
                       breaks = c((xx - 1) * num_step_w + 1, seq((xx - 1) * num_step_w, xx * num_step_w, 5)),
                       minor_breaks = c((xx - 1) * num_step_w + 1.5, seq((xx - 1) * num_step_w, xx * num_step_w, 1) + 0.5),
                       expand = expand_scale(add = -0.5)) +
    # play around with the theme
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_line(colour = "grey"),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black"))
  # up the page count
  page_count <- page_count + 1
  }
}

## combine the plots
# this is slow
system.time(
  all_plots <- marrangeGrob(p_list, nrow = 1, ncol = 1)
)

## save
system.time(
  ggsave(filename = "pattern.pdf",
         plot = all_plots,
         width = 21,
         height = 29.7,
         units = "cm")
)

##  show complete pattern
df_img %>% 
  ggplot(aes(x = x,
             y = y)) +
  # # use this one to show the picture
  geom_tile(aes(fill = dmc_rgb_code)
            # colour = "grey50"
            ) +
  scale_fill_identity() +
  # put the symbols in
  # geom_text(aes(label = symbols),
  #           size = 1) +
  # modify y-axis
  scale_y_continuous(trans = "reverse",
                     limits = c(step_h * num_step_h, 0),
                     breaks = c(1, seq(5, step_h * num_step_h, 5)),
                     minor_breaks = c(1.5, seq(1, step_h * num_step_h, 1) + 0.5),
                     expand = expand_scale(add = -0.5)) +
  # modify x-axis
  scale_x_continuous(limits = c(1, step_w * num_step_w),
                     breaks = c(1, seq(0, step_w * num_step_w, 5)),
                     minor_breaks = c(1.5, seq(1, step_w * num_step_w , 1) + 0.5),
                     expand = expand_scale(add = -0.5)) +
  # play around with the theme
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_line(colour = "grey"),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))
