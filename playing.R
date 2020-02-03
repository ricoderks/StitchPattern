library(tidyverse)
library(imager)

# load an image,
# this one is "quite" big 2.8 mb, resutls in large object 188 mb
org_image <- load.image("./Laura.vlinderslag.JPG")
org_image


plot(org_image)

# resize image
# there are some extra options
small_image <- imresize(im = org_image,
                        scale = 0.2)
plot(small_image)

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
