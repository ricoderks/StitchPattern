##### match a color from the image to a DMC color
# calculate the euclidian distance between the colors
# the DMC color with the smallest distance is the one to keep
# dmc = matrix with DMC thread colors
# red, green, blue = the color you want to match to a DMC color
# return the matched RGB code for the DMC thread 
calc_dist2 <- function(dmc, red, green, blue) {
  dist_m <- pdist::pdist(dmc[, c("Red", "Green", "Blue")], c(red, green, blue))@dist
  min_idx <- which.min(dist_m[1:length(dist_m)])
  rgb_code <- dmc[min_idx, "RGB code"]
  return(rgb_code)
}
