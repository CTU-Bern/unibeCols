library(hexSticker)
library(ggplot2)
library(unibeCols)
library(png)
library(grid)

dat <- unibePalettes(plot = FALSE)
dat <- dat[dat$x == 1, ]

b <- 2
a <- 1

segb <- b/6 # strip width
tanA <- .5
sega <- tanA * segb # 0.166667
sega <- .3

polys <- data.frame(x = c(0, segb, segb, 0, 0 #strip 1
                          , segb, segb*2, segb*2, segb, segb #strip 2
                          , segb*2, segb*3, segb*3, 2*segb, 2*segb #strip 3
                          , segb*3, segb*4, segb*4, 3*segb, 3*segb #strip 4
                          , segb*4, segb*5, segb*5, 4*segb, 4*segb #strip 5
                          , segb*5, segb*6, segb*6, 5*segb, 5*segb #strip 6
                          , segb*6, segb*7, segb*7, 6*segb, 6*segb #strip 7
                          , segb*7, segb*8, segb*8, 7*segb, 7*segb #strip 8
                          , segb*8, segb*9, segb*9, 8*segb, 8*segb #strip 9
                          , segb*9, segb*10, segb*10, 9*segb, 9*segb #strip 10
                          , segb*10, segb*11, segb*11, 10*segb, 10*segb #strip 11
                          , segb*11, segb*12, segb*12, 11*segb, 11*segb #strip 12
                          ),
                   y = c(-2, -2 - sega, 2 + sega, 2, -2 #strip 1
                         , -2 - sega, -2 - 2 * sega, 2 + 2 * sega, 2 + sega, -2 - sega #strip 2
                         , -2 - 2*sega, -2 - 3 * sega, 2 + 3 * sega, 2 + 2*sega, -2 - 2*sega #strip 3
                         , -2 - 3*sega, -2 - 4 * sega, 2 + 4 * sega, 2 + 3*sega, -2 - 3*sega #strip 4
                         , -2 - 4*sega, -2 - 5 * sega, 2 + 5 * sega, 2 + 4*sega, -2 - 4*sega #strip 5
                         , -2 - 5*sega, -2 - 6 * sega, 2 + 6 * sega, 2 + 5*sega, -2 - 5*sega #strip 6
                         , -2 - 6*sega, -2 - 5 * sega, 2 + 5 * sega, 2 + 6*sega, -2 - 6*sega #strip 7
                         , -2 - 5*sega, -2 - 4 * sega, 2 + 4 * sega, 2 + 5*sega, -2 - 5*sega #strip 8
                         , -2 - 4*sega, -2 - 3 * sega, 2 + 3 * sega, 2 + 4*sega, -2 - 4*sega #strip 9
                         , -2 - 3*sega, -2 - 2 * sega, 2 + 2 * sega, 2 + 3*sega, -2 - 3*sega #strip 10
                         , -2 - 2*sega, -2 - sega, 2 + sega, 2 + 2*sega, -2 - 2*sega #strip 11
                         , -2 - sega, -2, 2, 2 + sega, -2 - sega #strip 12
                         ),
                   poly = rep(1:12, each = 5),
                   cols = rep(dat$cols, each = 5))

# unibelogo <- "man/figures/ub_Logo_english_2019_RGB.png"
# img <- readPNG(unibelogo)
# g <- rasterGrob(img, interpolate = TRUE, )

p <- ggplot() +
  # geom_bar(aes(x = palette, y = x, fill = cols),
  #          data = dat,
  #          stat = "identity", width = 1) +
  geom_polygon(data = polys,
               aes(x = x, y = y, group = poly, fill = cols)) +
  # geom_polygon(data = data.frame(x = c(0,12,12,0,0),
  #                                y = c(.675, .675, 1, 1, .675)),
  #              aes(x = x, y = y),
  #              inherit.aes = FALSE,
  #              position = position_nudge(x = .5),
  #              fill = "white") +
  # annotation_custom(g, 4.5, 8.5, .65, .85) +
  scale_fill_identity() +
  theme_void() +
  xlim(0,4)

s <- sticker(p, package="",
             s_x=1, s_y=1, s_width=1.9, s_height=2.2,
             filename="man/figures/logo.png",
             h_fill = colorRampPalette(c("white", CTUtemplate::unibeRed()))(6)[3],
             h_color = CTUtemplate::unibeRed(),
             h_size = 2,
             url = "   unibeCols",
             u_size = 12,
             u_x = 1,
             u_y = 0.15,
             white_around_sticker = FALSE
)
s
