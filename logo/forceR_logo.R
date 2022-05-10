# this script creates the forceR logo

library(hexSticker)

library(forceR)

library(tidyverse)

bite_sin_1 <- simulate_bites(
  no.of.bites = 1,
  length.of.bite = 1000,
  length.of.series = 1100,
  max.y = 1,
  max.y.jit = NULL,
  peak.pos = 50,
  slope.perc.start = 10,
  slope.perc.end = slope.perc.start,
  jit = NULL,
  bite.type = "sin",
  plot = TRUE
) %>%
  mutate(type = "sin2")

bite_sin_2 <- simulate_bites(
  no.of.bites = 1,
  length.of.bite = 1000,
  length.of.series = 1100,
  max.y = 0.5,
  max.y.jit = NULL,
  peak.pos = 50,
  slope.perc.start = 10,
  slope.perc.end = slope.perc.start,
  jit = NULL,
  bite.type = "sin",
  plot = TRUE
) %>%
  mutate(type = "sin3")

bite_plat <- simulate_bites(
  no.of.bites = 1,
  length.of.bite = 1000,
  length.of.series = 1100,
  max.y = 0.75,
  max.y.jit = NULL,
  peak.pos = 50,
  slope.perc.start = 20,
  slope.perc.end = 20,
  jit = NULL,
  bite.type = "plat",
  plot = TRUE
) %>%
  mutate(type = "plat")

bite_both <- rbind(bite_sin_1, bite_sin_2, bite_plat)

plot <- ggplot(bite_both,
       aes(x = t ,
           y = y,
           colour=type)) +
  geom_line() +
  # theme_transparent() +
  theme_void() +
  theme(legend.position = "none")
plot

sticker(plot,
        package="forceR",


        s_x=1,
        s_y=1.11,

        s_width=1.5,
        s_height=1.6,

        p_size=17,
        p_x = 1,
        p_y = 0.45,
        p_color = "gray40",

        url = "https://github.com/Peter-T-Ruehr/forceR",
        u_color = "gray40",
        u_size = 2,

        h_fill="white",
        h_color="gray50",

        filename="logo/forceR_logo.png")

usethis::use_logo("logo/forceR_logo.png")
