# libraries:
library(ggplot2)
library(gganimate)
library(hrbrthemes)
library(gifski)

myPlot = rmsA %>%
  ggplot( aes(x=V1, y=V2)) +
  geom_line() +
  geom_point() +
  ggtitle("RMSD for subunit A") +
  theme_ipsum() +
  ylab("RMSD (angstroms)") +
  xlab("Simulation time (ns)") +
  transition_reveal(V1)

animate(myPlot, duration = 5, fps = 20, width = 800, height = 800, renderer = gifski_renderer())
anim_save("test.gif")

