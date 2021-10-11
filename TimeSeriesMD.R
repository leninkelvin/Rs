# libraries:
library(ggplot2)
library(gganimate)
library(hrbrthemes)
library(gifski)

#setwd("/Volumes/Zamasu/Mobile4/Dengue/DENV4-QND10")

#Create a new data frame with assigned titles
# x = data.frame("Steps" = c(dII_I$V1), "RMSD" = c(dII_I$V2))

y = rep(c("1st simulation", "2dn simulation", "3th simulation", "4th simulation","5th simulation"), each = 5000) 
# This steps generates a vector with columns fill 
# with the values that will be used to reveal the date. 

z = data.frame(y) 
# This transforms the above vector to a usable dataframe

#Joining the data in to a single continuous dates, this is the simulation derive data
x = rbind(dII_I, dII_II, dII_III, dII_IV, dII_V)

#Code to add data in columns, here we add the labels in the form of colors
x = cbind(x, z)

#Code to add data in columns
#x = cbind(x, Sim1 = c(dII_I$V2), Sim2 = c(dII_II$V2), Sim3 = c(dII_III$V2), Sim4 = c(dII_IV$V2), Sim5 = c(dII_V$V2))

#ggplot(x, aes(x = V1, y = V2, color = (y))) + geom_line() + transition_reveal(V1) 

myPlot = ggplot(x, aes(x = V1, y = V2, color = (y))) +
  geom_line(size = 1) +
  geom_point(size = 5) +
  ggtitle("Ligand to Catalytic Site distance") +
  theme_ipsum() +
  ylab("Distance (angstroms)") +
  xlab("Simulation step (*0.002 to nanoseconds)") +
  transition_reveal(V1) +
  facet_grid(rows = vars(y)) +
  geom_hline(aes (yintercept=8.8))

animate(myPlot, duration = 15, fps = 10, width = 800, height = 800, renderer = gifski_renderer())
anim_save("dynamic.gif")

static = ggplot(x, aes(x = V1, y = V2, color = (y))) +
  geom_line(size = 0.5) +
  ggtitle("Ligand to Catalytic Site distance") +
  theme_ipsum() +
  ylab("Distance (angstroms)") +
  xlab("Simulation step (*0.002 to nanoseconds)") + 
  facet_grid(rows = vars(y)) + 
  geom_hline(aes (yintercept=8.8))

ggsave(static, filename = "static.png")

tiff(filename = 'Sim1.tiff', width = 600, height = 350)
#ggplot(dII_I, aes(x = V2)) + geom_histogram() + coord_flip() # if ggplot is prefered
hist(dII_I$V2, #histogram
     col = "white", 
     main = "Histogram for catalytic site to QND10 distance",
     xlab = "Distance in angstroms",
     xlim = c(5, 35), # limits of the x axis
     las =1, # orientation of the labels
     probability = FALSE ) # use fraction of total, set FALSE for frequency
abline(v=8.8, col="black", lwd = 2)
#lines(density(dII_I$V2)) # add a line to the fit
dev.off()

tiff(filename = 'Sim2.tiff', width = 600, height = 350)
#ggplot(dII_II, aes(x = V2)) + geom_histogram() + coord_flip() # if ggplot is prefered
hist(dII_II$V2, #histogram
     col = "white", 
     main = "Histogram for catalytic site to QND10 distance",
     xlab = "Distance in angstroms",
     xlim = c(5,35), # limits of the x axis
     las =1, # orientation of the labels
     probability = FALSE ) # use fraction of total, set FALSE for frequency
abline(v=8.8, col="black", lwd = 2)
#lines(density(dII_II$V2)) # add a line to the fit
dev.off()

tiff(filename = 'Sim3.tiff', width = 600, height = 350)
#ggplot(dII_III, aes(x = V2)) + geom_histogram() + coord_flip() # if ggplot is prefered
hist(dII_III$V2, #histogram
     col = "white", 
     main = "Histogram for catalytic site to QND10 distance",
     xlab = "Distance in angstroms",
     xlim = c(5,35), # limits of the x axis
     las =1, # orientation of the labels
     probability = FALSE ) # use fraction of total, set FALSE for frequency
abline(v=8.8, col="black", lwd = 2)
#lines(density(dII_I$V2)) # add a line to the fit
dev.off()

tiff(filename = 'Sim4.tiff', width = 600, height = 350)
#ggplot(dII_I, aes(x = V2)) + geom_histogram() + coord_flip() # if ggplot is prefered
hist(dII_IV$V2, #histogram
     col = "white", 
     main = "Histogram for catalytic site to QND10 distance",
     xlab = "Distance in angstroms",
     xlim = c(5,35), # limits of the x axis
     las =1, # orientation of the labels
     probability = FALSE ) # use fraction of total, set FALSE for frequency
abline(v=8.8, col="black", lwd = 2)
#lines(density(dII_IV$V2)) # add a line to the fit
dev.off()

tiff(filename = 'Sim5.tiff', width = 600, height = 350)
#ggplot(dII_I, aes(x = V2)) + geom_histogram() + coord_flip() # if ggplot is prefered
hist(dII_V$V2, #histogram
     col = "white", 
     main = "Histogram for catalytic site to QND10 distance",
     xlab = "Distance in angstroms",
     xlim = c(5,35), # limits of the x axis
     las =1, # orientation of the labels
     probability = FALSE) # use fraction of total, set FALSE for frequency
abline(v=8.8, col="black", lwd = 2)
#lines(density(dII_I$V2)) # add a line to the fit
dev.off()


