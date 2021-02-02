#
# Resilience diagram
#
# Bill Kaye-Blake
# September 2020
#
# Purpose: draw Resilience diagram as per:
#
# Payne, P. R., Kaye-Blake, W. H., Stirrat, K. A., Ellison, R. A., Smith, M. J., & Brown, M. (2019).
# Identifying resilience dimensions and thresholds: evidence from four rural communities in New Zealand.
# Resilience, 7(2), 149-171.
#
# Brown, M., Kaye-Blake, B., & Payne, P. (Eds.). (2019).
# Heartland strong: How rural New Zealand can change and thrive. Massey University Press.
#
#
# NB: Inputs are changed in input section of script.
#

library(circlize)

#
# INPUT section
#

# Resilience dimensions for modelling
# Each dimension gets a score from 0 to 1. A score of 0.4 or lower is below the resilience threshold.

# For manual entry
Envt <- 0.4
Econ <- 0.5
Socl <- 0.6
Cult <- 0.7
Inst <- 0.8

Place_name <- "Anywhere" # Write your own inside the inverted commas

# End inputs -- the rest of the script runs from the inputs

# Set up data frame and wedge colours
rocky = data.frame(
        dimensions = c("Cultural", "Environmental", "Institutional", "Economic", "Social"),
        scores = c(Cult, Envt, Inst, Econ, Socl),
        place = Place_name,
        dim_colour = c("red3"),
        dim_writing = c("bending.inside"),
        stringsAsFactors = FALSE)

for (i in 1:5)
  {
  if (rocky$scores[i] > 0.40) {rocky$dim_colour[i] = "orange2"}
  if (rocky$scores[i] > 0.55) {rocky$dim_colour[i] = "gold"}
  if (rocky$scores[i] > 0.70) {rocky$dim_colour[i] = "lightblue3"}
  }

for (i in 1:5)
{
  if (i == 3) {rocky$dim_writing[i] = "bending.outside"}
}

#
# When outputting to pdf file -- change text cex to 1.6 and final lwd to 2
# Otherwise, = 1
#
# Set up output file with Region name (from above)

# pdf(file=paste(Place_name, ".pdf", sep=""))

# jpeg(filename = paste(Place_name, ".jpg", sep=""),
#    width = 480, height = 480, units = "px", pointsize = 12,
#    bg = "white")


#
# Draw the circular parts
#
circos.par("track.height" = 0.15, "start.degree" = 90)

circos.initialize(factors = rocky$dimensions, xlim = c(0,1))
circos.trackPlotRegion(factors = rocky$dimensions, ylim = c(0,0.1))

draw.sector(0, 360, rou1 = 1, rou2 = 0.84, col = "grey50", border = "grey50")

circos.text(3.64,0.04, labels = "External", facing = "bending.outside", col = "white", cex = 1)
circos.text(6.25,0.06, labels = "External", facing = "bending.inside", col = "white", cex = 1)

circos.trackPlotRegion(factors = rocky$dimensions,
                       ylim = c(0,0.1),
                       bg.col = "black",
                       bg.border = "white"
                       )
for (i in 1:5){
circos.text(0.5, 0.05,
            sector.index = rocky$dimensions[i],
            labels = rocky$dimensions[i],
            col = "white",
            facing = rocky$dim_writing[i],
            cex = 1)
}

#
# Draw the wedges
#
for (i in 1:5){

draw.sector(start.degree = 90-((i-1)*72),
            end.degree = 18-((i-1)*72),
            rou1 = rocky$scores[i]*0.65,
            rou2 = 0,
            col = rocky$dim_colour[i],
            border = "white")
}

# Minimum threshold dotted circle -- optional
draw.sector(start.degree = 0, end.degree = 360, rou1 = 0.4*0.65, lty = "dashed", lwd = 1)

# Close output file
# dev.off()

# Clear circos information
circos.clear()

# End
