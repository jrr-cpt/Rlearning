pkgs <- c("tidyverse", "ggprism", "ggrepel") # note tidyverse includes ggplot2 and dplyr
# Check if packages are installed
for (p in pkgs) {
  if(! p %in% installed.packages()){
    install.packages(p, dependencies = TRUE)
  }
}

# Load packages
invisible(lapply(pkgs, library, character.only=T))

# Read in WIDE FORMATED data

### USER INPUT - CHANGE THIS LINE ###
file <- "210915_JRR_phageholinsDNP.csv"
data <- read_csv(file)

### USER INPUT - CHANGE THIS LINE ###
### Input your variable names in quotes followed by ,
### like this c("var1", "var2")
### If you only have one variable like strain/genotype,
### you can leave this line UNCHANGED.
var <- c("strain", "dnp")

# Reformat data into long format
# Rename first column with time to be Time
# first column MUSTTTTT BE TIME
colnames(data)[1] <- "Time"

wideToLong <- function(data, variables=c()) {
  # check if any columns have the _ delimiter specifying multiple variables
  var_check <- grepl("_", colnames(data))
  
  if (TRUE %in% var_check && length(variables) != 0) { # if any columns have the _ delimiter
    data_long <- data %>% 
      gather(key="Sample", value = "OD", -Time) %>% 
      separate(Sample, sep="_", remove=F, into=variables)
  } else {
    data_long <- data %>% 
      gather(key="Sample", value = "OD", -Time)
  }
  
  return(data_long)
}

data_long <- wideToLong(data=data, var)
# print(data_long) to check variables were entered correctly

# define custom offset to move line labels away from axis
offset <- max(data_long$Time)*0.035

# ggprism has default colors to use, but I want to reorder them
cols = ggprism_data$colour_palettes$colors[c(6,1:5,7:20)]

# make ggplot object
simpleplot <- function(data_long) {
  if (max(data_long$OD, na.rm = T) > 1) {
    max_yax = 10
    y_minor = c(rep(1:9, 3)*(10^rep(-2:0, each=9)), 10) # minor ticks
  } else {
    max_yax = 1
    y_minor = c(rep(1:9, 2)*(10^rep(c(-2, -1), each=9)), 1) # minor ticks
  }
  
  g <- data_long %>% 
    ggplot(data_long, aes(x=Time, y=OD)) +
    geom_line(aes(color=Sample), size=1.25) +
    geom_point(aes(shape=Sample), fill="black", size=3.5) +
    geom_text_repel(data=subset(data_long, Time == max(data_long$Time)), # labels next to lines
                    aes(label=Sample, 
                        color=Sample, 
                        x=Inf, # put label off plot
                        y=OD), # put label at same height as last data point
                    direction="y",
                    xlim=c(max(data_long$Time)+offset, Inf), # offset labels
                    min.segment.length=Inf, # won't draw lines
                    hjust=0, # left justify
                    size=5,
                    fontface="bold") +
    scale_shape_prism(palette = "complete") + # change prism bullet shape palette
    scale_color_manual(values=cols) +
    scale_y_log10(limit=c(0.01,max_yax), # put y on log10 scale
                  minor_breaks=y_minor,
                  guide=guide_prism_minor(),
                  expand=c(0,0)) + 
    scale_x_continuous(minor_breaks=seq(0,max(data_long$Time),by=10),
                       guide=guide_prism_minor(),
                       expand=c(0,0)) + 
    labs(x="Time (min)",
         y="A550") +
    theme_prism(border=T) + # theme like prism plot
    coord_cartesian(clip="off") +
    theme(aspect.ratio=1/1, 
          legend.position = "none",
          plot.margin=unit(c(1,5,1,1), "lines"))
  return(g)
}

# complexplot <- function(data_long, variables) {
#   if (max(data_long$OD, na.rm = T) > 1) {
#     max_yax = 10
#     y_minor = c(rep(1:9, 3)*(10^rep(-2:0, each=9)), 10) # minor ticks
#   } else {
#     max_yax = 1
#     y_minor = c(rep(1:9, 2)*(10^rep(c(-2, -1), each=9)), 1) # minor ticks
#   }
#   
#   variables = rep(variables, 2)
#   g <- data_long %>% 
#     ggplot(aes_string(x="Time", y="OD", 
#                       color=variables[1], linetype=variables[2],
#                       shape=variables[3]
#     )) +
#     geom_line(size=1.25) +
#     geom_point(color="black", fill="black", size=3.5) +
#     scale_shape_prism(palette = "complete") + # change prism bullet shape palette
#     scale_color_manual(values=cols) +
#     scale_y_log10(limit=c(0.01,max_yax), # put y on log10 scale
#                   minor_breaks=y_minor,
#                   guide=guide_prism_minor(),
#                   expand=c(0,0)) + 
#     scale_x_continuous(breaks=seq(0,max(data_long$Time),by=10),
#                        guide=guide_prism_minor(),
#                        expand=c(0,0)) + 
#     labs(x="Time (min)",
#          y="A550",
#          color=variables[1], 
#          linetype=variables[2],
#          shape=variables[3]
#     ) +
#     theme_prism(border=T) + # theme like prism plot
#     coord_cartesian(clip="off") +
#     theme(aspect.ratio=1/1, 
#           legend.title = element_text(),
#           plot.margin=unit(c(1,5,1,1), "lines"))
#   return(g)
# }

# save plot as .png
save = paste0(strsplit(basename(file), ".csv")[[1]], ".png")
png(save, width=7.5, height=7.5, units="in", res=300)
if (ncol(data_long) == 3) {
  simpleplot(data_long=data_long)
} else {
  complexplot(data_long=data_long, variables=var)
}
dev.off()

## or if you want to save as a svg file for making full figures in inkscape
# save = paste0(strsplit(basename(file), ".csv")[[1]], ".svg")
# svg(save, width=7.5, height=7.5)
# if (ncol(data_long) == 3) {
#   simpleplot(data_long=data_long)
# } else {
#   complexplot(data_long=data_long, variables=var)
# }
# dev.off()