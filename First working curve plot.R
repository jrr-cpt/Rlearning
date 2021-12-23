library(tidyverse)
library(ggplot2)
library(dplyr)
library(readr)
library(ggprism)
library(ggrepel)
library(r2symbols)

## this is the data input defined as the object 'data'
data <- X210915_JRR_phageholinsDNP 

## transform the data into long format from wide
data %>%
  pivot_longer(
    ## don't change first column titled this
    !Time,
    ## split names in next column into these two columns
    names_to = c("Sample", "DNP_Add"),
    ## delimiter used to separate the name above
    names_sep = "_",
    ## put the integers in the next/last column into OD
    values_to = "OD"
    ## output to a dataframe called longdata for viewing and using
    )  -> longdata

## check the 'longdata' got formatted correctly
print(longdata)

## pipe the long data into mutate to add the -/+ conditions labels
longdata %>%
  mutate(
  DNP_Add = ifelse(DNP_Add == "MinusDNP", "-", "+")
  ## save new dataframe as 'parsedlongdata'
  ) -> parsedlongdata

## check the data format is correct and ready to plot
parsedlongdata %>% head()
parsedlongdata %>% tail()

# set minor ticks based on max OD, if OD values rise above 1 scale to 10
if (max(parsedlongdata$OD, na.rm = T) > 1) {
  max_yax = 10
  y_minor = c(rep(1:9, 3)*(10^rep(-2:0, each=9)), 10) # minor ticks
} else {
  max_yax = 1
  y_minor = c(rep(1:9, 2)*(10^rep(c(-2, -1), each=9)), 1) # minor ticks
}

# define custom offset to move line labels away from axis
offset <- max(parsedlongdata$Time)*0.035

## now to actually generate the plot with this data and defined axes
ggplot(parsedlongdata, 
       aes(x = Time, y = OD)) +
  
  ## add lines first, colors assigned by sample, line types based on variable
  geom_line(aes(color = Sample, linetype = DNP_Add), size=1.25)  +
 
   ## define color manually for the lines
  scale_colour_manual('',
                      values = c("#000000ff", "#696969ff", "#c60707ff", "#0000ffff")) +
      ## black = 000000ff
      ## red = c60707ff
      ## blue = 0000ffff
      ## green = 13c559ff
      ## pink = ff00ccff
      ## light grey = e6e6e6ff
      ## dark gray = 696969ff
  
   ## Add points on top all black, skip empty rows
  geom_point(aes(shape = Sample), size=2.5, fill="black", na.rm = T) +
  
  ## labels next to lines
  # geom_text_repel(data=subset(parsedlongdata, Time == max(parsedlongdata$Time)), # labels next to lines
  #                 aes(label=Sample, 
  #                     color=Sample, 
  #                     x=Inf, # put label off plot
  #                     y=OD), # put label at same height as last data point
  #                 direction="y",
  #                 xlim=c(max(parsedlongdata$Time)+offset, Inf), # offset labels
  #                 min.segment.length=Inf, # won't draw lines
  #                 hjust=0, # left justify
  #                 size=5,
  #                 fontface="bold") +
  
  ##to add greek letters displayed use
  ## expression(paste("text", delta, "text"))
  
  ## log scale on y depends on above obect that defines y max as max_yax
  scale_y_log10(limit=c(0.01,max_yax), # put y on log10 scale
                minor_breaks=y_minor,
                guide=guide_prism_minor(),
                expand=c(0,0)) + 
  
  ## set x intervals
  scale_x_continuous(minor_breaks=seq(0,max(parsedlongdata$Time),by=10),
                     guide=guide_prism_minor(),
                     expand=c(0,0)) + 
 
  ## give new axis labels
  labs(x="Time (min)",
       y= "A550") +
 
  ## background white, border, thick lines, and other nice theme things
  theme_prism(border=T) + # theme like prism plot
  
  ## apparently makes the border lines as thick as the ticks
  coord_cartesian(clip="off") +
  
  ## choose shapes based on prism defaults for now
  scale_shape_prism(palette = "filled") +

  ## removes legend and makes it a square
  theme(aspect.ratio=1/1, 
        #legend.position = "none",
        plot.margin=unit(c(1,5,1,1), "lines"))
