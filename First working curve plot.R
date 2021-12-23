library(tidyverse)
library(ggplot2)
library(dplyr)
library(readr)
library(ggprism)

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

## define my color vector? this isn't working yet and now it broke the colors
##cols <- c("8" = "red", "4" = "blue", "6" = "darkgreen", "10" = "orange")
  
## now to actually generate the plot with this data and defined axes
ggplot(parsedlongdata, aes(x = Time, y = OD)) +
  
  ## add lines first, colors assigned by sample, line types based on variable
  geom_line(aes(color = Sample, linetype = DNP_Add), size=1.25)  +
  scale_colour_manual('',
                      values = c("#00167B", "#9FA3FE", "#ff5733", "#974e3e")) +
  ## Add points on top all black, skip empty rows
  geom_point(aes(shape = Sample), size=2.5, fill="black", na.rm = T) +
  ## choose shapes based on prism defaults for now
  scale_shape_prism(palette = "default") #+
  ## Cody's custom-defined vector of colors added in here
#  scale_color_manual(values = cols)
# theme_prism(border=T, palette = "colors")