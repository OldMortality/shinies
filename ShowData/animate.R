#if(!require(devtools)) install.packages("devtools")
devtools::install_github("dgrtwo/gganimate")
library(devtools)
library(RCurl)
library(httr)
set_config( config( ssl_verifypeer = 0L ) )
devtools::install_github("RcppCore/Rcpp")
devtools::install_github("dgrtwo/gganimate")


library(gganimate)

install.packages("gapminder")
library(gapminder)
head(gapminder)
#Simple animation
#Plot the variable lifeExp (life expectancy at birth) by the variable gdpPercap (GDP per capita).
#Make animation by year using the aesthetic frame = year.
# Load required package
library(gapminder)
library(ggplot2)
library(gganimate)
# Basic scatter plot
mapping <- aes(x = gdpPercap, y = lifeExp, 
               size = pop, color = continent,
               frame = year) 
p <- ggplot(gapminder, mapping = mapping) +
  geom_point() +
  scale_x_log10()
# Animate
gganimate(p)
