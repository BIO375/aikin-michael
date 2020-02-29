####Header####
rm(list = ls())
getwd()

if(!require(Rmisc)){install.packages("Rmisc")}
if(!require(DescTools)){install.packages("DescTools")}
if(!require(boot)){install.packages("boot")}
if(!require(rcompanion)){install.packages("rcompanion")}
if(!require(summarytools)){install.packages("summarytools")}
if(!require(tidyverse)){install.packages("tidyverse")}
if(!require(tidyverse)){install.packages("modelr")}

tidyverse_update()
library(tidyverse)

####Chapter 3 Data Visualization####

####3.2.1 The mpg Data Frame####
ggplot2::mpg

####3.2.2 creating a ggplot####
ggplot(data = mpg)+
  geom_point(mapping = aes(x = displ, y= hwy))

#####3.2.3 ggplot template#####
#ggplot(data = <DATA>)+
  #geom_point(mapping = aes(<MAPPING>))

####3.2.4 exercises####
ggplot(data = mpg)
# I don't see anything #
#There are 234 rows of data and 11 columns of data #
#drv describes how the car drives #
ggplot(data = mpg)+
  geom_point(mapping = aes(x = hwy, y= cyl))
# this plot shows that less cylinders leads to greater mpg
ggplot(data = mpg)+
  geom_point(mapping = aes(x = class, y= drv))
# this plot is not helpful because there is no correlation between 
# these data points and they don't offer any data relevant to mpg

####3.3 aesthetic mapping####
ggplot(data = mpg)+
  geom_point(mapping = aes(x = displ, y= hwy, color = class))

ggplot(data = mpg)+
  geom_point(mapping = aes(x = displ, y= hwy, size = class))

ggplot(data = mpg)+
  geom_point(mapping = aes(x = displ, y= hwy, alpha = class))

ggplot(data = mpg)+
  geom_point(mapping = aes(x = displ, y= hwy, shape = class))

ggplot(data = mpg)+
  geom_point(mapping = aes(x = displ, y= hwy),color = "blue")

####3.3.1 exercises####
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, color = "blue"))
#the problem with this code is the location of the color argument
#it should be outside of the aes()argument, but in the mapping argument

#the following variables are categorical: Manufacturer model, trans, drv, fl, class
#the followowing variables are continuous: displ, cty, hwy
ggplot(data = mpg)+
  geom_point(mapping = aes(x = displ, y= hwy, shape = cty))

ggplot(data = mpg)+
  geom_point(mapping = aes(x = displ, y= hwy, color = cty))

ggplot(data = mpg)+
  geom_point(mapping = aes(x = displ, y= hwy, size = cty))
#the continuous variable is mapped on a specturm of color, over a range of sizes, and can't be mapped through shapes

ggplot(data = mpg)+
  geom_point(mapping = aes(x = displ, y= displ, color = displ))
#If you map one variable to multiple aesthetics it will be linear but have no real meaning

#the stroke aesthetic is used to modify the width of a border on a shape
#it can be used on all shapes because every shape has a border

ggplot(data = mpg)+
  geom_point(mapping = aes(x = cty, y= hwy, color = displ < 5))
#the colored variable shows one color as true to the parameter set and the other as false

####3.4 common problems####
#remember to complete parenthesis and quotations, and put the + in the right spot for ggplot

####3.5 Facets####
ggplot(data = mpg)+
  geom_point(mapping = aes(x = displ, y= hwy))+
  facet_wrap(~class, nrow = 2)

ggplot(data = mpg)+
  geom_point(mapping = aes(x = displ, y= hwy))+
  facet_wrap(drv ~ cyl)

ggplot(data = mpg)+
  geom_point(mapping = aes(x = displ, y= hwy))+
  facet_wrap(. ~ cyl)

####3.5.1 exercises####
#if you facet on a continuous variable, it will make seperate plots for each point of the continous variable

ggplot(data = mpg)+
  geom_point(mapping = aes(x = displ, y= hwy))+
  facet_wrap(drv ~ cyl)
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = drv, y = cyl))
#the empty cells in the facet plot are when two treatments overlap, but the data does not fall into it

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_grid(drv ~ .)

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_grid(. ~ cyl)
#the following code makes a two graphs that divide the data up by drv and cyl
#the . in the code tells R that you don't want to facet into columns or rows

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_wrap(~ class, nrow = 2)
#by using the facet option, you don't have to look at all of the plots on the same graph
#you also don't have to differentiate between the different colors, or worry about a legend
#Facet doesn't put the data on the same graph which can make comparing points difficult
#If there is a larger data set facet is far more helpful than color aesthetic

#nrow and ncol arguments stand for numner of rows and columns
#as.table argument also controls the layout of the individual panels
#facet_grid doesnt have nrow and ncol because it is a matrix of panels defined by row and col faceting variables

#by putting the variable with more unique levels in row it will shrink the y-axis making it harder to read
#but if the it is put into columns there will be less trouble reading it.

####3.6 Geometric Objects####

ggplot(data = mpg)+
  geom_point(mapping = aes(x = displ, y= hwy))

ggplot(data = mpg)+
  geom_smooth(mapping = aes(x = displ, y= hwy))

ggplot(data = mpg)+
  geom_smooth(mapping = aes(x = displ, y= hwy, linetype = drv))

ggplot(data = mpg) +
  geom_smooth(mapping = aes(x = displ, y = hwy))

ggplot(data = mpg) +
  geom_smooth(mapping = aes(x = displ, y = hwy, group = drv))

ggplot(data = mpg) +
  geom_smooth(
    mapping = aes(x = displ, y = hwy, color = drv),
    show.legend = FALSE
  )

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) +
  geom_smooth(mapping = aes(x = displ, y = hwy))

ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point() + 
  geom_smooth()

ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point(mapping = aes(color = class)) + 
  geom_smooth()

ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point(mapping = aes(color = class)) + 
  geom_smooth(data = filter(mpg, class == "subcompact"), se = FALSE)

####3.6.1 exercises####
#geom_line, geom_boxplot, geom_area, geom_histogram

#the code will show a scatterplot with dots colored according to drv
#the code will also have a smooth line
ggplot(data = mpg, mapping = aes(x = displ, y = hwy, color = drv)) + 
  geom_point() + 
  geom_smooth(se = FALSE)
#I was almost correct, the lines corresponded to the color and trend of drv

#show.legend = false will remove the legend from the plot
#the legend was not removed earlier in the lesson because it was not sown in the code

#the se argument displays the confidence interval around the smooth line

ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
geom_point() + 
  geom_smooth()

ggplot() + 
  geom_point(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_smooth(data = mpg, mapping = aes(x = displ, y = hwy))
# these graphs will not be different because r assumes that the mapping aesthetic is the same for all layers

#recreating graphs shown

ggplot(data = mpg, mapping = aes(x = displ, y = hwy))+
  geom_point()+
  geom_smooth()
  
ggplot(data = mpg, mapping = aes(x = displ, y = hwy, group = drv))+
  geom_point()+
  geom_smooth()

ggplot(data = mpg, mapping = aes(x = displ, y = hwy))+
  geom_point(mapping = aes(color = drv))+
  geom_smooth()

ggplot(data = mpg, mapping = aes(x = displ, y = hwy))+
  geom_point(mapping = aes(color = drv))+
  geom_smooth(mapping = aes(group = drv))

ggplot(data = mpg, mapping = aes(x = displ, y = hwy))+
  geom_point(mapping = aes(color = drv, stroke = 5))

####3.7 statistical transformations####

ggplot(data = diamonds)+
  geom_bar(mapping = aes(x = cut))

ggplot(data = diamonds)+
  stat_count(mapping = aes(x = cut))

demo <- tribble(
  ~cut,         ~freq,
  "Fair",       1610,
  "Good",       4906,
  "Very Good",  12082,
  "Premium",    13791,
  "Ideal",      21551
)

ggplot(data = demo) +
  geom_bar(mapping = aes(x = cut, y = freq), stat = "identity")

ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, y = stat(prop), group = 1))

ggplot(data = diamonds) + 
  stat_summary(
    mapping = aes(x = cut, y = depth),
    fun.ymin = min,
    fun.ymax = max,
    fun.y = median
  )

####3.7.1 exercises####
#the default geom for stat_summary is geom_pointrange
ggplot(data = diamonds)+
  geom_pointrange(mapping = aes(x = cut))

#geom_col uses stat_identity instead of stat_count

#Stat_smooth computes continuous variables
#

ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, y = ..prop..))
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = color, y = ..prop..))
#All proportions need to add up to one
#these charts do not have proportions that add up to one

####3.8 Position adjustments####
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, colour = cut))
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = cut))

ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = clarity))

ggplot(data = diamonds, mapping = aes(x = cut, fill = clarity)) + 
  geom_bar(alpha = 1/5, position = "identity")
ggplot(data = diamonds, mapping = aes(x = cut, colour = clarity)) + 
  geom_bar(fill = NA, position = "identity")

ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = clarity), position = "fill")

ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = clarity), position = "dodge")

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy), position = "jitter")

####3.8.1 exercises####

ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) + 
  geom_point()
#this plot can be improved by adding geom_jitter, which would show the rest of the points available
ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) + 
  geom_jitter()

#width and height control the amount of jittering in geom_jitter

ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) + 
  geom_count()
#geom_count layers points that overlap and adjusts the size of points based on the amount that overlap
#geom_jitter plots all points regardless if they align with the grid
#jitter shows all data while count does not

#the default position of geom_boxplot is "dodge2"
ggplot(data = mpg)+
  geom_boxplot(mapping = aes(x = hwy, y = cty), position = "dodge2")

####3.9 coordinate systems####

ggplot(data = mpg, mapping = aes(x = class, y = hwy)) + 
  geom_boxplot()
ggplot(data = mpg, mapping = aes(x = class, y = hwy)) + 
  geom_boxplot() +
  coord_flip()

#have to install map package for this one
install.packages(c("maps"))
library(maps)

nz <- map_data("nz")

ggplot(nz, aes(long, lat, group = group)) +
  geom_polygon(fill = "white", colour = "black")

ggplot(nz, aes(long, lat, group = group)) +
  geom_polygon(fill = "white", colour = "black") +
  coord_quickmap()

bar <- ggplot(data = diamonds) + 
  geom_bar(
    mapping = aes(x = cut, fill = cut), 
    show.legend = FALSE,
    width = 1
  ) + 
  theme(aspect.ratio = 1) +
  labs(x = NULL, y = NULL)

bar + coord_flip()
bar + coord_polar()

####3.9.1 Exercises####
rm(list = ls())

ggplot(data = mpg)+
  geom_bar(mapping = aes(x = hwy, fill = class))

bar <- ggplot(data = mpg) + 
  geom_bar(
    mapping = aes(x = hwy, fill = class), 
    show.legend = FALSE,
    width = 1
  ) + 
  theme(aspect.ratio = 1) +
  labs(x = NULL, y = NULL)

bar+coord_flip()
bar+coord_polar()

#labs() modifies axis legends and plot labels

#coord_qucikmap is better for small areas close to the equator because
#the approximation preserves straight lines

ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) +
  geom_point() + 
  geom_abline() +
  coord_fixed()
  
#this plot tells us that cty will always be slightly less efficient than hwy.
#coord_fixed controls the aspect ratio that way the plot isn't stretched out.
#geom_abline() adds a line of best fit to the plot by using slope and intercepts.

####3.10 The layered grammar of graphics####

#new ggplot template#
#ggplot(data = <DATA>) + 
#<GEOM_FUNCTION>(
  #mapping = aes(<MAPPINGS>),
  #stat = <STAT>, 
  #position = <POSITION>
#) +
  #<COORDINATE_FUNCTION> +
  #<FACET_FUNCTION>









