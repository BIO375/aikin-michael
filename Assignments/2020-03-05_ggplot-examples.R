####Header####
rm(list = ls())

library("tidyverse")
if(!require(nycflights13)){install.packages("nycflights13")}
if(!require(gapminder)){install.packages("gapminder")}
if(!require(Lahman)){install.packages("Lahman")}
tidyverse_update()

####Section 3.2####

#basics
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy))
#the data assigned to the ggplot is mpg and this is done through data = mpg
#+ is added at the end of the line so that R knows to apply this to the following plots
#geom_point is the function being used and the aes being mapped are x and y
#x is assigned to the discrete variable displ
#y is assigned to the continuous variable hwy

####Section 3.3####

#additional aes
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, color = class))
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, size = class))
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, alpha = class))
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, shape = class))
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy), color = "blue")
#some key additional aes are color, size, alpha, and shape
#color is used to assign color to points based on a certain variable
#size changes the size of points based on a certain variable
#alpha changes the transparency of points based on a certain variable
#shape changes the shape of points based on a certain variable
#all of these aes can be used outside of mapping but it will apply them to the entire plot

####Section 3.5####
#using facet wrap
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_wrap(~ class, nrow = 2)
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_grid(drv ~ cyl)
#the first two lines set up the parameters of the plot
#the third line is what facets
#it facets by class and nrow indicates the number of rows
#for the second plot the same thing is true for the first two lines
#the third line uses facet_grid instead of facet_wrap
#facet_grid creates a matrix of panels defined by row and column faceting variables
#in this case the variables would be drv and cyl

####Section 3.6 Geometric Objects####

#colored points with a smooth line
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point(mapping = aes(color = class)) + 
  geom_smooth(data = filter(mpg, class == "subcompact"), se = FALSE)
#this graph is mapped with displ on the x axis and hwy on the y axis
#This information was put in the initial line because both layers use it

#the second line is mapping out the scatter plot
#the scatterplot first uses the top line to map out x and y
#the second line is then used to assign color to the points based on the class of car

#the third line is mapping out the smooth line
#the code in this line is assigning the data to the smooth line
#the data being assigned is the class subcompact

#colored points with a white border
ggplot(data = mpg, mapping = aes(x = displ, y = hwy))+
  geom_point( size = 5, stroke = 2, color = "white")+
  geom_point( mapping = aes(color = drv), size = 3)
#this plot requires two layer in order to put a white border around the points
#the first line is used to map out the x and y variables

#the second line is the first layer that will be our white borders
#mapping = aes is not needed in this line because it is being applied to everything
#the size is set to 5 so that it can be seen behind the colored points
#the color is also set to white

#the third line is the next layer that will be our scatterplot colored by drv
#the aes is mapped so that they are colored based on drv type
#the size is set to 3 so that they are smaller than the white points

#using geom_smooth
ggplot(data = mpg) +
  geom_smooth(mapping = aes(x = displ, y = hwy, group = drv))
#the first line is being used to assign data
#the second line is being used for the geom_smooth function
#the aes being mapped are displ and hwy on the x and y axis respectively
#the last aes is the group aes, which creates a line for each group of drv

####section 3.7####
#changing the stat
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, y = stat(prop), group = 1))
#the aes to pay attention to here is stat(prop)
#this changes the statistic from count to proportion, which is more aesthetically pleasing
#also proportion is a little more practical than count 

####Section 3.8####
#using transparency to represent data
ggplot(data = diamonds, mapping = aes(x = cut, fill = clarity)) + 
  geom_bar(alpha = 1/5, position = "identity")
#the first line of this plot just sets up the basic parameters
#but it is important to look at the fill aes
#it is set to clarity, so the bars will be filled/colored based on the clarity data

#the second line is using alpha. alpha determines the transparency
#we use alpha here so that we can see the layers of fill = clarity on the plot

#using the dodge position as a psudeo facet_wrap
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = clarity), position = "dodge")
#the first line is used to assign the data to the plot
#the second line sets the parameter of x and fill 
#in addition to this, the position is changed
#position = dodge separates one bar into smaller bars to make a distinction in clarity
#this one is probably my favorite

####Section 3.9####
#using boxplots
ggplot(data = mpg, mapping = aes(x = class, y = hwy)) + 
  geom_boxplot()
ggplot(data = mpg, mapping = aes(x = class, y = hwy)) + 
  geom_boxplot() +
  coord_flip()
#the first line is always assigning data and universal parameters (x and y)
#the second line is used for the geom_boxplot function
#the third line in the second plot is used to flip the plot so that the lables can be read easily

#using maps!!!!! 
install.packages("maps")
library("maps")
nz <- map_data("nz")

ggplot(nz, aes(long, lat, group = group)) +
  geom_polygon(fill = "white", colour = "black")

ggplot(nz, aes(long, lat, group = group)) +
  geom_polygon(fill = "white", colour = "black") +
  coord_quickmap()
#okay so we need to install the necessary packages
#then we need to make an object and assign a map to it "nz"
#geom_polygon is the plot that lets us visualize a shape
#the aes we need to worry about are long,lat
#Fill white and colour black are just the colors of the map
#coord_quickmap is used in the second plot because it is a quick way to get a map
#with the correct aspect ratio

#converting a bar plot of coord_flip to coord_polar
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
#the first thing that happens in this is the ggplot is assigned to an object
#this is important because then we will be able to change it with coord_flip and coord_polar

#the data being used is diamonds
#the mapping aes is the same as the other bar charts, except for the fill which is now cut
#there are no labels, bin width is specified, there is no legend

#the coord_flip is used to rotate the chart on its side
#the coord_flip is used to make the chart in to something similar to a pie chart

####Section 3.10####
# Template
# ggplot(data = <DATA>) + 
# <GEOM_FUNCTION>(
#    mapping = aes(<MAPPINGS>),
#    stat = <STAT>, 
#    position = <POSITION>
#  ) +
#  <COORDINATE_FUNCTION> +
#  <FACET_FUNCTION>
