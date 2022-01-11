# This is just a brief intro into tidyverse, and some of the tidy verbs.

# you'll need to install r and (if you want) r studio
# go here
# https://support.rstudio.com/hc/en-us/articles/201141096-Getting-Started-with-R

# First, there are lots of datasets available in R preloaded for learning and testing.
data()

# Lots of cool stuff there...some of the most popular ones are iris, faithful, mtcars, toothgrowth, titanic
# you can see a snapshot of a data set by using head() to give you first five rows by default
head(faithful)
head(mtcars, 10) # first ten rows
tail(ToothGrowth) # last five rows
head(Titanic)
head(iris, 20)

# lets use iris for now...
# let's get an idea of what this dataset looks like
str(iris)
summary(iris)

# how about a histogram of Petal.Length?
hist(iris$Petal.Length)

# verrrrrrrrrrrrrry interesting
# hmmmm that bimodality sus tho
# maybe it's the species?
# run all five lines below together
par(mfrow = c(1, 3)) # sets a 1x3 grid for my three plots to go into
hist(iris$Petal.Length[iris$Species == 'setosa'])
hist(iris$Petal.Length[iris$Species == 'virginica'])
hist(iris$Petal.Length[iris$Species == 'versicolor'])
par(mfrow = c(1,1)) # resets the grid

# what about a scatterplot?
plot(iris$Sepal.Length, iris$Sepal.Width)

# hmmm not much relationship there...
plot(iris$Sepal.Length, iris$Petal.Length)

# ok there's an interesting pattern between those two...let's color it by species
plot(iris$Sepal.Length, iris$Petal.Length, col = iris$Species)

# dope

# wonder what the min, mean, max of sepal width is...
min(iris$Sepal.Width)
mean(iris$Sepal.Width)
max(iris$Sepal.Width)

# R has a lot of capability by itself, in its "base" package. 
# But there are lots of other packages, like "tidyverse", that allow the user to perform other types of actions
# If you have never installed these packages, you should do so now
install.packages('tidyverse')
install.packages('magrittr')
install.packages('janitor')

# you only have to install a package once but you have to load it before you can use it
library(tidyverse)
library(magrittr)
library(janitor)

# we're loading iris into a new dataframe called x.iris...think of <- as a "Save As"
# whenever i'm manipulating data i like to "Save As" so I have the original data preserved
x.iris <- iris

# So I can use the pipe %>% to apply tidy verbs (filter, select, mutate, etc) 
# For example, maybe i just want to see how many of each species there are.

# the base R way
table(iris$Species)

# the tidy R way 
iris %>%                           # reference the data frame
  group_by(Species) %>%            # apply a group_by to tell summarize what to count
  summarize(count = n())           # summarize, and name the new column "count"

# Now...I didn't actually create any data above, just some console results. If I wanted to create a dataframe,
# I would use the assignment operator <- just like in base R. Now here is where the advantage of tidy really starts 
# to show. In order to make this a dataframe i only have to slightly modify the code...the base R way is a bit less
# intuitive.

species.table <-           # this names and assigns the new object
  iris %>%                        
  group_by(Species) %>%            
  summarize(count = n())           

# the double pipe %<>% is a shortcut used when you want to overwrite the existing dataset
# so say i want to create a flag for some reason if species is virginica, but i want that flag in the existing 
# dataframe, not a new one.

x.iris %<>%
  mutate(species_flag = if_else(Species == 'virginica', 1, 0))

# the double pipe is the same thing as using the assignment operator and a single pipe, like this
# let's say i want to remove the flag in my existing dataset
x.iris <-
  x.iris %>%
  select(-species_flag)

# Let's suppose we have another table with flower colors
# i just got these from a quick google search...just making something up so i can do a join later
x.iris_colors <-
  tribble(
    ~Species, ~"flower color",
    "setosa", "deep violet blue",
    "versicolor", "deep blue to purple",
    "virginica", "light blue to deep violet"
  )

# but the great thing about tidy is i can chain a whole bunch of verbs together to do a lot of data transformation
x.iris %<>%
  # rename a variable
  rename(sepal_length = Sepal.Length) %>%
  # changed my mind and want all of the variable names to be clean
  clean_names() %>%
  # create attributes
  mutate(sepal_lw_ratio = sepal_length/sepal_width) %>%
  # join the other data
  inner_join(
    x.iris_colors %>%
      # i can't join because Species and species are mismatch, also 'flower color' has a space which is problematic
      clean_names() %>%
      # also species is character and i want it to be a factor
      mutate(species = as.factor(species)),
    by = 'species'
  ) %>%
  # maybe i want to create some indices...how does a particular flower compare to the median for the species?
  # so first i want to create some species level aggregates and add those
  inner_join(x.iris %>%
               clean_names() %>%
               group_by(species) %>%
               summarize(sepal_lw_ratio_species_med = median(sepal_length/sepal_width)) %>%
               ungroup(),
             by = 'species') %>%
  # now i can use those medians in a new metric
  mutate(sepal_lw_ratio_index = sepal_lw_ratio/sepal_lw_ratio_species_med) %>%
  # for some reason I'm actually not interested in virginica anymore
  filter(species != 'virginica') %>%
  # i only want a few variables and want to choose order
  select(species, flower_color, sepal_lw_ratio_index) %>%
  # maybe i want to sort the rows
  arrange(species, sepal_lw_ratio_index)

# Can also use this to prep data for a visualization before plotting, without creating a new df
iris %>%
  filter(Species != "virginica") %>%
  clean_names() %>%
  mutate(sepal_lw_ratio = sepal_length/sepal_width) %>%
  ggplot(aes(x = sepal_lw_ratio, fill = species)) +
  geom_histogram(color = "#e9ecef", alpha = 0.6, position = 'identity') +
  scale_fill_manual(values = c("#69b3a2", "#404080")) +
  theme_bw() +
  labs(fill = "")