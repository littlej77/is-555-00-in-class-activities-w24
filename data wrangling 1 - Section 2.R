library(tidyverse)

# Because `starwars` is a "hidden" dataset in memory for demonstration, it won't show up in our environment at 
# first, and it'll also be hard to reset it if we make a mistake. So assign it to `df` to make sure you can work
# with it.
df <- starwars

# glimpse turns a data frame on its side. Super useful.
df %>%
  glimpse()
  


# iteratively add operations: 
# height > 100, sex == female, 
# choose name, height, mass, species, films, 
# mass > 50, 
# arrange by mass
# note: filtering on some logical excludes NAs
#i saved the results as tall_females (which is now on the right panel)
tall_females <- df %>%
  filter(height > 100,
         sex == 'female') %>% 
  #above is the same as doing two lines like this
  # filter(height >100) %>% 
  # filter(sex == 'female')
  
  select(name, height, mass, species, films) %>% 
  filter(mass>50) %>% 
  arrange(mass)
#this arrange() is sorting it in ascending order. 

  
# calculate a new column,weight_lbs = mass * 2.204623
# Make sure it gets saved to the tibble...
df <- df %>% 
  mutate(weight_lbs = mass*2.204623) 
  #select(mass, weight_lbs) use this to double check your work or can use glimpse()

# group and summarize. Pay attention to NAs
# get a count and mean mass by species
df %>% 
  group_by(species) %>% 
  summarise(species_count = n(),
            avg_mass = mean(mass, na.rm = TRUE))
#n() counts the rows. so there are 6 droids and 3 gungans in our dataset
# Lots of NAs for average... why? Even for large groups it's NA...
# bc if there is a null then it will give NA.. if some are missing then it will show NA. so we had to explictly tell R to ignore those NAs

# Humans definitely have SOME mass data...

# so let's exclude NAs from the mean aggregation:



# why does Chagrian have a weird NaN?


# top 5 tallest overall - using slice_head
df %>%
  arrange(desc(height)) %>% 
  slice_head(n=5)
  #to remove nulls, you would do filter(!is.na(height)) after the arrange() function.
  #to get the top three tallest we need to do arrange(desc(height)) and then do slice_head(n=3)
  #slice_head(n=10)  a way to take the top (n) of whatever order the tibble is in. not ordered, just whatever the dataframe order is.
#slice(1:10) would give rows 1 -10. returns a sliced up tibble. 


# or just using slice_max
df %>% 
  slice_max(height, n=5)

# what is the shortest character for each species? 
df %>% 
  slice_min(height, n=5)

#what to do when you get a tie? 
df %>% 
  slice_min(eye_color, n=1)
#the black eye color has many so there are 10+ ties. so we have to exclude them or include them. be defensive. 

# ALso summarize isn't always the goal what if we want a relative 
# comparison of height for each species? I.e., are you tall for a human?
# calculate tallest for each species without summarizing, convert to percentile, show a few of the columns, sort

df %>% 
  #slice_sample(n=10)
  slice_sample(prop = .1)
#fast way to view the data if there is a ton. selects random lines from the dataset. prop is proportion so .1 is 10%


#group slicing
#this will give me the tallest character from each species. there are 38 rows returned and we have 38 species so there are no ties. 
df %>% 
  group_by(species) %>% 
  slice_max(height)

#by default that with_ties parameter for slice_min is set to true. if we really just want one, we can set that parameter to false.
#here is an example where there is a tie and saying "don't return a tie"
df %>% 
  group_by(species) %>% 
  slice_min(height, with_ties=F)

#this returns the ties. 
df %>% 
  group_by(species) %>% 
  slice_min(height)

#7 characters (from different species) have 183 height. 
df %>% 
  count(height, sort=1)

# Grouping by multiple
# Was is the average birth year for each gender from each homeworld
df %>% 
  group_by(homeworld,gender) %>% 
  summarize(count=n(),
            avg_birth_year = mean(birth_year, na.rm=T))


df %>% 
  group_by(species) %>% 
  slice_min(mass, n=1, with_ties = T) %>% 
  group_by(sex) %>% 
  summarize(avg_height= mean(height, na.rm=T))

