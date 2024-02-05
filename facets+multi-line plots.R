# FACETS
#   Facets allow you to use categories within your data to line up similar
#   charts next to each other, which can be a really useful way to highlight
#   differences between groups.
# 
# The two main facet functions are:
#   facet_wrap() <- usually best used to show a series of plots across a 
#                  single category variable, with you specify with `~varable`.
#                  This will show one plot per level of the category.
#   facet_grid() <- My favorite to show a "grid" of facets, with rows and 
#                  columns defined by two different variables. This is specified
#                  using the facet formula `var1~var2`
#       Note: Pay attention to the `scales` parameter in these facet functions, 
#             where you can specify whether you want to hold the x and/or y
#             axis scales constant across all of the facets.
library(tidyverse)


# Facets and multi-lines --------------------------------------------------------------------------------
tips <- read_csv('https://www.dropbox.com/s/rydxlxdarjdoj7a/tips.csv?dl=1')

# Let's plot tip_percentage vs. total_bill,
# then split that across lots of categories

tips %>% 
  mutate(tip_perc = tip/total_bill)%>%
  ggplot(aes(x=total_bill,y=tip_perc, color=as.factor(sex), alpha=sex, size=day))+ #to make it treat size as a cateogry do this as.factor() or as.character()
  geom_point(size= 3)+
  facet_wrap(~smoker) #facet_grid is better for doing multiple variables. can define row by nrow=3, ncol=3



econ <- read_csv('https://www.dropbox.com/s/8bq9rw0rk46hru2/econ.csv?dl=1')

# Let's plot two measures over time: savings rate & unemployment weeks
# It's easiest if we pivot to make this work

econ %>% 
  select(date, savings_rate, unempl_weeks) %>% 
  filter(date<mdy('01-01-1975')) %>% 
  pivot_longer(!date, names_to = "Measure", values_to = 'Rate') %>% 
  ggplot(aes(x=date, y=Rate, color=Measure)) +
  geom_line(size=1.5)+
  facet_wrap(~Measure, ncol=1)


#in class
library(tidyverse)


movies <- read_csv('https://www.dropbox.com/scl/fi/pi7nexxuoqnvviwfzwun9/movie_ratings.csv?rlkey=x419gluseq6p8e8xzu12ndfc9&dl=1')

# Scatter of RT critist vs RT Users. Or vs imdb. Or vs metacritic...
# add some facets...
movies %>% 
  ggplot(aes(x=rating, y = imdb, color= rating, fill =rating))+
  geom_col()

movies %>% 
  pivot_longer(!rating, names_to = 'source',
               values_to = 'percentage') %>%  #pivot everything other than rating. need to be pivoted to long formart b4 i can use facets
  filter(!is.na(percentage)) %>% #filters the NA out
  ggplot(aes(x=rating, y= percentage, color=rating, fill=rating))+ #bc we pivoted and they are the same scale (0-1) 
  geom_col(position = 'dodge')+ #if i want them side by side and not stacked use 'dodge'
  facet_wrap(~source)
  
# But how do we compare ratings from different ratings sites? 
# Or ratings from critics vs. users?

movies %>% 
  pivot_longer(!rating, names_to = 'source',
               values_to = 'percentage') %>%  #pivot everything other than rating. need to be pivoted to long formart b4 i can use facets
  filter(!is.na(percentage)) %>% #filters the NA out
  mutate(review_type = if_else(str_detect(source, 'user'), 'Audience','Critic')) %>% 
  ggplot(aes(x=rating, y= percentage, color=rating, fill=rating))+ #bc we pivoted and they are the same scale (0-1) 
  geom_col(position = 'dodge')+ #if i want them side by side and not stacked use 'dodge'
  facet_grid(review_type~source)




steak <- read_csv('https://www.dropbox.com/scl/fi/mzg5oxenh9oonbwpwgxzm/steak_data.csv?rlkey=2gbf1kfqfkln0zf2alwo32nza&dl=1')


# Here's a dumb chart:
steak %>% 
  ggplot(aes(x = steak_prep, fill = educ)) +
  geom_bar() +
  # facet_grid(educ ~ hhold_income) +
  labs(title = "Steak Preparation Preference by Education Level",
       x = "Steak Preparation",
       y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# What else can you find? What other variables are in the dataset that might 
# shed some light on steak-eaters' preferences?

