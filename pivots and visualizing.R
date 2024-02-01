bob <- read_csv('https://www.dropbox.com/s/mozqpceit51hia7/bob_ross.csv!dl=1')
#data starts in wide format

bob_long <- bob %>% 
  pivot_longer(
    cols = !1:4, #choosing by column index
    names_to = 'object',
    values_to = 'is_present'
  )

#which objects occur most frequently?
bob_long %>% 
  group_by(object) %>% 
  summarize(object_count = sum(is_present == 1)) %>% #summing the times where is_present is 1
  arrange(desc(object_count))

#what was the season when Bob painted the most mountains?
#we need object to me mountain and is_present to be 1
bob_long %>% 
  filter(str_detect(object, 'mountain')) %>% 
  group_by(season) %>% 
  summarize(mtn_count = sum(is_present == 1))

#other option
bob_long %>% 
  group_by(season) %>% 
  summarize(mtn_count = sum(is_present == 1 & str_detect(object, 'mountain'))) #counts the times that both conditions are true


#create a table that displays one line per attribute with a count of times that
#object was used in each season (one column per season)
bob_long %>% 
  select(season, object, is_present) %>% 
  pivot_wider(
    names_from = season,
    values_from = is_present
  ) #this gives you a warning and a really weird looking tibble
#each value is another vector of objects per episode per season
#he showed this just so we can know what this warning message means and how to fix it
bob_long %>% 
  select(season, object, is_present) %>% 
  group_by(season, object) %>% #this fixes that issue we were having
  summarize(count = sum(is_present == 1)) %>% #and this
  pivot_wider( #now you can pivot the same as above and it works
    names_from = season,
    values_from = count
  )



# Function Summary --------------------------------------------------------------------------------------

# JUST TWO FUNCTIONS:
# 
# I'm going to narrate the two functions with their parameters as a new way to help...?
# 
# Pay special attention to the parameters with the ** as they are the most important.
# 
# pivot_longer(
#   data,                             <-- The incoming tibble, usually comes in via a pipe
#   **cols,                           <-- This is where you say which columns you're pulling down to long format
#                                           (any that you don't name here are assumed to be "id columns" that 
#                                           will repeat with each of the variables being converted to long format)
#   **names_to = "name",              <-- This is the column name that will "house" the column names that you have 
#                                           specified in the `cols` parameter. Often you can just leave this as the 
#                                           default and it will name it "name", but you can also be more descriptive
#   names_prefix = NULL,              <-- You can (optionally) indicate a text-based prefix to auto-remove as the
#                                           function is pulling the columns down. So for columns like `year1`, `year2`,
#                                           you could specify "year" as the prefix and the function would remove it.
#   names_sep = NULL,                 <-- beyond our scope - explore this on your own if you want. 
#   names_pattern = NULL,             <-- beyond our scope - explore this on your own if you want.
#   names_ptypes = NULL,              <-- beyond our scope - explore this on your own if you want.
#   names_transform = NULL,           <-- beyond our scope - explore this on your own if you want.
#   names_repair = "check_unique",    <-- beyond our scope - explore this on your own if you want.
#   **values_to = "value",              <-- this is the column that will "house" the values corresponding to each of 
#                                           the column name labels in the `names_to` column that you specified above.
#   values_drop_na = FALSE,           <-- wider data is often "sparse", meaning that it has lots of nulls when not all
#                                           rows have a value for each wide column. It's usually efficient to filter 
#                                           those out when pivoting longer, so this is an option for convenience.
#   values_ptypes = NULL,             <-- beyond our scope - explore this on your own if you want.
#   values_transform = NULL,          <-- beyond our scope - explore this on your own if you want.
# )
# 
# 
# pivot_wider(                        
#   data,                             <-- The incoming tibble, usually comes in via a pipe
#   id_cols = NULL,                   <-- "id columns" are the ones that you DON'T want to spread out as wide
#                                           columns, but you can usually ignore this parameter because you will
#                                           be explicitly specifying the `names_from` and `values_from` parameters
#   id_expand = FALSE,                <-- beyond our scope - explore this on your own if you want.
#   **names_from = name,                <-- Here you specify the column that contains the value names or "labels"
#                                           (it defaults to `name` because that's the default from `pivot_longer()`)
#   names_prefix = "",                <-- beyond our scope - explore this on your own if you want.
#   names_sep = "_",                  <-- beyond our scope - explore this on your own if you want.
#   names_glue = NULL,                <-- beyond our scope - explore this on your own if you want.
#   names_sort = FALSE,               <-- beyond our scope - explore this on your own if you want.
#   names_vary = "fastest",           <-- beyond our scope - explore this on your own if you want.
#   names_expand = FALSE,             <-- beyond our scope - explore this on your own if you want.
#   names_repair = "check_unique",    <-- beyond our scope - explore this on your own if you want.
#   **values_from = value,              <-- Here you specify the column that contains the values corresponding to the
#                                           names specified in the `names_from` parameter.
#                                           (it defaults to `value` because that's the default from `pivot_longer()`)
#   values_fill = NULL,               <-- This option allows you to provide the "default value" for any rows in the
#                                           wider data that don't have a value in the longer format list. (Leaving
#                                           this out will result in NAs in the wider data, which is usually fine.)
#   values_fn = NULL,                 <-- beyond our scope - explore this on your own if you want.
#   unused_fn = NULL,                 <-- beyond our scope - explore this on your own if you want.
# )

# Sample Code -------------------------------------------------------------------------------------------

library(tidyverse)


drinks <- tribble(
  ~country,  ~soda, ~tea,  ~sparkling_water,
  'China',     79,    192,     8,
  'Italy',     85,     42,   237,
  'USA',      249,     58,    84
)

drinks



# Let's make it more tidy. pivot_longer
drinks %>% 
  pivot_longer(
    cols = c(soda, tea, sparkling_water),
    names_to = 'drink_type',
    values_to = 'liters_per_capita'
  ) 





# New data:
gap <- read_csv('https://www.dropbox.com/s/dv1a1ldkuyoftn2/gap_smaller.csv?dl=1')

# Start with just country, year, lifeExp, pivot wider
gap %>% 
  select(country, year, lifeExp) %>% 
  pivot_wider(
    names_from = year,
    values_from = lifeExp
  )




# Now let's go the other way: pivot all three measures longer
gap %>% 
  pivot_longer(
    cols = lifeExp:gdpPercap, # c(lifeExp, pop, gdpPercap)
    names_to = 'measure',
    values_to = 'value'
  )





# Pivot all three measures wider. Gets messy, but 
# sometimes data comes to you in this format
gap_wide <- gap %>% 
  pivot_wider(
    names_from = year,
    values_from = lifeExp:gdpPercap
  )

gap_wide %>% 
  pivot_longer(
    cols = lifeExp_1952:gdpPercap_2007,
    names_to = 'convert_me',
    values_to = 'value'
  ) %>% 
  separate_wider_delim(convert_me, delim = '_', names = c('measure','year'))




gap_wide %>% 
  pivot_longer(lifeExp_1952:gdpPercap_2007,
               names_sep = '_',
               names_to = c('year','name'))



# New data:
ri <- read_csv('https://www.dropbox.com/s/dfjgfytyek44u61/rel_inc.csv?dl=1')
bnames <- read_csv('https://www.dropbox.com/s/6bck5fy4aag76kw/baby_names.csv?dl=1')
bob <- read_csv('https://www.dropbox.com/s/mozqpceit51hia7/bob_ross.csv?dl=1')

# see if you can pivot the religious income data into a tidier format
# what is the most common income bracket for each religion?




# IMPORTANT: Notice how EASY it is to find the top income for each religion because
# of the tidying of the data we've done. It's a simple filter, rather than a 
# group_by(), which is what we used to have to do when the data was wider.

ri %>% 
  pivot_longer(
    cols = !religion,
    names_to = 'income_bracket',
    values_to = 'household_count'
  ) %>% 
  group_by(religion) %>% 
  slice_max(household_count, with_ties = F)



#VISUALIZATION
# TYPES OF GEOMETRIES ("GEOMs")
# 
# Single Variable
#   Distributions of continuous
#     geom_histogram() <- histogram (distribution of continuous variable)
#     geom_density() <- Generic curve of distribution, good for comparing things of different sizes (but can be misleading!)
#   Distributions of discrete
#     geom_bar() <- when used with just one categorical, makes a super basic bar chart (counts of discretes)
# 
# Two Variables
#   Continuous by categories
#     geom_bar() <- when used with both x and y, summarizes continuous (y) by category (x)
#     geom_violin() <- combines distribution and relative position of continuous by category
#   Continuous by Continuous
#     geom_point() <- scatter plot, standard "show the relationship between x and y" chart
#     geom_line() <- basic line graph. Most useful: show changes over time or across a cumulative population.  
# 
# MAPPINGS (AESTHETICS, defined inside the aes() )
#   These are ways of "mapping" variables to characteristics of your chart to columns in your dataset.
#     Aside from establishing the main (usually x,y) data driving your plot, additional aesthetic mappings
#     can be SUPER helpful in highlighting other relevant groupings or trends from other columns.
#     Example: You set x=height and y=weight for a scatter plot, but then "map" sex to define each point's color
# 
# Important aesthetics (mappings)
#   x, y <- Specify which columns should be used for x and y. At least one is required, usually two.
#   color, fill <- most useful for overlaying additional categories
#   alpha, shape, labels, size  <- Can be useful, but I don't use them nearly as much
#   
# ATTRIBUTES (static features of individual Geoms)
#   Don't confuse these with aesthetic mappings! The key difference is that aesthetics are "mapped" to columns
#     in the dataset such that different values in those columns create differences in the appearance of the
#     aesthetic. Attributes, on the other hand, are typically static aspects used to customize a given geom. 
#     Example: Income bracket is mapped to the color aesthetic such that lower incomes are dark blue and higher
#       incomes are light blue. But then size is provided as ATTRIBUTES of the geom_point, which simply sets the 
#       size of the scatter plot points (i.e., all points would be the same size).
# 
# General attributes useful across most geoms:
#   position <- defines how to arrange groups that overlap.
#       Most relevant: identity, stack, dodge
#   color <- defines the color of the elements produced by that geom
#   alpha <- defines the opacity (or "see-through-ness") of the elements produced by that geom
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
# 

library(tidyverse)

penguins <- read_csv('https://www.dropbox.com/s/65jmvgoed4i0r57/penguins.csv?dl=1')

penguins %>% 
  glimpse

# First: one-variable plots

# histogram with body_mass_g
# add species as fill color, pay attention to position
penguins %>% 
  ggplot(mapping = aes(x=body_mass_g, color=species, fill=species))+
  #mapping species to fill == will determine differences in the fill column based on how many categories are in the species column
  
  geom_histogram(alpha=.5, position = 'identity', bins = 15)+
  #geometry 
  theme_bw()+
  #ggthemes::theme_clean()    you need to install ggthemes first tho
  labs(title = 'This is the title', x='Penguin Weight',y= 'Count') #short for labels
  

# density curves are sometimes nicer. Let's do the above but with 
# a density curve. Alpha is nice here.



# bar charts with categoricals: quick, easy counts summary
# stacked vs. side-by-side






# Second: two-variable plots (relationships)
# Numeric + categorical

# Violin plots are cool. Lets look at body mass across islands
# Notice what happens when we add color now


# Bar plots with both x and  y are a bit more flexible.
# summary will give us means






# Numeric + numeric

# The classic scatter
# Important: attributes vs aesthetics. Inheritance

penguins %>% 
  ggplot(aes(x=bill_length_mm, y= bill_depth_mm, color=species, shape= sex, size=body_mass_g))+
  #fill is how you change anything with area, color is for dots
  geom_point()+
  theme_minimal()
  #geom_point(size=2) to change all the sizes of the points
  #theme_bw()+
  #labs(title = 'This is the title', x='flipper length',y= 'Count') #short for labels


# More inheritance general vs. local aesthetics: if we have time
# flipper vs. body mass generally, species mapped to color, then add a smoother


# versus: mapping the color specific to the scatter points only
