library(tidyverse)
library(tidymodels)




cars <- read_csv('https://www.dropbox.com/scl/fi/xavej23qpauvx3xfdq7zh/car_sales.csv?rlkey=4mfp6tpia0uqkcoiqf9jleau3&dl=1')

cars %>% glimpse

set.seed(42)
cars_split <- initial_split(cars, strata = sellingprice_log)
cars_training <- cars_split %>% training()
cars_testing <- cars_split %>% testing()

# Let's briefly revisit this recipe and the various steps:
#category handeling
cars_rec <- recipe(sellingprice_log ~.,
                   data = cars_training) %>% 
  step_impute_median(all_numeric_predictors()) %>% #    -replace all the missing data with the median
  step_YeoJohnson(all_numeric_predictors()) %>% # try to make it less skewed (log, boxcox, and yeojohnson(easiest) all accomplish the same idea)   -throw at all your nominals... easy to use.
  step_normalize(all_numeric_predictors()) %>% #    - normalizes all numeric predictors
  step_novel(make, model) %>% #       - assign a previously unseen factor level to "new"...for a category that we know is messy, use this to account for it.
  step_unknown(make, model) %>% #   - take anything that is NA and mark it as unknown.. basically imputation for categories.
  step_other(make, threshold = 0.03) %>% #    - look at the make column and keep anything that is in the top 97% of the category counts, but the bottom 3% are then lumped together into a category called "other". useful for data with long tails in categorical data
  step_other(model, threshold = 0.01) %>% 
  step_dummy(all_nominal_predictors())  #    - dummy codes all categories

# cars_rec %>% prep() %>% juice() %>% glimpse()
#cars_rec %>%  prep() %>% bake(new_data = cars_testing)



# Now we'll make a workflow:
xgb_spec <- boost_tree() %>% 
  set_mode('regression')

cars_wkfl <- workflow() %>% 
  add_model(xgb_spec) %>% 
  add_recipe(cars_rec)


# And we'll use dedicated functions to get a crossvalidation object and
# perform the fit

set.seed(42)
cars_folds <- vfold_cv(data = cars_training, #vfold_cv is designed to work the same way ast the initial_split()
                       #v=10,
                       strata = sellingprice_log)
                       #repeats = 3)

cars_cv_fit <- cars_wkfl %>% #give the workflow to the fit_resamples and then give it the folds object.
  fit_resamples(resamples = cars_folds)



cars_cv_fit %>% collect_metrics()
#gives mean and std_err bc the goal of our cross validation is to see consistency accross the 10 folds that we did. we want to see relatively small variation in the means of the folds. 



# Final Example - Zillions of Features ------------------------------------


claims_raw <- read_csv('https://www.dropbox.com/scl/fi/yak22stqfsq3aaz4qvxn1/claims.csv?rlkey=hj42vra7wpi6odnqvmrgxb797&dl=1')

# A handful of numeric features:
claims_raw %>% 
  select(where(is.numeric)) %>% 
  pivot_longer(everything()) %>% 
  ggplot(aes(x = value, fill = name)) +
  geom_density(alpha = .4) +
  facet_wrap(~name, scales = 'free')

# But over 100 categoricals:
claims_raw %>% 
  select(where(is.character)) %>% glimpse()

# Note the highly skewed outcome (claim loss amount):
claims_raw %>%
  ggplot(aes(x = loss)) +
  geom_histogram(alpha = .4, fill = 'blue') +
  theme_bw() +
  labs(title = 'Distribution of Claim Loss Amount (Outcome)')

# It's recommended to handle any transformations of the outcome BEFORE entering
# the model flow. (In case you're curious, here's a brief explanation: 
# https://stackoverflow.com/questions/75762005/error-in-step-log-when-trying-to-make-predictions-with-my-model)
claims <- claims_raw %>% 
  mutate(log_loss = log(loss)) %>% 
  select(-loss)

# Model setup
set.seed(42)
claims_split <- initial_split(claims, strata = log_loss)

claims_training <- claims_split %>% training()
claims_testing <- claims_split %>% testing()

# Create a recipe that applies best-practice pre-processing operations:
claims_rec <- recipe(log_loss ~.,
                     data = claims_training) %>% 
  step_YeoJohnson(all_numeric_predictors()) %>% 
  step_normalize(all_numeric_predictors()) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_nzv(all_predictors())

claims_rec %>% prep() %>% juice() %>% glimpse()


# And setup and train a model:
linreg_spec <- linear_reg()

claims_wkfl <- workflow() %>% 
  add_recipe(claims_rec) %>% 
  add_model(linreg_spec)


#Lastly, cross-validation!