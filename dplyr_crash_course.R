# Tidyverse crash course
library(tidyverse)

# Variable types:
#   1. character (string)
#   2. integer
#   3. numeric (decimal)
#   4. logical (TRUE, FALSE)
#   5. complex (3i)
#   6. raw (raw bytes, printed as hex digits)

char_vec <- c('hello', 'bonjour', 'guten tag')
int_vec <- c(1L, 2L, 3L)
numeric_vec <- c(2.3, 1/4, -7.888)
logical_vec <- c(TRUE, FALSE, TRUE)
complex_vec <- c(3i, 0.5i+2)


# Data structures in R
# 1. Atomic vector (all elements are of same type)
# 2. Matrix (2d rectangular data set)
# 3. Array (multidimensional matrices)
# 4. List (generic vector that can contain different types of objects)
# 5. Data frame (tabular data, made up of vectors of equal length)

dframe <- data.frame(a = c(1, 2, 3), b = c('a', 'b', 'c'), row.names = c("blah1", "blah2", "blah3"))

# Data manipulation in the Tidyverse

# Dplyr verbs
#   1. select()
#   2. filter()
#   3. arrange()
#   4. mutate()
#   5. summarise()
#   6. group_by() - not a verb, but very important

# Calculate the power-to-weight ratio (horsepower/weight) of 8-cylinder cars with displacement greater than 300cc
# and arrange in descending order.

cars <- arrange(mutate(filter(rownames_to_column(mtcars), cyl == 8, disp > 300), power_to_weight = hp/wt), desc(power_to_weight))

# Or...

cars = mtcars
cars <- rownames_to_column(cars)
cars <- filter(cars, cyl == 8, disp > 300)
cars <- mutate(cars, power_to_weight = hp/wt)
cars <- arrange(cars, desc(power_to_weight))
cars <- select(cars, car = rowname, power_to_weight)
cars_avg <- summarise(cars, mean_power_to_weight = mean(power_to_weight))

# Even better...

# Ceci n'est pas une pipe (%>%) Adolfo Álvarez & Stefan Bache (magrittr)
# f(g(x)) > f◦g
# Pipelines make reading code easier
# Read from left to right (instead of inside out)
# No need to redefine the object on every line (which obfuscates what operation is being performed)
# Easy to insert steps
# Makes debugging easier (no need to re-run all the steps)

cars2 = mtcars %>%
    rownames_to_column(var = 'car') %>%
    filter(cyl == 8, disp > 300) %>%
    mutate(power_to_weight = hp/wt) %>%
    arrange(desc(power_to_weight)) %>%
    select(car, power_to_weight) %>%
    summarise(mean_power_to_weight = mean(power_to_weight))

# Principles of tidy data:
#   1. Each variable must have its own column.
#   2. Each observation must have its own row.
#   3. Each value must have its own cell.


messy_data <- read_csv('messy_data.csv')

tidy_data <- read_csv('tidy_data.csv')

messy_to_tidy <- spread(messy_data, key = var_type, value = value)


# Case study: Clean up a messy customer table

# Read customer table and clean data
dat <- read_csv('example_dat.csv')

replacement_values <- dat %>%
    group_by(id) %>%
    replace_na(list(email_address = "NULL", name = "NULL", phone_number = "NULL", physical_address = "NULL")) %>%
    gather(key = var_name,
           value = var_value,
           email_address:phone_number) %>%
    group_by(id, var_name, var_value) %>%
    summarise(n = n()) %>%
    group_by(id, var_name) %>%
    summarise(first_value = first(var_value, order_by = desc(n)),
              second_value = nth(var_value, 2, order_by = desc(n)),
              replacement_value = case_when(
                  first_value != 'NULL' ~ first_value,
                  !is.na(second_value) ~ second_value,
                  TRUE ~ 'NULL')) %>%
    group_by(id) %>%
    select(id, var_name, replacement_value) %>%
    spread(var_name, replacement_value)


dat <- dat %>%
    rename_at(.vars = vars(-matches('^id$')),
              .funs = funs(paste0(., '_tmp'))) %>%
    left_join(replacement_values, by = 'id') %>%
    select(-contains('_tmp'))



# Defining functions

library(rlang)

# Define function to homogenize variables (populate column with the most frequently occurring value for each ID)
homogenize_vars <- function(df, id_var, ...){
    
    # capture name of ID variable as a quosure
    id_var <- enquo(id_var)
    
    # convert ID quosure to a string
    id_var_name <- quo_name(id_var)
    
    # capture names of variables to be homogenized (as a quosure)
    ns_vars <- quos(...)
    
    # convert quosure containing variable names to strings
    ns_var_names <- flatten_chr(map(ns_vars, quo_name))
    
    # create a character vector of all column names input by the user
    all_var_names <- c(id_var_name, ns_var_names)
    
    
    replacement_values <- df %>%
        # select only variables that were provided as user inputs
        select_at(.vars = vars(one_of(all_var_names))) %>%
        # group by ID variable (!! to unquote)
        group_by(!!id_var) %>%
        # for the variables to be homogenized, replace NA values with '0'
        mutate_at(.vars = vars(one_of(ns_var_names)),
                  .funs = funs(ifelse(is.na(.) | . == '', '0', .))) %>%
        # gather variable and values into name/value pairs
        gather(var_name, var_value, ns_var_names) %>%
        group_by(!!id_var, var_name, var_value) %>%
        # count the number of occurences for each value by ID and variable name
        summarise(n = n()) %>%
        group_by(!!id_var, var_name) %>%
        # select most frequently occuring value for each ID/variable pair (NA's appear as '0')
        summarise(row_count = n(),
                  first_value = first(var_value, order_by = desc(n)),
                  second_value = nth(var_value, 2, order_by = desc(n)),
                  replacement_value = case_when(
                      first_value != '0' ~ first_value,
                      !is.na(second_value) ~ second_value,
                      TRUE ~ '0')) %>%
        group_by(!!id_var) %>%
        select(!!id_var, var_name, replacement_value) %>%
        spread(var_name, replacement_value)
    
    
    df <- df %>%
        # rename variables to be homogenized in original table
        rename_at(.vars = vars(one_of(ns_var_names)),
                  .funs = funs(paste0(., '_tmp'))) %>%
        # join to data frame containing new values
        left_join(replacement_values, by = id_var_name) %>%
        # drop original, non-homogenized, variables
        select(-contains('_tmp'))
}


clean_dat <- homogenize_vars(dat, id, email_address, name, physical_address, phone_number)

