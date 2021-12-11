if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")

library(tidyverse)

# ---------------------------------------------------------------------------------------------------------------
#  STEP 1: Initialize variables
# ---------------------------------------------------------------------------------------------------------------

# The number of Monte Carlo replication
B <- 10000

# The number of drawing
n <- 1000

# The maximum integer value of the range
max_value <- 100


# ---------------------------------------------------------------------------------------------------------------
#  STEP 2: Strategy 1 - Random Drawing
# ---------------------------------------------------------------------------------------------------------------

set.seed(1)
S <- replicate(B, {
  stones <- sample(1:max_value, n, replace=TRUE)
  draw <- sample(stones, 1)
})

mean(S)

acc_results <- tibble("Method" = "Random Drawing",
                      "Comparator Size" = "N.A.",
                      Mean = mean(S))
acc_results %>% knitr::kable()


# ---------------------------------------------------------------------------------------------------------------
#  STEP 3: Strategy 2 - Sample with the first 10/20/.../90% of stones, compare to average
# ---------------------------------------------------------------------------------------------------------------

# Array of sample size to be collected for comparison = 10,20,...,90%
# If number of stones = 1000, then the array will be [100,200,...,900]
size_array <- seq(from = (10*n/100), to = (90*n/100), by = n/10)

for (my_size in size_array) {
  set.seed(1)
  S <- replicate(B, {
    stones <- sample(1:max_value, n, replace=TRUE)
    last_stone <- stones[n]
    
    # Take the first x stones and get the mean
    # x = the currency my_size
    my_sample <- stones[1:my_size]
    comparator <- mean(my_sample)
    
    # From x+1 to n, compare to the comparator, if it is larger, pick.
    pick <- -1;
    s_index <- my_size + 1
    for (i in s_index:n) {
      if (stones[i] >= comparator) {
        pick <- stones[i]
        break;
      }
    }
    
    if (pick < 0) {
      pick <- last_stone
    }
    
    pick
  })

  mean(S)
  
  acc_results <- bind_rows(acc_results, tibble("Method" = "Compare to Average",
                                               "Comparator Size" = as.character(my_size),
                                               Mean = mean(S)))
}

acc_results %>% knitr::kable()


# ---------------------------------------------------------------------------------------------------------------
#  STEP 4: Strategy 3 - Sample with the first 10/20/.../90% of stones, compare to 75th percentile
# ---------------------------------------------------------------------------------------------------------------

# Array of sample size to be collected for comparison = 10,20,...,90%
size_array <- seq(from = (10*n/100), to = (90*n/100), by = n/10)

for (my_size in size_array) {
  set.seed(1)
  S <- replicate(B, {
    stones <- sample(1:max_value, n, replace=TRUE)
    last_stone <- stones[n]
    
    # Take the first x stones and get the mean
    # x = the currency my_size
    my_sample <- stones[1:my_size]
    comparator <- quantile(my_sample, probs = 0.75)
    
    # From x+1 to n, compare to the comparator, if it is larger, pick.
    pick <- -1;
    s_index <- my_size + 1
    for (i in s_index:n) {
      if (stones[i] >= comparator) {
        pick <- stones[i]
        break;
      }
    }
    
    if (pick < 0) {
      pick <- last_stone
    }
    
    pick
  })
  
  mean(S)
  
  acc_results <- bind_rows(acc_results, tibble("Method" = "Compare to 75th percentile",
                                               "Comparator Size" = as.character(my_size),
                                               Mean = mean(S)))
}

acc_results %>% knitr::kable()


# ---------------------------------------------------------------------------------------------------------------
#  STEP 5: Strategy 4 - Sample with the first 10/20/.../90% of stones, compare to 90th percentile
# ---------------------------------------------------------------------------------------------------------------

# Array of sample size to be collected for comparison = 10,20,...,90%
size_array <- seq(from = (10*n/100), to = (90*n/100), by = n/10)

for (my_size in size_array) {
  set.seed(1)
  S <- replicate(B, {
    stones <- sample(1:max_value, n, replace=TRUE)
    last_stone <- stones[n]
    
    # Take the first x stones and get the mean
    # x = the currency my_size
    my_sample <- stones[1:my_size]
    comparator <- quantile(my_sample, probs = 0.9)
    
    # From x+1 to n, compare to the comparator, if it is larger, pick.
    pick <- -1;
    s_index <- my_size + 1
    for (i in s_index:n) {
      if (stones[i] >= comparator) {
        pick <- stones[i]
        break;
      }
    }
    
    if (pick < 0) {
      pick <- last_stone
    }
    
    pick
  })
  
  mean(S)
  
  acc_results <- bind_rows(acc_results, tibble("Method" = "Compare to 90th percentile",
                                               "Comparator Size" = as.character(my_size),
                                               Mean = mean(S)))
}

acc_results %>% knitr::kable()
