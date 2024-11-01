Assignment B-1: Making a function
================

## Exercise 1 & 2: Make a Function (25 & 20 points)

### `check_outliers()`

I’m going to create a function that checks a numeric vector for any
outlier values. An outlier is typically considered to be more than 3
standard deviations from the mean, or 1.5 times greater than the
interquartile range. Instead of having to plot a box plot or checking
manually, this function will automatically return all the outlier values
in a vector. There are options to choose between methods, and set custom
thresholds or quantile calculation algorithm if needed.

``` r
# Documenting the function ------------------------------------------------------------------------------------------------------------

#' Outlier Detection in Numeric Vectors
#'
#' This function checks for outlier values in a numeric vector based on either the Z-score method or the Interquartile Range (IQR) method. It returns a numeric vector containing the detected outliers, or an empty numeric vector with a message if no outliers were found or an error occurred. It provides flexibility in defining thresholds for outliers and the type of algorithm for calculating quartiles.
#'
#' @param x A numeric vector. This is the primary input for which outliers will be calculated. NA values are automatically omitted within the function to ensure accurate calculations. Named "x" because it is the standard name for a variable in statistics.
#' @param method A string specifying the method for detecting outliers. Options are "zscore" for Z-score method or "iqr" for IQR method. Inputted values are case insensitive. Named "method" to clearly indicate the desired approach to determine outliers.
#' @param zscore_threshold Numeric value equal to or greater than 1. The Z-score threshold (i.e number of standard deviations) above which values are considered outliers. Default is 3. Named "zscore_threshold" to specify the cut off for identifying outliers based on the Z-score method.
#' @param iqr_threshold Numeric value greater than 0. The multiplier for the IQR to define the bounds for detecting outliers. Default is 1.5. Named "iqr_threshold" to specify the multiplier for identifying outliers based on the IQR method. 
#' @param quantile_type Numeric integer (1-9). The algorithm used to compute quantiles when calculating the IQR. Default is 7. Please see quantile() documentation from the stats package for details. Named "quantile_type" to refer to the specific quantile algorithm chosen for the quantile() function based on input.
#'
#' @return A numeric vector of outlier values. If no outliers are found, it returns a message "No outliers found" and an empty numeric vector.
#'


# Function code  ---------------------------------------------------------------------------------------------------------------------

check_outliers <- function(x, method, zscore_threshold = 3, iqr_threshold = 1.5, quantile_type = 7) {
  
  # Remove all NA values. We are only calculating and returning outliers, and including NAs would disrupt our mean and SD calculations
  x <- na.omit(x)
  
  # Check if the input x is a numeric vector greater than length 0
  if (!is.numeric(x) || length(x) == 0) {
    stop('This function only works for a numeric vector greater than length 0, please provide the correct class and/or length')
    }


  # Z-score method: calculates z-score using the standard score formula (x-mean/SD) and indexes the vector for outliers outside the threshold
  if (tolower(method) == 'zscore') {
    
    # Validate the z-score threshold
    if (!is.numeric(zscore_threshold) || zscore_threshold < 1) {
      stop("The z-score threshold must be a numeric value at least 1 SD or greater.")
    }
    
    # Calculate mean and SD
    mean_x <- mean(x)
    sd_x <- sd(x)
    
    # Before continuing, check for SD = 0 or NA because then we cannot do the calculations below
    if (sd_x == 0 || is.na(sd_x)) {
      message("Standard deviation is zero or NA; no outliers can be determined using this method.")
      return(numeric(0))  # Return an empty vector to keep output consistent
    }
    
    zscores <- (x - mean_x)/sd_x # Calculate z-scores
    outliers <- x[abs(zscores) > zscore_threshold] # only include values above the threshold
  }
  
  # IQR method: calculates IQR using the inputting quantile algorithm and indexes the vector for outliers outside the threshold
  else if (tolower(method) == 'iqr') {
    
    # Validate the IQR threshold
    if (!is.numeric(iqr_threshold) || iqr_threshold <= 0) {
      stop("The IQR threshold must be a numeric value greater than or equal to 0.")
    }
    
    # Validate the quantile algorithm type
    if (!quantile_type %in% 1:9) {
    stop("Invalid quantile algorithm type; must be an integer between 1 and 9.")
}
    
    # Calculate first and third quartiles, and IQR, using inputted algorithm type
    q1 <- quantile(x, prob = 0.25, type = quantile_type)
    q3 <- quantile(x, prob = 0.75, type = quantile_type)
    iqr_x <- q3 - q1
    
    # Calculate upper and lower bounds
    upper_bound <- q3 + (iqr_threshold * iqr_x)
    lower_bound <- q1 - (iqr_threshold * iqr_x)
    outliers <- x[x > upper_bound | x < lower_bound] # only include values outside these bounds
  }
  
  # If incorrect method input
  else{
    stop('The only supported methods are "zscore" or "iqr" [case insensitive]. Please check for typos and try again')
  }
  
  # Check if any outliers were found; print a message if not
   if (length(outliers) == 0) {
    message("No outliers found.")
     return(numeric(0))
  }
  
  # Return the outliers in a numeric vector
  return(outliers)
}
```

## Exercise 3: Include examples (15 points)

For these examples, we are going to create numeric vectors and test the
function on existing datasets.

### Example 1 - Simple vector with outliers for both methods

To start off, I’ll demonstrate how this function can detect clear
outliers using both methods:

``` r
test_1 <- c(1:20, -50000)

check_outliers(test_1, "iqr")
```

    ## [1] -50000

``` r
check_outliers(test_1, "zscore")
```

    ## [1] -50000

### Example 2 - Simple vector with no outliers

In this example, no values qualify as outliers, demonstrating how the
function returns an empty vector and message when none are present:

``` r
test_2 <-c(1:10)

check_outliers(test_2, "iqr")
```

    ## No outliers found.

    ## numeric(0)

``` r
check_outliers(test_2, "zscore")
```

    ## No outliers found.

    ## numeric(0)

### Example 3 - Simple vector with outliers for one method, not the other

Although both methods detected the outlier in example 1, there are
different underlying calculations between them, sometimes leading to
cases where one method may identify values differently than the other.
In this example, two apparent outliers are only identified by the IQR
method because of the way it is calculated:

``` r
test_3 <- c(0:10, -300, 500)

check_outliers(test_3, "iqr")
```

    ## [1] -300  500

``` r
# However, if that same vector is checked using the Z-score method...
check_outliers(test_3, "zscore")
```

    ## No outliers found.

    ## numeric(0)

Although they seem like obvious outliers, if you check the calculations
both values are \<3 SD from the mean and therefore not outliers.

### Example 4 - Adjusting the threshold values

Let’s try lowering the Z-score threshold from 3 to 1.5 SDs and see how
that changes the sensitivity for detection:

``` r
check_outliers(test_3, "zscore", zscore_threshold = 1.5)
```

    ## [1] -300  500

The function now returns a vector with two values.

### Example 5 - Checking a vector with NA values

Next, let’s try a vector with missing (NA) values. As mentioned in the
documentation, the function should automatically omit NA values from the
calculation, ensuring it does not interfere with the underlying mean, SD
or quantile functions.

``` r
test_4 <- c(1:10, NA, 10000)
check_outliers(test_4, "iqr")
```

    ## [1] 10000

``` r
check_outliers(test_4, "zscore")
```

    ## [1] 10000

Inclusion of the NA value in the inputted vector did not disrupt the
output.

### Example 6 - Large vector with outliers

So far all the examples have been for smaller vectors, but how does it
perform with more realistic, larger data? Lets create a vector with
multiple clear outliers and see if they are detected (along with the
ones generated in the vector):

``` r
test_5 <- rnorm(2000, mean = 40, sd = 15)
test_5 <- c(test_5, -800, 1200)

check_outliers(test_5, "iqr")
```

    ##  [1]    0.1662284   -0.3164230   83.9519510   -0.4635048   83.4945395
    ##  [6]  -11.5706581   -0.6986280   85.1138894   85.4260969   81.1763117
    ## [11]   -9.7049123   79.5180239 -800.0000000 1200.0000000

``` r
check_outliers(test_5, "zscore")
```

    ## [1] -800 1200

Both methods are able to detect the outliers, with the IQR method
detected additional ones.

### Example 7 - Real dataset

Finally, lets test how the function works in a real dataset. I have
chosen the `gapminder` dataset for this example:

``` r
library(gapminder)
head(gapminder)
```

    ## # A tibble: 6 × 6
    ##   country     continent  year lifeExp      pop gdpPercap
    ##   <fct>       <fct>     <int>   <dbl>    <int>     <dbl>
    ## 1 Afghanistan Asia       1952    28.8  8425333      779.
    ## 2 Afghanistan Asia       1957    30.3  9240934      821.
    ## 3 Afghanistan Asia       1962    32.0 10267083      853.
    ## 4 Afghanistan Asia       1967    34.0 11537966      836.
    ## 5 Afghanistan Asia       1972    36.1 13079460      740.
    ## 6 Afghanistan Asia       1977    38.4 14880372      786.

``` r
# Lets check for outliers within the population variable, 'pop'

check_outliers(gapminder$pop, "iqr")
```

    ##   [1]   46886859   51365468   56839289   62821884   70759295   80428306
    ##   [7]   93074406  103764241  113704579  123315288  135656790  150448339
    ##  [13]   56602560   65551171   76039390   88049823  100840058  114313951
    ##  [19]  128962939  142938076  155975974  168546719  179914212  190010647
    ##  [25]  556263527  637408000  665770000  754550000  862030000  943455000
    ##  [31] 1000281000 1084035000 1164970000 1230075000 1280400000 1318683096
    ##  [37]   47798986   55379852   64606759   45681811   52799062   59402198
    ##  [43]   66134291   73312559   80264543   52088559   59861301   67946797
    ##  [49]   76511887   47124000   49569000   51732000   53165019   54433565
    ##  [55]   55630100   57374179   58623428   59925035   61083916   69145952
    ##  [61]   71019069   73739117   76368453   78717088   78160773   78335266
    ##  [67]   77718298   80597764   82011073   82350671   82400996  372000000
    ##  [73]  409000000  454000000  506000000  567000000  634000000  708000000
    ##  [79]  788000000  872000000  959000000 1034172547 1110396331   82052000
    ##  [85]   90124000   99028000  109343000  121282000  136725000  153343000
    ##  [91]  169276000  184816000  199278000  211060000  223547000   51889696
    ##  [97]   60397973   63327987   66907826   69453570   47666000   49182000
    ## [103]   50843200   52667100   54365564   56059245   56535636   56729703
    ## [109]   56840847   57479469   57926999   58147733   86459025   91563009
    ## [115]   95831757  100825279  107188273  113872473  118454974  122091325
    ## [121]  124329269  125956499  127065841  127467972   46173816   47969150
    ## [127]   49044790   47995559   55984294   63759976   71640904   80122492
    ## [133]   88111030   95895146  102479927  108700891   45598081   47761980
    ## [139]   47287752   53740085   62209173   73039376   81551520   93364244
    ## [145]  106207839  119901274  135031164   46679944   53100671   60641899
    ## [151]   69325921   78152686   91462088  105186881  120065004  135564834
    ## [157]  153403524  169270617   46850962   53456774   60017788   67185766
    ## [163]   75012988   82995088   91077287   48827160   52910342   56667095
    ## [169]   60216677   62806748   65068149   47328791   52881328   58179144
    ## [175]   63047647   67308928   71158647   50430000   51430000   53292000
    ## [181]   54959000   56079000   56179000   56339704   56981620   57866349
    ## [187]   58808266   59912431   60776238  157553000  171984000  186538000
    ## [193]  198712000  209896000  220239000  232187835  242803533  256894189
    ## [199]  272911760  287675526  301139947   50533506   56142181   62826491
    ## [205]   69940728   76048996   80908147   85262356

``` r
check_outliers(gapminder$pop, "zscore")
```

    ##  [1]  556263527  637408000  665770000  754550000  862030000  943455000
    ##  [7] 1000281000 1084035000 1164970000 1230075000 1280400000 1318683096
    ## [13]  372000000  409000000  454000000  506000000  567000000  634000000
    ## [19]  708000000  788000000  872000000  959000000 1034172547 1110396331

We get a vector of outliers for both methods, with IQR again being more
sensitive.

## Exercise 4: Test the Function (25 points)

``` r
# Load the testthat package
library(testthat)

# 1) For these tests, we should expect to return an empty numeric vector and a message for both methods
test_that("Vector with no NA and no outliers", {
  test_vec1 <- c(1:5)
  expect_equal(check_outliers(test_vec1, "zscore"), numeric(0)) 
  expect_message(check_outliers(test_vec1, "zscore"), "No outliers found.") 
  expect_equal(check_outliers(test_vec1, "iqr"), numeric(0))
  expect_message(check_outliers(test_vec1, "iqr"), "No outliers found.")
})
```

    ## Test passed 🥇

``` r
# 2) The NAs within the vector should not interfere with the return since they are being omitted, and return 5000 for both methods
test_that("Vector with NAs and outliers", {
  test_vec2 <- c(1:10, NA, 11:16, NA, 5000)
  expect_equal(check_outliers(test_vec2, "zscore"), 5000)
  expect_equal(check_outliers(test_vec2, "iqr"), 5000)
  
  # Lets also test that the tolower() function is working properly
  expect_equal(check_outliers(test_vec2, "ZsCoRe"), 5000)
  expect_equal(check_outliers(test_vec2, "IQr"), 5000)
  
})
```

    ## Test passed 🎉

``` r
# 3) These should all return errors as they are not numeric vectors greater than length 0
test_that("Non-numeric or >0 length vectors", {
  test_vec3 <- c("a", "b", "c")
  test_vec4 <- c(TRUE, FALSE, FALSE)
  test_vec5 <- c(1, 2, "s")
  expect_error(check_outliers(test_vec3, "zscore"))
  expect_error(check_outliers(test_vec3, "iqr"))
  expect_error(check_outliers(test_vec4, "zscore"))
  expect_error(check_outliers(test_vec4, "iqr"))
  expect_error(check_outliers(test_vec5, "zscore"))
  expect_error(check_outliers(test_vec5, "iqr"))
  expect_error(check_outliers(numeric(0), "zscore"))
  expect_error(check_outliers(numeric(0), "iqr"))
})
```

    ## Test passed 🥳

``` r
# 4) Test other miscellaneous errors and stops based on input ranges

test_that("invalid inputs or other messages", {
  
  # Test for when SD of a vector is 0 using the zscore method
  test_vec6 <- c(5, 5, 5, 5, 5)
  test_vec2 <- c(1:10, NA, 11:16, NA, 5000)
  expect_message(check_outliers(test_vec6, "zscore"), "Standard deviation is zero or NA; no outliers can be determined using this method.")
  expect_equal(check_outliers(test_vec6, "zscore"), numeric(0))  # Expect empty numeric vector
  
  
  # Test for when the z-score threshold is a non-numeric or less than 1
  expect_error(check_outliers(test_vec2, "zscore", zscore_threshold = TRUE), "The z-score threshold must be a numeric value at least 1 SD or greater.")
  expect_error(check_outliers(test_vec2, "zscore", zscore_threshold = 0.7), "The z-score threshold must be a numeric value at least 1 SD or greater.")
  
  # However, this should not interfere if the chosen method is not "zscore"
  expect_equal(check_outliers(test_vec2, "iqr", zscore_threshold = 0.2), 5000)
  
  # Test for when the IQR threshold multipler is a non-numeric or negative number
  expect_error(check_outliers(test_vec2, "iqr", iqr_threshold = -5), "The IQR threshold must be a numeric value greater than or equal to 0.")
 expect_error(check_outliers(test_vec2, "iqr", iqr_threshold = "7"), "The IQR threshold must be a numeric value greater than or equal to 0.")
 
 
 # Test for when IQR quantile algorithm is a non-integer between 1-9
  expect_error(check_outliers(test_vec2, "iqr", iqr_threshold = 1, quantile_type = 4.5), "Invalid quantile algorithm type; must be an integer between 1 and 9.")
 expect_error(check_outliers(test_vec2, "iqr", quantile_type = -3), "Invalid quantile algorithm type; must be an integer between 1 and 9.")
 
 # However, this should not interfere if the chosen method is not "iqr"
  expect_equal(check_outliers(test_vec2, "zscore", iqr_threshold = 0.2, quantile_type = 50), 5000)
 
  
})
```

    ## Test passed 😸
