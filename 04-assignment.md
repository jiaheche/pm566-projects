Assignment 04 - HPC and SQL
================

``` r
library(tidyverse)
library(data.table)
library(parallel)
library(RSQLite)
library(DBI)
```

HPC

Problem 1

``` r
# Total row sums
fun1 <- function(mat) {
  n <- nrow(mat)
  ans <- double(n) 
  for (i in 1:n) {
    ans[i] <- sum(mat[i, ])
  }
  ans
}

fun1alt <- function(mat) {
  rowSums(mat)
}

# Cumulative sum by row
fun2 <- function(mat) {
  n <- nrow(mat)
  k <- ncol(mat)
  ans <- mat
  for (i in 1:n) {
    for (j in 2:k) {
      ans[i,j] <- mat[i, j] + ans[i, j - 1]
    }
  }
  ans
}

fun2alt <- function(mat) {
  ans <- mat
  for (i in 1:nrow(mat)) {
    ans[i, ] <- cumsum(mat[i, ])
  }
  ans
}


# Use the data with this code
set.seed(2315)
dat <- matrix(rnorm(200 * 100), nrow = 200)

# Test for the first
microbenchmark::microbenchmark(
  fun1(dat),
  fun1alt(dat), unit = "relative", check = "equivalent"
)
```

    ## Unit: relative
    ##          expr      min       lq    mean   median       uq       max neval
    ##     fun1(dat) 6.114136 6.010881 4.55221 5.793534 5.907998 0.2266896   100
    ##  fun1alt(dat) 1.000000 1.000000 1.00000 1.000000 1.000000 1.0000000   100

``` r
# Test for the second
microbenchmark::microbenchmark(
  fun2(dat),
  fun2alt(dat), unit = "relative", check = "equivalent"
)
```

    ## Unit: relative
    ##          expr      min       lq     mean   median       uq     max neval
    ##     fun2(dat) 7.388982 7.229613 6.000652 6.991387 6.313719 1.68093   100
    ##  fun2alt(dat) 1.000000 1.000000 1.000000 1.000000 1.000000 1.00000   100

``` r
# The last argument, check = “equivalent”, is included to make sure that the functions return the same result.
```

Problem 2

``` r
sim_pi <- function(n = 1000, i = NULL) {
  p <- matrix(runif(n*2), ncol = 2)
  mean(rowSums(p^2) < 1) * 4
}

# Here is an example of the run
set.seed(156)
sim_pi(1000) # 3.132
```

    ## [1] 3.132

``` r
# This runs the simulation a 4,000 times, each with 10,000 points
set.seed(1231)
system.time({
  ans <- unlist(lapply(1:4000, sim_pi, n = 10000))
  print(mean(ans))
})
```

    ## [1] 3.14124

    ##    user  system elapsed 
    ##    3.02    0.02    3.03

``` r
system.time({
  cl <- makePSOCKcluster(4L)
  clusterSetRNGStream(cl, 1231)
  ans <- unlist(parLapply(cl, 1:4000, sim_pi, n = 10000))
  print(mean(ans))
  stopCluster(cl)
})
```

    ## [1] 3.141578

    ##    user  system elapsed 
    ##    0.00    0.06    1.54

SQL

``` r
# Initialize a temporary in memory database
con <- dbConnect(SQLite(), ":memory:")

# Download tables
film <- read.csv("https://raw.githubusercontent.com/ivanceras/sakila/master/csv-sakila-db/film.csv")
film_category <- read.csv("https://raw.githubusercontent.com/ivanceras/sakila/master/csv-sakila-db/film_category.csv")
category <- read.csv("https://raw.githubusercontent.com/ivanceras/sakila/master/csv-sakila-db/category.csv")

# Copy data.frames to database
dbWriteTable(con, "film", film)
dbWriteTable(con, "film_category", film_category)
dbWriteTable(con, "category", category)
```

Question 1

``` sql
SELECT rating, count(*) AS count 
FROM film
GROUP BY rating
```

<div class="knitsql-table">

| rating | count |
| :----- | ----: |
| G      |   180 |
| NC-17  |   210 |
| PG     |   194 |
| PG-13  |   223 |
| R      |   195 |

5 records

</div>

Question 2

``` sql
SELECT rating, avg(rental_rate) AS avg_rental_rate, avg(replacement_cost) AS avg_replacement_cost
FROM film
GROUP BY rating
```

<div class="knitsql-table">

| rating | avg\_rental\_rate | avg\_replacement\_cost |
| :----- | ----------------: | ---------------------: |
| G      |          2.912222 |               20.12333 |
| NC-17  |          2.970952 |               20.13762 |
| PG     |          3.051856 |               18.95907 |
| PG-13  |          3.034843 |               20.40256 |
| R      |          2.938718 |               20.23103 |

5 records

</div>

Question 3

``` sql
SELECT b.category_id, count(a.film_id) AS count
FROM film a
  INNER JOIN film_category b
    ON a.film_id = b.film_id
GROUP BY category_id
```

<div class="knitsql-table">

| category\_id | count |
| :----------- | ----: |
| 1            |    64 |
| 2            |    66 |
| 3            |    60 |
| 4            |    57 |
| 5            |    58 |
| 6            |    68 |
| 7            |    62 |
| 8            |    69 |
| 9            |    73 |
| 10           |    61 |

Displaying records 1 - 10

</div>

Question 4

``` sql
SELECT c.name, b.category_id, count(a.film_id) AS count
FROM film a
  INNER JOIN film_category b
    ON a.film_id = b.film_id
  INNER JOIN category c
    ON b.category_id = c.category_id
GROUP BY c.category_id
ORDER BY count DESC
```

<div class="knitsql-table">

| name        | category\_id | count |
| :---------- | -----------: | ----: |
| Sports      |           15 |    74 |
| Foreign     |            9 |    73 |
| Family      |            8 |    69 |
| Documentary |            6 |    68 |
| Animation   |            2 |    66 |
| Action      |            1 |    64 |
| New         |           13 |    63 |
| Drama       |            7 |    62 |
| Sci-Fi      |           14 |    61 |
| Games       |           10 |    61 |

Displaying records 1 - 10

</div>

  - The most popular category is sports.
