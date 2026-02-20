## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(tidyaudit)
library(dplyr)

## ----validate-join------------------------------------------------------------
orders <- data.frame(
  id     = c(1L, 2L, 3L, 3L, 4L, 5L),
  amount = c(100, 200, 150, 175, 300, 50)
)
customers <- data.frame(
  id   = c(2L, 3L, 6L),
  name = c("Alice", "Bob", "Carol")
)

validate_join(orders, customers, by = "id")

## ----validate-join-named------------------------------------------------------
products <- data.frame(prod_id = 1:3, price = c(10, 20, 30))
sales    <- data.frame(item_id = c(1L, 1L, 2L), qty = c(5, 3, 7))

validate_join(products, sales, by = c("prod_id" = "item_id"))

## ----validate-join-stat-------------------------------------------------------
x <- data.frame(id = 1:4, revenue = c(100, 200, 300, 400))
y <- data.frame(id = c(2L, 3L, 5L), cost = c(10, 20, 30))

validate_join(x, y, by = "id", stat_x = "revenue", stat_y = "cost")

## ----validate-pk--------------------------------------------------------------
df <- data.frame(
  id    = c(1L, 2L, 3L, 3L, 4L),
  group = c("A", "A", "B", "B", "A"),
  value = c(10, 20, 30, 40, 50)
)

# Single column — not unique
validate_primary_keys(df, "id")

# Composite key — unique
validate_primary_keys(df, c("id", "group"))

## ----validate-var-rel---------------------------------------------------------
df2 <- data.frame(
  dept    = c("Sales", "Sales", "Engineering", "Engineering"),
  manager = c("Ann", "Ann", "Bob", "Bob")
)
validate_var_relationship(df2, "dept", "manager")

## ----compare-tables-----------------------------------------------------------
before <- data.frame(id = 1:5, value = c(10.0, 20.0, 30.0, 40.0, 50.0))
after  <- data.frame(id = 1:5, value = c(10.0, 22.5, 30.0, 40.0, 55.0))

compare_tables(before, after)

## ----filter-keep--------------------------------------------------------------
sales <- data.frame(
  id     = 1:10,
  amount = c(500, 25, 1200, 80, 3000, 15, 750, 40, 2000, 60),
  status = rep(c("valid", "suspect"), 5)
)

result <- filter_keep(sales, amount > 100, .stat = amount)

## ----filter-drop--------------------------------------------------------------
result2 <- filter_drop(sales, status == "suspect", .stat = amount)

## ----filter-warn, warning=TRUE------------------------------------------------
filter_keep(sales, amount > 1000, .stat = amount, .warn_threshold = 0.5)

## ----diagnose-nas-------------------------------------------------------------
messy <- data.frame(
  id    = 1:6,
  name  = c("A", NA, "C", "D", NA, "F"),
  score = c(10, 20, NA, NA, 50, NA),
  grade = c("A", "B", "C", NA, "A", "B")
)

diagnose_nas(messy)

## ----summarize-column---------------------------------------------------------
summarize_column(c(1, 2, 3, NA, 5, 10, 100))
summarize_column(c("apple", "banana", "apple", "cherry", NA))

## ----get-summary-table--------------------------------------------------------
get_summary_table(messy)

## ----diagnose-strings---------------------------------------------------------
firms <- c("Apple", "APPLE", "apple", "  Microsoft ", "Google", NA, "")
diagnose_strings(firms)

## ----audit-clean--------------------------------------------------------------
audit_clean(firms, trimws)
audit_clean(firms, tolower)

