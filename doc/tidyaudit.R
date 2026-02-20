## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(tidyaudit)
library(dplyr)

## ----basic-trail--------------------------------------------------------------
# Sample data
orders <- data.frame(
  id       = 1:20,
  customer = rep(c("Alice", "Bob", "Carol", "Dan", "Eve"), 4),
  amount   = c(150, 200, 50, 300, 75, 120, 400, 90, 250, 60,
               180, 210, 45, 320, 85, 130, 380, 95, 270, 55),
  status   = rep(c("complete", "pending", "complete", "cancelled", "complete"), 4)
)

trail <- audit_trail("order_pipeline")

result <- orders |>
  audit_tap(trail, "raw") |>
  filter(status == "complete") |>
  audit_tap(trail, "complete_only") |>
  mutate(tax = amount * 0.1) |>
  audit_tap(trail, "with_tax")

## ----print-trail--------------------------------------------------------------
print(trail)

## ----join-tap-----------------------------------------------------------------
customers <- data.frame(
  customer = c("Alice", "Bob", "Carol", "Dan"),
  region   = c("East", "West", "East", "North")
)

trail2 <- audit_trail("join_pipeline")

result2 <- orders |>
  audit_tap(trail2, "raw") |>
  left_join_tap(customers, by = "customer",
                .trail = trail2, .label = "with_region")

print(trail2)

## ----filter-tap---------------------------------------------------------------
trail3 <- audit_trail("filter_pipeline")

result3 <- orders |>
  audit_tap(trail3, "raw") |>
  filter_tap(status == "complete",
             .trail = trail3, .label = "complete_only") |>
  filter_tap(amount > 100,
             .trail = trail3, .label = "high_value",
             .stat = amount)

print(trail3)

## ----audit-diff---------------------------------------------------------------
audit_diff(trail3, "raw", "high_value")

## ----audit-report-------------------------------------------------------------
audit_report(trail3)

## ----custom-fns---------------------------------------------------------------
trail4 <- audit_trail("custom_example")
result4 <- orders |>
  audit_tap(trail4, "raw", .fns = list(
    mean_amount = ~mean(.x$amount),
    n_customers = ~length(unique(.x$customer))
  ))

audit_report(trail4)

## ----null-trail---------------------------------------------------------------
# Plain filter — no diagnostics
orders |> filter_tap(amount > 100) |> nrow()

# Diagnostics without a trail
orders |> filter_tap(amount > 100, .stat = amount) |> invisible()

