library(tidyverse)

# Parte I

## DRob

data <- read.delim("AoC_d1p1.txt", header = FALSE)
head(data)
class(data)
sum(data$V1 > lag(data$V1), na.rm = TRUE)

## Curso_r

"AoC_d1p1.txt" |>
  readr::read_table(col_names = "depth") |>
  dplyr::mutate(
    prev_depth = dplyr::lag(depth),
    is_deeper = depth > prev_depth
  ) |>
  dplyr::summarise(n_deeper = sum(is_deeper, na.rm = TRUE)) |>
  dplyr::pull(n_deeper)

# Parte 2

## DRob
sum(data$V1 > lag(data$V1, n = 3), na.rm = TRUE)

## Minha
data2 <- data %>% 
  mutate(V2 = lag(V1),
         V3 = lag(V1, 2),
         sum1=V1+V2+V3)

data2$sum1 > lag(data2$sum1)

sum(data2$sum1 > lag(data2$sum1), na.rm = TRUE)

data2 <- data2 %>% 
  mutate(Cond = data$V1 > lag(data$V1, n = 3))

head(data2, 20)

## Curso_r

"AoC_d1p1.txt" |>
  readr::read_table(col_names = "depth") |>
  dplyr::mutate(
    depth1 = dplyr::lead(depth, n = 1),
    depth2 = dplyr::lead(depth, n = 2),
    sum_depth = depth + depth1 + depth2,
    prev_sum_depth = dplyr::lag(sum_depth),
    is_deeper = sum_depth > prev_sum_depth
  ) |>
  dplyr::summarise(n_deeper = sum(is_deeper, na.rm = TRUE)) |>
  dplyr::pull(n_deeper)
