library(tidyverse)

data <- read.delim("AoC_d2.txt", header = FALSE)

head(data)

# Parte 1

## DRob

directions <- tribble(
  ~ direction, ~ x, ~ y,
  "down",0,1,
  "up", 0,-1,
  "forward", 1,0
)

joined <- data %>% 
  separate(V1, c("direction", "distance"), sep = " ", convert = TRUE) %>% 
  inner_join(directions, by = 'direction')

head(joined)

joined %>% 
  summarize(horizontal = sum(distance*x),
            depth = sum(distance*y),
            product = horizontal*depth)

joined10 <- head(joined, 12) 

joined10 %>% 
  summarize(horizontal = sum(distance*x),
            depth = sum(distance*y),
            product = horizontal*depth)

## Curso_r

"AoC_d2.txt" |>
  readr::read_delim(" ", col_names = c("command", "x")) |>
  dplyr::mutate(x = ifelse(command == "up", -x, x)) |>
  dplyr::group_by(command == "forward") |>
  dplyr::summarise(x = sum(x)) |>
  dplyr::summarise(x = prod(x)) |>
  dplyr::pull(x)

# Parte 2

## DRob

joined %>% 
  mutate(aim = cumsum(y*distance)) %>% 
  summarize(horizontal = sum(distance*x),
            depth = sum(aim*distance*y),
            product = horizontal*depth)

joined10 <- head(joined, 10)

joined10 %>% 
  mutate(aim = cumsum(y*distance)) %>% 
  summarize(horizontal = sum(distance*x),
            depth = sum(aim*distance*y),
            product = horizontal*depth)

## Curso_r

"AoC_d2.txt" |>
  readr::read_delim(" ", col_names = c("command", "x")) |>
  dplyr::mutate(
    horizontal = ifelse(command == "forward", x, 0),
    horizontal = cumsum(horizontal),
    aim = ifelse(command == "down", x, 0),
    aim = ifelse(command == "up", -x, aim),
    aim = cumsum(aim),
    depth = ifelse(command == "forward", aim * x, 0),
    depth = cumsum(depth),
    output = horizontal * depth
  ) |>
  utils::tail(1) |>
  dplyr::pull(output)

# Fim
