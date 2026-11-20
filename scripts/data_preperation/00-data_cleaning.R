# data cleaning - date

library(tidyverse)
library(tsibble)


# read data ---------------------------------------------------------------

product_master <- read.csv('data/product.csv') |> 
  janitor::clean_names() |> 
  select(product_code, product_type, product_name)

# we have 36 products including contraceptives and associate medications

contraceptives <- c(
  "MICROGYNON 0.03/0.15 MG BTE / 3 CYCLES BOITE  -",
  "MICROLUT 0.03 MG BTE / 3 CYCLES BOITE  -",
  "IMPLANON IMPLANT 68 MG BTE/1 BOITE  -",
  "JADELLE (IMPLANT + TROCARD) 150 µG BTE/10 BOITE  -",
  "LEVONORGESTREL 1.5 MG CP BTE/2 BOITE  -",
  "STERILET DIU CUIVRE  BTE/50 BOITE  -",
  "DEPO PROVERA/PETOGEN 150 MG / 3ML INJ BTE/25 BOITE  -",
  "NORISTERAT 200 MG AMP INJ BTE/100 BOITE  -",
  "PRESERVATIF FEMININ UNITE  -",
  "PRESERVATIF MASCULIN BTE/144 BOITE  -",
  "COPPER TCU 380 A (CUIVRE) UNITE  -",
  "MEDROXYPROGESTERONE 104MG/0.65ML INJ UNITE (SAYANA PRESS) UNITE",
  "MEDROXYPROGESTERONE (SAYANA PRESS) 104 mg / 0.65 ml inj. UNITE UNITE  ml"
)

stock_df <- read.csv('data/stock_data_original.csv') |> 
  janitor::clean_names() |> 
  left_join(product_master |>  select(-product_name), by = 'product_code')|>
  filter(product_name %in% contraceptives) |> 
  mutate(product_type = case_when(product_name == 'STERILET DIU CUIVRE  BTE/50 BOITE  -' ~ 'Intrauterine Device (IUD)',
                                      product_name == 'STERILET DIU CUIVRE  BTE/50 BOITE  -' ~ 'Injectable Contraceptive',
                                      .default = product_type))


# data cleaning -----------------------------------------------------------

# Create a lookup table for month names

months_french <- c("JANVIER", "FEVRIER", "MARS", "AVRIL", "MAI", "JUIN", "JUILLET", "AOUT", "SEPTEMBRE", "OCTOBRE", "NOVEMBRE", "DECEMBRE")
months_english <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")
month_lookup <- data.frame(months_french, months_english)

# Translate French month names to English

stock_tidy <- stock_df  |> 
  mutate(month = gsub(" .*$", "", period),
         year = gsub("^.* ", "", period)) |> 
  left_join(month_lookup, by = c("month" = "months_french"))  |> 
  mutate(date = paste(months_english, year)) |> 
  mutate(date = yearmonth(my(date)),
         comments = tolower(comments),
         comments = ifelse(comments == "" | is.na(comments) | comments == '#name?', "empty", comments)) |> 
  select(date, region, district, site_code, site, product_type, product_name, everything(), -c(period, year, month, months_english))

write_rds(stock_tidy, 'data/tidy_data/stock_tidy.rds')

stock_tidy |> 
  select(comments) |> 
  unique() |> 
  write.csv('data/tidy_data/french_comments.csv', row.names = F)








