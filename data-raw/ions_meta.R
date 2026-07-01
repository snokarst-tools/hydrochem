library(data.table)

## code to prepare `ions` dataset goes here
ions_meta <- readODS::read_ods("data-raw/ions_meta.ods") |> 
  as.data.table()

usethis::use_data(ions_meta, overwrite = TRUE, internal = TRUE)

# INFO: charge for non-charged element is set to 1 in ions_meta to ensure proper calculation in convert_unit()