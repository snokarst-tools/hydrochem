library(data.table)

## code to prepare cloutier2004 dataset goes here
data_cloutier <- readODS::read_ods("data-raw/cloutier2004.ods") |> 
  as.data.table()

usethis::use_data(data_cloutier, overwrite = TRUE)
