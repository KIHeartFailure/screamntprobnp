# default is to use tidyverse functions
select <- dplyr::select 
rename <- dplyr::rename
filter <- dplyr::filter
mutate <- dplyr::mutate
complete <- tidyr::complete

# colours 

global_colsblue <- rev(c(
  #"#F0FAFF",
  #"#D6F0F7",
  "#9BD4E5",
  "#70C1DA",
  "#4FB3D1",
  "#2F99BA",
  "#0F83A3",
  "#006E8A",
  "#034F69",
  "#023647"
))

global_colsgrey <- rev(c(
  "#C2C2C2",
  "#A6A6A6",
  "#8C8C8C",
  "#666666",
  "#4D4D4D",
  "#333333",
  "#262626",
  "#1A1A1A"
))

# used for calculation of ci 
global_z05 <- qnorm(1 - 0.025)