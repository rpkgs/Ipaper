library(Ipaper)
library(magrittr)
library(openxlsx)
library(data.table)

file <- "papers.txt"
x <- fread(file, sep = "\t")

dois <- x$`DOI号（选填）`

# src_SciDirect_I(dois, "paper/")

download_aria2(dois, journal = "paper", srcFUN = src_SciDirect_I, n = 8, Rshell = TRUE)