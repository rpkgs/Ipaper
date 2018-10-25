# Ipaper  
[![Travis Build Status](https://travis-ci.org/kongdd/Ipaper.svg?branch=master)](https://travis-ci.org/kongdd/Ipaper) 
[![codecov](https://codecov.io/gh/kongdd/Ipaper/branch/master/graph/badge.svg)](https://codecov.io/gh/kongdd/Ipaper)

### Batch download paper with `aria2` and `httr`.

## Installation
```{r}
install.packages("devtools")
devtools::install_github("kongdd/Ipaper")
```
you also need to download [aria2](https://github.com/aria2/aria2/releases/tag/release-1.32.0), and add 
aria2 path to your system environment variables.

## 1. Get doi  
If you want to download paper use that package, you need to get the corresponding DOI. First of all, you should be confirm that your IP have the right to download that paper. And if you want to batch download paper, I suggest you get the DOIs from [Web Of Science](http://login.webofknowledge.com/).

## 2. Select suitable `srcFUN` according to paper database. 
If this package have not yet, you can consider to extend the srcFUN, or contact me directly.

Their are two way, we provide to download paper.    
- 1.[aria2](https://aria2.github.io/manual/en/html/), which is same like wget, but can download parallel.   

- 2.[httr](https://cran.r-project.org/web/packages/httr/index.html) package by hadley, which download files with the help of `curl`.

## 3. Examples

```{r}
# download_httr
download_httr("10.1175%2FJHM-D-15-0157.1", journal = '.', srcFUN = src_AMS)

# download_aria2
DOIs <- rep("10.1175%2FJHM-D-15-0157.1", 4) #test aria2 parallel download
download_aria2(DOIs, journal = "JHM", srcFUN = src_AMS, n = 4, Rshell = TRUE)
```
