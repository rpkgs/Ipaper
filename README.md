# Ipaper  
R package: Ipaper  

### Batch download paper with the help `aria2` and `httr` package.

Install:

```{r}
install.packages("devtools")
library(devtools)
install_github("kongdd/Ipaper")
```

you also need to download [aria2](https://github.com/aria2/aria2/releases/tag/release-1.32.0), and add 
aria2 path to your system environment variables. For the convenience of spell in cmd, I rename aira2c as aria2.


### 01. You need to get doi;

If you want to download paper use that package, you need to get the corresponding DOI. First of all, you should be confirm that your IP have the right to download that paper. And if you want to batch download paper, I suggest you get the DOIs from [Web Of Science](http://login.webofknowledge.com/).

### 02. You need to select the suitable srcFUN of corresponding database. 
If this package have not yet, you can consider to extend the srcFUN, or contact me directly.


Their are two way, we provide to download paper.    
- 1. [aria2](https://aria2.github.io/manual/en/html/), which is same like wget, but can download parallel.  

  

- 2. [httr](https://cran.r-project.org/web/packages/httr/index.html) package by hadley, which download files with the help of `curl`.

`httr`, `curl`, `xml2`, `magrittr`, `stringr` are needed to be installed.


### 03. Examples

```{r}
# First, you need to get doi;
# Second, you need to select the suitable srcFUN of corresponding database. If
#   this package have not yet, you can consider to extend the srcFUN, or 
#   contact me directly.

## download_httr
download_httr("10.1175%2FJHM-D-15-0157.1", journal = '.', srcFUN = src_AMS)

## download_aria2
DOIs <- rep("10.1175%2FJHM-D-15-0157.1", 4) #test aria2 parallel download
download_aria2(DOIs, journal = "JHM", srcFUN = src_AMS, n = 4, Rshell = TRUE)
```


