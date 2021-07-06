
      
install.packages(c("ggplot2",
                 "ggiraph",
                  "tidyr",
                "dygraphs",
                "grid",
                 "reshape2",
                 "plyr",
                 "dplyr",
                 "gridExtra",
                 "yaml",
                 "ggsignif",
                 "effsize",
                 "pwr",
                 "BayesFactor",
                 "genefilter",
                 "seewave",
                 "lubridate",
                 "rmarkdown",
                 "markdown",
                 "knitr",
                 "dabestr",
                 "zoo",
                 "tidyverse",
                 "questionr",
                 "data.table",
                 "DescTools",
                 "magick",
                "reactable",
                "sicegar"))

if (!require(remotes)) {
  install.packages("remotes")
}
remotes::install_github('jorvlan/raincloudplots')


if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("genefilter")

