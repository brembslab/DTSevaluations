
      
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
                "sicegar",
                "wrMisc",
                "raincloudplots",
                "ggpmisc",
                "ggrain"))

if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("genefilter")

