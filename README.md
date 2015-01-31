# Barcelona_Database_Apps
Applications designed to carry out operations with data from the [Barcelona Database](https://github.com/AnalisiGuanyem/Barcelona_Database).

## 'Fitxes dels barris'
R-script to automatically get the a small report with useful information from Barcelona neighborhoods. 

This script takes relevant information from the Barcelona Database. It has been optimized to get a PDF document, summarizing the information of the selected neighborhood (or district), and a complementary HTML file with the same information. The script is based on R and takes advantage of the [*knitr*](https://github.com/yihui/knitr#readme) possibilities.

*Knitr* allows you to write in the simple [*Markdown*](http://en.wikipedia.org/wiki/Markdown) language, without taking care on the formatting of the documents. It also allow adding *chunks* of R programming code to easily process the data. To install *knitr*, install the package as any other R package: `install.packages('knitr', dependencies = TRUE)`

The R code *chunks* could be inserted in the middle of the text (e.g.: I counted `r 1 + 1` red trucks on the highway) or as block of code: 

    ``` {r}
    # quick summary and plot
    library(ggplot2)
    summary(cars)
    qplot(speed, dis, data=cars) + geom_smooth()
    ```
For more information on the combination of R code and the very simple Markdown language, you must also visit the [RMarkdown website](https://github.com/rstudio/rmarkdown#readme).

## Tips for Emacs users

It's possible to edit, weave, export from, and execute R code from RMarkdown files within Emacs. You need to install the following software packages in your system:

* GNU R (version >= 3.1.2, [Debian packages available](http://cran.es.r-project.org/bin/linux/debian/))
* [Pandoc](http://johnmacfarlane.net/pandoc/)

You'll need Emacs version >= 24.4, and install the following packages:

* [polymode](https://github.com/vspinu/polymode)
* [ESS](http://ess.r-project.org/)

Package [pandoc-mode](http://joostkremers.github.io/pandoc-mode/) can also be useful.

## Contact

For more information on the scripts, suggestions (very welcome!) or any other thing, please contact us at analisi.guanyem@gmail.com.
