if (!require('knitr')) {
  install.packages("knitr")
}
if (!require('devtools')) {
  install.packages("devtools")
}
if (!require('RWordPress')) {
  devtools::install_github(c("duncantl/XMLRPC", "duncantl/RWordPress"))
}
library(RWordPress)
library(knitr)

filename <- "5.Rmd"
titel <- "Project Euler 5 - Smallest multiple"


options(WordpressLogin=c(ChristianKnudsen="2ZKwcFS6vD*fqWMQ"),
        WordpressURL="https://christianknudsen.info/xmlrpc.php")
knit2wp(filename, title = titel ,publish = FALSE)
options(WordpressLogin=c(admin="7g0G9!27"),
        WordpressURL="https://nusse.dk/xmlrpc.php")
knit2wp(filename, title = titel ,publish = FALSE)

# sudo su - -c "R -e \"install.packages('curl')\""
# sudo su - -c "R -e \"devtools::install_github(c('duncantl/XMLRPC', 'duncantl/RWordPress'))\""
# devtools::install_github(c("duncantl/XMLRPC", "duncantl/RWordPress"))