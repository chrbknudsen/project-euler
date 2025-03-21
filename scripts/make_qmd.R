make_qmd <- function(nr){
  side <- stringr::str_pad(string = nr, width = 4, side = "left", pad = "0")
  side <- paste("solutions/",side,".qmd", sep = "")
  indhold <- stringr::str_glue('---
title: "Euler {nr}"
author: "Christian Knudsen"
format: html
date: "{date()}"
params:
  done: FALSE
---')
  if(!file.exists(side)){
    write(indhold, side)
  }else{
    message("filen eksisterer allerede!")
  }
  
}




