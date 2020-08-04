library(shiny)
library(shinyjs)
library(dplyr)
library(stringr)
library(magrittr)
library(shinyBS)
library(shinyWidgets)
library(hashids)

#***********************************************************


options(warn = -1, showWarnCalls = FALSE) # silence all warnings


## >>>>>> Gen ref functions. <<<<<< --------

# Master scenario list ====
scenMaster <- list( "Scenario 1 (Monotonic Increase)"       = 1,
                    "Scenario 2 (Monotonic Decrease)"       = 2,
                    "Scenario 3 (Log-Normal)"               = 3,
                    "Scenario 3.1 (Concave Down)"           = 3.1,
                    "Scenario 4 (Long Initial Descent)"     = 4,
                    "Scenario 4.1 (Quick Initial Descent)"  = 4.1,
                    "Scenario 5 (Complex Hazard)"           = 5,

                    "Scenario 7 (Monotonic Increase, Logit Link)"    = 7,
                    "Scenario 8 (Monotonic Increase, cloglog Link)"  = 8,
                    "Scenario 9 (Monotonic Decrease, Logit Link)"    = 9,
                    "Scenario 10 (Monotonic Decrease, cloglog Link)" = 10
              )

# List of scope scenarios ====
scopeList <- list("Fails per Unit Interval w/Covariate" = 6)

# The relevant index numbers for the scenarios (for baseline hazard graphs)
scenIdxs <- NULL
    # Classic simple
    scenIdxs[["1, No, 1"]] <- c(1, 2, 3, 3.1, 4, 4.1, 5) # contin
    scenIdxs[["1, No, 2"]] <- c(1, 2, 3, 3.1, 4, 4.1, 5) # coerced SS
    scenIdxs[["1, No, 3"]] <- c(7, 8, 9, 10)             # discrete
    
    # Simple + PH: coerced start-stop only
    scenIdxs[["1, Yes, 2"]] <- c(1,2)                    # coerced SS
    
    # two trans + no PH: coerced start-stop only
    scenIdxs[["2, No, 2"]]  <- c(1,2)                    # coerced SS
    
    # two trans + PH
    scenIdxs[["2, Yes, 2"]] <- c(1,2)                    # coerced SS
    scenIdxs[["2, Yes, 3"]] <- c(7,9)                    # discrete
    
#***********************************************************

lightbox_gallery_mod <- function (df, gallery, css, caption, width = 80, display = "block")
{
    setwd("../") #jump back up to root
    css <- "www/lightbox-2.10.0/styles.css" 
    #         "styles.css")
    if (!(dir.exists("www/lightbox-2.10.0"))) {
        fs::dir_copy(system.file("js/lightbox-2.10.0", package = "gallerier"),
            "www/lightbox-2.10.0")
    }
    if (!("uid" %in% colnames(df)))
        df$uid <- strtrim(digest::sha1(df$src), 5)
    outpt <- 
      tags$div(style = sprintf("display: %s;", display), tagList(tags$head(tags$link(rel = "stylesheet",
        type = "text/css", href = "lightbox-2.10.0/lightbox.min.css")),
        tags$div(class = "card-deck", lapply(seq_len(nrow(df)),
            function(i) {
                tags$div(
                    `data-type` = "template",
                    class = "card",
                    tags$p(class="lbox-card-title",        # title in gallery
                           caption[i]),
                    tags$a(
                        id = df$uid[i],
                        href = paste0("img/",
                                      df$src[i]),
                        `data-lightbox` = gallery,
                        `data-title` = caption[i],
                        tags$img(
                            class = "card-img-top",
                            src = paste0("img/", df$src[i]),
                            width = paste0(width, "px"),
                            height = "auto"
                        )
                    )
                )
            })), includeScript("www/lightbox-2.10.0/lightbox.min.js"),
        includeCSS(css)))
    setwd("results")    # jump back to results
    
    outpt
}
