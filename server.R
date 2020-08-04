# Shiny app to make it easier organize and view simulation results from Metzger
# and Jones, "Getting Time Right"
## First version: 19NOV18
## Last update: 18AUG20

#*****************************************************************
# Set results dir as de facto root for later path builds
setwd("results")

## >>>>>> SERVER CODE <<<<<< --------
server <- function(input, output, session) {
    withMathJax()
    
    # For file done
    rv <- reactiveValues(fileDone = FALSE)

    # INPUT: Updating the model ddown ====
    observe(priority=100, x={
        # ~~ for the coeffs and pred quants ~~
        if(input$simSet<=2) {

            # Get list of scenarios
            scList <- list.files(findDir()) %>%
                        str_extract(., "- [0-9]* - | - [0-9]\\.[0-9] - ") %>%
                        gsub("-", "", .) %>%
                        na.omit(.) %>%
                        trimws(.) %>%
                        unique(.) %>%
                        as.numeric(.) %>%
                        sort(.)

            # For selected
             ## Keep as current selection, if still valid
             ## If invalid, go with first in the list
            ifelse(sum(scList==input$h0), {sel <- input$h0}, {sel <- scList[1]})

            # Update
            updateSelectInput(session, "h0",
                choices = scenMaster[scenMaster %in% scList], selected = sel
            )

        # ~~ for the PH test sims ~~
        } else if (input$simSet==4){
            updateSelectInput(session, "h0",
                choices = list("t"=1,
                               "ln(t)" = 2,
                               "|0.4t^2 - 0.16t -1.3|" = 3
               ), selected = 1
            )
        }
    })

    # INPUT: Updating the available Ns ====
    observe(priority=100, x={
        if(input$simSet<=2){
            # If it's the classic simple, there are options
            if(input$complexity_trs==1 & 
               input$complexity_phViol=="No") valList <- list(100, 250, 500)   
            
            # If it's the other three, there's one option
            else    valList <- list(250)
                    
            # Update
            updateSelectInput(session, "nObs",
                              choices = valList,
                              selected = 250        # since 250 is always valid
            )
        }
    })
        
    # INPUT: Updating the ranges for x's main effect for scenarios w/CC or PH====
    observe(priority=100, x={
        if(input$simSet<=2){
            # TWO TRS
            ## (doing it this way, b/c complexity won't be an input unless we're
            ## looking at coeffs/trProbs, so will eval as null and make R
            ## angry.)
            if(input$complexity_trs=="2"){ 
                ## Needs to be tweaked regardless:
                # x1: If current value's |0.5| or |0.05|, tweak
                ifelse(input$x1==abs(0.5) | input$x1==abs(0.05), 
                        {sel1 <- sign(input$x1)*0.2}, 
                        {sel1 <- input$x1})

                updateSelectInput(session, "x1",
                                  choices = list(-0.2, 0, 0.2),
                                  selected = sel1
                )

                # x2
                ifelse(input$x2==abs(0.5) | input$x2==abs(0.05), 
                        {sel2 <- sign(input$x2)*0.2}, 
                        {sel2 <- input$x2})

                updateSelectInput(session, "x2",
                                  choices = list(-0.2, 0, 0.2),
                                  selected = sel2
                )

                # PH EFFECT
                if(input$complexity_phViol=="Yes"){ # equiv to straight-up ccPH sims
                    # tde1: If current value's |0.5| or |0.05|, tweak
                    ifelse(input$tde1==abs(0.5) | input$tde1==abs(0.05), 
                            {sel11 <- sign(input$tde1)*0.2}, 
                           {sel11 <- input$tde1})

                    updateSelectInput(session, "tde1",
                                      choices = list(-0.2, 0, 0.2),
                                      selected = sel11
                    )

                    # tde2
                    ifelse(input$tde2==abs(0.5) | input$tde2==abs(0.05), 
                            {sel2 <- sign(input$tde2)*0.2}, 
                            {sel2 <- input$tde2})

                    updateSelectInput(session, "tde2",
                                      choices = list(-0.2, 0, 0.2),
                                      selected = sel2
                    )
                }

            # ONE TR - only relv case is for new PH stuff
            } else {
                if(input$complexity_phViol=="Yes"){
                    
                    ## Needs to be tweaked regardless:
                    # x1: If current value's |0.5| or |0.05|, tweak
                    ifelse(input$x1==abs(0.5) | input$x1==abs(0.05), 
                            {sel1 <- sign(input$x1)*0.2}, 
                            {sel1 <- input$x1})
    
                    updateSelectInput(session, "x1",
                                      choices = list(-0.2, 0, 0.2),
                                      selected = sel1
                    )
    
                    # x2
                    ifelse(input$x2==abs(0.5) | input$x2==abs(0.05), 
                            {sel2 <- sign(input$x2)*0.2}, 
                            {sel2 <- input$x2})
    
                    updateSelectInput(session, "x2",
                                      choices = list(-0.2, 0, 0.2),
                                      selected = sel2
                    )
                    
                    # tde1: If current value's |0.5| or |0.05|, tweak
                    ifelse(input$tde1==abs(0.5) | input$tde1==abs(0.05), 
                            {sel11 <- sign(input$tde1)*0.2}, 
                            {sel11 <- input$tde1})

                    updateSelectInput(session, "tde1",
                                      choices = list(-0.2, 0.2),
                                      selected = sel11
                    )
                }
            }
        }
    })

    # INPUT: Updating the available DGP types depending on ====
    # whether simple or complex 
    observe(priority=100, x={
        if(input$simSet<=2){
            # TWO TRS
            if(input$complexity_trs=="2"){
                # PH VIOLATION (classic ccPH)
                if(input$complexity_phViol=="Yes"){
                    updateSelectInput(session, "DGP",
                                          choices = list("Coerced Start-Stop" = 2,
                                                         "Discrete" = 3),
                                          selected = ifelse(input$DGP==1,  2, input$DGP))
                # NO PH VIOLATION (new)
                } else{
                    updateSelectInput(session, "DGP",
                                          choices = list("Coerced Start-Stop" = 2),
                                          selected = 2)
                }

            # ONE TR
            } else {
                # PH VIOLATION (new)
                if(input$complexity_phViol=="Yes"){
                    updateSelectInput(session, "DGP",
                                          choices = list("Coerced Start-Stop" = 2),
                                          selected = 2)
                # NO PH VIOLATION (classic simple)
                } else{
                updateSelectInput(session, "DGP",
                                          choices = list("Continuous" = 1,
                                                         "Coerced Start-Stop" = 2,
                                                         "Discrete" = 3),
                                          selected = input$DGP)
                }

            }
        }
    })

    # HELPER: find the directory for this selected scenario ====
    findDir <- reactive({
        # Base directory, can set up front.
        dir <- dirStart("", input$simSet)

        # If it's simSet==1|2, pull the directory for this (# transit)-(PH viol?) combo
        if(input$simSet<=2){
            dir <- paste0(dir, "/", 
                            dirStart(dir, paste0("\\[", 
                                            input$complexity_trs, ", ", 
                                            input$complexity_phViol, 
                                          "\\]")
                            )
                    )
        }
        
        # Get to correct DGP folder
        dir <- paste0(dir, "/", dirStart(dir, input$DGP))  ## pull directory that starts with input$DGP's number

            ## ^ this will 'throw' an error if the complexity's advanced but the
            ## DGP option hasn't updated from continuous yet. Not an error I
            ## care about, and everything still works fine.

        # Return, done.
        return(dir)
    })


    # HELPER: build the file name ====
         # Advanced, trans prob graphs: trProbs - # - ...
         # Advanced, datasets/Mata: sims - sc # - ...
    fNameBuild <- reactive({

        # coefficients
        if(input$simSet==1){
            ifelse(input$DGP==1, 
                    stub <- "cont", 
                    ifelse(input$DGP==2, stub <- "start-stop", stub <- "discrete"))

            ## simple DGP, no PH
            if(input$complexity_trs=="1" & input$complexity_phViol=="No"){
                fileName <- paste0("HIPPO = ", input$nObs, ", ", stub) %>% 
                                paste0(., ".png")

            ## simple DGP, PH    
            } else if (input$complexity_trs=="1" & input$complexity_phViol=="Yes"){
                # build
                fileName <- paste0("HIPPO = ",    "tde1=",  input$tde1,
                                                ", xEff1=",  input$x1,
                                                " - x1",
                                                ".png"
                                               ) %>%
                                gsub("0.", ".", ., fixed=TRUE) # Toss leading zeros   

            ## CC + PH (fine for no PH, too)
            } else {
                # coerced SS vs. discrete entry for sh2 (needs to stay this way
                # for the conditional to evaluate properly)
                ifelse(input$DGP==2, 
                        {sh2Expr <- paste0("sh2=",input$sh2)}, 
                        {sh2Expr <- paste0("dgp2=sc", input$h0_tr2)} )

                # trans num
                ifelse(input$advTrSelect=="Transition 1", 
                        {trNo <- 1}, 
                        {trNo <- 2})

                # TDE vals
                if(input$complexity_phViol=="Yes"){
                    tde1 <- input$tde1
                    tde2 <- input$tde2
                } else {
                    tde1 <- 0
                    tde2 <- 0
                }
                
                # build
                fileName <- paste0("HIPPO = ",    "tde1=",   tde1,
                                                ", xEff1=",  input$x1,
                                                ", tde2=",   tde2,
                                                ", xEff2=",  input$x2,
                                                ", ", sh2Expr,
                                                " - x", trNo,
                                                ".png"
                                               ) %>%
                                gsub("0.", ".", ., fixed=TRUE) # Toss leading zeros
            }

        # pred quant
        } else if(input$simSet==2){
            ## get the correct stub, given continuous, coerced SS, or discrete
            ifelse(input$DGP==1, 
                    stub <- "t", 
                    ifelse(input$DGP==2, stub <- "exitInt", stub <- "discrete"))

            ## simple DGP, no PH
            if(input$complexity_trs=="1" & input$complexity_phViol=="No"){
                fileName <- paste0("trProbs - ", input$h0,
                                                " - ", stub,
                                                ", n=", input$nObs,
                                                ", x=MOOSE",
                                                ", sims=1000") %>%
                                gsub("0.", ".", ., fixed=TRUE) %>% # Toss leading zeros
                                paste0(., ".png") # append extension, rather than mess with gsub

            ## simple DGP, PH
            } else if(input$complexity_trs=="1" & input$complexity_phViol=="Yes"){
                
                fileName <- paste0("trProbs - ", input$h0,
                                                " - tde1=",  input$tde1,
                                                ", xEff1=",  input$x1,
                                                ", n=",      input$nObs,
                                                ", x=MOOSE [nSims=1000].png"
                                               ) %>%
                                gsub("0.", ".", ., fixed=TRUE) # Toss leading zeros
                
            ## CC + PH (this is fine for no PH, too, b/c you kept the tdes in
            ## the file name specifically to make app!life easier)
            } else {
                # coerced SS vs. discrete entry for sh2 (needs to stay this way
                # for the conditional to evaluate properly)
                ifelse(input$DGP==2, 
                        {sh2Expr <- paste0("sh2=",input$sh2)}, 
                        {sh2Expr <- paste0("dgp2=sc", input$h0_tr2)} )

                # TDE vals
                if(input$complexity_phViol=="Yes"){
                    tde1 <- input$tde1
                    tde2 <- input$tde2
                } else {
                    tde1 <- 0
                    tde2 <- 0
                }
                
                fileName <- paste0("trProbs - ", input$h0,
                                                " - tde1=",  tde1,
                                                ", xEff1=",  input$x1,
                                                ", tde2=",   tde2,
                                                ", xEff2=",  input$x2,
                                                ", ", sh2Expr,
                                                ", n=",       input$nObs,
                                                ", x=MOOSE [nSims=1000].png"
                                               ) %>%
                                gsub("0.", ".", ., fixed=TRUE) # Toss leading zeros
            }


        # Scope logs
        } else if(input$simSet==3){
            if(input$scopeCond==1){
                fileName <- "aggregation.log"
            } else if(input$scopeCond==2){
                fileName <- "# fails in interval (all).log"
            } else if(input$scopeCond==6){
                fileName <- "# fails in interval (all) - w covars.log"
            } else if(input$scopeCond==3){
                fileName <- "mean + median.log"
            } else if(input$scopeCond==4){
                fileName <- "mean + median (smaller).log"
            } else{
                fileName <- "mean + median (smaller + higher mean max).log"
            }

        # PH stuff
        } else{
            fileName <- "PH test - all.log"
        }

        # Return the file name; done.
        return(fileName)

    })

    # HELPER: pull the relevant set of x values, given whether it's ====
    # the simple or CC+PH stuff 
    xVals <- reactive({
        # Trigger update
        input$simSet
        
        # Get list
        vals <- list.files(findDir()) %>%
                        str_extract(., "x=-?\\.?[0-9]*") %>%
                        gsub("x=", "", .) %>%
                        na.omit(.) %>%
                        trimws(.) %>%
                        unique(.) %>%
                        as.numeric(.) %>%
                        sort(.)

        return(vals)
    })

    # HELPER HELPER: given a set of starting characters, return the full ====
    # directory name within the specified folder (specified in dir) 
    ## presumes each directory has unique ID
    dirStart <- function(dirInp, starter){

        #if you're in root, don't type the path arg at all
        ifelse(dirInp=="", {dir <- ""}, {dir <- paste0(", path= '", dirInp,"'")})
        cmd <- paste0("list.dirs(recursive=FALSE", dir, ", full.names=FALSE)")

        # Escape any left and right brackets in dirInp
        dirInpEsc <- dirInp %>%
                        gsub("\\[", "\\\\[", .) %>% # left brackets
                        gsub("\\]", "\\\\]", .)     # right brackets
        
        # Get subdir list
        subdir <- eval(parse(text=cmd)) %>%
                    gsub("\\./", "", .) %>% 
                    gsub(paste0(dirInpEsc,"/"), "", .)  # chop off current path from the front, if present
        
        # Do start of directory first PLUS check end of direct (the former = to
        # be compatible w/the older syntax)
        chunk <- paste0(starter, ifelse(dirInpEsc=="", " -", ""))
        subdir <- subdir[grep(paste0("^", chunk, "|", chunk, "$"), subdir)]

        return(subdir)
    }

    # HELPER: Build clean file name ====
    cleanPath <- function(dir, fName){
        return(
            normalizePath(
                file.path(
                    paste0(dir, "/", fName)
                )
            )
        )
    }

    # HELPER: reactives for xVals file names ====
    xValFName <- reactive({
        input$simSet
        f1 <- gsub("MOOSE", gsub("0\\.", ".",xVals()[1]), fNameBuild())
        f2 <- gsub("MOOSE", gsub("0\\.", ".",xVals()[2]), fNameBuild())
        f3 <- gsub("MOOSE", gsub("0\\.", ".",xVals()[3]), fNameBuild())

        return(list(f1, f2, f3))
    })

    # HELPER: reactives for coeff/estimate OR xVal file names ====
    megaFName <- reactive({
        scs <- NULL
        
        # COEFFICIENTS
        if(input$simSet==1){

            # Build
            ## ONE TR
            if(input$complexity_trs=="1"){
                ## No TDEs requested
                if(input$complexity_phViol=="No" | 
                   (input$complexity_phViol=="Yes" & input$advTrEff!="Time-Varying")){ 
                    f1 <- gsub("HIPPO", "estims" , fNameBuild())
                    f2 <- gsub("HIPPO", "diffAbs", fNameBuild())
                    f3 <- gsub("HIPPO", "rmse"   , fNameBuild())
                ## TDEs
                } else{
                    scs <- c(1,2)
                    
                    f1 <- gsub("HIPPO =", paste0("tde - sc 1 -"), fNameBuild())
                    f2 <- gsub("HIPPO =", paste0("tde - sc 2 -"), fNameBuild())
                    f3 <- gsub("HIPPO", "rmse"   , fNameBuild())  # just filler to prevent an error
                }
                
            ## TWO TRS + TDEs (just hijack the diffAbs for the other scenario)
            } else{
                ## No TDEs requested
                if(input$complexity_phViol=="No" | 
                   (input$complexity_phViol=="Yes" & input$advTrEff!="Time-Varying")){ 
                    f1 <- gsub("HIPPO", "estims" , fNameBuild())
                    f2 <- gsub("HIPPO", "diffAbs", fNameBuild())
                    f3 <- gsub("HIPPO", "rmse"   , fNameBuild())
                ## TDEs requested
                } else{
                    ifelse(input$DGP==2, {scs <- c(1,2)}, {scs <- c(7,9)})
    
                    f1 <- gsub("HIPPO =", paste0("tde - sc ", scs[1], " -"), fNameBuild())
                    f2 <- gsub("HIPPO =", paste0("tde - sc ", scs[2], " -"), fNameBuild())
                    f3 <- gsub("HIPPO", "rmse"   , fNameBuild())  # just filler to prevent an error
                }
            }
        
        # ANYTHING OTHER THAN COEFFS
        } else {
            f1 <- gsub("MOOSE", gsub("0\\.", ".",xVals()[1]), fNameBuild())
            f2 <- gsub("MOOSE", gsub("0\\.", ".",xVals()[2]), fNameBuild())
            f3 <- gsub("MOOSE", gsub("0\\.", ".",xVals()[3]), fNameBuild())
        }

        return(list(f1, f2, f3, scs))
    })

    # HELPER: TDE graph title visibility ====
    observeEvent(c(input$complexity_trs, 
                   input$complexity_phViol, 
                   input$advTrEff), {
        # Any of this only applies when TDE exists, so check that first
        if(input$complexity_phViol=="Yes"){
            # TDE requested
            if(input$advTrEff=="Time-Varying"){
                shinyjs::show("tdeTitle1")
                shinyjs::hide("mainTitle1")
                
                shinyjs::show("tdeTitle2")
                shinyjs::hide("mainTitle2")
                    
                # tr2 header if two stages
                if(input$complexity_trs=="2"){
                    
                }
                
            # Main effect requested
            } else{
                shinyjs::hide("tdeTitle1")
                shinyjs::show("mainTitle1")
                
                shinyjs::hide("tdeTitle2")
                shinyjs::show("mainTitle2")
                    
                # tr2 header if two stages
                if(input$complexity_trs=="2"){
                    
                }
            }
        }
        
    })

    # HELPER: output idxs ====
    output$scenIdx_h0 <- renderText({
        idx <- sapply(intersect(scenMaster, scenIdxs[[paste0(input$complexity_trs, ", ",
                                                      input$complexity_phViol, ", ",
                                                      input$DGP)]]),    # find the relevant scenarios by checking the canonical scen nums in 
                                                                        # scenIdxs with scenMaster (and keep the entries that appear in both vectors)...
                           function(x) which(scenMaster==x))  
        return(unlist(idx))
    })
    
    # OUTPUT: fluidRow w/lightbox gallery ====
    output$images <- renderUI({
        
        regExExpr <- intersect(scenMaster, scenIdxs[[paste0(input$complexity_trs, ", ",
                                                            input$complexity_phViol, ", ",
                                                            input$DGP)]]) %>%
                     paste0(., collapse="|") %>%
                     paste0(" (", ., ")\\.")
        
        # Get list
        imgs <- list.files('../www/img') %>% 
                    .[grep(regExExpr, .)] %>%
                    data.frame(fName=., scNum=.)
        imgs$scNum <- gsub(".png", "", imgs$scNum) %>%
                        gsub("baseline haz - scenario ", "", .)
        
        # Sort on scenario number
        imgs <- imgs[order(as.numeric(imgs$scNum)),]
        
        # Generate unique hashes required for lightbox
        imgs <- imgs %>% 
                    rowwise %>%
                    mutate(uid = hashids::encode(1e3 + as.numeric(scNum)*10, 
                                                hashid_settings(salt = 'theSalt')),
                           src = paste0(fName),
                           caption = paste0("Scenario ", scNum))
    
        # Convert the PH viol to 0/1
        ifelse(input$complexity_phViol=="Yes", {ph01 <- 1}, {ph01 <- 0})
        
        # Do the fluidRow
        fluidRow(style="margin-left:-15px;",
            lightbox_gallery_mod(imgs, paste0('gallery', input$complexity_trs,
                                                         ph01, input$DGP),
                                 caption = imgs$caption, display = TRUE) 
        )

    })
    
    # OUTPUT: scenario descriptions ====
    output$scenRefList <- renderUI({
        # Get the scenario names ready for a LaTeX align env
        str <-  lapply(names(scenMaster), 
                      function(x){ gsub("\\(", "} &\\( \\\\text{", x)} # align on start of descriptor (and close \text from 
                ) %>%                                                  # start of string, begin \text for end)    
                lapply(., 
                      function(x){ gsub("\\)", "} \\)", x)}            # end second text chunk
                ) %>%
                lapply(., function(x) { paste0("\\text{", x)}) %>%     # open first text chunk to print as text, not math
                unlist 
        
        # Get relevant indices
        idxs <- sapply(intersect(scenMaster, scenIdxs[[
                                                  paste0(input$complexity_trs, ", ",
                                                  input$complexity_phViol, ", ",
                                                  input$DGP)    # find the relevant scenarios by checking the canonical scen nums in 
                                              ]]),              # scenIdxs with scenMaster (and keep the entries that appear in both vectors)...
                                                                    
                       function(x) which(scenMaster==x))        # ...then get the index positions of those relevant scenarios in scenMaster
        
        # Subset str
        str <- str[idxs]
            
        # Add line breaks
        str <- paste0(str, collapse = " \\\\ ")
        
        # Insert the MJ + align tags
        str <- paste0("\\( \\begin{align}", str, " \\end{align} \\)")
        
        return(withMathJax(str))
    })
    
    # OUTPUT: TDE graph titles ====
    output$tdeTitle1 <-  renderText({
        paste0("Scenario ", megaFName()[[4]][[1]])
    })

    output$tdeTitle2 <-  renderText({
        paste0("Scenario ", megaFName()[[4]][[2]])
    })


    # OUTPUT: load the graphs ====
    ## Was planning to do all this with modules, but the reactives were updating
    ## funny for a not-easily-found reason.  Brute forcing was faster.
    output$coeff <- renderImage({
        filename <- cleanPath(findDir(), megaFName()[[1]])
        list(src = filename, width = 634, height = 461, alt="Image not outputting.")
    }, deleteFile=FALSE)
    output$bias <- renderImage({
        filename <- cleanPath(findDir(), megaFName()[[2]])
        list(src = filename, width = 634, height = 461, alt="Image not outputting.")
    }, deleteFile=FALSE)
    output$rmse <- renderImage({
        filename <- cleanPath(findDir(), megaFName()[[3]])
        list(src = filename, width = 634, height = 461, alt="Image not outputting.")
    }, deleteFile=FALSE)

    output$x_neg <- renderImage({
        filename <- cleanPath(findDir(), megaFName()[1])
        list(src = filename, width = 634, height = 461, alt="Image not outputting.")
    }, deleteFile=FALSE)
    output$x_0 <- renderImage({
        filename <- cleanPath(findDir(), megaFName()[2])
        list(src = filename, width = 634, height = 461, alt="Image not outputting.")
    }, deleteFile=FALSE)
    output$x_pos <- renderImage({
        filename <- cleanPath(findDir(), megaFName()[3])
        list(src = filename, width = 634, height = 461, alt="Image not outputting.")
    }, deleteFile=FALSE)

    ## OUTPUT: load h4 titles for graphs ====
    output$x_negH4 <- renderUI({
        HTML(paste0("<h4><em>x</em> = ", xVals()[1], "</h4>"))
    })
    output$x_0H4 <- renderUI({
        HTML(paste0("<h4><em>x</em> = ", xVals()[2], "</h4>"))
    })
    output$x_posH4 <- renderUI({
        HTML(paste0("<h4><em>x</em> = ", xVals()[3], "</h4>"))
    })

    # OUTPUT: link to log file ====
    output$logFileLink <- downloadHandler(   
        filename = function(){
            paste0(fNameBuild())  #what the saved file's default name should be
        },
        content = function(file){ #what the existing file's name is, on the server
            file.copy(paste0(cleanPath(findDir(),fNameBuild())), file)   
            ## Takes a few seconds for the file to be ready.  If you click before that, no dice.
        }
    )
    # NOTE: the default file name will be as expected if you're running the app
    # from a browser.  If you're running it from an R viewer window, the file
    # will be extensionless and named after the button object. The from-R-viewer
    # downloaded file still contains the requested output and is readable if you
    # add ".log" to the end.


    # OUTPUT: log file download hack ====
    output$dwnldLink <- renderUI({
        input$scopeCond
        names <- names(scopeList)
        paste0(
            "Done!  The following ",
            ifelse(input$simset == 3, "scope ", ""),
            "log is ready for download: ",
            ifelse(input$simSet == 3, 
                    names[scopeList == as.integer(input$scopeCond)], 
                    "omnibus log (all scenarios)")
        )
    })

}
