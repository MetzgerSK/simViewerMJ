# Shiny app to make it easier organize and view simulation results from Metzger
# and Jones, "Getting Time Right"
## First version: 19NOV18
## Last update: 09NOV20

#*****************************************************************

## >>>>>> UI CODE <<<<<< --------
ui <- fluidPage(

    # Font
    tags$head(
    tags$style(HTML(
       "@import url('//fonts.googleapis.com/css?family=PT+Sans:400,400italic,500,500italic,700,700italic');"  # Google Font: PT Sans
    ))),
    
    # Load up CSS
    includeCSS("style.css"),
    includeCSS("bsCallout.css"),
    
    # Enable
    withMathJax(),
    useShinyjs(),
    
    # Header (initially written w/footer tag, but the HTML doesn't care where the content actually appears.)
    HTML("<footer><main>For: \"Getting Time Right,\" by Shawna K. Metzger and Benjamin T. Jones</main>
         <note>(full cite will be inserted)</note></footer>"),
    
    # Title
    titlePanel("Simulation Graphs"),

    ## Left Panel ----
    sidebarLayout(
        sidebarPanel(
            # Which set of simulations do we want to view?
            selectInput("simSet", 
                          label = "Simulation Sets",
                          choices = list("Cox vs. L/P: Coeffs" = 1,
                                         "Cox vs. L/P: Pred Quants" = 2,
                                         "Cox: Scope Conditions" = 3,
                                         "Cox vs. L/P: Testing for PH" = 4), 
                          selected = 1
                        ),

            # number of observations
            conditionalPanel(
                condition = "input.simSet < 3",
                selectInput("nObs", label = "Sample Size",
                              choices = list(100,
                                             250,
                                             500), selected = 250
                            )
            ),

            # If we're in 1 or 2, ask which DGP
            conditionalPanel(
                condition = "input.simSet == '1' || input.simSet == '2'",        
                selectInput("DGP", label = "DGP",
                                choices = list("Continuous" = 1,
                                               "Coerced Start-Stop" = 2,
                                               "Discrete" = 3), 
                                selected = 1
                            )
            ),

            # DGP complexity
            conditionalPanel(
                condition = "input.simSet == '1' || input.simSet == '2'",
                
                div(style = "margin-left:-8px;", 
                    HTML( "<i class=\"fas fa-cubes fa-lg\"></i> 
                          <span style='font-weight: bold; font-family:\'PT Sans\';'>DGP Complexity:</span>")),
                fluidRow(
                    column(6,
                        div(class="inline-block-center",
                            radioGroupButtons(
                               inputId = "complexity_trs",
                               label = HTML("<span style='font-size:90%; color:#595959;'>Number of Transitions?</span>"),
                               choices = c("1", "2"),
                               selected = "1"
                            )
                        )
                    ),
                    column(6,
                        div(class="inline-block-center",   
                            radioGroupButtons(
                               inputId = "complexity_phViol",
                               label = HTML("<span style='font-size:90%; color:#595959;'>PH Violation?</span>"),
                               choices = c("No", "Yes"),
                               selected = "No"
                            )
                        )
                    )
                )
            ),

            # Log selection
            conditionalPanel(
                condition = 'input.simSet == 3',
                selectInput("scopeCond", label = "Scope Conditions",
                                          choices = scopeList, selected = 1)
            ),

            # Baseline hazard functional form
            conditionalPanel(
                condition = "input.simSet==2",
                selectInput("h0", label = "Baseline Hazard \\( \\left( h_0\\!\\left(t\\right) \\right) \\) (Transition 1)",
                    choices = list("LOTS"=1, "Because we"=2, "have quite a few"=3, "possibilities"=4), selected=1)
            ),
            
            # If we're doing the PH violation scenarios, get h0(t)/DGP info about tr2
            conditionalPanel(
                condition = "(input.simSet == '1' || input.simSet == '2') && 
                              input.complexity_trs == '2' && input.complexity_phViol == 'Yes' &&
                              input.DGP == 3",  

                selectInput("h0_tr2", label = "Baseline Hazard \\( \\left( h_{0 \\left[\\text{tr}2\\right]}\\!\\left(t\\right) \\right) \\) (Transition 2)",
                    choices = list("Monotonically Increasing"=7, "Monotonically Decreasing"=9), selected=7),

                p( em("Transition 2's link function is same as transition 1's."),
                   style = "color: #999; font-size: 73%; margin-top: -15px;") 
            ),
            conditionalPanel(
                condition="(input.simSet == '1' || input.simSet == '2') &&
                            input.complexity_trs == '2' && input.DGP == 2",
                
                selectInput("sh2", label = "Shape Parameter \\( p \\) (Transition 2)",
                    choices = list("0.75 (Monotonically Decreasing)" = 0.75, "1.25 (Monotonically Increasing)"=1.25),
                    selected=0.75)
            ),
            
            # If we're doing the PH violation scenarios, get x's main and TVC
            conditionalPanel(
                condition = "(input.simSet == '1' || input.simSet == '2') && 
                            !(input.complexity_trs == '1' && input.complexity_phViol == 'No')",
                
                selectInput("x1", label = HTML("<em>x</em>'s Main Effect (Transition 1)"),
                            choices = list(-0.5 , -0.2, -0.05, 0, 0.05, 0.2, 0.5),
                            selected = -0.2
                ),
                conditionalPanel(
                    condition = "input.complexity_phViol == 'Yes'",
                    selectInput("tde1", label = HTML("<em>x</em>'s Time-Varying Effect (Transition 1)"),
                                choices = list(-0.2, -0.05, 0, 0.05, 0.2),
                                selected = -0.2
                    )
                )
            ),
            conditionalPanel(
                condition = "(input.simSet == '1' || input.simSet == '2') &&
                             input.complexity_trs == '2'",
                selectInput("x2", label = HTML("<em>x</em>'s Main Effect (Transition 2)"),
                            choices = list(-0.5 , -0.2, -0.05, 0, 0.05, 0.2, 0.5),
                            selected = -0.2
                ),
                conditionalPanel(
                    condition = "input.complexity_phViol == 'Yes'",
                    selectInput("tde2", label = HTML("<em>x</em>'s Time-Varying Effect (Transition 2)"),
                                choices = list(-0.2, -0.05, 0, 0.05, 0.2),
                                selected = -0.2
                    )
                )
            ),
            # Print out a list of the scenario descriptors
            conditionalPanel(
                condition ="(input.simSet == '1' || input.simSet == '2')",
                HTML("<hr style='border-color: #999'>
                      <h4 style='margin-top: 0px; margin-left:-8px;
                                 font-size:1.2em; font-weight:bold;'>
                         Scenario Key (Baseline Hazard, Tr. 1)
                     </h4>"),
                uiOutput("scenRefList"),
                br(),
                div(class="inline-block-center",
                    actionButton("gallery", "View \\(h_0(t)\\)s", class = "btn-info")
                ),
                source("uiPt__popup_h0Gallery.R", local=TRUE)$value       # load the popup's code      
            )
        ),

        ## Right Panel ====
        mainPanel(
            ## Both coeffs + trPrs need image enlarge FYI
            conditionalPanel(
                condition = "input.simSet == 1 | input.simSet == 2",
                
                div(style="text-align: center;",
                div(class="bs-callout bs-callout-info", style="display: inline-block; width: 55%;",
                    HTML("<span style='font-weight:bold; font-size:1.15em;'>
                            <span style='color:#5bc0de;'><i class='fas fa-question-circle'></i></span>&nbsp;&nbsp;
                         Hover over an image to enlarge.</span>")
                ))
            ),
            
            ## ~~ RMSE et al., if coefficient plot ~~
            # More expedient to brute force it.
            conditionalPanel(
                condition = "input.simSet == 1",
                
                # For causal complexity, give further options about which x effect and which transition to display
                ## Any TDE: tr1 will need to show
                conditionalPanel(
                    condition = "input.complexity_phViol == 'Yes' ",
                    radioGroupButtons(
                       inputId = "advTrEff", label = "Display \\(x\\)'s main effect or time-varying effect?",
                       choices = c("Main", "Time-Varying"),
                       selected = "Main",
                       checkIcon = list(
                          yes = icon("ok",
                          lib = "glyphicon")),
                       justified = TRUE,
                       status = "primary"
                    ),
                ),
                # If also classic ccPH, tr2 will need to show, so give second set of buttons
                conditionalPanel(
                    condition = "input.complexity_trs == '2'",
                    
                    radioGroupButtons(
                       inputId = "advTrSelect", label = "View \\(x\\)'s effect for which transition?",
                       choices = c("Transition 1", "Transition 2"),
                       selected = "Transition 1",
                       checkIcon = list(
                          yes = icon("ok",
                          lib = "glyphicon")),
                       justified = TRUE
                    )
                ),
                
                # Begin the regular right panel content
                h3("Estimates of \\( \\beta_x\\)"),
                
                h4(id="mainTitle1", "Coefficient Estimates"),
                h4(hidden(textOutput("tdeTitle1"))), 
                imageOutput("coeff", height="auto"),
                br(),
                
                h4(id="mainTitle2", "Bias"),
                h4(hidden(textOutput("tdeTitle2"))), 
                imageOutput("bias", height="auto"),
                conditionalPanel(
                    condition = "input.simSet == '1' && 
                                    (input.complexity_trs==1 && input.x1!=0) ||
                                    (input.complexity_trs==2 && 
                                     (input.advTrSelect == 'Transition 1' && input.x1!=0) ||
                                     (input.advTrSelect == 'Transition 2' && input.x2!=0)
                                    )
                                ",
                    div(id="pBiasNote", class="bs-callout bs-callout-warning",
                        style="font-size: 1.05em; line-height: 1.5; width: 750px;",
                        HTML("<p><span style='font-variant:small-caps;'><strong>Note</strong></span>:
                              a value of 1 on <i>y</i>-axis corresponds to 100% bias (absolute value).  The estimate is, 
                              on average, off from the truth by a magnitude of 2.</p> 
                              <p style='font-size:90%'>
                                <strong>Example</strong>: if \\(\\beta\\) = -0.2, a \\(\\hat{\\beta}\\) with a bar height equal to 1 on the <i>y</i>-axis 
                                would correspond to a mean \\( \\left(\\hat{\\beta} - \\beta \\right)\\)  of -0.4 <i>or</i> 0.</p>")
                    )
                ),
                
                conditionalPanel(
                    condition="(input.complexity_trs=='1' && input.complexity_phViol=='No') ||
                               (input.complexity_trs=='1' && input.complexity_phViol=='Yes' &&
                               input.advTrEff=='Main') ||
                               (input.complexity_trs=='2' && input.complexity_phViol=='No') ||
                               (input.complexity_trs=='2' && input.complexity_phViol=='Yes' &&
                               input.advTrEff=='Main')",
                    br(),
                    h4("RMSE"),
                    imageOutput("rmse", height="auto")
                ),
            ),

            # ~~ trans probs ~~
            conditionalPanel(
                condition = "input.simSet == 2",
                
                div(class="zoomFYI"),
                
                h4("Transition Probabilities for Different Covariate Profiles"),
                
                uiOutput("x_negH4"),
                imageOutput("x_neg", height="auto"),
                br(),
                
                uiOutput("x_0H4"),
                imageOutput("x_0", height="auto"),
                br(),
                
                uiOutput("x_posH4"),
                imageOutput("x_pos", height="auto")
            ),

            ## ~~ download button for scope or PH logs ~~
            conditionalPanel(
                condition = "input.simSet == 3 || input.simSet == 4",
                downloadButton("logFileLink", label = "Log File (.txt)", icon("file-download"),
                               style="color: #fff; background-color: #546912; border-color: #10800c;"
                ),
                HTML("<p>May take a moment to prepare the file for download&mdash;wait for 'Done!' message to appear in black below.</p>"),
                br(),
                uiOutput("dwnldLink")
            ),
            br()
        )
    )
)

