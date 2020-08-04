# Popup window with gallery of h0(t)s - list of images to display gets updated
# in server.R (output$images), based on (1) which set of sims, (2) the DGP, and
# (3) the sim scen's complexity (# transits, PH viol?)

#*****************************

bsModal("h0Gallery", "\\(h_0(t)\\) Gallery", "gallery", size="medium",
    div(style="text-align: center;",
        
        div(class="bs-callout bs-callout-info", style="display: inline-block; width: 50%; text-align:left;",
                    HTML("<span style='font-weight:bold; font-size:1.15em;'>
                            <span style='color:#5bc0de;'><i class='fas fa-question-circle'></i></span>&nbsp;&nbsp;
                         Click to enlarge the image</span>")
                ),
        
        uiOutput("images")
        
    )
)