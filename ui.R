#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyjqui)
library(shinyjs)
library(shinytoastr)
library(shinyWidgets)
library(shinybrowser)
library(tippy)
library(bslib)

library(shinycssloaders) # - Dependency

source("ui_util.R")

dropdown_options_alt <- function(content, titletxt = "Options", 
                                 tooltip = "Click to see options.", 
                                 width = NULL, style_inner_div = NULL){
  # style = "max-width: 70px; float: right; vertical-align: top; right: 0%; top:0px; z-index:100"
  tags$div(class = "dropdown_div", 
           dropdown(
             size = "md", 
             icon = icon("cog"), #status = "info", 
             right = F, 
             up = T, 
             width = width, 
             tooltip = tooltipOptions(title = tooltip, placement = "top"), 
             tags$div(
               style = style_inner_div, 
               tags$h4(style = "font-weight:bold; margin-bottom: 10px; white-space: nowrap;", titletxt), 
               content, 
               tags$p(style = "margin-bottom: 10px;", "")
             )
           ))
}


pickers = preparePickers()
view_picker = pickers$view_picker
riddlefocus_picker = pickers$riddlefocus_picker
whattoask_picker = pickers$whattoask_picker
numofcountries_picker = pickers$numofcountries_picker
difficulty_picker = pickers$difficulty_picker
continent_picker = pickers$continent_picker

options_dropdown <- tags$span(style = "font-size: 80%", 
  dropdown_options_alt(
    style_inner_div = "max-height: calc(60vh + 50px); overflow:auto;", 
    tags$div(
    whattoask_picker, 
    riddlefocus_picker, 
    numofcountries_picker,
    continent_picker, 
    difficulty_picker,
    
    tags$div(uiOutput("requirerestart", inline = TRUE),
             actionButton("restartButton", "Restart", style = "font-size:100%"))
    ),  width = "calc(30vw + 100px)"),
)

# Define UI for application that draws a histogram
fluidPage(
    shinybrowser::detect(),
    useToastr(),
    useShinyjs(),
    theme = bs_theme(version = 5, font_scale = 0.85),
    
    # Application title
    titlePanel("MapQuizzr"),
    
    tags$head(
      includeCSS("www/style.css"),
      includeScript(path = "www/script.js"),
      tags$meta(name = "viewport", content = "width=device-width, initial-scale=1, maximum-scale=8, user-scalable=1")
      # tags$meta(name = "viewport", content = "width=device-width, initial-scale=1, maximum-scale=1, user-scalable=0")
    ),
    tags$div(fluidRow(
             column(4, class = "footercol", 
              tags$div(view_picker, options_dropdown, style = "margin-bottom:calc(0.1vh + 2px); margin-top:calc(0.1vh + 2px); max-height:calc(25px + 3vh); display:flex; align-items:center; justify-content: center;")
             ),
             column(4, class = "footercol", 
                    uiOutput("whereis_text", inline = TRUE)
             ),
             column(4, class = "footercol", style = "display:flex; justify-content: flex-end; align-items:center;",
                    tags$div(uiOutput("progress_text", inline = TRUE), style = "display:inline-block; padding-right: calc(1vw + 2px);"),
                    tags$div(actionButton("revealButton", "Reveal", style = "font-size:105%"), class = "revealbutton-div"),
                    tags$div(actionButton("hintButton", "Hint", style = "font-size:105%"), class = "revealbutton-div"),
             ),
             # tags$div(, class="footerleft"),
             # tags$div("-", style = "color:rgba(0, 0, 0, 0); float:left; padding-left: 10vw"), 
             # tags$div(class = "footercenter", uiOutput("whereis_text", inline = TRUE)),
             # tags$div(class = "footerright", 
             # tags$div(uiOutput("progress_text", inline = TRUE), style = "padding-right: calc(1vw + 2px);"),
             # tags$div(actionButton("revealButton", "Reveal", style = "font-size:105%"), class = "revealbutton-div"),
             # ),
             ), id="loadmessage"),
    
    # tags$div(tags$div(tags$div(view_picker, dropdown_options_alt("abcd")), class="footerleft"),
    #          # tags$div("-", style = "color:rgba(0, 0, 0, 0); float:left; padding-left: 10vw"), 
    #          tags$div(class = "footercenter", uiOutput("whereis_text", inline = TRUE)),
    #          tags$div(class = "footerright", 
    #                   tags$div(uiOutput("progress_text", inline = TRUE), style = "padding-right: calc(1vw + 2px);"),
    #                   tags$div(actionButton("revealButton", "Reveal", style = "font-size:105%"), class = "revealbutton-div"),
    #          ),
    #          id="loadmessage"),
    # 
    # tags$div(, id="footerright"),
    
    # tags$div("Loading ", tags$span(class = "loadingbar-custom"), id="loadmessage"), 
    
    
    fluidRow(
      jqui_resizable(
        tags$div(id = "map_div", 
                              style = "touch-action: manipulation;", 
                              style = "min-height:100px; height:500px; display:flex; 
                              align-items: stretch; overflow: hidden; margin-bottom:8vh; padding-left: 0px;", 
      # tags$img(src = "wmap4.png", id = "imgx")
      # shinycssloaders::withSpinner(
        tags$div(id = "imagediv", style = "height:100%; width:100%;", 
                 style = "touch-action: manipulation;", 
                 tags$div(id = "image", style = "width:100%; height:100%;touch-action: manipulation;",
                   tags$img(id = "imgx", src = "wmap4_2xres_processed.png", 
                            style = "-webkit-user-drag: none;touch-action: manipulation;", #image-rendering: pixelated;
                            class = "nocursor", width = "1600", height = "900")
                   ),
                 
        #          imageOutput("image", height = "100%",
        #           click = "image_click",
        #           # style = "cursor:none;"
        #           # hover = hoverOpts(
        #           #   id = "image_hover",
        #           #   delay = 500,
        #           #   delayType = "throttle"
        #           # )
        #           # )
        # ),
        tags$div(
          class = "searchingcircle"
        ),
        
      ),
      tags$div(class = "coordtext",
               actionLink("developer_info_button", "", icon = icon("info"), 
                          style = "pointer-events: auto; font-size: calc(0.97 * var(--coordtext-fontsize)); margin-right:calc(0.4vw + 1px);"),
               tags$span(
                 id = "coordtext", 
                 "(345, 674)"
               ),
               
      ),
      # tags$div(
      #   id = "coordtext", 
      #   class = "coordtext", 
      #   "(345, 674)"
      # ),
      tags$div(
        tags$div(class = "horizontal-plus"), 
        tags$div(class = "vertical-plus"), 
        # tags$span("+", class = "customcrosshair"), 
        id = "cursor", 
        class = "cursor"), 
      )
      ), 
      tags$div("-", style = "height:1px;color: rgb(255, 255, 255, 0.01);pointer-events: none;font-size:1px;")
    ),
    
    # Sidebar with a slider input for number of bins
    # sidebarLayout(
    #     sidebarPanel(
    #         sliderInput("bins",
    #                     "Number of bins:",
    #                     min = 1,
    #                     max = 50,
    #                     value = 30)
    #     ),
    # 
    #     # Show a plot of the generated distribution
    #     mainPanel(
    #       verbatimTextOutput("image_clickinfo"),
    #     )
    # ),
)
