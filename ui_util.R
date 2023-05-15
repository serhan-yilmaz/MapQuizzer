tipify_customwidth <- function(el, tooltip = NULL, tooltip_width = NULL, identifier = NULL){
  out = el
  if(!is.null(tooltip)){
    out = tipify(out, tooltip)
    if(!is.null(tooltip_width) && !is.null(identifier)){
      tooltip_div = paste0(identifier, "_tooltipdiv")
      css = multigsub(c("ELEMENT_NAME", "TOOLTIP_WIDTH"), c(tooltip_div, tooltip_width), 
                      "#ELEMENT_NAME > .tooltip > .tooltip-inner {
          min-width: TOOLTIP_WIDTH;
        }"
      )
      out = tags$div(
        id = tooltip_div, 
        out, 
        tags$style(HTML(css))
      )
    }
  }
  return(out)
}

multiChoicePicker <- function(id, label, choices, selected = choices[1], 
                              isInline = "T", multiple = F, max_opts = 2, 
                              max_opts_txt = "No more!", width = "fit", 
                              style = NULL, style_label = NULL, style_picker = NULL,
                              style_choices = NULL, tooltip_width = NULL, 
                              picker_inline = T, class_names = NULL, tooltip = NULL) {
  
  if(!is.null(style_choices)){
    choicesOpt = list(style=c(rep(style_choices, length(choices)))) 
  } else {
    choicesOpt = NULL 
  }
  
  picker_ui <- shinyWidgets::pickerInput(id, NULL, choices, selected = selected, 
                                         width = width, inline = picker_inline, 
                                         multiple = multiple,
                                         choicesOpt = choicesOpt, 
                                         options = pickerOptions(
                                           maxOptions = max_opts,
                                           maxOptionsText = max_opts_txt
                                         ))
  if(!is.null(style_picker)){
    picker_ui$attribs$style = paste0(picker_ui$attribs$style, style_picker)
  }
  
  label_el = tags$b(label, style = style_label)
  label_el = tipify_customwidth(label_el, tooltip, tooltip_width, id)
  # if(!is.null(tooltip)){
  #   label_el = tipify(label_el, tooltip)
  # }
  
  switch(isInline, 
         "T" = R <- tags$div(
           class = "inline-block", 
           class = class_names,
           id = paste(id, "_div", sep = ""), 
           style = "justify-content: space-between;", 
           style = style,
           label_el, 
           picker_ui
         ),
         "F" = R <- tags$div(
           class = class_names, 
           id = paste(id, "_div", sep = ""), 
           style = style,
           label_el, 
           #selectInput(id, label, choices, selected = selected, width = "auto")
           picker_ui
         )
  )
  
  # if(!is.null(tooltip)){
  #   R = tipify(R, tooltip)
  # }
  
  return (R)
}

preparePickers <- function(extra = "", difficulty = "Hard", numcountries = "All"){
  view_picker <-  multiChoicePicker(paste0("viewpoint", extra), "View:", c("All", "Europe", "Africa", "Asia", "Oceania", "Middle East", "North America", "Middle America", "South America"), 
                                    selected = "All", isInline = "F", 
                                    multiple = F, width = "auto", 
                                    style = "display:inline-flex; align-items:center; margin-right:1vw;", #max-height:20px;
                                    # style = "margin-bottom:6px;display:inline-block;margin-right:1vw;", #max-height:20px;
                                    style_picker = "display:inline-flex;align-items:center; margin-bottom:0px; margin-left: 0.5vw;", 
                                    picker_inline = F)
  
  whattoask_picker <-  multiChoicePicker(paste0("whattoask", extra), "What to Ask:", c("Country", "Capital", "Flag", "Riddle"), 
                                         selected = "Country", isInline = "F", 
                                         multiple = F, width = "fit", 
                                         style = "margin-bottom:0px;",
                                         picker_inline = T)
  
  riddlefocus_picker <-  conditionalPanel(sprintf("input.%s == 'Riddle'", paste0("whattoask", extra)), 
                                    multiChoicePicker(
                                         paste0("riddlefocus", extra), 
                                         "Riddle Focus:", c("Balanced", "Politics", "Geography", "Economy", "Demographics"), 
                                         selected = "Country", isInline = "F", 
                                         multiple = F, width = "fit", 
                                         style = "margin-bottom:0px;",
                                         picker_inline = T))
  
  continent_picker <-  multiChoicePicker(paste0("country_continent", extra), "Continent:", c("All", "Europe", "Africa", "Asia", "Oceania", "North America", "South America"), 
                                         selected = "Country", isInline = "F", 
                                         multiple = F, width = "fit", 
                                         style = "margin-bottom:0px;",
                                         picker_inline = T)
  
  numofcountries_picker <-  multiChoicePicker(paste0("num_countries", extra), "Number of Countries:", c("All", "50", "20", "10", "5", "2", "1"), 
                                              selected = numcountries, isInline = "F", 
                                              multiple = F, width = "fit", 
                                              style = "margin-bottom:0px;",
                                              picker_inline = T)
  
  difficulty_picker <-  multiChoicePicker(paste0("country_difficulty", extra), "Difficulty:", c("Easy", "Moderate", "Hard"), 
                                          selected = difficulty, isInline = "F", 
                                          multiple = F, width = "fit", 
                                          style = "margin-bottom:0px;",
                                          picker_inline = T)
  return(list(view_picker = view_picker, whattoask_picker = whattoask_picker, 
              numofcountries_picker = numofcountries_picker, difficulty_picker = difficulty_picker,
              continent_picker = continent_picker, riddlefocus_picker = riddlefocus_picker))
}


fancyCheckbox <- function(identifier, label, default = F, status = "warning", tooltip = NULL, style = NULL, tooltip_width = NULL){
  out = tags$div(
    style = "display:flex; align-items:center;", 
    style = style, 
    class = "custom-materialSwitch", 
    tags$b(label), 
    shinyWidgets::materialSwitch(inputId = identifier, label = "", status = status, value = default, inline = T)
  )
  
  out = tipify_customwidth(out, tooltip, tooltip_width, identifier)
  return(out)
}


