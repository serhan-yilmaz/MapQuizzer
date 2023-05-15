#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinytoastr)
library(shinyalert)
library(pracma)
library(stringr)
source("map_data.R")
source("ui_util.R")

foCheckPosition <- function(x, y){
  out = as.numeric(as.logical(x) & FALSE);
  for(iCountry in (1:map_num_country)){
    val = map_list[[iCountry]](x, y);
    out = pmax(out, val * iCountry)
  }
  out[out == 0] = NA
  return(out)
}

foGetCountriesByDifficulty <- function(difficulty){
  switch(difficulty, 
         "Easy" = {out <- (map_areas >= 1000)},
         "Moderate" = {out <- (map_areas >= 200)},
         "Hard" = {out <- (map_areas >= 0)},
         stop("Invalid difficulty option.")
  )
  return(out)
}
foGetCountriesByContinent <- function(continent){
  if(identical(continent, "All")){
    return(rep(TRUE, map_num_country))
  }
  out = map_continents == continent
  return(out)
}

Tmisc = read.csv("map_data_misc.csv");

foGetCategoryTitle <- function(cat){
  cat_title = str_to_title(cat)
  if(identical(cat, "expectancy_un")){
    cat_title = "Life Expectancy"
  }
  if(identical(cat, "forest")){
    cat_title = "Forest Percentage"
  }
  
  if(identical(cat, "agriculturalarea")){
    cat_title = "Agricultural Area Percentage"
  }
  
  if(identical(cat, "gdp")){
    cat_title = "Economy (GDP)"
  }
  
  if(identical(cat, "density_un")){
    cat_title = "Density"
  }
  
  if(identical(cat, "population_un")){
    cat_title = "Population"
  }
  
  if(identical(cat, "sexratio")){
    cat_title = "Sex Ratio (M/F)"
  }
  
  if(identical(cat, "gdppercapita")){
    cat_title = "Wealth (GDP per capita)"
  }
  
  if(identical(cat, "youngpopulation")){
    cat_title = "Child Population (0-14 years)"
  }
  
  if(identical(cat, "oldpopulation")){
    cat_title = "Senior Population (60+ years)"
  }
  
  if(identical(cat, "location")){
    cat_title = "Region"
  }
  
  if(identical(cat, "energyusage")){
    cat_title = "Energy Consumption (per capita)"
  }
  
  if(identical(cat, "energyexportratio")){
    cat_title = "Energy Export (relative to production)"
  }
  
  return(cat_title)
}

all_game_categories = c(
               'continent', 'location', 
               'independence_category', 'government_category', 'religion_category',
               'area_category', 'population_un_category', 
               'density_un_category', 'expectancy_un_category', 'sexratio_category', 
               'youngpopulation_category', 'oldpopulation_category', 
               'gdp_category', 'gdppercapita_category', 'currency_category', 
               'energyusage_category', 'energyexportratio_category', 
               'landlocked_category', 'equator_category', 
               'temperature_category', 'forest_category', 
               'agriculturalarea_category'
               );

nGameCategory = length(all_game_categories)

numeric_categories = list('area' = list(unit = ' km2'), 
                       'population_un' = list(unit = ''), 
                       'density_un' = list(unit = ' people/km2'), 
                       'independence' = list(unit = '', modify = FALSE), 
                       'expectancy_un' = list(unit = ' years', modify = FALSE), 
                       'temperature' = list(unit = '&#8451;', modify = FALSE, digits = 1), 
                       'forest' = list(unit = '%'), 
                       'gdp' = list(unit = ' USD', multiply = 1e6, sigdigits = 3), 
                       'gdppercapita' = list(unit = ' USD'), 
                       'sexratio' = list(unit = '%'), 
                       'youngpopulation' = list(unit = '%'), 
                       'oldpopulation' = list(unit = '%'), 
                       'agriculturalarea' = list(unit = '%'), 
                       'energyusage' = list(unit = ' kWh/day', modify = FALSE), 
                       'energyexportratio' = list(unit = '%'))

riddlefocus_types = list(
  "Balanced" = list("Primary" = c(), "Secondary" = c()), 
  "Demographics" = list("Primary" = c('population_un_category', 'expectancy_un_category', 
                                      'sexratio_category', 'religion_category', 
                                      'youngpopulation_category', 'oldpopulation_category'), 
                        "Secondary" = c('gdppercapita_category', 'area_category', 
                                        'density_un_category', 'energyusage_category')), 
  "Economy" = list("Primary" = c('gdp_category', 'gdppercapita_category', 
                                 'currency_category', 'energyexportratio_category'), 
                   "Secondary" = c('government_category', 'area_category', 
                                   'population_un_category', 'energyusage_category')),
  "Geography" = list("Primary" = c('area_category', 'landlocked_category', 
                                   'equator_category', 'temperature_category', 
                                   'forest_category', 'agriculturalarea_category'), 
                     "Secondary" = c('continent', 'population_un_category', 
                                     'density_un_category', 'location')), 
  "Politics" = list("Primary" = c('government_category', 'independence_category'), 
                    "Secondary" = c('gdp_category', 'religion_category', 'currency_category'))
)

riddledifficulty_types = list(
  "Hard" = list("Primary" = c(), "Secondary" = c()), 
  "Moderate" = list("Primary" = c('continent', 'location', 'area_category', 'population_un_category', 
                              'currency_category', 'gdppercapita_category', 'landlocked_category', 'equator_category'), 
                "Secondary" = c('religion_category', 'expectancy_un_category', 
                                'temperature_category', 'gdp_category')), 
  "Easy" = list("Primary" = c('continent', 'location', 'area_category', 'population_un_category', 
                              'currency_category', 'gdppercapita_category', 'landlocked_category', 'equator_category'), 
                "Secondary" = c('religion_category', 'expectancy_un_category', 
                                'temperature_category', 'gdp_category'))
)


foAdjustRiddleFocus <- function(weights, focus, categories = all_game_categories, 
                                types = riddlefocus_types, 
                                primary_multiplier = 5, 
                                secondary_multiplier = 2){
  info = types[[focus]]
  primary = info$Primary
  secondary = info$Secondary
  primary_indices = which(!is.na(match(categories, primary)))
  secondary_indices = which(!is.na(match(categories, secondary)))
  weights[primary_indices] = weights[primary_indices] * primary_multiplier
  weights[secondary_indices] = weights[secondary_indices] * secondary_multiplier
  
  Tq = data.frame(category = categories, weights = weights)
  # browser()
  # message(Tq)
  
  # for(iCategory = 1:length(primary)){
  #   if(length(primary) <= 0){ break; }
  #   
  # }
  
  return(weights)
}

foGenerateRiddle <- function(country_index, focus=NULL, difficulty = NULL, 
                             newline = "<br>", additional = 0, randoms = NULL, 
                             showAll = FALSE, secondary_riddle = NULL, 
                             revealNumbers = FALSE){
  categories = all_game_categories
  weights = rep(1, length(categories))
  weights[match('continent', categories)] = 0.5
  weights[match('forest_category', categories)] = 0.65
  weights[match('agriculturalarea_category', categories)] = 0.45
  weights[match('sexratio_category', categories)] = 0.75
  weights[match('youngpopulation_category', categories)] = 0.4
  weights[match('oldpopulation_category', categories)] = 0.35
  weights[match('location', categories)] = 0.15
  weights[match('energyusage_category', categories)] = 0.55
  weights[match('energyexportratio_category', categories)] = 0.45
  
  if(is.null(focus)){
    focus = "Balanced"
  }
  
  if(is.null(difficulty)){
    difficulty = "Easy"
  }
  
  if(is.null(revealNumbers)){
    revealNumbers = FALSE
  }
  
  # browser()
  
  iseconomyfocus = identical(focus, "Economy");
  
  weights = foAdjustRiddleFocus(weights, focus)
  
  primary_multiplier = 5;
  secondary_multiplier = 2;
  ismoderate = F
  if(identical(difficulty, "Moderate")){
    primary_multiplier = sqrt(primary_multiplier);
    secondary_multiplier = sqrt(secondary_multiplier);
    ismoderate = T
  }
  
  weights = foAdjustRiddleFocus(weights, difficulty, types = riddledifficulty_types, 
                                primary_multiplier = primary_multiplier, secondary_multiplier = secondary_multiplier)
  
  if(!identical(difficulty, "Hard")){
    indices = match(c('continent', 'location'), categories)
    if(ismoderate){
      m = sqrt(5)
    } else {
      m = 5
    }
    weights[indices] = weights[indices] * m
    message(weights[indices])
  }
  
  if(is.null(randoms)){
    randoms = rand(100, 1);
  }
  random_start = 0
  
  rand_vals = weights * randoms[(1+random_start):(length(categories) + random_start)]
  permuted = order(rand_vals, decreasing=T)
  random_start = random_start + length(categories)
  if(showAll == TRUE){
    permuted = 1:length(categories)
  }
  
  marked = rep(F, length(categories));
  # permuted = randperm(length(categories));
  outstr = '';
  combined_string = rep('', map_num_country)
  cat_values = rep('', length(categories))
  nCategory = length(categories)
  additional_remaining = additional
  latest = ""
  
  for(iCategory in (1:length(categories))){
    # message(iCategory)
    index = permuted[iCategory]
    category = categories[index]
    values = Tmisc[[category]];
    # message(category)
    
    if(iseconomyfocus && identical(category, "currency_category")){
      values = Tmisc[["currency_category_specific"]];
    }
    value = values[country_index];
    cat_values[iCategory] = value
    
    out = str_split_1(category, "_category")
    cat = out[1]
    
    isnumeric = !is.na(match(cat, names(numeric_categories)));
    
    extra_str = '';
    # Numeric categories
    value_str = value
    if(isnumeric){
      numeric_value = Tmisc[[cat]][country_index];
      if(revealNumbers == TRUE){
        out = str_split_1(value, "\\[")
        cat_val = out[1]
        unit_val = numeric_categories[[cat]]$unit
        modify_val = numeric_categories[[cat]]$modify
        digits = numeric_categories[[cat]]$digits
        multiply_val = numeric_categories[[cat]]$multiply
        sigdigits = numeric_categories[[cat]]$sigdigits
        if(is.null(modify_val)){modify_val = TRUE;}
        if(identical(unit_val, '%')){modify_val = FALSE;}
        if(is.null(digits)){digits = 0}
        if(is.null(multiply_val)){multiply_val = 1}
        if(is.null(sigdigits)){sigdigits = 2}
        
        n_value = numeric_value * multiply_val
        n_postfix = ''
        if(modify_val){
          if(abs(n_value) >= 1e12){
            n_value = n_value / 1e12
            n_postfix = 'T'
          } else
          if(abs(n_value) >= 1e9){
            n_value = n_value / 1e9
            n_postfix = 'B'
          } else
          if(abs(n_value) >= 1e6){
            n_value = n_value / 1e6
            n_postfix = 'M'
          } else 
          if(abs(n_value) >= 1e3){
            n_value = n_value / 1e3
            n_postfix = 'K'
          }
          if(abs(n_value) < 1000){
            digits = pmax(sigdigits - 3, digits)
          }
          if(abs(n_value) < 100){
            digits = pmax(sigdigits - 2, digits)
          }
          if(abs(n_value) < 10){
            digits = pmax(sigdigits - 1, digits)
          }
          # message(n_value)
        }
        format_str = paste0('%s [%.', digits, 'f%s%s]');
        value_str = sprintf(format_str, cat_val, n_value, n_postfix, unit_val)
      } else {
        if(identical(cat, 'temperature')){
          value_str = gsub(' C', ' &#8451;', value_str)
        }
      }
      
      title = Tmisc[[paste0(cat, '_title')]][country_index];
      extra_str = paste0(extra_str, cat, ': ', num2str(numeric_value), newline);
      if(!identical(title, "")){
        out = str_split_1(title, "\\(")
        if(revealNumbers == FALSE){
          title = out[1]
        }
        value_str = paste0(value_str, ' - ', '<em>', title, '</em>'); 
      }
    }
    if(revealNumbers == TRUE){
      if(identical(cat, "currency") & (showAll == TRUE)){
        value_str = sprintf('%s (%s)', Tmisc$currency_name[country_index], Tmisc$currency_code[country_index])
      }
      if(identical(cat, "religion") & (showAll == TRUE)){
        value_str = Tmisc$religion[country_index]
      }
    }
    
    cat_title = foGetCategoryTitle(cat)
    
    latest = paste0("<b>", cat_title, "</b>", ': ', value_str)
    latest_str = latest;
    if(!is.null(secondary_riddle)){
      cats = secondary_riddle$sorted_categories[1:secondary_riddle$nCategory]
      isAsked = !is.na(match(category, cats))
      
      if(isAsked == TRUE){
        # browser()
        secondary_value = values[secondary_riddle$country_index];
        isEqual = identical(value, secondary_value)
        extrachar = ''
        if(isEqual){
          classx = 'correct-answer'
        } else {
          classx = 'incorrect-answer'
          if(isnumeric){
            numeric_value = Tmisc[[cat]][country_index];
            secondary_numeric_value = Tmisc[[cat]][secondary_riddle$country_index];
            islarger = numeric_value > secondary_numeric_value;
            if(islarger){
              # extrachar = '&#8679;';
              # extrachar = '&#x25B2;';
              extrachar = '&#x25BC;';
            } else {
              # extrachar = '&#8593;';
              # extrachar = '&#8681;';
              extrachar = '&#x25B2;';
            }
            extrachar =  paste0(' <b style = "font-size:95%;">', extrachar, '</b>');
          }
        }
        latest_str = paste0(sprintf("<span class = '%s'>", classx), latest_str, extrachar, "</span>")
      }
    }
    outstr = paste0(outstr, latest_str, newline)
    
    if(showAll == FALSE){
      if(iCategory == 1){
        combined_string = values;
      } else {
        combined_string = paste(combined_string, values, sep = '_');
      }
      u = unique(combined_string)
      ic = match(combined_string, u)
      out = histc(ic, 1:length(u))
      c = out$c
      numCountry = c[ic[country_index]];
    } else {
      numCountry = Inf
    }
    # if(revealNumbers == T){
    #   browser()
    # }
    if((numCountry <= 1) && (additional_remaining <= 0)){
      nCategory = iCategory
      break;
    }
    if(numCountry <= 1){
      additional_remaining = additional_remaining - 1
    }
  }
  cat_values = cat_values[1:nCategory]
  sorted_categories = categories[permuted]
  # message(paste0("numCountry: ", numCountry))
  out = list(outstr = outstr, country_index = country_index, 
             sorted_categories = sorted_categories, 
             cat_values = cat_values, nCategory = nCategory, 
             additional = additional, randoms = randoms, latest = latest)
  return(out)
}

foCheckCountryForRiddle <- function(riddle, country_index, newline = "<br>", fullExplanation=FALSE){
  nCategory = riddle$nCategory
  categories = riddle$sorted_categories
  values = riddle$cat_values
  response = TRUE
  explanation_text = ""
  initial_explanation = TRUE
  num_violation = 0;
  # browser()
  for(iCategory in (1:nCategory)){
    index = iCategory
    category = categories[index]
    all_values = Tmisc[[category]];
    value = all_values[country_index];
    out = str_split_1(category, "_")
    cat = out[1]
    cat_title = foGetCategoryTitle(cat)
    
    if(!identical(value, values[iCategory])){
      response = FALSE
      name = map_names[country_index]
      explanation = sprintf("- %s category of %s is: %s", cat_title, name, value)
      num_violation = num_violation + 1
      if(initial_explanation){
        initial_explanation = FALSE
        explanation_text = explanation
      } else {
        explanation_text = paste0(explanation_text, newline, explanation)
      }
      if(!fullExplanation){
        break;
      }
      # break;
    }
  }
  out = list(response = response, explanation_text = explanation_text, num_violation = num_violation)
  return(out)
}

# Define server logic required to draw a histogram
function(input, output, session) {

  # observe({
  #   a = input$image_click
  #   browser
  # })
  
  rep.row<-function(x,n){
    matrix(rep(x,each=n),nrow=n)
  }
  rep.col<-function(x,n){
    matrix(rep(x,each=n), ncol=n, byrow=TRUE)
  }
  
  output$image_clickinfo <- renderPrint({
    cat("Click:\n")
    str(input$image_click)
  })
  
  mapImgInitialized <- reactiveVal(FALSE)

  current_countries <- reactiveVal(randperm(map_num_country))
  country_index <- reactiveVal(1)
  reveal_counter <- reactiveVal(0)
  reveal_used <- reactiveVal(FALSE)
  
  wrong_guess_counter <- reactiveVal(0)
  current_wrong_guesses <- reactiveVal(list())
  
  hint_counter <- reactiveVal(0)
  hint_used <- reactiveVal(FALSE)
  
  additional_info_counter <- reactiveVal(0)
  
  current_country_index <- reactive({
    countries <- current_countries()
    return(countries[country_index()])
  })
  
  current_country <- reactive({
    name = map_names[current_country_index()]
    return(name)
  })
  
  current_capital <- reactive({
    name = map_capitals[current_country_index()]
    return(name)
  })
  
  foGetCountryAlpha2 <- function(index){
    return(tolower(alpha2_codes[index]))
  }
  
  current_country_alpha2 <- reactive({
    alpha2 = foGetCountryAlpha2(current_country_index())
    return(alpha2)
  })
  
  current_whattoask_option <- reactive({
    askFlag = FALSE
    askCapital = FALSE
    askRiddle = FALSE
    switch(input$whattoask, 
           "Country" = {},
           "Capital" = {askCapital <- TRUE;},
           "Flag" = {askFlag <- TRUE;},
           "Riddle" = {askRiddle <- TRUE;}, 
           stop("Invalid what to ask option.")
    )
    return(list(askFlag = askFlag, askCapital = askCapital, askRiddle = askRiddle))
  })
  
  observeEvent(input$whattoask, {
    if(current_whattoask_option()$askRiddle == TRUE){
      foCreateRiddle();
      foDisplayRiddle()
    }
  })
  
  # current_numcountries <- reactive({
  #   
  # })
  
  current_numcountries <- reactiveVal(map_num_country)
  current_difficulty <- reactiveVal("Hard")
  current_continent <- reactiveVal("All")
  
  foGetNumberOfCountries <- function(option){
    if(length(option) == 0){
      warning("Number of countries option is null, reverting to default...")
      return(map_num_country)
    }
    switch(option,
           "All" = map_num_country,
           "50" = 50, 
           "20" = 20, 
           "10" = 10,
           "5" = 5, 
           "2" = 2, 
           "1" = 1,
           stop("Invalid number of countries option.")
    )
  }
  
  foAdditionalRiddleInfo <- function(){
    # out = ""
    out = sprintf("Additionally, requested %d new information about riddles. \n", additional_info_counter())
    # if(additional_info_counter() > 0){
    #   out = sprintf("Additionally, requested %d new information about riddles.", additional_info_counter())
    # } 
    if(current_whattoask_option()$askRiddle == FALSE){
      out = ""
    }
    return(out)
  }
  
  foComputeAlertInfo <- function(charset = "!", correction = 0, newline = "\n", extra = ""){
    sprintf("You have found %d out of %d countries%s %s While using %d reveals, %d hints and %d wrong guesses. %s %s", country_index() + correction, current_numcountries(), charset, newline, reveal_counter(), hint_counter(), wrong_guess_counter(), foAdditionalRiddleInfo(), extra)
  }
  
  foHintAlert <- function(){
    shinyalert(
      title = "Need another Hint?",
      text = sprintf("%d hints are given so far. You have unlimited hints remaining.", hint_counter()),
      size = "s", 
      closeOnEsc = TRUE,
      closeOnClickOutside = FALSE,
      html = FALSE,
      type = "",
      showConfirmButton = TRUE,
      showCancelButton = TRUE,
      confirmButtonText = "Request New",
      confirmButtonCol = "#787CE6",
      cancelButtonText = "Show Previous",
      timer = 0,
      imageUrl = "",
      animation = TRUE,
      callbackR = function(x){if(x==T){foRequestHint();}else{foDisplayHint();}}
    )
  }
  
  observeEvent(input$hostname, {
    message(paste0("Hostname is: ", input$hostname[1]))
    message(paste0("URL is: ", input$hostname[2]))
  })
  
  observeEvent(input$developer_info_button, {
    current_width = shinybrowser::get_width()
    current_height = shinybrowser::get_height()
    dimension = input$dimension;
    
    line1 = paste0(sprintf("browser-dim: (%d, %d)", dimension[1], dimension[2]));
    line2 = paste0(sprintf("shinybrowser-dim: (%d, %d)", current_width, current_height));
    line3 = paste0(sprintf("screen-dim: (%d, %d)", dimension[3], dimension[4]));
    txt = paste(line1, line2, line3, sep = "\n")
    foShowDeveloperLogAlert(txt)
  })
  
  foRequestHint <- function(){
    # width = imgdims()$width
    # height = imgdims()$height
    # width  <- session$clientData$output_image_width
    # height <- session$clientData$output_image_height
    center = map_centers[[current_country_index()]]
    
    message(paste(center[1], center[2]))
    x = center[1]
    y = center[2]
    param = map_view_param()
    
    
    searcharea_visible(TRUE)
    if(searcharea_level() <= 2){
      hint_counter(hint_counter() + 1)
      searcharea_level(searcharea_level() + 1)
      if(!searcharea_determined()){
        searcharea_determined(TRUE)
        
        km_mean = 2500; km_plusminus = 500;
        
        if(current_continent() != "All"){
          km_mean = 2000; km_plusminus = 250;
        }
        switch(current_continent(),
               "North America" = {km_mean <- 1250; km_plusminus <- 250;},
               "Europe" = {km_mean <- 1000; km_plusminus <- 250;},
               "Asia" = {km_mean = 2250; km_plusminus = 250;}
        )
        km = km_mean + (sample.int(3, 1) - 2) * km_plusminus
        rands = c(rand(), rand())
      } else {
        km = round(searcharea_km()/2)
        rands = searcharea_prev_rands()
        previous_coef = 0.3
        rands[1] = rands[1]*previous_coef + rand() * (1 - previous_coef)
        rands[2] = rands[2]*previous_coef + rand() * (1 - previous_coef)
      }
      xx = x + (2 * (rands[1] - 0.5) * 0.8 * km / 29000) * 1584
      yy = y + (2 * (rands[2] - 0.5) * 0.8 * km / 29000) * 862
      searcharea_km(km)
      searcharea_pos(c(xx,yy))
      searcharea_prev_rands(rands)
    }
    foDisplaySearchHint()
  }
  
  foShowToastrHint <- function(msg){
    delay(150, toastr_info(msg, closeButton = F, timeOut=1500, hideDuration = 1800))
  }
  
  foDisplaySearchHint <- function(){
    foShowToastrHint(sprintf("Hint: An area within %d km radius of hidden country is revealed.", searcharea_km()))
  }
  
  foDisplayHint <- function(){
    foShowToastrHint(sprintf("Hint: An area within %d km radius of hidden country is revealed.", searcharea_km()))
    # delay(150, toastr_info(, closeButton = F, timeOut=(1500), hideDuration = 1800))
  }
  
  foSuccessAlert <- function(){
    shinyalert(
      title = "Congratulations!",
      text = foComputeAlertInfo(charset = "!"),
      size = "s", 
      closeOnEsc = TRUE,
      closeOnClickOutside = FALSE,
      html = FALSE,
      type = "success",
      showConfirmButton = TRUE,
      showCancelButton = FALSE,
      confirmButtonText = "Play Again?",
      confirmButtonCol = "#13C23F",
      timer = 0,
      imageUrl = "",
      animation = TRUE,
      callbackR = function(x){foRestart();}
    )
  }
  
  foFailAlert <- function(){
    shinyalert(
      title = "Game Over!",
      text = foComputeAlertInfo(charset = ".", correction = -1),
      size = "s", 
      closeOnEsc = TRUE,
      closeOnClickOutside = TRUE,
      html = FALSE,
      type = "error",
      showConfirmButton = TRUE,
      showCancelButton = FALSE,
      confirmButtonText = "Play Again?",
      confirmButtonCol = "#E06F2D",
      timer = 0,
      imageUrl = "",
      animation = TRUE,
      callbackR = function(x){foRestart();}
    )
  }
  
  foRestartAlert <- function(){
    shinyalert(
      title = "Game is restarting...",
      text = foComputeAlertInfo(charset = ".", correction = -1, newline = "<br>", extra = "<br><div style = 'margin-top:15px'><span class='loadingbar-custom bigloading'></span></div>"),
      size = "s", 
      closeOnEsc = TRUE,
      closeOnClickOutside = TRUE,
      html = TRUE,
      type = "info",
      showConfirmButton = TRUE,
      showCancelButton = FALSE,
      confirmButtonText = "Restart",
      confirmButtonCol = "#a0cF8D",
      timer = 1700,
      imageUrl = "",
      animation = TRUE,
      callbackR = function(x){foRestart();}
    )
  }
  
  foShowDeveloperLogAlert <- function(txt){
    shinyalert(
      title = "Developer Log",
      text = txt,
      size = "s", 
      closeOnEsc = TRUE,
      closeOnClickOutside = TRUE,
      html = FALSE,
      type = "",
      showConfirmButton = TRUE,
      showCancelButton = FALSE,
      confirmButtonText = "OK",
      confirmButtonCol = "#4936C7",
      timer = 0,
      imageUrl = "",
      animation = TRUE,
      callbackR = function(x){foComputeDimension();}
    )
  }
  
  observe({
    num_countries <- foGetNumberOfCountries(input$num_countries)
    num_countries = pmin(num_countries, length(current_countries()))
    current_numcountries(num_countries)
    # if(num_countries > isolate(current_numcountries())){
    #   
    # }
  })
  
  searcharea_prev_rands <- reactiveVal(c(rand(), rand()))
  searcharea_km <- reactiveVal(2000)
  searcharea_level <- reactiveVal(0)
  searcharea_pos <- reactiveVal(c(800, 300))
  searcharea_determined <- reactiveVal(FALSE)
  searcharea_visible <- reactiveVal(FALSE)
  
  foUpdateSearchArea <- function(param){
    width = param$width
    height = param$height
    magnify = param$magnify
    
    km = searcharea_km()
    search_ratio = 2*km/29000
    search_pos = searcharea_pos()
    search_w = magnify * width * search_ratio
    search_h = magnify * (16 * height / 9) * search_ratio 
    
    search_x = (search_pos[1] / 1584) - param$starting_x
    search_y = (search_pos[2] / 862) - param$starting_y
    search_x = search_x * magnify * width
    search_y = search_y * magnify * height
    # 
    # search_x = width * search_pos[1] / 1584
    # search_y = height * search_pos[2] / 862
    search_x = search_x - search_w/2
    search_y = search_y - search_h/2
    
    search_display = "none"
    if(searcharea_visible()){
      search_display = "inline-block"
    }
    
    runjs(sprintf("document.documentElement.style.setProperty('--search-area-width', '%.0fpx');", search_w))
    runjs(sprintf("document.documentElement.style.setProperty('--search-area-height', '%.0fpx');", search_h))
    runjs(sprintf("document.documentElement.style.setProperty('--search-area-left', '%.1fpx');", search_x))
    runjs(sprintf("document.documentElement.style.setProperty('--search-area-top', '%.1fpx');", search_y))
    runjs(sprintf("document.documentElement.style.setProperty('--search-area-display', '%s');", search_display))
  }
  
  imgdims <- reactive({
    imgdims <- input$imgdims
    if(!is.null(imgdims)){
      width <- imgdims[1]
      height <- imgdims[2]
    } else {
      width = 1600
      height = 900
    }
    return(list(width = width, height = height))
  })
  
  map_view_param <- reactive({
    # # Get width and height of image output
    width = imgdims()$width
    height = imgdims()$height
    
    # width  <- session$clientData$output_image_width
    # height <- session$clientData$output_image_height
    # browser()
    
    magnify = 1;
    starting_x = 0
    starting_y = 0
    clip_class = '';
    switch(input$viewpoint, 
           "All" = {},
           "Europe" = {magnify <- 4;
           starting_x <- 0.33
           starting_y <- 0.075
           clip_class <- 'clip-europe';},
           "Asia" = {magnify <- 2;
           starting_x <- 0.35
           starting_y <- 0.11
           clip_class <- 'clip-asia';},
           "Middle East" = {magnify <- 4;
           starting_x <- 0.40
           starting_y <- 0.24
           clip_class <- 'clip-middleeast';},
           "Africa" = {magnify <- 2;
           starting_x <- 0.25
           starting_y <- 0.305
           clip_class <- 'clip-africa';},
           "Oceania" = {magnify <- 2;
           starting_x <- 0.50
           starting_y <- 0.4175
           clip_class <- 'clip-australia';},
           "North America" = {magnify <- 2;
           starting_x <- 0
           starting_y <- 0.0175
           clip_class <- 'clip-northamerica';},
           "Middle America" = {magnify <- 4;
           starting_x <- 0.04
           starting_y <- 0.31
           clip_class <- 'clip-middleamerica';},
           "South America" = {magnify <- 2;
           starting_x <- 0
           starting_y <- 0.455
           clip_class <- 'clip-southamerica';}
    )
    overall_width = width * magnify
    overall_height = height * magnify
    
    ratio = 0.0425;
    # ratio = 2*1000/29000;
    # ratio = 2*500/29000 * magnify;
    runjs(sprintf("starting_x = %f", starting_x));
    runjs(sprintf("starting_y = %f", starting_y));
    runjs(sprintf("document.documentElement.style.setProperty('--cursor-area-width', '%.0fpx');", width * ratio))
    runjs(sprintf("document.documentElement.style.setProperty('--cursor-area-height', '%.0fpx');", (16 * height / 9) * ratio ))
    runjs(sprintf("document.documentElement.style.setProperty('--map-margin-left', '%.0fpx');", -1* magnify * width * starting_x))
    runjs(sprintf("document.documentElement.style.setProperty('--map-margin-top', '%.0fpx');", -1* magnify * height * starting_y))
    
    out = list(width = width, height = height, ratio = ratio, magnify = magnify, 
         starting_x = starting_x, starting_y = starting_y, 
         clip_class = clip_class, 
         overall_width = overall_width, 
         overall_height = overall_height)
    
    isolate(foUpdateSearchArea(out))
    
    return(out)
  })
  
  observe({
    param <- map_view_param()
    foUpdateSearchArea(param)
  })
  
  observe({
    message("Image refreshed....")
    param <- map_view_param()
    width = param$width
    height = param$height
    magnify = param$magnify
    clip_class = param$clip_class
    mapImgInitialized(TRUE)
    # message(paste0("xyz:", magnify*width, ",", magnify*height))
    runjs(sprintf('update_image(%d, %d, "%s")', magnify*width, magnify*height, clip_class))
    # list(
    #   id = "imgx", 
    #   src = "www/wmap4.png",
    #   contentType = "image/png",
    #   width = magnify*width,
    #   height = magnify*height,
    #   class = paste(clip_class, 'nocursor')
    # )
  })
  
  # output$image <- renderImage({
  # 
  #   
  #   mapImgInitialized(TRUE)
  #   
  #   # Return a list containing information about the image
  #   return(list(
  #     id = "imgx", 
  #     src = "www/wmap4.png",
  #     contentType = "image/png",
  #     width = magnify*width,
  #     height = magnify*height,
  #     class = paste(clip_class, 'nocursor')
  #   ))  
  #   }, deleteFile = FALSE)
  
  previous_width = reactiveVal(0)
  # previous_height = reactiveVal(0)
  
  foComputeDimension <- function(){
    tryCatch({
      current_width = shinybrowser::get_width()
      # max_width = pmin(current_width, 1600)
      # max_width = pmin(input$dimension[1], 1600)
      # max_width = input$dimension[1]
      # max_width = pmin(input$dimension[1], input$dimension[3])*0.99
      # max_width = input$dimension[1] * 0.995
      max_width = input$dimension[1] * 1
      width = max_width
      height = max_width * 9 / 16
      
      if(width < 800){
        coordtext_fc = 95
        if(width < 600){coordtext_fc = 80}
        if(width < 500){coordtext_fc = 70}
        if(width < 300){coordtext_fc = 50}
        runjs(sprintf("document.documentElement.style.setProperty('--coordtext-fontsize', '%d%%');", coordtext_fc))
      }
      
      # width2 = previous_width()
      # width2 = session$clientData$output_image_width
      # height2 = session$clientData$output_image_width * 9 / 16
      
      # diff = (width - width2)/width
      # if(is.na(diff)){
      #   diff = 1
      # }
      # message(paste("width: ", width, "width2: ", width2, "diff:", diff))
      
      # diff = 1
      # if(abs(diff) >= 0.025){
        # previous_width(width)
        runjs(sprintf("document.getElementById('map_div').style.width = %.0f + 'px';", width))
        runjs(sprintf("document.getElementById('map_div').style.height = %.0f + 'px';", height))
        runjs("document.getElementById('imgx').focus();")
      # }
      message(paste(input$dimension[1], input$dimension[2]))
    }, error = function(e) {message(e); stop(e); }
    )
  }
  
  observeEvent(input$dimension, {
    foComputeDimension()
  })
  
  observeEvent(mapImgInitialized, {
    delay(50, runjs(
      "const image1 = document.getElementById('imgx');
      const coordtext = document.getElementById('coordtext');
      const img_container = document.getElementById('imagediv');
      const cursor = document.getElementById('cursor');
      const animations = [];
      
      image1.addEventListener('mousemove', function(e) {
          var RelPos = RelativePos(image1, e);
          const width = 60
          var isvalid = RelPos[0] >= width * 0.5
          isvalid = isvalid & RelPos[1] >= width * 0.5
          if(isvalid){
            cursor.style.left = e.clientX + 'px',
            cursor.style.top = e.clientY + 'px';
          }
      });
      
      function getMousePos(e){
            var x = 0
            var y = 0
            if(e.type == 'touchstart' || e.type == 'touchmove' || e.type == 'touchend' || e.type == 'touchcancel'){
                var evt = (typeof e.originalEvent === 'undefined') ? e : e.originalEvent;
                var touch = evt.touches[0] || evt.changedTouches[0];
                x = touch.pageX;
                y = touch.pageY;
            } else if (e.type == 'mousedown' || e.type == 'mouseup' || e.type == 'mousemove' || e.type == 'mouseover'|| e.type=='mouseout' || e.type=='mouseenter' || e.type=='mouseleave' || e.type == 'click') {
                x = e.clientX;
                y = e.clientY;
            }
            return [x, y]
      }
      
      function MouseMove(e, image1) {
          var RelPos = RelativePos(image1, e);
          const width = 60
          var isvalid = RelPos[0] >= width * 0.5
          isvalid = isvalid & RelPos[1] >= width * 0.5
          if(isvalid){
            var xy = getMousePos(e)
            var x = xy[0]
            var y = xy[1]
            cursor.style.left = x + 'px',
            cursor.style.top = y + 'px';
            
            const x_map = 1584 * RelPos[0] / image1.width;
            const y_map = 862 * RelPos[1] / image1.height;
            const pos = '(' + Math.round(x_map) + ', ' + Math.round(y_map) + ')'
            coordtext.innerHTML = pos
          }
      }
      image1.addEventListener('mousemove', function(event){
        MouseMove(event, image1)
      });
      
      image1.addEventListener('touchmove', function(event){
        // MouseMove(event, image1)
      });
      
      var mylatesttap = new Date().getTime();
      var doubletap_prev_click_x = 0
      var doubletap_prev_click_y = 0
      function doubletap(event){
         var xy = getMousePos(event)
         var x = xy[0]
         var y = xy[1]
         var now = new Date().getTime();
         var timesince = now - mylatesttap;
         mylatesttap = new Date().getTime();
         var out = (timesince < 600) && (timesince > 0);
         out = out && (Math.abs((doubletap_prev_click_x - x)) < 20) && (Math.abs((doubletap_prev_click_y - y)) < 20)
         doubletap_prev_click_x = x
         doubletap_prev_click_y = y
         return out
      }
      
      var mylatestdoubleclick = new Date().getTime();
      function DoubleClick(event){
        var now = new Date().getTime();
        var timesince = now - mylatestdoubleclick;
        if(timesince<500){return;}
        mylatestdoubleclick = new Date().getTime();
      
        var RelPos = RelativePos(image1, event);
        var x = RelPos[0]
        var y = RelPos[1]
        
        const x_map = 1584 * x / image1.width;
        const y_map = 862 * y / image1.height;
        
        x = x - image1.width * starting_x
        y = y - image1.height * starting_y
        
        console.log(`Clicked on image at (${x}, ${y}) at map point (${Math.round(x_map)}, ${Math.round(y_map)})`);
        console.log(`(${starting_x}, ${starting_y})`)
        
        const pos = '(' + Math.round(x_map) + ', ' + Math.round(y_map) + ')'
        
        const circle = document.createElement('div');
        circle.classList.add('smallcircle');
        img_container.appendChild(circle);
        circle.style.left = x + 'px';
        circle.style.top = y + 'px';
        
        /*
        const pos_txt = document.createElement('p');
        pos_txt.classList.add('maptxt');
        pos_txt.style.left = x + 'px';
        pos_txt.style.top = y + 'px';
        pos_txt.innerHTML = pos;
        img_container.appendChild(pos_txt);
        */
        
        animations.push(circle);
        
        Shiny.setInputValue('map_click', {x_map: x_map, y_map: y_map, x: x, y: y}, {priority: 'event'})
        
        img_container.classList.add('clicked');
        
        setTimeout(function() {
          img_container.removeChild(circle);
          animations.splice(animations.indexOf(circle), 1);

          // if there are no more running animations, remove the 'clicked' class from the image
          if (animations.length === 0) {
            img_container.classList.remove('clicked');
          }
        }, 2000);
      }
      
      image1.addEventListener('click', function(event){
        if(doubletap(event)){
          console.log('dbltap')
          DoubleClick(event)
        } else {
          MouseMove(event, image1)
        }
      });
      
      Shiny.addCustomMessageHandler('clicked-country', function(e) {
        const circle = document.createElement('div');
        if(e.correct){
          circle.classList.add('greencircle');
        } else {
          circle.classList.add('circle');
        }
        img_container.appendChild(circle);
        circle.style.left = e.x + 'px';
        circle.style.top = e.y + 'px';
      
        const pos_txt = document.createElement('p');
        pos_txt.classList.add('maptxt');
        pos_txt.style.left = e.x + 'px';
        pos_txt.style.top = e.y + 'px';
        pos_txt.innerHTML = e.country;
        img_container.appendChild(pos_txt);
        setTimeout(function() {
          img_container.removeChild(pos_txt);
        }, 2000);
        setTimeout(function() {
          img_container.removeChild(circle);
        }, 2000);
        
      });
      
      image1.addEventListener('dblclick', function(event) {
        //DoubleClick(event)
      });"
    ))
  }, once = TRUE)
  
  # observe({
  #   # name = current_country()
  #   # message(paste("sdf:", name))
  #   # runjs(sprintf("document.getElementById('whereis_text').innerHTML = 'Where is %s?';", name))
  #   # runjs(sprintf("document.getElementById('progress_text').innerHTML = '%d/%d';", country_index(), length(current_countries())))
  # })
  
  output$whereis_text <- renderUI({
    req(current_country())
    name = current_country()
    txt = sprintf('Where is %s?', name)
    # <img src = "happy.svg" alt="My Happy SVG"/>
    out = tags$span(txt)
    # out = tags$img(src = "www/flags/4x2/tr.svg", width = 20, height = 15)
    # out = tags$img(src = "wmap4.png", width = 60, height = 45)
    # out = tags$img(src = "tr.svg", width = 60, height = 45, contentType = 'image/svg+xml')
    # style = "margin-left:1px; margin-right:1px;"
    show_flag = current_whattoask_option()$askFlag
    show_capital = current_whattoask_option()$askCapital
    show_riddle = current_whattoask_option()$askRiddle
    if(show_capital){
      out = tags$span(sprintf('Where is %s?', current_capital()))
    }
    
    if(show_flag){
      out = tags$span(
        "Where is ",
        tags$img(src = paste0("flags/4x3/", current_country_alpha2(), ".svg"), width = 52, height = 39, contentType = 'image/svg+xml'),
        "?"
      )
    }
    
    if(show_riddle){
      out = tags$span(
        "Where is ",
        actionButton("countryXbutton" ,"X", style = "font-size: 130%; font-weight:bold;"), 
        "?"
      )
    }
    
    return(out)
  })
  
  riddle_numbers_revealed <- reactiveVal(FALSE)
  
  output$riddle_modalfooter <- renderUI({
    button = actionButton("riddle_okayButton", "OK")
    newinfo_button = actionButton("riddle_newinfoButton", "Request New Info")
    if(riddle_txt()$nCategory == nGameCategory){
      newinfo_button = tags$div()
    }
    ask_button = actionButton("riddle_askButton", "Ask (Next Guess)")
    ask_div = tags$span(ask_button)
    # mark_div = tags$span(actionButton("riddle_markButton", "Mark (Hint)"), style = "margin-right:4px;margin-top:4px;")
    revealnumbers_div = tags$span(actionButton("riddle_revealnumbersButton", "Reveal Numbers (Hint)"), style = "margin-right:4px;margin-top:8px;")
    if(riddle_numbers_revealed()){
      revealnumbers_div = tags$span(fancyCheckbox("riddle_revealNumbers", "Reveal Numbers", isolate(cached_riddle_revealNumbers())), style = "margin-right:0px;margin-top:6px;")
      # revealnumbers_div = tags$span()
    }
    
    newinfo_div = tags$span(newinfo_button)
    
    flex_column_style = "display:flex; flex-direction: column;justify-content: center; align-items: center;"
    left_div = tags$div(ask_div, style = flex_column_style, style = "margin-right:8px;")
    # left_div = tags$div(ask_div, mark_div)
    center_div = tags$div(class = "footerdiv", button, revealnumbers_div, style = flex_column_style)
    # center_div = button
    right_div = tags$div(newinfo_div, style = flex_column_style, style = "margin-left:6px;")
    
    out = tags$div(class = "footerdiv", left_div, center_div, right_div, style = "display:flex;justify-content: center;")
    return(out)
  })
  
  output$riddle_modaldescription <- renderUI({
    return(tags$div(HTML(riddle_txt()$outstr), style = "max-height:35vh; overflow-y:auto;"))
  })
  
  observeEvent(input$riddle_revealnumbersButton, {
    if(riddle_numbers_revealed() == FALSE){
      cached_riddle_revealNumbers(TRUE)
      # cached_countryask_revealNumbers(TRUE)
      riddle_numbers_revealed(TRUE)
      hint_counter(hint_counter() + 1)
      foX <- function(){
        foCreateRiddle(preservePrevious = TRUE, incrementAdditional = FALSE); 
        foDisplayRiddle();
      }
      removeModal()
      toastr_info("Hint: The numbers for the clues of country X are revealed.", closeButton = F, timeOut=1000, hideDuration = 1800)
      delay(50, foX())
    }
  })
  
  observeEvent(input$riddle_revealNumbers, {
    if(!is.null(input$riddle_revealNumbers)){
      cached_riddle_revealNumbers(input$riddle_revealNumbers)
    }
    foCreateRiddle(preservePrevious = TRUE, incrementAdditional = FALSE)
  })
  
  foDisplayRiddle <- function(){
    showModal(
      modalDialog(
        tags$div(style = "display:flex; align-items:center; flex-direction: column;",
                 tags$span(style = "font-size:115%;margin-bottom:6px;", "Country X has the following properties:"),
                 uiOutput("riddle_modaldescription"),
        ),
        title = tags$b("Who is Country X?", style = "font-size:135%;"),
        # title = uiOutput("abcd_title"),
        footer = uiOutput("riddle_modalfooter", style = "width:100%"),
        # footer = uiOutput("modalfooter", style = "width:100%"),
        # footer = tags$div(loading, modalButton("Let's Go!"), style = "display:flex;justify-content: center;"),
        size = "m",
        easyClose = TRUE,
        fade = FALSE
      )
      # )
    )
  }
  
  countryask_riddle <- reactive({
    validate(need(ask_country() != -1, ""))
    riddle = foGenerateRiddle(ask_country(), focus = input$riddlefocus, difficulty = input$country_difficulty, showAll = TRUE, secondary_riddle = riddle_txt(), revealNumbers = input$countryask_revealNumbers)
    return(HTML(riddle$outstr))
  })
  
  output$countryask_modaldescription <- renderUI({
    validate(need(ask_country() != -1, ""))
    name = map_names[ask_country()]
    top_line = tags$span(style = "font-size:115%;margin-bottom:6px;", sprintf("%s has the following properties:", name))
    middle = tags$div(countryask_riddle(), style = "max-height:35vh; overflow-y:auto;")
    return(tags$div(top_line, middle))
  })
  
  output$countryask_modaltitle <- renderUI({
    validate(need(ask_country() != -1, ""))
    alpha2 = alpha2_codes[ask_country()]
    alpha3 = alpha3_codes[ask_country()]
    calling_code = paste0("+", Tmisc$calling_code[ask_country()])
    capital = map_capitals[ask_country()]
    name = map_names[ask_country()]
    size = 16
    height = size * 3
    width = size * 4
    flag_img = sprintf('<img src = "%s" width = "%d" height = "%d" style="border:1px solid;border-color:#3338;")>', paste0("flags/4x3/", tolower(alpha2), ".svg"), width, height)
    name_el = tags$b(name, style = "font-size:135%;")
    mid = tags$div(name_el, HTML(flag_img))
    left = tags$div(HTML(paste0("Capital: <br> ", capital)), style = "width:20%; font-size:75%;")
    right = tags$div(calling_code, tags$br(), paste0(alpha2, "-", alpha3), style = "width:20%; font-size:80%; text-align:right;")
    return(tags$div(left, mid, right, style = "display: flex; align-items: center; justify-content: space-between;"))
  })
  
  cached_riddle_revealNumbers <- reactiveVal(TRUE)
  cached_countryask_revealNumbers <- reactiveVal(FALSE)
  
  output$countryask_modalfooter <- renderUI({
    button = actionButton("countryask_okayButton", "OK")
    revealNumbersCheckbox = fancyCheckbox("countryask_revealNumbers", "Reveal Numbers", cached_countryask_revealNumbers())
    out = tags$div(class = "footerdiv", tags$div(style = "position: absolute;left: 16px;", revealNumbersCheckbox), button, style = "display:flex;justify-content: center;")
    return(out)
  })
  
  observeEvent(input$countryask_okayButton, {
    removeModal()
    foClickOnCountry(ask_country())
    revealNumbers = input$countryask_revealNumbers
    if(!is.null(revealNumbers)){
      cached_countryask_revealNumbers(revealNumbers)
    }
    
  })
  
  foDisplayCountryInfo <- function(){
    showModal(
      modalDialog(
        tags$div(style = "display:flex; align-items:center; flex-direction: column;",
                 uiOutput("countryask_modaldescription"),
        ),
        title = uiOutput("countryask_modaltitle"),  
                         # tags$b("Who is Country X?", style = "font-size:135%;"),
        footer = uiOutput("countryask_modalfooter", style = "width:100%"),
        size = "m",
        easyClose = FALSE,
        fade = FALSE
      )
      # )
    )
  }
  
  riddle_txt = reactiveVal("")
  
  foCreateRiddle <- function(country_index = current_country_index(), preservePrevious = FALSE, incrementAdditional = FALSE){
    if(preservePrevious == TRUE){
      additional = riddle_txt()$additional
      if(is.null(additional)){
        additional = 0
      }
      randoms = riddle_txt()$randoms
      if(incrementAdditional){
        additional = additional + 1
      }
    } else {
      additional = 0
      randoms = NULL
    }
    message(additional)
    focus = input$riddlefocus
    revealNumbers = FALSE
    if(riddle_numbers_revealed()){
      revealNumbers = input$riddle_revealNumbers
    }
    riddle = foGenerateRiddle(country_index, focus = focus, difficulty = input$country_difficulty, randoms = randoms, additional = additional, showAll = FALSE, revealNumbers = revealNumbers)
    riddle_txt(riddle)
  }
  
  observeEvent(input$riddle_okayButton, {
    removeModal();
  })
  
  observeEvent(input$riddle_newinfoButton, {
    removeModal();
    foCreateRiddle(preservePrevious = TRUE, incrementAdditional = TRUE)
    additional_info_counter(additional_info_counter() + 1)
    delay(50, toastr_info(paste0('New info about country X: <br> ', riddle_txt()$latest)))
  })
  
  ask_enabled <- reactiveVal(FALSE)
  ask_country <- reactiveVal(NULL)
  
  observeEvent(input$riddle_askButton, {
    removeModal();
    ask_enabled(TRUE)
    delay(50, toastr_info(paste0("Ask Enabled: The next guess will reveal information about the target country."), timeOut = 1200, hideDuration = 1800))
  })
  
  observeEvent(input$countryXbutton, {
    foDisplayRiddle()
  })
  
  output$progress_text <- renderUI({
    req(country_index())
    index = country_index()
    countries = current_countries()
    txt = sprintf('%d/%d', index, current_numcountries())
    return(tags$span(txt))
  })
  
  observeEvent(input$hintButton, {
    if(!hint_used()){
      hint_used(TRUE)
    } else {
      foHintAlert()
      return()
    }
    foRequestHint()
  })
  
  observeEvent(input$revealButton, {
    if(!reveal_used()){
      reveal_used(TRUE)
      reveal_counter(reveal_counter() + 1)
    }
    width = imgdims()$width
    height = imgdims()$height
    # width  <- session$clientData$output_image_width
    # height <- session$clientData$output_image_height
    
    center = map_centers[[current_country_index()]]
    
    message(paste(center[1], center[2]))
    x = center[1]
    y = center[2]
    param = map_view_param()
    
    # searcharea_visible(TRUE)
    # if(!searcharea_determined()){
    #   searcharea_determined(TRUE)
    #   km = 2000
    #   xx = x + (2 * (rand() - 0.5) * 0.8 * km / 29000) * 1584
    #   yy = y + (2 * (rand() - 0.5) * 0.8 * km / 29000) * 862
    #   searcharea_pos(c(xx,yy))
    # }
    
    x = x / 1584
    y = y / 862
    
    x = x - param$starting_x
    y = y - param$starting_y
    
    x = width * x * param$magnify
    y = height * y * param$magnify
    
    name = current_country()
    session$sendCustomMessage("clicked-country", list(country = name, x = x, y = y, correct = F))
  })
  
  foRestart <- function(reset_footer = T){
    country_index(1);
    num_countries <- foGetNumberOfCountries(input$num_countries)
    difficulty <- input$country_difficulty
    continent <- input$country_continent
    current_difficulty(difficulty)
    current_continent(continent)
    # current_countries(randperm(map_num_country, num_countries))
    
    countriesx <- foGetCountriesByDifficulty(difficulty)
    countriesx <- countriesx & foGetCountriesByContinent(continent)
    
    countriesx = which(countriesx)
    nCountry = length(countriesx)
    indices = randperm(nCountry)
    indices = countriesx[indices]
    current_countries(indices)
    
    num_countries = pmin(num_countries, length(current_countries()))
    current_numcountries(num_countries)
    
    fprintf("Restarted with %d countries\n", num_countries)
    ask_country(NULL)
    reveal_counter(0)
    reveal_used(FALSE)
    additional_info_counter(0)
    hint_counter(0)
    hint_used(FALSE)
    wrong_guess_counter(0)
    ask_enabled(FALSE)
    current_wrong_guesses(list())
    searcharea_visible(FALSE)
    searcharea_level(0)
    searcharea_determined(FALSE)
    riddle_txt(NULL)
    riddle_numbers_revealed(FALSE)
    cached_riddle_revealNumbers(TRUE)
    
    updatePickerInput(session, "viewpoint", selected = current_continent())
    # if(continent != "All"){
    #   
    # }
    if(reset_footer == TRUE){
      runjs('reset_footer_animation()');
    }
    if(current_whattoask_option()$askRiddle == TRUE){
      foCreateRiddle()
      delay(50, foDisplayRiddle())
    }
  }
  
  output$requirerestart <- renderUI({
    require_restart = F
    if(!identical(current_difficulty(), input$country_difficulty)){
      require_restart = T
    }
    if(!identical(current_continent(), input$country_continent)){
      require_restart = T
    }
    
    if(require_restart){
      out = tags$span("Changes require restart ", style = "font-size:90%; color:#ee3322; margin-right:4px;")
    } else {
      out = tags$span()
    }
    return(out)
  })
  
  observeEvent(input$restartButton,
    foRestartAlert()
    # if(country_index()>1){
    #   foRestartAlert()
    # } else {
    #   foRestart(reset_footer = FALSE)
    # }
  )
  
  initialized = reactiveVal(FALSE)
  
  output$modalfooter <- renderUI({
    loading <- conditionalPanel(
      condition="$('html').hasClass('shiny-busy')", 
      tags$div(style = "margin-top:15px;text-align:center;", tags$span(class = "loadingbar-custom bigloading"))
    )
    if(initialized() == TRUE){
      # , style = "background-color:#13C23F"
      # button = modalButton("Let's Go!");
      button = actionButton("modalButton", "Let's Go!")
      out = tags$div(class = "footerdiv", button, style = "display:flex;justify-content: center;")
    } else {
      out = loading;
    }
    return(out)
  })
  
  observeEvent(input$modalButton, {
    # message("abcd")
    updatePickerInput(session, "country_continent", selected = input$country_continent_modal)
    updatePickerInput(session, "country_difficulty", selected = input$country_difficulty_modal)
    updatePickerInput(session, "num_countries", selected = input$num_countries_modal)
    updatePickerInput(session, "whattoask", selected = input$whattoask_modal)
    if(identical(input$whattoask_modal, "Riddle")){
      updatePickerInput(session, "riddlefocus", selected = input$riddlefocus_modal)
    }
    removeModal()
    delay(50, foRestart())
    
    foShowLandscape <- function(){
      shinyalert(
        title = "",
        text = "<img class = 'landscape-img' src = 'lanscape2.png' width = '300' height = '300'>",
        size = "s", 
        closeOnEsc = TRUE,
        closeOnClickOutside = FALSE,
        html = TRUE,
        type = "",
        showConfirmButton = TRUE,
        showCancelButton = FALSE,
        timer = 3100,
        # imageUrl = "lanscape2.png", 
        # imageUrl = "https://www.shutterstock.com/image-vector/phone-rotate-icon-260nw-601819463.jpg",
        # imageWidth = 300,
        # imageHeight = 300,
        animation = TRUE
      )
    }
    current_width = shinybrowser::get_width()
    if(current_width <= 400){
      delay(500, foShowLandscape());
    }
  })
  

  observeEvent(input$initialized, {
    message("Initialized")

    pickers = preparePickers(extra = "_modal", difficulty = "Easy", numcountries = "20")
    whattoask_picker = pickers$whattoask_picker
    riddlefocus_picker = pickers$riddlefocus_picker
    numofcountries_picker = pickers$numofcountries_picker
    difficulty_picker = pickers$difficulty_picker
    continent_picker = pickers$continent_picker
    
    showModal(
      # tags$div(class = "modalcentering-div",
      modalDialog(
        # uiOutput("abcd_ui"),
        tags$div(style = "display:flex; align-items:center; flex-direction: column;",
          # tags$span(style = "font-size:115%;margin-bottom:6px;", "Select options to get started:"),
          whattoask_picker,
          riddlefocus_picker, 
          numofcountries_picker,
          continent_picker, 
          difficulty_picker,
        ),
        title = tags$b("Welcome to MapQuizzr!", style = "font-size:140%;"),
        # title = uiOutput("abcd_title"),
        footer = uiOutput("modalfooter", style = "width:100%"),
        # footer = tags$div(loading, modalButton("Let's Go!"), style = "display:flex;justify-content: center;"),
        size = "m",
        easyClose = FALSE,
        fade = FALSE
      )
      # )
    )
    
    # modalDialog(
    #   footer = modalButton("Close Modal")
    # )
    foX <- function(){
      foCheckPosition(0, 0); initialized(TRUE); 
    }
    delay(100, foX())
  }, once = TRUE)
  
  foClickOnCountry <- function(country){
    name = '';
    correct = F
    if(!is.na(country)){
      name = map_names[country]
      
      ask_was_enabled = FALSE
      if(ask_enabled() == TRUE){
        ask_enabled(FALSE)
        ask_was_enabled = TRUE
        ask_country(country)
        foDisplayCountryInfo()
        if(ask_was_enabled == TRUE){
          # x = input$map_click$x
          # y = input$map_click$y
          # message(paste("Country:", name))
          # session$sendCustomMessage("clicked-country", list(country = name, x = x, y = y, correct = correct))
          return();
        }
      }
      
      ask_flag = current_whattoask_option()$askFlag
      ask_capital = current_whattoask_option()$askCapital
      ask_riddle = current_whattoask_option()$askRiddle
      if(identical(current_country(), name)){
        
        correct = T
        reveal_used(FALSE)
        hint_used(FALSE)
        current_wrong_guesses(list())
        
        searcharea_level(0)
        searcharea_visible(FALSE)
        searcharea_determined(FALSE)
        riddle_numbers_revealed(FALSE)
        finished = F
        index = country_index()
        if(index >= current_numcountries()){
          fprintf("Finished...\n")
          finished = T
          delay(500, foSuccessAlert())
          # foRestart()
        } else {
          country_index(country_index() + 1)
        }
        
        # tags$img(src = paste0("flags/4x3/", current_country_alpha2(), ".svg"), width = 52, height = 39
        delay(150, toastr_success(sprintf("That is correct!"), closeButton = F, timeOut=400, hideDuration = 1800))
        if(!finished){
          flag_img = sprintf('<img src = "%s" width = "52" height = "39")>', paste0("flags/4x3/", current_country_alpha2(), ".svg"))
          whereis_infotxt = sprintf("Where is %s? %s", current_country(), flag_img)
          if(ask_capital == TRUE){
            whereis_infotxt = sprintf("Where is %s?", current_capital())
          }
          if(ask_flag == TRUE){
            whereis_infotxt = sprintf("Where is  %s  ?", flag_img)
          }
          if(ask_riddle == TRUE){
            whereis_infotxt = sprintf("Where is country X?")
            foCreateRiddle()
            delay(850, foDisplayRiddle())
          }
          delay(800, toastr_info(whereis_infotxt, closeButton = F, timeOut=3000, hideDuration = 1800))
          runjs('reset_footer_animation()');
        }
      } else {
        country_str = sprintf("%d", country)
        wrong_guesses = current_wrong_guesses()
        if(is.null(wrong_guesses[[country_str]])){
          wrong_guesses[[country_str]] = TRUE;
          current_wrong_guesses(wrong_guesses)
          wrong_guess_counter(wrong_guess_counter()+1)
        }
        capital = map_capitals[country]
        flag = foGetCountryAlpha2(country)
        no_infotext = sprintf("No, that is %s", name);
        flag_img = sprintf('<img src = "%s" width = "52" height = "39")>', paste0("flags/4x3/", flag, ".svg"))
        timeoutplus = 0
        if(ask_capital == TRUE){
          no_infotext = sprintf("No, capital of %s is %s.", name, capital)
          timeoutplus = 200;
        }
        if(ask_flag == TRUE){
          no_infotext = sprintf("No, flag of %s is %s", name, flag_img)
          timeoutplus = 900
        }
        if(ask_riddle == TRUE){
          # index = match(country, map_names)
          info = foCheckCountryForRiddle(riddle_txt(), country)
          no_infotext = sprintf("No, %s is not X. <br> %s.", name, info$explanation_text)
          timeoutplus = 400 + info$num_violation * 800
        }
        delay(150, toastr_warning(no_infotext, closeButton = F, timeOut=(600+timeoutplus), hideDuration = 1800))
      }
    }
    x = input$map_click$x
    y = input$map_click$y
    message(paste("Country:", name))
    session$sendCustomMessage("clicked-country", list(country = name, x = x, y = y, correct = correct))
  }
  
  observeEvent(input$map_click, {
    message(paste("Map clicked: ", input$map_click$x_map, input$map_click$y_map))
    country = foCheckPosition(input$map_click$x_map, input$map_click$y_map)
    foClickOnCountry(country)
  })

}
