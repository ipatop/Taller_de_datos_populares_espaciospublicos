# Note: percent map is designed to work with the counties var set
# It may not work correctly with other var sets if their row order does 
# not exactly match the order in which the maps package plots counties
percent_map1 <- function(var, color, legend.title, min = 0, max = 100) {

  # generate vector of fill colors for map
  shades <- colorRampPalette(c("white", color))(100)
  
  # constrain gradient to percents that occur between min and max
  var <- pmax(var, min)
  var <- pmin(var, max)
  percents <- as.integer(cut(var, 100, 
    include.lowest = TRUE, ordered = TRUE))
  fills <- shades[percents]

  # plot choropleth map
  map("county", fill = TRUE, col = fills, 
    resolution = 0, lty = 0, projection = "polyconic", 
    myborder = 0, mar = c(0,0,0,0))
  
  # overlay state borders
  map("state", col = "white", fill = FALSE, add = TRUE,
    lty = 1, lwd = 1, projection = "polyconic", 
    myborder = 0, mar = c(0,0,0,0))
  
  # add a legend
  inc <- (max - min) / 4
  legend.text <- c(paste0(min, " % or less"),
    paste0(min + inc, " %"),
    paste0(min + 2 * inc, " %"),
    paste0(min + 3 * inc, " %"),
    paste0(max, " % or more"))
  
  legend("bottomleft", 
    legend = legend.text, 
    fill = shades[c(1, 25, 50, 75, 100)], 
    title = legend.title)
}


percent_map <- function(data, var, min = 0, max = 100) {
  
  if(is.numeric(data[,var])){
    
    ggplot() + theme_minimal() + theme(legend.position = c(.8,.15)) +
      geom_sf(var=ciudad,fill='white') +
      geom_sf(var=parques_full %>% filter(area>=min & area<=max), mapping=aes(fill= var)) +
      scale_fill_continuous(name= !!sym(var)) +
      ggtitle(paste0('Parques coloreados por ',var))
    
  }else{
    
    ggplot() + theme_minimal() + theme(legend.position = c(.8,.15)) +
      geom_sf(var=ciudad,fill='white') +
      geom_sf(var=parques_full %>% filter(area>=min & area<=max), mapping=aes(fill= !!sym(var))) +
      scale_fill_manual(values = c("darkgrey","darkorange")) +
      ggtitle(paste0('Parques coloreados por ',var))
    
  }
  
  
}



percent_map_res <- function(data, var, color, legend.title, min = 0, max = 100) {
  
  if(is.numeric(var[,var])){
    
    ggplot() + theme_minimal() + theme(legend.position = c(.8,.15)) +
      geom_sf(var=ciudad,fill='white') +
      geom_sf(var=parques_full %>% filter(area>=min & area<=max), mapping=aes(fill= var)) +
      scale_fill_continuous(name=!!legend) +
      ggtitle(paste0('Parques coloreados por ',legend))
    
  }else{
    
    ggplot() + theme_minimal() + theme(legend.position = c(.8,.15)) +
      geom_sf(var=ciudad,fill='white') +
      geom_sf(var=parques_full %>% filter(area>=min & area<=max), mapping=aes(fill= var)) +
      scale_fill_manual(values = c("darkgrey","darkorange")) +
      ggtitle(paste0('Parques coloreados por ',legend))
    
  }
  
  
}

