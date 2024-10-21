##########################################################################################################
#                                      mRS graph ("Grotta bars")                                         #
#                                                                                                        #
#                                              By @ElijT                                                 #
#                                 https://github.com/ElijT/R-graphs                                      #
##########################################################################################################
#
# @param mRS_data : Data frame with the mRS you want to represent
# @param mRS_name : Column name given as a string where mRS data is stored. The function is expecting the mRS to be coded as integer from 0 to 6.
# @param obs_name : Column name given as a string for the binary comparison of mRS. Typically this column will have an intervention name.
# @param top_name : Name (string) inside the obs_name column. This is the intervention that will be displayed on top - Usually "Placebo" / "No intervention".
# @param bottom_name  : Name (string) inside the obs_name column. This is the intervention that will be displayed on the bottom - Usually  "Drug" / "Intervention".
# @param color : can be one of three choices: "predefined" (default) : Used colors set defined by me in the group.colors, "spectral" use brewer Spectral palette, "greys" use brewer Greys palette.
#
# For now the function cannot accept multiple intervention observation and is not compatible with facetting.
#
#
#


mRS_grotta_bars <- function(mRS_data, mRS_name, obs_name, top_name, bottom_name, color = "predefined") {
  require(tidyverse)
  require(ggplot2)
  
  mRS_data <- mRS_data %>% 
    rename(OBS_name = obs_name, mRS = mRS_name) %>%
    filter(!is.na(mRS)) %>%
    group_by(mRS, OBS_name) %>%
    count()
  
  
  
  Sum_ARM <- aggregate(n ~ OBS_name, data = mRS_data, FUN = sum) 
  Sum_P <- Sum_ARM[Sum_ARM$OBS_name == top_name, "n"]
  Sum_D <- Sum_ARM[Sum_ARM$OBS_name == bottom_name, "n"]
  
  
  group.colors <- c("0" = "#AEF43E", "1" = "#008000", "2" ="#376D5C", "3" = "#FF6600", "4" = "#FF3300", "5" = "#CC3300", "6" = "#000000", "grey" = "#7F7F7F")
  
  
  
  
  all_combinations_x <- expand_grid(mRS = 0:6, OBS_name = c(top_name, bottom_name))
  
  
  mRS_data <- all_combinations_x %>%
    left_join(mRS_data, by = c("mRS", "OBS_name")) %>%
    mutate(n = ifelse(is.na(n), 0, n))
  
  plotreturn <-   mRS_data %>% 
    mutate(percent = case_when(
      OBS_name == top_name ~ n / Sum_P,
      OBS_name == bottom_name ~ n / Sum_D)
    ) %>%
    
    mutate(mRS = factor(mRS), y = as.numeric(factor(OBS_name, levels = c(bottom_name, top_name), ordered = TRUE))) %>%
    ggplot(aes(percent, y, fill = mRS)) +
    geom_segment(data = . %>% select(-y, -n) %>% 
                   pivot_wider(names_from = 'OBS_name', values_from = 'percent') %>%
                   mutate(across(c(2,3), cumsum)),
                 aes(y = 1.2, yend = 1.8, x = .data[[bottom_name]], xend = .data[[top_name]]),
                 alpha = 0.3) +
    geom_col(orientation = 'y', position = position_stack(reverse = TRUE),
             width = 0.4, col = 'gray60', size = 0.2) +
    geom_text(data = . %>% 
                group_by(OBS_name) %>% 
                mutate(value = cumsum(percent),
                       stackval = lag(value, default = 0) + 0.5 * percent),
              aes(x = stackval, label = paste0(n," (" ,scales::percent(percent, accuracy = 1), ")")), color = "grey", fontface = "bold") +
    scale_y_continuous(NULL, breaks = 1:2, labels = c(paste(bottom_name,"\n(N=", Sum_D, ")"), paste(top_name,"\n(N=", Sum_P, ")")),
                       expand = c(0.3, 0)) +
    scale_x_continuous(NULL, labels = scales::percent, expand = c(0, 0)) +
    guides(fill = guide_legend(nrow = 1, byrow = TRUE)) +
    
    theme_minimal() +
    theme(legend.position = 'top',
          panel.grid = element_blank(),
          axis.line.y = element_line(color = 'gray50'),
          plot.margin = margin(20, 20, 20, 20),
          axis.text.y = element_text(hjust = 0.5))
  if (color == "predefined" ) {
    plotreturn <- plotreturn + 
      scale_fill_manual(values=group.colors)
  } else if (color == "spectral" ) {
    plotreturn <- plotreturn + 
      scale_fill_brewer(palette = 'Spectral')
  } else if (color == "greys" ) {
    plotreturn <- plotreturn + 
      scale_fill_brewer(palette = 'Greys')
  }
  return(plotreturn)
}
