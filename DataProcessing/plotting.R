

library(plotly)
library(scatterplot3d)

# Define a consistent color palette across index designations
color_palette <- c(
  'iTraxx Europe' = '#1f77b4',
  'iTraxx Europe Crossover' = '#ff7f0e',
  'iTraxx Europe Senior Financials' = '#2ca02c',
  'iTraxx Europe Sub Financials' = '#d62728',
  'CDX.EM' = '#9467bd',
  'CDX.NA.IG' = '#8c564b',
  'CDX.NA.HY' = '#e377c2',
  'iTraxx Asia ex-Japan IG' = '#7f7f7f',
  'iTraxx Australia' = '#bcbd22'
)


#' Plot 3D Scatter PLot for Priced Surfaces for Payer and Receiver Options
#'
#' Reads option pricing data from a CSV file
#' generates 3D scatter plots for payer and receiver options. 
#'
#' @param config list: of configuration parameters. 
#' @param in_path string: path to the input option pricing CSV file 
#' @param out_path string: path to the output png file to save 
#'
#' @return nothing 
#'
plot_option_3d_priced_surfaces_scatter <- function(config, in_path, out_path){
  
  # Read the input CSV file into a data.table
  all_opt_priced <- fread(in_path)
  
  # get unique parameter values 
  val_dates <- unique(all_opt_priced$val_date)
  mats <- unique(all_opt_priced$maturity)
  exps <- unique(all_opt_priced$expiryDate)
  cpns <- unique(all_opt_priced$coupon)
  recs <- unique(all_opt_priced$recovery)
  ivs <- unique(all_opt_priced$impliedVol)
  
  # subset df for mtm against spread and implied vol for;
  # val_date = "2024-05-30"
  # maturity = "2028-12-20" 
  # coupon = 500 
  # recovery = 0.3
  # impliedVol = 0.303 
  # optionType = payer 
  df_payer <- all_opt_priced[val_date == val_dates[1] &
                               maturity == mats[1] &
                               expiryDate == exps[1] &
                               coupon == cpns[1]  & 
                               recovery == recs[1] & 
                               optionType == 1 &    
                               impliedVol == ivs[2]] 
  
  df_receiver <- all_opt_priced[val_date == val_dates[1] &
                                  maturity == mats[1] &
                                  expiryDate == exps[1] &
                                  coupon == cpns[1]  & 
                                  recovery == recs[1] & 
                                  optionType == -1 &    
                                  impliedVol == ivs[2]] 
  
  # Save the plots to a PNG file
  png(filename = out_path, width = 1600, height = 800)
  
  # Set up the layout: 1 row, 2 columns
  par(mfrow = c(1, 2), cex.main = 1.8, cex.lab = 1.6, cex.axis = 1.2)
  
  # Plot for df_payer
  scatterplot3d(df_payer$logM, df_payer$spread, df_payer$mtm, color = "blue", pch = 20, 
                main = paste0('Payer MtM Surface: Expiry (', exps[1], ') implied vol (', round(ivs[2], 2), ')'),
                xlab = "logM", ylab = "Spread", zlab = "MTM")
  
  # Plot for df_receiver
  scatterplot3d(df_receiver$logM, df_receiver$spread, df_receiver$mtm, color = "red", pch = 20, 
                main = paste0('Receiver MtM Surface: Expiry (', exps[1], ') implied vol (', round(ivs[2], 2), ')'),
                xlab = "logM", ylab = "Spread", zlab = "MTM")
  
  # Close the PNG device
  dev.off()
}

#' Plot 3D Priced Surfaces for Payer and Receiver Options
#'
#' Reads option pricing data from a CSV file
#' generates 3D surface plots for payer and receiver options. 
#'
#' @param config list: of configuration parameters. 
#' @param in_path string: path to the input option pricing CSV file 
#' @param out_path string: path to the output png file to save  (NOT USED)
#' @param type string: 'Payer' or 'Receiver'  
#'
#' @return nothing 
#'
plot_option_3d_priced_surface <- function(config, in_path, out_path, type='Payer'){
  
  if (type == 'Payer'){
    otype = 1
  }else if (type == 'Receiver'){
    otype = -1
  }else{
    stop(paste0('plot_option_3d_priced_surface: unrecognised type ', type) )
  }
  
  iv_index <- 3
  
  all_opt_priced <- fread(in_path)
  
  # get unique parameter values 
  val_dates <- unique(all_opt_priced$val_date)
  mats <- unique(all_opt_priced$maturity)
  exps <- unique(all_opt_priced$expiryDate)
  cpns <- unique(all_opt_priced$coupon)
  recs <- unique(all_opt_priced$recovery)
  ivs <- unique(all_opt_priced$impliedVol)
  
  # subset df for mtm against spread and implied vol for;
  # val_date = "2024-05-30"
  # maturity = "2028-12-20" 
  # coupon = 500 
  # recovery = 0.3
  # impliedVol = 0.303 
  # optionType = payer or receiver 
  df <- all_opt_priced[val_date == val_dates[1] &
                         maturity == mats[1] &
                         expiryDate == exps[1] &
                         coupon == cpns[1]  & 
                         recovery == recs[1] & 
                         optionType == otype &    
                         impliedVol == ivs[iv_index]] 
  
  # Create the 3D surface plot
  fig <- plot_ly(
    df,
    x = ~logM,
    y = ~spread,
    z = ~mtm,
    type = "mesh3d",
    intensity = ~mtm,
    colors = colorRamp(c("yellow", "green", "red"))
  )
  
  # Customize the layout with font sizes
  fig <- fig %>% layout(
    scene = list(
      xaxis = list(
        title = list(text = "logM", font = list(size = 16)),
        tickfont = list(size = 14)
      ),
      yaxis = list(
        title = list(text = "Spread", font = list(size = 16)),
        tickfont = list(size = 14)
      ),
      zaxis = list(
        title = list(text = "MTM", font = list(size = 16)),
        tickfont = list(size = 14)
      )
    ),
    title = list(
      text = paste0(type, '  MtM Surface: ', 'Expiry (', exps[1], 
                    ') implied vol  (', round(ivs[iv_index],2), ')' ),
      font = list(size = 18)
    )
  )
  
  #  display in the RStudio viewer
  fig 
}


#' Create a Plot of Spreads Over Relative Series
#'
#' This function creates a plot of spreads over a relative series
#'
#' @param data data table including cols `relOTR`, `spread`, `indexDesignation`
#' @param tenor  character string '3Y', '5Y', '7Y' or '10Y'
#' @param show_legend boolean T = display legend  
#' @param y_limits numeric vector of length 2 for the limits for the y-axis.
#' @param show_y_label boolean T = display the y-axis label 
#' @return ggplot2 plot object.
#'
create_plot <- function(data, tenor, show_legend, y_limits, show_y_label) {
  p <- ggplot(data, aes(x = relOTR, y = spread, color = indexDesignation)) +
    geom_line() +
    labs(title = paste0('Maturity ', tenor), x = "rel OTR") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(size = 7),  
          axis.text.y = element_text(size = 7)) +  
    # Set x-axis ticks every 2 units
    scale_x_continuous(breaks = seq(min(data$relOTR), max(data$relOTR), by = 2)) + 
    scale_y_continuous(limits = y_limits) +  # Set y-axis limits
    scale_color_manual(values = color_palette)  
  
  if (show_legend) {
    p <- p + theme(legend.position = c(0, 1), legend.justification = c(0, 1),
                   legend.title = element_blank(), legend.text = element_text(size = 7))
  } else {
    p <- p + theme(legend.position = "none")
  }
  
  if (show_y_label) {
    p <- p + labs(y = "Spread")
  } else {
    p <- p + labs(y = NULL)
  }
  
  return(p)
}



#' Plot Index Spreads for Various Tenors
#'
#' Creates and arranges plots of index spreads for various tenors. 
#' It includes customizations for specific tenors and sets the overall y-axis 
#' limits based on the provided data.
#'
#' @param in_path string: path to index market data 
#' @return grid of ggplot2 plots arranged in a single row.
#' 
plot_index_spreads <- function(in_path) {
  
  idx_data <- fread(in_path)
  
  tenors <- c('3Y', '5Y', '7Y', '10Y')
  plot_list <- list()
  show_legend <- FALSE
  
  # Determine the overall y-axis limits
  y_min <- min(idx_data$spread) * 10000 
  y_max <- max(idx_data$spread) * 10000
  y_limits <- c(y_min, y_max)
  
  for (i in seq_along(tenors)) {
    t <- tenors[i]
    data <- idx_data[(indexDesignation == 'iTraxx Europe' | indexDesignation == 'iTraxx Europe Crossover') & tenor == t,
                     .(relOTR, spread = spread * 10000, indexDesignation)]
    
    if (t == '5Y') {
      additional_data <- idx_data[(indexDesignation == 'iTraxx Europe Senior Financials' | indexDesignation == 'iTraxx Europe Sub Financials' |
                                     indexDesignation == 'CDX.EM' | indexDesignation == 'CDX.NA.IG' | indexDesignation == 'CDX.NA.HY' |
                                     indexDesignation == 'iTraxx Asia ex-Japan IG' | indexDesignation == 'iTraxx Australia') & tenor == t,
                                  .(relOTR, spread = spread * 10000, indexDesignation)]
      data <- rbind(data, additional_data)
    }
    
    if (t == '10Y') {
      additional_data <- idx_data[(indexDesignation == 'iTraxx Europe Senior Financials' | indexDesignation == 'iTraxx Europe Sub Financials') & tenor == t,
                                  .(relOTR, spread = spread * 10000, indexDesignation)]
      data <- rbind(data, additional_data)
    }
    show_legend <- TRUE  
    show_y_label <- (i == 1)  # Only show y-axis label for the first plot
    plot_list[[t]] <- create_plot(data, t, show_legend, y_limits, show_y_label)
  }
  
  grid.arrange(grobs = plot_list, nrow = 1)
}


#' Plot t-Distribution Fit to Returns
#'
#' Creates a density plot of the provided returns and overlays a Student-t 
#' distribution fit with specified mean, standard dev, and degrees of freedom.
#'
#' @param in_path string: path to index market data 
#' @return ggplot2 plot object of density of returns and fitted t-distribution.
#'
plot_t_distribution_fit_to_returns <- function(in_path) {
  
  # Load 7D historic returns (2008-2024) for iTraxx Europe Crossover -1 10Y
  # This is the index with largest spread as of "2024-05-30" 
  returns <- fread(in_path)
  
  # Fit Student-t distribution to 7D returns 
  suppressWarnings(
    fit <- fitdistr(returns$return7D, "t")
  )
  print(fit)
  
  df <- fit$estimate["df"] # degrees of freedom 
  mean <- fit$estimate["m"]
  sd <- fit$estimate["s"]
  
  data <- data.frame(value = returns)
  
  p <- ggplot(data, aes(x = return7D)) +
    geom_density(aes(y = ..density..), fill = "blue", alpha = 0.4) +
    stat_function(fun = function(x) dt((x - mean) / sd, df) / sd, color = "black", size = 1) +
    labs(title = sprintf("Student-t fit to iTraxx Europe Crossover -1 10Y: \nmean = %.2f, sd = %.3f, df = %.2f", 
                         mean, sd, df),
         x = "Return", y = "Density") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, size=15),
          axis.text.x = element_text(size=13),
          axis.text.y = element_text(size=13),
          axis.title.x = element_text(size=14),
          axis.title.y = element_text(size=14)) +
    scale_x_continuous(limits = c(-0.4, 0.4))
  
  print(p)
}

#' Plot All Option Smiles from market Data 
#'
#' @param in_path string: path to option market data 
#' @return 2 x (2x2 grid) of plots for each relOTR, showing implied volatility smiles
plot_all_smiles <- function(in_path){
  
  # Load raw option data for spreads as of "2024-05-30" 
  opt_data <- fread(in_path)
  
  # Plot some option smiles per index, relOTR and expiry date 
  relOTRs <- c(0,-1)
  for (r in 1:length(relOTRs)){
    rel <- relOTRs[r]
    relOTR_name <- as.character(rel)
    plot_option_smile(opt_data, rel, relOTR_name) 
  }
  
}


#' Plot Option Smiles
#'
#' generates a 2x2 grid of plots displaying the implied volatility smile for 
#' options for a given expiry month. 
#'
#' @param option_data data table: containing option data including cols for `expiryDate`, `indexDesignation`, `logM`, `impliedVol`, and `relOTR`.
#' @param rel Date: specifying the relative series (rel OTR) 0 or -1
#' @param relOTR_name character string: representing the relative OTR  
#' @return A 2x2 grid of plots, showing implied volatility smiles
plot_option_smile <- function(opt_data, rel, relOTR_name) {
  
  # Filter data to include only the specified expiry date
  data <- opt_data[relOTR == rel]
  # Determine the common x and y limits
  #x_limits <- range(opt_data$logM, na.rm = TRUE)
  x_limits <- c(-0.5, 1.0)
  y_limits <- range(data$impliedVol, na.rm = TRUE)
  y_limits[2] <- y_limits[2]+0.1
  
  plots <- list()
  plot_index <- 1
  
  # Generate plots for each unique indexDesignation
  for (index in unique(data$indexDesignation)) {
    # Filter data for the current indexDesignation
    data_filtered <- data[indexDesignation == index]
    y_label <- ifelse(plot_index %% 2 == 0, "", "Implied Volatility")
    
    p <- ggplot(data_filtered, aes(x = logM, y = impliedVol, color = as.factor(as.character(expiryMonth)))) +
      geom_point(size=1) +
      labs(title = paste0(index, ' : OTR  ', relOTR_name),
           x = "logM",
           y = y_label,
           color = "expiry") +
      scale_x_continuous(limits = x_limits) +
      scale_y_continuous(limits = y_limits) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 10),
        legend.position = c(0, 1),
        legend.justification = c(0, 1)
      )
    
    plots[[index]] <- p
    plot_index <- plot_index + 1
  }
  
  # Arrange the plots in a 2x2 grid
  grid.arrange(grobs = plots, ncol = 2)
}



#' Generate a Histogram
#'
#' generates a histogram for a specified column in a data frame 
#'
#' @param data data frame: containing the data to be plotted.
#' @param column_name character string: specifying the name of the column 
#' @param bwidth numeric value: specifying the bin width for the histogram.
#' @return ggplot object representing the histogram.
generate_histogram <- function(data, column_name, bwidth) {
  ggplot(data, aes_string(x = column_name)) +
    geom_histogram(binwidth = bwidth, fill = "blue", color = "black") +
    labs(title = paste0(column_name, ' Synthetic Distribution' ),
         x = column_name,
         y = "Frequency") +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
      axis.title.x = element_text(size = 9),
      axis.title.y = element_text(size = 9),
      axis.text.x = element_text(size = 9),
      axis.text.y = element_text(size = 9))
}


#' Plot Mean MTM and MTM Difference vs Mat Duration Increment
#'
#' Read file containing pricing data, filters the data to include only rows for a recovery, coupon snapshot
#' computes the mean mark-to-market (MTM) and the mean absolute difference
#'
#' @param in_path string:  pricing data file path 
#' 
#' @return gradient of the mean MTM difference with respect to the maturity duration increment.
#' 
plot_mtm_vs_duration_increment <- function(in_path){
  # avg mtm diff per spread increment per maturity duration increment 
  idx_priced <- fread(in_path)
  idx_priced[, abs_mtm := abs(mtm)]
  idx_priced <- idx_priced[recovery==0.4 & coupon == 100]
  all_cd <- sort(unique(idx_priced$mat_duration))
  out <- data.table(durations = all_cd, 
                    mean_mtm = numeric(length(all_cd)),
                    mean_mtm_diff = numeric(length(all_cd)))
  for (i in 1:length(all_cd)){
    
    df <- idx_priced[mat_duration == all_cd[i]][order(spread)]
    df$abs_mtm_diff <- c(NA, diff(df$abs_mtm))
    mean_diff <- mean(df$abs_mtm_diff, na.rm = T)
    mean_mtm <- mean(df$mtm)
    out[i,]$mean_mtm_diff <- mean_diff
    out[i,]$mean_mtm <- mean_mtm
  }
  
  plot(out$durations, out$mean_mtm_diff)
  plot(out$durations, out$mean_mtm)
  grad_mtm_per_cd_increment <- (out$mean_mtm_diff[nrow(out)] - out$mean_mtm_diff[1] ) / (out$durations[nrow(out)] - out$durations[1])
  
  return(grad_mtm_per_cd_increment)
}
