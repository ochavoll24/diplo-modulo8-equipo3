


# server.R
shinyServer(function(input, output, session) {
  
  # ============================================
  # REACTIVE DATA FILTERING
  # ============================================
  
  # Filtered data for summary tab
  summary_filtered_data <- reactive({
    data <- healthcare_data
    
    # Filter by country
    if (input$summary_country != "All") {
      data <- data %>% filter(country == input$summary_country)
    }
    
    # Filter by year range
    data <- data %>% filter(yr >= input$summary_year[1] & yr <= input$summary_year[2])
    
    return(data)
  })
  
  # Filtered data for trends tab
  trend_filtered_data <- reactive({
    healthcare_data %>% filter(country %in% input$trend_countries)
  })
  
  # ============================================
  # SUMMARY TAB OUTPUTS
  # ============================================
  
  # Summary statistics
  output$avg_health_spend <- renderText({
    avg_val <- mean(summary_filtered_data()$health_spend, na.rm = TRUE)
    paste0("$", format(round(avg_val, 2), big.mark = ","))
  })
  
  output$avg_mortality <- renderText({
    avg_val <- mean(summary_filtered_data()$mortality, na.rm = TRUE)
    paste0(format(round(avg_val, 2), nsmall = 2), " per 1,000")
  })
  
  output$avg_beds <- renderText({
    avg_val <- mean(summary_filtered_data()$beds, na.rm = TRUE)
    paste0(format(round(avg_val, 2), nsmall = 2), " per 1,000")
  })
  
  output$avg_physicians <- renderText({
    avg_val <- mean(summary_filtered_data()$physicians, na.rm = TRUE)
    paste0(format(round(avg_val, 2), nsmall = 2), " per 1,000")
  })
  
  # Summary data table
  output$summary_table <- renderDT({
    datatable(
      summary_filtered_data() %>%
        arrange(country, yr) %>%
        mutate(
          population = format(population, big.mark = ","),
          health_spend = paste0("$", format(round(health_spend, 2), big.mark = ","))
        ),
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        dom = 'Bfrtip'
      ),
      rownames = FALSE,
      colnames = c("Country", "Year", "Beds", "Physicians", "Population", 
                   "Health Spending", "Mortality")
    )
  })
  
  # Health spending boxplot
  output$health_spend_boxplot <- renderPlotly({
    p <- ggplot(summary_filtered_data(), aes(x = reorder(country, health_spend, median), 
                                             y = health_spend, fill = country)) +
      geom_boxplot(alpha = 0.7) +
      scale_fill_manual(values = country_colors) +
      coord_flip() +
      labs(x = "Country", y = "Health Spending (USD per capita)", 
           title = "Health Spending Distribution by Country") +
      theme_minimal() +
      theme(legend.position = "none")
    
    ggplotly(p, tooltip = c("y", "x"))
  })
  
  # Mortality boxplot
  output$mortality_boxplot <- renderPlotly({
    p <- ggplot(summary_filtered_data(), aes(x = reorder(country, mortality, median), 
                                             y = mortality, fill = country)) +
      geom_boxplot(alpha = 0.7) +
      scale_fill_manual(values = country_colors) +
      coord_flip() +
      labs(x = "Country", y = "Mortality Rate (per 1,000)", 
           title = "Mortality Rate Distribution by Country") +
      theme_minimal() +
      theme(legend.position = "none")
    
    ggplotly(p, tooltip = c("y", "x"))
  })
  
  # ============================================
  # TRENDS TAB OUTPUTS
  # ============================================
  
  # Main trend line plot
  output$trend_line_plot <- renderPlotly({
    data <- trend_filtered_data()
    metric_name <- var_labels[input$trend_metric]
    
    p <- ggplot(data, aes(x = yr, y = .data[[input$trend_metric]], 
                          color = country, group = country)) +
      geom_line(size = 1.2) +
      geom_point(size = 2.5) +
      scale_color_manual(values = country_colors) +
      scale_x_continuous(breaks = unique(data$yr)) +
      labs(x = "Year", y = metric_name, 
           title = paste(metric_name, "Trends Over Time"),
           color = "Country") +
      theme_minimal() +
      theme(legend.position = "bottom")
    
    ggplotly(p, tooltip = c("x", "y", "colour"))
  })
  
  # Year-over-year change plot
  output$yoy_change_plot <- renderPlotly({
    data <- trend_filtered_data() %>%
      arrange(country, yr) %>%
      group_by(country) %>%
      mutate(yoy_change = (.data[[input$trend_metric]] - lag(.data[[input$trend_metric]])) / 
               lag(.data[[input$trend_metric]]) * 100) %>%
      filter(!is.na(yoy_change))
    
    metric_name <- var_labels[input$trend_metric]
    
    p <- ggplot(data, aes(x = yr, y = yoy_change, fill = country)) +
      geom_col(position = "dodge", alpha = 0.8) +
      scale_fill_manual(values = country_colors) +
      scale_x_continuous(breaks = unique(data$yr)) +
      labs(x = "Year", y = "YoY Change (%)", 
           title = paste("Year-over-Year Change in", metric_name),
           fill = "Country") +
      theme_minimal() +
      theme(legend.position = "bottom")
    
    ggplotly(p, tooltip = c("x", "y", "fill"))
  })
  
  # Country comparison bar chart for latest year
  output$country_comparison_bar <- renderPlotly({
    latest_year <- max(healthcare_data$yr)
    data <- trend_filtered_data() %>%
      filter(yr == latest_year)
    
    metric_name <- var_labels[input$trend_metric]
    
    p <- ggplot(data, aes(x = reorder(country, .data[[input$trend_metric]]), 
                          y = .data[[input$trend_metric]], fill = country)) +
      geom_col(alpha = 0.8) +
      scale_fill_manual(values = country_colors) +
      coord_flip() +
      labs(x = "Country", y = metric_name, 
           title = paste(metric_name, "in", latest_year)) +
      theme_minimal() +
      theme(legend.position = "none")
    
    ggplotly(p, tooltip = c("x", "y"))
  })
  
  # Correlation heatmap
  output$correlation_heatmap <- renderPlotly({
    cor_data <- healthcare_data %>%
      select(beds, physicians, health_spend, mortality, yr) %>%
      cor(use = "complete.obs")
    
    cor_melted <- as.data.frame(as.table(cor_data))
    names(cor_melted) <- c("Var1", "Var2", "Correlation")
    
    p <- ggplot(cor_melted, aes(Var1, Var2, fill = Correlation)) +
      geom_tile(color = "white") +
      scale_fill_gradient2(low = "#d73027", mid = "white", high = "#4575b4",
                           midpoint = 0, limit = c(-1, 1)) +
      geom_text(aes(label = round(Correlation, 2)), color = "black", size = 4) +
      labs(x = "", y = "", title = "Correlation Matrix of Healthcare Variables") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p, tooltip = c("x", "y", "fill"))
  })
  
  # ============================================
  # ANALYSIS TAB OUTPUTS
  # ============================================
  
  # Reactive regression model
  regression_model <- eventReactive(input$run_regression, {
    req(input$dependent_var, input$independent_vars)
    
    # Create formula
    formula_str <- paste(input$dependent_var, "~", 
                         paste(input$independent_vars, collapse = " + "))
    formula_obj <- as.formula(formula_str)
    
    # Fit model
    model <- lm(formula_obj, data = healthcare_data)
    
    return(model)
  })
  
  # Regression summary output
  output$regression_summary <- renderPrint({
    req(regression_model())
    summary(regression_model())
  })
  
  # Residual plot
  output$residual_plot <- renderPlotly({
    req(regression_model())
    model <- regression_model()
    
    residual_data <- data.frame(
      fitted = fitted(model),
      residuals = residuals(model)
    )
    
    p <- ggplot(residual_data, aes(x = fitted, y = residuals)) +
      geom_point(alpha = 0.6, color = "#2c3e50") +
      geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
      geom_smooth(method = "loess", se = TRUE, color = "#3498db") +
      labs(x = "Fitted Values", y = "Residuals", 
           title = "Residuals vs Fitted Values") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Q-Q plot
  output$qq_plot <- renderPlotly({
    req(regression_model())
    model <- regression_model()
    
    residuals_std <- rstandard(model)
    theoretical_quantiles <- qqnorm(residuals_std, plot.it = FALSE)
    
    qq_data <- data.frame(
      theoretical = theoretical_quantiles$x,
      sample = theoretical_quantiles$y
    )
    
    p <- ggplot(qq_data, aes(x = theoretical, y = sample)) +
      geom_point(alpha = 0.6, color = "#2c3e50") +
      geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
      labs(x = "Theoretical Quantiles", y = "Sample Quantiles", 
           title = "Normal Q-Q Plot") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Actual vs Predicted plot
  output$actual_vs_predicted <- renderPlotly({
    req(regression_model())
    model <- regression_model()
    
    pred_data <- data.frame(
      actual = healthcare_data[[input$dependent_var]],
      predicted = fitted(model),
      country = healthcare_data$country
    )
    
    p <- ggplot(pred_data, aes(x = actual, y = predicted, color = country)) +
      geom_point(alpha = 0.6, size = 3) +
      geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
      scale_color_manual(values = country_colors) +
      labs(x = paste("Actual", var_labels[input$dependent_var]), 
           y = paste("Predicted", var_labels[input$dependent_var]),
           title = "Actual vs Predicted Values",
           color = "Country") +
      theme_minimal() +
      theme(legend.position = "bottom")
    
    ggplotly(p, tooltip = c("x", "y", "colour"))
  })
  
  # Coefficient plot for variable importance
  output$coefficient_plot <- renderPlotly({
    req(regression_model())
    model <- regression_model()
    
    coef_data <- data.frame(
      variable = names(coef(model))[-1],  # Exclude intercept
      coefficient = coef(model)[-1],
      std_error = summary(model)$coefficients[-1, 2]
    ) %>%
      mutate(
        lower = coefficient - 1.96 * std_error,
        upper = coefficient + 1.96 * std_error
      )
    
    p <- ggplot(coef_data, aes(x = reorder(variable, coefficient), y = coefficient)) +
      geom_point(size = 4, color = "#2c3e50") +
      geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, color = "#3498db") +
      geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
      coord_flip() +
      labs(x = "Variable", y = "Coefficient (95% CI)", 
           title = "Regression Coefficients with Confidence Intervals") +
      theme_minimal()
    
    ggplotly(p, tooltip = c("x", "y"))
  })
  
})