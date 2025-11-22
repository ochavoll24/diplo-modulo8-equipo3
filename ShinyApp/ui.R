
# ui.R
shinyUI(
  navbarPage(
    title = "[WHO] Capacidad Hospitalaria y métricas complementarias",
    theme = bslib::bs_theme(version = 5, bootswatch = "cosmo"),
    windowTitle = "Healthcare Analytics",
    
    # Custom CSS for styling
    tags$head(
      tags$style(HTML("
        .navbar-default {
          background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
          border: none;
        }
        .navbar-default .navbar-brand,
        .navbar-default .navbar-nav > li > a {
          color: white !important;
          font-weight: 600;
        }
        .well {
          background: linear-gradient(135deg, #f5f7fa 0%, #c3cfe2 100%);
          border: none;
          border-radius: 15px;
          box-shadow: 0 4px 6px rgba(0,0,0,0.1);
        }
        .metric-card {
          background: white;
          border-radius: 15px;
          padding: 20px;
          box-shadow: 0 4px 15px rgba(0,0,0,0.1);
          transition: transform 0.3s ease;
        }
        .metric-card:hover {
          transform: translateY(-5px);
          box-shadow: 0 6px 20px rgba(0,0,0,0.15);
        }
        .metric-value {
          font-size: 2em;
          font-weight: bold;
          color: #667eea;
        }
        .metric-label {
          font-size: 1em;
          color: #6c757d;
          margin-top: 5px;
        }
        h2, h3 {
          color: #2c3e50;
          font-weight: 600;
        }
      "))
    ),
    
    # ============================================
    # SUMMARY TAB
    # ============================================
    tabPanel(
      "Resumen de Resultados",
      icon = icon("chart-bar"),
      
      fluidRow(
        column(
          width = 12,
          tags$div(
            style = "background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); 
                     padding: 30px; border-radius: 15px; margin-bottom: 20px; color: white;",
            h2("Cuidado de la Salud - Métricas y Datos", style = "color: white; margin: 0;"),
            p("Reporte analítico acerca del cuidado de la salud por país y año (2015-2024). La data no está disponible para todas las opciones, por lo que el reporte cuenta con una pre-selección con lo mejor de los datos.", 
              style = "color: white; opacity: 0.9; margin-top: 10px;")
          )
        )
      ),
      
      fluidRow(
        column(
          width = 3,
          wellPanel(
            style = "background: white; border-radius: 15px; box-shadow: 0 4px 15px rgba(0,0,0,0.1);",
            h4(icon("filter"), " Filtros"),
            pickerInput(
              "summary_country",
              "Seleccionar un país:",
              choices = c("All", unique(healthcare_data$country)),
              selected = c("United States"),
              multiple = FALSE,
              options = list(
                `style` = "btn-primary",
                `live-search` = TRUE,
                `size` = 10
              )
            ),
            sliderTextInput(
              "summary_year",
              "Periodo de tiempo (años):",
              choices = seq(min(healthcare_data$yr), max(healthcare_data$yr)),
              selected = c(2019,2021),
              grid = TRUE,
              force_edges = TRUE
            ),
            br(),
            actionBttn(
              "update_summary",
              "selecciona y actualiza los visuales",
              style = "gradient",
              color = "primary",
              block = TRUE #,
              #icon = icon("refresh")
            )
          )
        ),
        
        column(
          width = 9,
          fluidRow(
            column(
              width = 3, 
              tags$div(
                class = "metric-card",
                style = "background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); color: white;",
                icon("dollar-sign", style = "font-size: 2em;"),
                br(), br(),
                tags$div(class = "metric-value", style = "color: white;", textOutput("avg_health_spend")),
                tags$div(class = "metric-label", style = "color: white; opacity: 0.9;", "Gasto en Salud [prom.;mio. USD]")
              )
            ),
            column(
              width = 3,
              tags$div(
                class = "metric-card",
                style = "background: linear-gradient(135deg, #f093fb 0%, #f5576c 100%); color: white;",
                icon("heartbeat", style = "font-size: 2em;"),
                br(), br(),
                tags$div(class = "metric-value", style = "color: white;", textOutput("avg_mortality")),
                tags$div(class = "metric-label", style = "color: white; opacity: 0.9;", "Mortalidad [promedio]")
              )
            ),
            column(
              width = 3,
              tags$div(
                class = "metric-card",
                style = "background: linear-gradient(135deg, #4facfe 0%, #00f2fe 100%); color: white;",
                icon("bed", style = "font-size: 2em;"),
                br(), br(),
                tags$div(class = "metric-value", style = "color: white;", textOutput("avg_beds")),
                tags$div(class = "metric-label", style = "color: white; opacity: 0.9;", "Camas de Hospital [promedio]")
              )
            ),
            column(
              width = 3,
              tags$div(
                class = "metric-card",
                style = "background: linear-gradient(135deg, #43e97b 0%, #38f9d7 100%); color: white;",
                icon("user-md", style = "font-size: 2em;"),
                br(), br(),
                tags$div(class = "metric-value", style = "color: white;", textOutput("avg_physicians")),
                tags$div(class = "metric-label", style = "color: white; opacity: 0.9;", "Médicos [promedio]")
              )
            )
          )
        )
      ),
      
      br(),
      
      fluidRow(
        column(
          width = 12,
          wellPanel(
            style = "background: white; border-radius: 15px; box-shadow: 0 4px 15px rgba(0,0,0,0.1);",
            h3(icon("table"), " Tabla de Datos"),
            DTOutput("summary_table")
          )
        )
      ),
      
      fluidRow(
        column(
          width = 6,
          wellPanel(
            style = "background: white; border-radius: 15px; box-shadow: 0 4px 15px rgba(0,0,0,0.1);",
            h3(icon("chart-area"), "Gasto en Salud - Distribución"),
            plotlyOutput("health_spend_boxplot", height = "400px")
          )
        ),
        column(
          width = 6,
          wellPanel(
            style = "background: white; border-radius: 15px; box-shadow: 0 4px 15px rgba(0,0,0,0.1);",
            h3(icon("chart-line"), "Mortalidad - Distribución"),
            plotlyOutput("mortality_boxplot", height = "400px")
          )
        )
      )
    ),
    
    # ============================================
    # TRENDS TAB
    # ============================================
    tabPanel(
      "Tendencias",
      icon = icon("chart-line"),
      
      fluidRow(
        column(
          width = 12,
          tags$div(
            style = "background: linear-gradient(135deg, #fa709a 0%, #fee140 100%); 
                     padding: 30px; border-radius: 15px; margin-bottom: 20px; color: white;",
            h2("Tendencias sobre el cuidado de la salud en el tiempo", style = "color: white; margin: 0;"),
            p("Identificar tendencias y patrones en las métricas de salud disponibles", 
              style = "color: white; opacity: 0.9; margin-top: 10px;")
          )
        )
      ),
      
      fluidRow(
        column(
          width = 3,
          wellPanel(
            style = "background: white; border-radius: 15px; box-shadow: 0 4px 15px rgba(0,0,0,0.1);",
            h4(icon("sliders-h"), " Controles"),
            
            
            pickerInput(
              "trend_countries",
              "Seleccionar países:",
              choices = unique(healthcare_data$country),
              selected = c("United States", "Canada", "United Kingdom"),
              multiple = TRUE,
              options = list(
                `style` = "btn-primary",
                `live-search` = TRUE,
                `size` = 10
              )
            ),
            
            
            # prettyCheckboxGroup(
            #   "trend_countries",
            #   "Select Countries:",
            #   choices = unique(healthcare_data$country),
            #   selected = c("United States", "Canada", "Germany", "United Kingdom"),
            #   status = "primary",
            #   shape = "curve",
            #   animation = "smooth",
            #   icon = icon("check")
            # ),
            
            br(),
            
            radioGroupButtons(
              "trend_metric",
              "Seleccionar Métrica:",
              choices = list(
                `<i class='fa fa-dollar-sign'></i> Health Spending` = "health_spend",
                `<i class='fa fa-heartbeat'></i> Mortality` = "mortality",
                `<i class='fa fa-bed'></i> Beds` = "beds",
                `<i class='fa fa-user-md'></i> Physicians` = "physicians"
              ),
              selected = "beds",
              justified = FALSE,
              direction = "vertical",
              status = "primary"
            )
          )
        ),
        
        column(
          width = 9,
          wellPanel(
            style = "background: white; border-radius: 15px; box-shadow: 0 4px 15px rgba(0,0,0,0.1);",
            h3(icon("chart-line"), " Tendencia en el tiempo"),
            plotlyOutput("trend_line_plot", height = "450px")
          )
        )
      ),
      
      fluidRow(
        column(
          width = 6,
          wellPanel(
            style = "background: white; border-radius: 15px; box-shadow: 0 4px 15px rgba(0,0,0,0.1);",
            h3(icon("percentage"), " Cambio año contra año"),
            plotlyOutput("yoy_change_plot", height = "400px")
          )
        ),
        column(
          width = 6,
          wellPanel(
            style = "background: white; border-radius: 15px; box-shadow: 0 4px 15px rgba(0,0,0,0.1);",
            h3(icon("flag"), " Comparación entre paises - vs. el último año"),
            plotlyOutput("country_comparison_bar", height = "400px")
          )
        )
      ),
      
      fluidRow(
        column(
          width = 12,
          wellPanel(
            style = "background: white; border-radius: 15px; box-shadow: 0 4px 15px rgba(0,0,0,0.1);",
            h3(icon("th"), " Mapa de Correlación"),
            plotlyOutput("correlation_heatmap", height = "500px")
          )
        )
      )
    ),
    
    # ============================================
    # ANALYSIS TAB
    # ============================================
    tabPanel(
      "Análisis",
      icon = icon("calculator"),
      
      fluidRow(
        column(
          width = 12,
          tags$div(
            style = "background: linear-gradient(135deg, #30cfd0 0%, #330867 100%); 
                     padding: 30px; border-radius: 15px; margin-bottom: 20px; color: white;",
            h2("Análisis de regresión lineal múltiple", style = "color: white; margin: 0;"),
            p("Construya y evalue modelos de regresión para medir y explicar la relación entre las métricas", 
              style = "color: white; opacity: 0.9; margin-top: 10px;")
          )
        )
      ),
      
      fluidRow(
        column(
          width = 3,
          wellPanel(
            style = "background: white; border-radius: 15px; box-shadow: 0 4px 15px rgba(0,0,0,0.1);",
            h4(icon("cog"), " Configuración del Modelo"),
            
            pickerInput(
              "dependent_var",
              label = tags$b("Variable Dependiente (Y):"),
              choices = list(
                "Mortalidad" = "mortality",
                "Gasto en Salud" = "health_spend",
                "Camas de Hospital" = "beds",
                "Médicos" = "physicians"
              ),
              selected = "mortality",
              options = list(
                `style` = "btn-info"
              )
            ),
            
            br(),
            
            prettyCheckboxGroup(
              "independent_vars",
              label = tags$b("Variables Independientes (X):"),
              choices = list(
                "Gasto en Salud" = "health_spend",
                "Camas de Hospital" = "beds",
                "Medicos" = "physicians",
                "Mortalidad" = "mortality",
                "Población" = "population",
                "Años" = "yr"
              ),
              selected = c("health_spend", "beds", "physicians"),
              status = "success",
              shape = "round",
              animation = "pulse",
              icon = icon("check")
            ),
            
            br(),
            
            actionBttn(
              "run_regression",
              "Ejecutar Modelo",
              style = "gradient",
              color = "success",
              block = TRUE,
              icon = icon("play"),
              size = "lg"
            ),
            
            br(), br(),
            
            materialSwitch(
              inputId = "show_diagnostics",
              label = "Mostrar diagnóstico del Modelo",
              value = TRUE,
              status = "primary"
            )
          )
        ),
        
        column(
          width = 9,
          wellPanel(
            style = "background: white; border-radius: 15px; box-shadow: 0 4px 15px rgba(0,0,0,0.1);",
            h3(icon("file-alt"), " Resultados del Modelo"),
            verbatimTextOutput("regression_summary")
          ),
          
          conditionalPanel(
            condition = "input.show_diagnostics == true",
            wellPanel(
              style = "background: white; border-radius: 15px; box-shadow: 0 4px 15px rgba(0,0,0,0.1);",
              h3(icon("stethoscope"), " Diagnósticos"),
              fluidRow(
                column(
                  width = 6,
                  plotlyOutput("residual_plot", height = "350px")
                ),
                column(
                  width = 6,
                  plotlyOutput("qq_plot", height = "350px")
                )
              )
            )
          )
        )
      ),
      
      fluidRow(
        column(
          width = 6,
          wellPanel(
            style = "background: white; border-radius: 15px; box-shadow: 0 4px 15px rgba(0,0,0,0.1);",
            h3(icon("bullseye"), " Valores Actuales vs Predicción"),
            plotlyOutput("actual_vs_predicted", height = "400px")
          )
        ),
        column(
          width = 6,
          wellPanel(
            style = "background: white; border-radius: 15px; box-shadow: 0 4px 15px rgba(0,0,0,0.1);",
            h3(icon("balance-scale"), " Importancia de Variables"),
            plotlyOutput("coefficient_plot", height = "400px")
          )
        )
      )
    ),
    
    # ============================================
    # FOOTER
    # ============================================
    footer = tags$div(
      style = "background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); 
               padding: 20px; text-align: center; color: white; margin-top: 30px;",
      p("[WHO] Capacidad Hospitalaria y métricas complementarias", 
        style = "margin: 0; font-size: 14px;")
    )
  )
)