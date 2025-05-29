
library(shiny)

# UI
ui <- fluidPage(
  titlePanel("TAVR 1-Year Mortality Risk Calculator"),
  sidebarLayout(
    sidebarPanel(
      numericInput("baseline_kccq", "Baseline KCCQ:", value = 50),
      numericInput("frailty_5m_wt", "5m Walk Time (seconds):", value = 10),
      numericInput("hemoglobin", "Hemoglobin (g/dL):", value = 12),
      numericInput("sts_avmortality", "STS AV Mortality Score (%):", value = 4),
      numericInput("baseline_bmi", "BMI:", value = 26),
      
      radioButtons("pul_htn_group", "PASP:",
                   choices = c("PASP >50" = "high", "PASP <50" = "low")),
      
      radioButtons("currently_on_dialysis", "Currently on Dialysis:",
                   choices = c("Yes" = 1, "No" = 0)),
      
      radioButtons("dvt", "History of DVT:",
                   choices = c("Yes" = 1, "No" = 0)),
      
      radioButtons("history_of_chf", "History of CHF:",
                   choices = c("Yes" = 1, "No" = 0)),
      
      radioButtons("chf_2wks_tavr", "CHF within 2 Weeks of TAVR:",
                   choices = c("Yes" = 1, "No" = 0)),
      
      radioButtons("known_pad", "Known PAD:",
                   choices = c("Yes" = 1, "No" = 0)),
      
      selectInput("rhythm", "Rhythm:",
                  choices = c("Sinus Rhythm" = 1, "AFib" = 2,
                              "Aflutter" = 3, "Paced" = 4)),
      
      radioButtons("frailty_albumin_tertile", "Frailty Albumin:",
                   choices = c("<3.6" = "low", "3.6–4.0" = "mid", ">4.0" = "high")),
      
      actionButton("calc", "Calculate Risk")
    ),
    
    mainPanel(
      h3("Predicted 1-Year Mortality Risk:"),
      verbatimTextOutput("risk")
    )
  )
)

# Server
server <- function(input, output) {
  
  observeEvent(input$calc, {
    # Coefficients from your logistic model (example values shown)
    intercept <- -4.123
    coeffs <- list(
      baseline_kccq = -0.015,
      frailty_5m_wt = 0.042,
      hemoglobin = -0.145,
      sts_avmortality = 0.085,
      baseline_bmi = -0.018,
      pul_htn_grouphigh = 0.85,
      currently_on_dialysis1 = 1.12,
      dvt1 = 0.65,
      history_of_chf1 = 0.49,
      chf_2wks_tavr1 = 0.77,
      known_pad1 = 0.56,
      rhythm2 = 0.33, rhythm3 = 0.41, rhythm4 = 0.92,
      frailty_albumin_tertilemid = -0.22,
      frailty_albumin_tertilehigh = -0.45
    )
    
    # Build logit step by step
    logit <- intercept +
      coeffs$baseline_kccq * input$baseline_kccq +
      coeffs$frailty_5m_wt * input$frailty_5m_wt +
      coeffs$hemoglobin * input$hemoglobin +
      coeffs$sts_avmortality * input$sts_avmortality +
      coeffs$baseline_bmi * input$baseline_bmi +
      ifelse(input$pul_htn_group == "high", coeffs$pul_htn_grouphigh, 0) +
      ifelse(input$currently_on_dialysis == 1, coeffs$currently_on_dialysis1, 0) +
      ifelse(input$dvt == 1, coeffs$dvt1, 0) +
      ifelse(input$history_of_chf == 1, coeffs$history_of_chf1, 0) +
      ifelse(input$chf_2wks_tavr == 1, coeffs$chf_2wks_tavr1, 0) +
      ifelse(input$known_pad == 1, coeffs$known_pad1, 0) +
      ifelse(input$rhythm == 2, coeffs$rhythm2,
             ifelse(input$rhythm == 3, coeffs$rhythm3,
                    ifelse(input$rhythm == 4, coeffs$rhythm4, 0))) +
      ifelse(input$frailty_albumin_tertile == "mid", coeffs$frailty_albumin_tertilemid,
             ifelse(input$frailty_albumin_tertile == "high", coeffs$frailty_albumin_tertilehigh, 0))
    
    risk <- round(100 * (1 / (1 + exp(-logit))), 1)
    output$risk <- renderText({ paste0(risk, "%") })
  })
}

shinyApp(ui = ui, server = server)




