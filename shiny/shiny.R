library(shiny)
library(mrgsolve)
library(dplyr)
library(tidyr)
library(ggplot2)
library(readxl)

# Load models
mod1 <- mrgsolve::mread("mod_Abdelgawad2022.txt")
mod2 <- mrgsolve::mread("mod_Chang2015.txt")
mod3 <- mrgsolve::mread("mod_Chirehwa2016.txt")
mod4 <- mrgsolve::mread("mod_Denti2016.txt")
mod5 <- mrgsolve::mread("mod_Gao2021.txt")
mod6 <- mrgsolve::mread("mod_Jeremiah2014.txt")
mod7 <- mrgsolve::mread("mod_Jing2016.txt")
mod8 <- mrgsolve::mread("mod_Karballaei2022.txt")
mod9 <- mrgsolve::mread("mod_King2021.txt")
mod10 <- mrgsolve::mread("mod_Kloprogge2020.txt")
mod11 <- mrgsolve::mread("mod_Marsot2017.txt")
mod12 <- mrgsolve::mread("mod_Medellin2020.txt")
mod13 <- mrgsolve::mread("mod_Milan2013.txt")
mod14 <- mrgsolve::mread("mod_Mukonzo2020.txt")
mod15 <- mrgsolve::mread("mod_Nishimura2020.txt")
mod16 <- mrgsolve::mread("mod_Perumal2022.txt")
mod17 <- mrgsolve::mread("mod_Savic2015.txt")
mod18 <- mrgsolve::mread("mod_Schipani2016.txt")
mod19 <- mrgsolve::mread("mod_Sekaggya2019.txt")
mod20 <- mrgsolve::mread("mod_Seng2015.txt")
mod21 <- mrgsolve::mread("mod_Sloan2017.txt")
mod22 <- mrgsolve::mread("mod_Soedarsono2023.txt")
mod23 <- mrgsolve::mread("mod_Wilkins2008.txt")
mod24 <- mrgsolve::mread("mod_Naidoo2019.txt")
mod25 <- mrgsolve::mread("mod_Aruldhas2019.txt")
mod26 <- mrgsolve::mread("mod_Denti2022.txt")
mod27 <- mrgsolve::mread("mod_Horita2018.txt")
mod28 <- mrgsolve::mread("mod_Panjasawatwong2020.txt")
mod29 <- mrgsolve::mread("mod_Zvada2014.txt")

mod_list <- list(
  "Abdelgawad2022" = mod1,
  "Chang2015" = mod2,
  "Chirehwa2016" = mod3,
  "Denti2016" = mod4,
  "Gao2021" = mod5,
  "Jeremiah2014" = mod6,
  "Jing2016" = mod7,
  "Karballaei2022" = mod8,
  "King2021" = mod9,
  "Kloprogge2020" = mod10,
  "Marsot2017" = mod11,
  "Medellin2020" = mod12,
  "Milan2013" = mod13,
  "Mukonzo2020" = mod14,
  "Nishimura2020" = mod15,
  "Perumal2022" = mod16,
  "Savic2015" = mod17,
  "Schipani2016" = mod18,
  "Sekaggya2019" = mod19,
  "Seng2015" = mod20,
  "Sloan2017" = mod21,
  "Soedarsono2023" = mod22,
  "Wilkins2008" = mod23,
  "Naidoo2019" = mod24,
  "Aruldhas2019" = mod25,
  "Denti2022" = mod26,
  "Horita2018" = mod27,
  "Panjasawatwong2020" = mod28,
  "Zvada2014" = mod29
)

ui <- fluidPage(
  titlePanel(
    fluidRow(
      column(8, "Rifampin PopPK Model Repository"),
      column(4, tags$img(src = "logo.png", height = "50px", align = "right"))
    )
  ),
  sidebarLayout(
    sidebarPanel(
      selectInput("model", "Select Model", choices = names(mod_list)),
      numericInput("BW", "Body Weight (kg)", value = 70),
      numericInput("FFM", "Fat-Free Mass (kg)", value = 56),
      numericInput("DOSE", "Dose (mg)", value = 450),
      numericInput("AGE", "Age (years)", value = 30),
      selectInput("SEX", "Sex", choices = c("Female", "Male")),
      selectInput("DM", "Diabetes Mellitus", choices = c("No", "Yes")),
      actionButton("simulate", "Simulate")
    ),
    mainPanel(
      plotOutput("concentrationPlot")
    )
  )
)

server <- function(input, output) {
  observeEvent(input$simulate, {
    BW <- input$BW
    FFM <- input$FFM
    DOSE <- input$DOSE
    Age <- input$AGE
    DM <- input$DM
    SEX <- input$SEX
    model_name <- input$model
    model <- mod_list[[model_name]]

    # 确定模型文件中是否有OCC参数
    model_params <- names(param(model))
    has_OCC <- "OCC" %in% model_params

    process_data <- function(BW, FFM, DOSE, AGE, DM, SEX, model, OCC = 1, nsim = 1000) {
      input_data <- data.frame(ID = 1:nsim, BW = BW, FFM = FFM, DOSE = DOSE, Age = Age, DM = DM, SEX = SEX)
      input_data$DM <- as.numeric(factor(input_data$DM, levels = c("No", "Yes"))) - 1
      input_data$SEX <- as.numeric(factor(input_data$SEX, levels = c("Female", "Male"))) - 1
      input_data$OCC <- OCC

      sim_data <- tidyr::expand_grid(ID = input_data$ID, time = seq(0, 24, by = 0.1)) %>%
        left_join(input_data, by = "ID") %>%
        mutate(amt = DOSE, evid = ifelse(time == 0, 1, 0), cmt = 1) %>%
        select(ID, time, amt, evid, cmt, BW, FFM, Age, DM, SEX, OCC)

      out <- model %>%
        data_set(sim_data) %>%
        carry_out(amt) %>%
        mrgsim()

      out <- as.data.frame(out)

      summary <- out %>%
        group_by(time) %>%
        summarise(
          mean_CP = mean(CP),
          lower_CI = quantile(CP, 0.05),
          upper_CI = quantile(CP, 0.95)
        )

      return(summary)
    }

    if(has_OCC) {
      # 如果模型包含IOV并且有OCC参数，则进行两次仿真，分别表示不同场合
      summary1 <- process_data(BW, FFM, DOSE, AGE, DM, SEX, model, OCC = 1)
      summary2 <- process_data(BW, FFM, DOSE, AGE, DM, SEX, model, OCC = 2)

      output$concentrationPlot <- renderPlot({
        ggplot() +
          geom_line(data = summary1, aes(x = time, y = mean_CP), color = "blue") +
          geom_ribbon(data = summary1, aes(x = time, ymin = lower_CI, ymax = upper_CI), fill = "blue", alpha = 0.2) +
          geom_line(data = summary2, aes(x = time, y = mean_CP), color = "red") +
          geom_ribbon(data = summary2, aes(x = time, ymin = lower_CI, ymax = upper_CI), fill = "red", alpha = 0.2) +
          labs(title = paste("Drug Concentration Over Time (", model_name, ")", sep = ""),
               x = "Time after administration (hours)",
               y = "Concentration (mg/L)") +
          scale_y_continuous(limits = c(-1,35)) +
          annotate("rect", xmin = 0, xmax = 24, ymin = 8, ymax = 24, alpha = 0.1, fill = "blue") +
          theme_minimal(base_size = 15) +
          theme(
            axis.title.x = element_text(size = 16),
            axis.title.y = element_text(size = 16),
            axis.text.x = element_text(size = 14),
            axis.text.y = element_text(size = 14),
            plot.title = element_text(size = 18, face = "bold"),
            legend.position = "none",
            plot.background = element_blank(),
            panel.grid.minor = element_blank(),
            panel.grid.major = element_blank(),
            panel.border = element_blank(),
            axis.line = element_line(color = "black")
          ) +
          annotate("text", x = 12.5, y = 30, label = "Solid line (Blue): Occasion 1", hjust = 0, size = 5) +
          annotate("text", x = 12.5, y = 28, label = "Shaded area (Blue): 90% PI for Occasion 1", hjust = 0, size = 5) +
          annotate("text", x = 12.5, y = 26, label = "Solid line (Red): Occasion 2", hjust = 0, size = 5) +
          annotate("text", x = 12.5, y = 24, label = "Shaded area (Red): 90% PI for Occasion 2", hjust = 0, size = 5) +
          annotate("text", x = 12.5, y = 22, label = "Purple shade: Therapeutic range", hjust = 0, size = 5)
      })

    } else {
      # 如果模型不包含IOV或没有OCC，只进行一次仿真
      summary <- process_data(BW, FFM, DOSE, AGE, DM, SEX, model)

      output$concentrationPlot <- renderPlot({
        ggplot(summary, aes(x = time)) +
          geom_line(aes(y = mean_CP), color = "black") +
          geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI), fill = "orange", alpha = 0.2) +
          labs(title = paste("Drug Concentration Over Time (", model_name, ")", sep = ""),
               x = "Time after administration (hours)",
               y = "Concentration (mg/L)") +
          scale_y_continuous(limits = c(-1,35)) +
          annotate("rect", xmin = 0, xmax = 24, ymin = 8, ymax = 24, alpha = 0.1, fill = "blue") +
          theme_minimal(base_size = 15) +
          theme(
            axis.title.x = element_text(size = 16),
            axis.title.y = element_text(size = 16),
            axis.text.x = element_text(size = 14),
            axis.text.y = element_text(size = 14),
            plot.title = element_text(size = 18, face = "bold"),
            legend.position = "none",
            plot.background = element_blank(),
            panel.grid.minor = element_blank(),
            panel.grid.major = element_blank(),
            panel.border = element_blank(),
            axis.line = element_line(color = "black")
          ) +
          annotate("text", x = 12.5, y = 30, label = "Solid line: Typical population profile", hjust = 0, size = 5) +
          annotate("text", x = 12.5, y = 28, label = "Orange shade: 90% Prediction Interval", hjust = 0, size = 5) +
          annotate("text", x = 12.5, y = 26, label = "Purple shade: Therapeutic range", hjust = 0, size = 5)
      })
    }
  })
}

shinyApp(ui = ui, server = server)
