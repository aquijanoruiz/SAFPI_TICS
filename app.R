library(shiny)
library(dplyr)
library(ggplot2)
library(scales)

submission_timing_counts <- readRDS("data/submission_timing_counts.rds")

status_levels <- c(
  "on time",
  "1 week late",
  "> 1 week late",
  "not submitted"
)

status_labels <- c(
  "on time" = "On time",
  "1 week late" = "One week late",
  "> 1 week late" = "More than one week late",
  "not submitted" = "Not submitted"
)

status_colors <- c(
  "on time" = "#1F78B4",
  "1 week late" = "#6BAED6",
  "> 1 week late" = "#BDD7E7",
  "not submitted" = "#B85C5C"
)

week_date_labels <- submission_timing_counts %>%
  distinct(dashboard_week, week_end_date) %>%
  arrange(dashboard_week)

date_axis_breaks <- week_date_labels$dashboard_week[
  seq(1, nrow(week_date_labels), by = 4)
]

date_axis_labels <- format(
  week_date_labels$week_end_date[
    match(date_axis_breaks, week_date_labels$dashboard_week)
  ],
  "%m/%d"
)

first_weeks_activated <- as.Date("2025-09-23")

first_weeks_activated_week <- week_date_labels %>%
  filter(first_weeks_activated <= week_end_date) %>%
  slice(1) %>%
  pull(dashboard_week)

government_outage_week <- week_date_labels %>%
  slice(n() - 2) %>%
  pull(dashboard_week)

new_year_holiday_week <- 18

ui <- fluidPage(
  titlePanel("SAFPI Teacher Submission Timing"),
  fluidRow(
    column(
      width = 3,
      selectInput(
        inputId = "form_number",
        label = "Form",
        choices = c("Form 2" = "2", "Form 3" = "3", "Form 4" = "4"),
        selected = "3"
      )
    ),
    column(
      width = 4,
      selectInput(
        inputId = "scope",
        label = "Teachers",
        choices = c("General: all teachers pooled", "Zona", "Distrito"),
        selected = "General: all teachers pooled"
      )
    ),
    column(
      width = 4,
      uiOutput("scope_value_ui")
    )
  ),
  fluidRow(
    column(
      width = 12,
      plotOutput("submission_plot", height = "650px")
    )
  ),
  fluidRow(
    column(
      width = 12,
      tags$hr(),
      h4("How to read this dashboard"),
      fluidRow(
        column(
          width = 6,
          h5("Forms"),
          tags$ul(
            tags$li(tags$b("Form 2:"), " student attendance and visit records."),
            tags$li(tags$b("Form 3:"), " pedagogical planning, including domains, activities, materials, skills, achievements, and related content."),
            tags$li(tags$b("Form 4:"), " weekly report on whether the planned content was covered.")
          )
        ),
        column(
          width = 6,
          h5("Submission timing"),
          tags$ul(
            tags$li(tags$b("On time:"), " submitted on or before the end date of the week."),
            tags$li(tags$b("One week late:"), " submitted after the week end date, but within seven days."),
            tags$li(tags$b("More than one week late:"), " submitted more than seven days after the week end date."),
            tags$li(tags$b("Not submitted:"), " no submission date is registered for that teacher-week.")
          )
        )
      ),
      p("Each bar represents 100% of teachers in the selected group for that dashboard week.")
    )
  )
)

server <- function(input, output, session) {
  output$scope_value_ui <- renderUI({
    if (input$scope == "Zona") {
      zona_choices <- submission_timing_counts %>%
        filter(scope == "Zona") %>%
        distinct(scope_value) %>%
        arrange(scope_value) %>%
        pull(scope_value)

      selectInput(
        inputId = "scope_value",
        label = "Zona",
        choices = zona_choices
      )
    } else if (input$scope == "Distrito") {
      distrito_choices <- submission_timing_counts %>%
        filter(scope == "Distrito") %>%
        distinct(scope_value) %>%
        arrange(scope_value) %>%
        pull(scope_value)

      selectInput(
        inputId = "scope_value",
        label = "Distrito",
        choices = distrito_choices
      )
    }
  })

  filtered_counts <- reactive({
    if (input$scope == "Zona" || input$scope == "Distrito") {
      req(input$scope_value)

      submission_timing_counts %>%
        filter(
          scope == input$scope,
          scope_value == input$scope_value,
          form_number == as.integer(input$form_number)
        )
    } else {
      submission_timing_counts %>%
        filter(
          scope == "General: all teachers pooled",
          form_number == as.integer(input$form_number)
        )
    }
  })

  output$submission_plot <- renderPlot({
    plot_data <- filtered_counts() %>%
      mutate(
        submission_timing = factor(
          submission_timing,
          levels = status_levels
        )
      )

    validate(
      need(nrow(plot_data) > 0, "No data available for this selection.")
    )

    ggplot(
      plot_data,
      aes(
        x = dashboard_week,
        y = n,
        fill = submission_timing
      )
    ) +
      geom_col(position = "fill") +
      geom_vline(
        xintercept = first_weeks_activated_week,
        linetype = "dashed",
        color = "gray35"
      ) +
      annotate(
        "text",
        x = first_weeks_activated_week + 0.4,
        y = 1.03,
        label = "First weeks activated",
        hjust = 0,
        vjust = 0,
        size = 3.2,
        color = "gray25"
      ) +
      geom_vline(
        xintercept = government_outage_week,
        linetype = "dashed",
        color = "gray35"
      ) +
      annotate(
        "text",
        x = new_year_holiday_week,
        y = 0.5,
        label = "New Year holiday",
        angle = 90,
        hjust = 0.5,
        vjust = 0.5,
        size = 3.2,
        color = "gray25"
      ) +
      annotate(
        "text",
        x = government_outage_week - 0.4,
        y = 1.03,
        label = "Government website outage",
        hjust = 1,
        vjust = 0,
        size = 3.2,
        color = "gray25"
      ) +
      scale_x_continuous(
        breaks = sort(unique(c(plot_data$dashboard_week, new_year_holiday_week))),
        sec.axis = dup_axis(
          breaks = date_axis_breaks,
          labels = date_axis_labels,
          name = "Week end date"
        )
      ) +
      scale_y_continuous(
        labels = percent_format()
      ) +
      scale_fill_manual(
        values = status_colors,
        labels = status_labels,
        drop = FALSE
      ) +
      labs(
        title = paste0("Weekly Form ", input$form_number, " submissions"),
        subtitle = "Share of teachers by submission timing",
        x = "Dashboard week",
        y = "",
        fill = "Submission timing:"
      ) +
      coord_cartesian(ylim = c(0, 1), clip = "off") +
      theme_minimal() +
      theme(
        legend.position = "bottom",
        plot.margin = margin(t = 20, r = 10, b = 10, l = 10)
      )
  })
}

shinyApp(ui = ui, server = server)
