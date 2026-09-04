# ── Clinical Trial Enrollment Dashboard ──────────────────────────────────
# A demo Shiny app using 100% synthetic data.
# Safe to publish on Posit Connect Cloud — no real patient or study data.
#
# Packages: shiny, bslib, ggplot2  (all on CRAN)
# ─────────────────────────────────────────────────────────────────────────

library(shiny)
library(bslib)
library(ggplot2)

# ── Synthetic data ───────────────────────────────────────────────────────
set.seed(42)

studies <- c("STUDY-101", "STUDY-204", "STUDY-317", "STUDY-450")

make_enrollment <- function(study) {
  n_sites  <- sample(8:20, 1)
  n_months <- sample(12:24, 1)
  dates    <- seq(as.Date("2024-01-15"), by = "month", length.out = n_months)

  # cumulative enrollment curve (logistic-ish growth)
  target   <- sample(150:400, 1)
  mid      <- n_months / 2
  enrolled <- round(target / (1 + exp(-0.4 * (seq_len(n_months) - mid))))

  data.frame(
    study    = study,
    date     = dates,
    enrolled = enrolled,
    sites    = pmin(n_sites, cumsum(sample(0:2, n_months, replace = TRUE,
                                           prob = c(0.3, 0.5, 0.2)))),
    target   = target,
    stringsAsFactors = FALSE
  )
}

all_data <- do.call(rbind, lapply(studies, make_enrollment))

# ── UI ───────────────────────────────────────────────────────────────────
ui <- page_sidebar(
  title = "Enrollment Dashboard",
  theme = bs_theme(
    preset  = "shiny",
    bg      = "#0D1826",
    fg      = "#DDE6F0",
    primary = "#57B393",
    "navbar-bg" = "#16243A"
  ),

  sidebar = sidebar(
    title = "Filters",
    bg    = "#16243A",

    selectInput("study", "Study", choices = studies, selected = studies[1]),

    dateRangeInput("dates", "Enrollment window",
                   start = min(all_data$date),
                   end   = max(all_data$date)),

    radioButtons("show_target", "Show target line?",
                 choices = c("Yes", "No"), selected = "Yes", inline = TRUE)
  ),

  # ── Value boxes row ──────────────────────────────────────────────────
  layout_columns(
    col_widths = c(4, 4, 4),
    value_box("Enrolled",     textOutput("n_enrolled"),
              showcase = icon("user-plus"),  theme = "primary"),
    value_box("Active Sites", textOutput("n_sites"),
              showcase = icon("hospital"),   theme = "info"),
    value_box("% of Target",  textOutput("pct_target"),
              showcase = icon("bullseye"),   theme = "warning")
  ),

  # ── Chart + table ────────────────────────────────────────────────────
  layout_columns(
    col_widths = c(8, 4),
    card(
      card_header("Cumulative Enrollment"),
      plotOutput("enrollment_plot", height = "340px")
    ),
    card(
      card_header("Monthly Snapshot"),
      tableOutput("summary_table")
    )
  )
)

# ── Server ───────────────────────────────────────────────────────────────
server <- function(input, output, session) {

  # Reactive filtered data
  df <- reactive({
    d <- all_data[all_data$study == input$study, ]
    d[d$date >= input$dates[1] & d$date <= input$dates[2], ]
  })

  # Value box outputs
  output$n_enrolled <- renderText({
    d <- df()
    if (nrow(d) == 0) return("—")
    max(d$enrolled)
  })

  output$n_sites <- renderText({
    d <- df()
    if (nrow(d) == 0) return("—")
    max(d$sites)
  })

  output$pct_target <- renderText({
    d <- df()
    if (nrow(d) == 0) return("—")
    paste0(round(max(d$enrolled) / d$target[1] * 100), "%")
  })

  # Enrollment chart
  output$enrollment_plot <- renderPlot({
    d <- df()
    if (nrow(d) == 0) return(NULL)

    p <- ggplot(d, aes(date, enrolled)) +
      geom_line(colour = "#57B393", linewidth = 1.2) +
      geom_point(colour = "#57B393", size = 2.5) +
      labs(x = NULL, y = "Subjects enrolled") +
      theme_minimal(base_size = 14) +
      theme(
        plot.background  = element_rect(fill = "#0D1826", colour = NA),
        panel.background = element_rect(fill = "#0D1826", colour = NA),
        panel.grid.major = element_line(colour = "#2A3A55"),
        panel.grid.minor = element_blank(),
        text  = element_text(colour = "#DDE6F0"),
        axis.text = element_text(colour = "#8A9BB2")
      )

    if (input$show_target == "Yes") {
      p <- p + geom_hline(yintercept = d$target[1],
                          linetype = "dashed", colour = "#E9A83C") +
        annotate("text", x = min(d$date), y = d$target[1] + 12,
                 label = paste("Target:", d$target[1]),
                 colour = "#E9A83C", hjust = 0, size = 4.5)
    }
    p
  }, bg = "#0D1826")

  # Summary table
  output$summary_table <- renderTable({
    d <- df()
    if (nrow(d) == 0) return(NULL)
    tail(data.frame(
      Month    = format(d$date, "%b %Y"),
      Enrolled = d$enrolled,
      Sites    = d$sites
    ), 8)
  }, striped = TRUE, hover = TRUE, width = "100%")
}

# ── Run ──────────────────────────────────────────────────────────────────
shinyApp(ui, server)
