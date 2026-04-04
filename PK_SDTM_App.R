# ============================================================
#  PK Graph — Shiny App per dati in formato SDTM (PC domain)
# ============================================================

library(shiny)
library(dplyr)
library(ggplot2)
library(scales)
library(DT)

# ============================================================
#  UI
# ============================================================
ui <- fluidPage(
  title = "PK Concentrations from SDTM",
  
  tags$head(tags$style(HTML("
    @import url('https://fonts.googleapis.com/css2?family=IBM+Plex+Sans:wght@300;400;600&family=IBM+Plex+Mono:wght@500&display=swap');

    * { box-sizing: border-box; }
    body {
      font-family: 'IBM Plex Sans', sans-serif;
      background: #0f1117;
      color: #e2e8f0;
      margin: 0;
    }

    /* Header */
    .app-header {
      background: #141821;
      border-bottom: 1px solid #2d3748;
      padding: 15px 28px;
      display: flex;
      align-items: baseline;
      gap: 12px;
    }
    .app-title {
      font-family: 'IBM Plex Mono', monospace;
      font-size: 1.15rem;
      font-weight: 500;
      color: #6DE9F7;
      margin: 0;
    }
    .app-subtitle { font-size: 1.15rem; color: #6DE9F7; margin: 0; }

    /* Layout */
    .main-layout { display: flex; min-height: calc(100vh - 52px); }

    /* Sidebar */
    .sidebar-panel {
      width: 280px;
      min-width: 280px;
      background: #141821;
      border-right: 1px solid #2d3748;
      padding: 20px 16px;
      display: flex;
      flex-direction: column;
      gap: 5px;
      overflow-y: auto;
    }
    .section-label {
      font-size: 0.90rem;
      font-weight: 600;
      letter-spacing: 0.1em;
      text-transform: uppercase;
      color: #FFFFFF;
      margin-bottom: 4px;
    }
    label { font-size: 0.82rem; color: #a0aec0; }

    /* File input */
    .btn-file {
      background: #2d3748 !important;
      color: #e2e8f0 !important;
      border: none !important;
      border-radius: 6px !important;
      font-size: 0.82rem !important;
      font-family: 'IBM Plex Sans', sans-serif !important;
    }
    .btn-file:hover { background: #3d4a5c !important; }
    input[type='text'].form-control {
      background: #1a1f2e !important;
      border: 1px solid #2d3748 !important;
      color: #e2e8f0 !important;
      border-radius: 6px !important;
      font-size: 0.82rem !important;
    }

    /* Selectize */
    .selectize-input {
      background: #1a1f2e !important;
      border: 1px solid #2d3748 !important;
      border-radius: 6px !important;
      color: #e2e8f0 !important;
      font-size: 0.82rem !important;
      box-shadow: none !important;
    }
    .selectize-input.focus { border-color: #4299e1 !important; }
    .selectize-dropdown {
      background: #1a1f2e !important;
      border: 1px solid #2d3748 !important;
      border-radius: 6px !important;
      color: #e2e8f0 !important;
      font-size: 0.82rem !important;
    }
    .selectize-dropdown-content .option { color: #e2e8f0 !important; padding: 7px 10px !important; }
    .selectize-dropdown-content .option.active,
    .selectize-dropdown-content .option:hover { background: #2d3748 !important; }
    .selectize-input .item {
      background: #2b4c7e !important;
      color: #6DE9F7 !important;
      border-radius: 4px !important;
      font-size: 0.76rem !important;
    }

    /* Radio / checkbox */
    .radio label, .checkbox label { font-size: 0.82rem; color: #a0aec0; }
    input[type='radio'], input[type='checkbox'] { accent-color: #4299e1; }

    /* Disabled select (before upload) */
    .selectize-control.disabled .selectize-input {
      opacity: 0.4 !important;
      cursor: not-allowed !important;
    }

    hr { border: none; border-top: 1px solid #2d3748; margin: 0; }

    /* Placeholder messaggio */
    .upload-msg {
      display: flex;
      flex-direction: column;
      align-items: center;
      justify-content: center;
      height: 100%;
      gap: 10px;
      color: #4a5568;
      font-size: 0.85rem;
      text-align: center;
    }
    .upload-msg svg { opacity: 0.35; }

    /* Content */
    .content-panel { flex: 1; padding: 24px 28px; }
    .plot-card {
      background: #141821;
      border: 1px solid #2d3748;
      border-radius: 10px;
      padding: 20px;
      height: calc(100vh - 110px);
    }
    .plot-card-title {
      font-size: 0.68rem;
      font-weight: 600;
      text-transform: uppercase;
      letter-spacing: 0.09em;
      color: #4a5568;
      margin-bottom: 14px;
    }

    /* Tabs */
    .nav-tabs {
      border-bottom: 1px solid #2d3748 !important;
      margin-bottom: 16px;
    }
    .nav-tabs > li > a {
      font-family: 'IBM Plex Sans', sans-serif;
      font-size: 0.78rem;
      font-weight: 600;
      letter-spacing: 0.06em;
      text-transform: uppercase;
      color: #4a5568 !important;
      background: transparent !important;
      border: none !important;
      border-bottom: 2px solid transparent !important;
      border-radius: 0 !important;
      padding: 8px 16px !important;
    }
    .nav-tabs > li.active > a,
    .nav-tabs > li > a:hover {
      color: #6DE9F7 !important;
      border-bottom: 2px solid #4299e1 !important;
      background: transparent !important;
    }
    .tab-content { background: transparent; }

    /* DT table dark theme */
    .dataTables_wrapper { color: #a0aec0; font-size: 0.82rem; }
    table.dataTable thead th {
      background: #1a1f2e !important;
      color: #4a5568 !important;
      font-size: 0.66rem !important;
      font-weight: 600 !important;
      letter-spacing: 0.08em !important;
      text-transform: uppercase !important;
      border-bottom: 1px solid #2d3748 !important;
    }
    table.dataTable tbody tr { background: #141821 !important; }
    table.dataTable tbody tr:hover td { background: #1a1f2e !important; }
    table.dataTable tbody td {
      color: #e2e8f0 !important;
      border-top: 1px solid #1e2433 !important;
      font-family: 'IBM Plex Mono', monospace;
      font-size: 0.8rem !important;
    }
    table.dataTable tbody tr.odd  td { background: #141821 !important; }
    table.dataTable tbody tr.even td { background: #111520 !important; }
    .dataTables_info, .dataTables_length label,
    .dataTables_filter label { color: #4a5568 !important; font-size: 0.76rem !important; }
    .dataTables_filter input {
      background: #1a1f2e !important;
      border: 1px solid #2d3748 !important;
      color: #e2e8f0 !important;
      border-radius: 5px !important;
      font-size: 0.78rem !important;
    }
    .dataTables_length select {
      background: #1a1f2e !important;
      border: 1px solid #2d3748 !important;
      color: #e2e8f0 !important;
      border-radius: 5px !important;
    }
    .paginate_button {
      color: #4a5568 !important;
      background: transparent !important;
      border: none !important;
      border-radius: 5px !important;
    }
    .paginate_button.current, .paginate_button:hover {
      color: #6DE9F7 !important;
      background: #2d3748 !important;
      border: none !important;
    }
    .dt-download-btn {
      background: #2d3748;
      color: #6DE9F7;
      border: 1px solid #4299e1;
      border-radius: 6px;
      padding: 5px 14px;
      font-size: 0.76rem;
      font-family: 'IBM Plex Sans', sans-serif;
      font-weight: 600;
      letter-spacing: 0.05em;
      cursor: pointer;
      margin-bottom: 12px;
    }
    .dt-download-btn:hover { background: #3d4a5c; }
    .step-badge {
      display: inline-block;
      background: #2b4c7e;
      color: #6DE9F7;
      border-radius: 50%;
      width: 17px;
      height: 17px;
      font-size: 0.62rem;
      font-weight: 600;
      text-align: center;
      line-height: 17px;
      margin-right: 5px;
      vertical-align: middle;
    }
  "))),
  
  div(class = "app-header",
      h1("PK Graph", class = "app-title"),
      p("Mean concentrations at nominal timepoints — SDTM PC domain", class = "app-subtitle")
  ),
  
  div(class = "main-layout",
      
      # === SIDEBAR ===
      div(class = "sidebar-panel",
          
          # 0 – Upload
          div(
            div("Dataset", class = "section-label"),
            fileInput("file_input", label = NULL,
                      accept = c(".csv", ".rds", ".txt", ".sas7bdat"),
                      buttonLabel = "Upload file",
                      placeholder = "No file")
          ),
          
          hr(),
          
          # 1-4 – Filters
          div(
            div(
              HTML('<span class="step-badge">1</span>'),
              span("PCTEST — Analyte", style = "font-size:0.66rem; font-weight:600; letter-spacing:0.1em; text-transform:uppercase; color:#4a5568;")
            ),
            br(),
            selectizeInput("sel_pctest", label = NULL,
                           choices = NULL,
                           options = list(placeholder = "Upload a file",
                                          maxItems = 1))
          ),
          
          div(
            div(
              HTML('<span class="step-badge">2</span>'),
              span("PCSPEC — matrix", style = "font-size:0.66rem; font-weight:600; letter-spacing:0.1em; text-transform:uppercase; color:#4a5568;")
            ),
            br(),
            selectizeInput("sel_pcspec", label = NULL,
                           choices = NULL,
                           options = list(placeholder = "— select PCTEST —",
                                          maxItems = 1))
          ),
          
          div(
            div(
              HTML('<span class="step-badge">3</span>'),
              span("VISITNUM — Visita", style = "font-size:0.66rem; font-weight:600; letter-spacing:0.1em; text-transform:uppercase; color:#4a5568;")
            ),
            br(),
            selectizeInput("sel_visitnum", label = NULL,
                           choices = NULL, multiple = TRUE,
                           options = list(placeholder = "— select PCSPEC —"))
          ),
          
          div(
            div(
              HTML('<span class="step-badge">4</span>'),
              span("PCTPT — Timepoint", style = "font-size:0.66rem; font-weight:600; letter-spacing:0.1em; text-transform:uppercase; color:#4a5568;")
            ),
            br(),
            selectizeInput("sel_pctpt", label = NULL,
                           choices = NULL, multiple = TRUE,
                           options = list(placeholder = "— select VISITNUM —"))
          ),
          
          hr(),
          
          # Braccio
          div(
            div("Treatment Arm (ACTARMCD)", class = "section-label"),
            selectizeInput("sel_arm", label = NULL,
                           choices = NULL, multiple = TRUE,
                           options = list(placeholder = "Upload a file"))
          ),
          
          hr(),
          
          # Opzioni Graph
          div(
            div("Graph options", class = "section-label"),
            checkboxInput("log_y",   "Log scale (Y)",          value = TRUE),
            checkboxInput("show_sd", "Add standard deviation (±SD)", value = TRUE)
          )
      ),
      
      # === CONTENUTO A SCHEDE ===
      div(class = "content-panel",
          div(class = "plot-card",
              tabsetPanel(id = "main_tabs", type = "tabs",
                          
                          tabPanel("\U0001f4c8  Graph",
                                   br(),
                                   uiOutput("plot_or_msg")
                          ),
                          
                          tabPanel("\U0001f4cb  Table",
                                   br(),
                                   uiOutput("table_or_msg")
                          )
              )
          )
      )
  )
)

# ============================================================
#  SERVER
# ============================================================
server <- function(input, output, session) {
  
  # ----------------------------------------------------------
  # 1. Caricamento dati
  # ----------------------------------------------------------
  raw_data <- reactive({
    req(input$file_input)
    ext <- tolower(tools::file_ext(input$file_input$name))
    switch(ext,
           rds        = readRDS(input$file_input$datapath),
           sas7bdat   = {
             if (!requireNamespace("haven", quietly = TRUE)) stop("Install 'haven' to read SAS file.")
             haven::read_sas(input$file_input$datapath)
           },
           read.csv(input$file_input$datapath, stringsAsFactors = FALSE)
    )
  })
  
  # Dati PC: normalizza colonne chiave
  pc_data <- reactive({
    req(raw_data())
    df <- raw_data()
    # Forza nomi maiuscoli per robustezza
    names(df) <- toupper(names(df))
    
    # Colonne obbligatorie minime
    required <- c("PCTEST", "PCSTRESN", "PCTPTNUM")
    missing  <- setdiff(required, names(df))
    validate(need(length(missing) == 0,
                  paste("Missing columns:", paste(missing, collapse = ", "))))
    
    df <- df |>
      mutate(
        PCSTRESN = as.numeric(PCSTRESN),
        PCTPTNUM = as.numeric(PCTPTNUM),
        VISITNUM = if ("VISITNUM" %in% names(df)) as.numeric(VISITNUM) else NA_real_,
        PCSPEC   = if ("PCSPEC"   %in% names(df)) as.character(PCSPEC)   else "N/A",
        ACTARMCD = if ("ACTARMCD" %in% names(df)) as.character(ACTARMCD) else "N/A",
        PCTPT    = if ("PCTPT"    %in% names(df)) as.character(PCTPT)    else as.character(PCTPTNUM),
        PCTEST   = as.character(PCTEST)
      )
    df
  })
  
  # ----------------------------------------------------------
  # 2. Cascata Step 1 — PCTEST
  # ----------------------------------------------------------
  observeEvent(pc_data(), {
    tests <- sort(unique(pc_data()$PCTEST))
    updateSelectizeInput(session, "sel_pctest",
                         choices  = tests,
                         selected = tests[1],
                         options  = list(placeholder = "select analyte", maxItems = 1))
    # Popola anche ACTARMCD
    arms <- sort(unique(pc_data()$ACTARMCD))
    updateSelectizeInput(session, "sel_arm",
                         choices  = arms,
                         selected = arms,
                         options  = list(placeholder = "All arms"))
  })
  
  # ----------------------------------------------------------
  # 3. Cascata Step 2 — PCSPEC (filtrato per PCTEST)
  # ----------------------------------------------------------
  observeEvent(input$sel_pctest, {
    req(pc_data(), input$sel_pctest)
    specs <- pc_data() |>
      filter(PCTEST == input$sel_pctest) |>
      pull(PCSPEC) |> unique() |> sort()
    updateSelectizeInput(session, "sel_pcspec",
                         choices  = specs,
                         selected = specs[1],
                         options  = list(placeholder = "select matrix", maxItems = 1))
  })
  
  # ----------------------------------------------------------
  # 4. Cascata Step 3 — VISITNUM (filtrato per PCTEST + PCSPEC)
  # ----------------------------------------------------------
  observeEvent(c(input$sel_pctest, input$sel_pcspec), {
    req(pc_data(), input$sel_pctest, input$sel_pcspec)
    visits <- pc_data() |>
      filter(PCTEST == input$sel_pctest,
             PCSPEC == input$sel_pcspec) |>
      pull(VISITNUM) |> unique() |> sort()
    visits_chr <- as.character(visits)
    updateSelectizeInput(session, "sel_visitnum",
                         choices  = visits_chr,
                         selected = visits_chr,
                         options  = list(placeholder = "All visits"))
  })
  
  # ----------------------------------------------------------
  # 5. Cascata Step 4 — PCTPT (filtrato per tutto sopra)
  # ----------------------------------------------------------
  observeEvent(c(input$sel_pctest, input$sel_pcspec, input$sel_visitnum), {
    req(pc_data(), input$sel_pctest, input$sel_pcspec, length(input$sel_visitnum) > 0)
    tpts <- pc_data() |>
      filter(PCTEST   == input$sel_pctest,
             PCSPEC   == input$sel_pcspec,
             VISITNUM %in% as.numeric(input$sel_visitnum)) |>
      distinct(PCTPT, PCTPTNUM) |>
      arrange(PCTPTNUM) |>
      pull(PCTPT)
    updateSelectizeInput(session, "sel_pctpt",
                         choices  = tpts,
                         selected = tpts,
                         options  = list(placeholder = "All timepoints"))
  })
  
  # ----------------------------------------------------------
  # 6. Dati filtrati definitivi
  # ----------------------------------------------------------
  filtered_data <- reactive({
    req(pc_data(),
        input$sel_pctest, input$sel_pcspec,
        length(input$sel_visitnum) > 0,
        length(input$sel_pctpt)   > 0,
        length(input$sel_arm)     > 0)
    
    pc_data() |>
      filter(
        PCTEST   == input$sel_pctest,
        PCSPEC   == input$sel_pcspec,
        VISITNUM %in% as.numeric(input$sel_visitnum),
        PCTPT    %in% input$sel_pctpt,
        ACTARMCD %in% input$sel_arm,
        !is.na(PCSTRESN),
        PCSTRESN > 0
      )
  })
  
  # ----------------------------------------------------------
  # 7. Statistiche riassuntive per timepoint × braccio
  # ----------------------------------------------------------
  summary_tbl <- reactive({
    req(filtered_data())
    filtered_data() |>
      group_by(ACTARMCD, PCTPTNUM, PCTPT) |>
      summarise(
        N    = n(),
        Mean = mean(PCSTRESN, na.rm = TRUE),
        SD   = sd(PCSTRESN,   na.rm = TRUE),
        .groups = "drop"
      ) |>
      mutate(
        SD   = ifelse(is.na(SD), 0, SD),
        ymin = pmax(Mean - SD, 1e-6),
        ymax = Mean + SD
      ) |>
      arrange(ACTARMCD, PCTPTNUM)
  })
  
  # ----------------------------------------------------------
  # 8. Placeholder vs plot
  # ----------------------------------------------------------
  output$plot_or_msg <- renderUI({
    if (is.null(input$file_input)) {
      div(class = "upload-msg",
          tags$svg(xmlns = "http://www.w3.org/2000/svg", width = "48", height = "48",
                   viewBox = "0 0 24 24", fill = "none", stroke = "#718096",
                   `stroke-width` = "1.5", `stroke-linecap` = "round", `stroke-linejoin` = "round",
                   tags$path(d = "M21 15v4a2 2 0 0 1-2 2H5a2 2 0 0 1-2-2v-4"),
                   tags$polyline(points = "17 8 12 3 7 8"),
                   tags$line(x1 = "12", y1 = "3", x2 = "12", y2 = "15")
          ),
          p("Carica un file CSV o RDS (SDTM PC)", style = "margin:0; color:#718096;"),
          p("per visualizzare il Graph", style = "margin:0; color:#4a5568; font-size:0.78rem;")
      )
    } else {
      plotOutput("pk_plot", height = "calc(100vh - 175px)")
    }
  })
  
  # ----------------------------------------------------------
  # 9. Palette colori
  # ----------------------------------------------------------
  palette_arms <- c("#4299e1", "#f6ad55", "#68d391", "#fc8181", "#b794f4",
                    "#76e4f7", "#f687b3", "#9ae6b4", "#fbd38d", "#a3bffa")
  
  # ----------------------------------------------------------
  # 10. Graph
  # ----------------------------------------------------------
  output$pk_plot <- renderPlot({
    tbl <- summary_tbl()
    validate(need(nrow(tbl) > 0, "No data available with selected filters."))
    
    p <- ggplot(tbl, aes(x = PCTPTNUM, y = Mean,
                         color = ACTARMCD, group = ACTARMCD))
    
    if (isTRUE(input$show_sd))
      p <- p + geom_errorbar(aes(ymin = ymin, ymax = ymax),
                             width = 0.6, alpha = 0.45, linewidth = 0.7)
    
    p <- p +
      geom_line(linewidth = 1) +
      geom_point(size = 3.5, shape = 21, fill = "#141821", stroke = 2) +
      scale_color_manual(values = palette_arms) +
      labs(
        x     = "Nominal timepoint (PCTPTNUM)",
        y     = if (input$log_y) "Mean PCSTRESN (log scale)" else "Mean PCSTRESN",
        color = "Treatment arm\n(ACTARMCD)",
        caption = paste0(
          "PCTEST: ", input$sel_pctest,
          "  |  PCSPEC: ", input$sel_pcspec,
          "  |  VISITNUM: ", paste(input$sel_visitnum, collapse = ", ")
        )
      ) +
      theme_minimal(base_family = "sans") +
      theme(
        plot.background   = element_rect(fill = "#141821", color = NA),
        panel.background  = element_rect(fill = "#141821", color = NA),
        panel.grid.major  = element_line(color = "#1e2433", linewidth = 0.5),
        panel.grid.minor  = element_blank(),
        axis.text         = element_text(size = 14, color = "#718096"),
        axis.title        = element_text(size = 16, color = "#a0aec0"),
        legend.background = element_rect(fill = "#141821", color = NA),
        legend.text       = element_text(size = 14, color = "#a0aec0"),
        legend.title      = element_text(size = 12, color = "#4a5568"),
        plot.caption      = element_text(size = 10, color = "#4a5568", hjust = 0)
      )
    
    if (input$log_y)
      p <- p + scale_y_log10(labels = label_number(accuracy = 0.01))
    else
      p <- p + scale_y_continuous(labels = label_number(accuracy = 0.01))
    
    p
  }, bg = "#141821")
  # ----------------------------------------------------------
  # 11. Table riassuntiva
  # ----------------------------------------------------------
  output$table_or_msg <- renderUI({
    if (is.null(input$file_input)) {
      div(class = "upload-msg",
          p("Upload a file to visualize the table",
            style = "margin:0; color:#718096;")
      )
    } else {
      tagList(
        downloadButton("dl_table", "Download CSV",
                       class = "dt-download-btn"),
        DTOutput("pk_table")
      )
    }
  })
  
  output$pk_table <- renderDT({
    tbl <- summary_tbl()
    validate(need(nrow(tbl) > 0, "No data available with selected filters."))
    
    tbl_display <- tbl |>
      select(
        `Treatment arm` = ACTARMCD,
        `PCTPTNUM`      = PCTPTNUM,
        `Timepoint`     = PCTPT,
        `N`             = N,
        `Mean`          = Mean,
        `SD`            = SD
      ) |>
      mutate(
        Mean = round(Mean, 4),
        SD   = round(SD,   4)
      )
    
    datatable(
      tbl_display,
      rownames  = FALSE,
      selection = "none",
      options   = list(
        pageLength  = 20,
        dom         = "ftip",
        ordering    = TRUE,
        scrollX     = FALSE,
        language    = list(
          search      = "Search:",
          info        = "Righe _START_–_END_ di _TOTAL_",
          infoEmpty   = "Nessuna riga",
          paginate    = list(previous = "‹", `next` = "›")
        )
      ),
      class = "cell-border"
    ) |>
      formatStyle(
        columns    = "Mean",
        fontWeight = "600",
        color      = "#6DE9F7"
      ) |>
      formatStyle(
        columns         = "Treatment arm",
        backgroundColor = styleEqual(
          levels = sort(unique(tbl_display$`Treatment arm`)),
          values = rep("#1a1f2e", length(unique(tbl_display$`Treatment arm`)))
        )
      )
  })
  
  output$dl_table <- downloadHandler(
    filename = function() {
      paste0("PK_summary_", input$sel_pctest, "_", input$sel_pcspec, "_",
             format(Sys.Date(), "%Y%m%d"), ".csv")
    },
    content = function(file) {
      tbl <- summary_tbl() |>
        select(
          ACTARMCD, PCTPTNUM, PCTPT, N, Mean, SD
        ) |>
        mutate(Mean = round(Mean, 4), SD = round(SD, 4))
      write.csv(tbl, file, row.names = FALSE)
    }
  )
}

shinyApp(ui, server)