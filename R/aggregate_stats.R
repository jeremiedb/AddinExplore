#' RStudio Addin for Visualising Summarised Data Frame
#'
#'@import rstudioapi shiny miniUI dplyr plotly
#'
#'@export
aggregate_stats <- function() {

  objects_list <- ls(envir = .GlobalEnv)
  if (length(objects_list) == 0) {
    shiny::stopApp()
    message("no object in environment")
  }

  data_list <- objects_list[sapply(objects_list, function(x) "data.frame" %in% class(get(x, envir = .GlobalEnv)))]
  if (length(data_list) == 0) {
    shiny::stopApp()
    message("no data.frame in environment")
  }
  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar("Exploration"),

    miniUI::miniButtonBlock(
      shiny::selectInput("list_data", label = "Input Data", choices = data_list, multiple = F),
      shiny::selectInput("targets_var", label = "Targets", choices = "", selected = "", multiple = T, selectize = T),
      shiny::selectInput("weight_var", label = "Weight", choices = "", multiple = F),
      shiny::selectInput("group_var", label = "Group", choices = "", multiple = F),
      shiny::numericInput("num_bins", label = "Bins #", value = 10, min = 1, max = 256, step = 1),
      border = "bottom"),

    miniTabstripPanel(
      miniTabPanel("Graph", icon = shiny::icon("area-chart"),
                   shiny::fillRow(
                     plotly::plotlyOutput("plot", height = "100%")
                   )),
      miniTabPanel("Data", icon = shiny::icon("table"),
                   miniContentPanel(
                     DT::dataTableOutput("table", height = "100%")
                   ))
    )
  )

  server <- function(input, output, session) {

    data_input <- reactive({
      data_select <- input$list_data
      if (!data_select=="") {
        raw_data <- get(data_select, envir = .GlobalEnv)
      } else raw_data <- data.frame(xx=1:5, yy=1:5, zz=1:5)
      vars <- names(raw_data)
      var_types <- sapply(raw_data, class)
      list(data=raw_data, vars=vars, var_types=var_types)
    })

    observe({
      var_list <- data_input()$vars
      var_types <- data_input()$var_types
      num_vars <- names(var_types)[var_types %in% c("numeric", "integer")]

      shiny::updateSelectizeInput(session, inputId = "targets_var", choices = num_vars, selected = num_vars[1])
      shiny::updateSelectInput(session, inputId = "weight_var", choices = num_vars)
      shiny::updateSelectInput(session, inputId = "group_var", choices = var_list)
    })

    ### Generate plot
    observe({

      data_select <- input$list_data
      targets_var <- input$targets_var
      weight_var <- input$weight_var
      group_var <- input$group_var
      num_bins <- input$num_bins

      var_types <- data_input()$var_types
      num_vars <- names(var_types)[var_types %in% c("numeric", "integer")]

      data_raw <- data_input()$data

      ### Patch for no selected target
      if (is.null(targets_var)) {
        targets_var <- num_vars[1]
      }
      if (anyNA(targets_var) | targets_var[1]=="") {
        targets_var <- num_vars[1]
      }

      ### Patch to assume vector of 1s if no weight is specified
      if (is.na(weight_var) | weight_var==""){
        weight_var <- "AllOnes"
        data_raw$AllOnes <-1
      }

      ### Patch for no selected group
      if (is.na(group_var) | group_var==""){
        group_var <- "TOTAL"
        data_raw$TOTAL <- "TOTAL"
      }

      ### Patch to reset variables when changing dataset
      if (!all(c(targets_var, weight_var, group_var) %in% colnames(data_raw))){
        targets_var <- num_vars[1]
        weight_var <- num_vars[1]
        group_var <- num_vars[1]
      }

      data_raw[, c(".group", ".weight")] <- data_raw[, c(group_var, weight_var)]

      ### bin numeric variable
      if (group_var %in% num_vars & !is.na(num_bins) & !all(data_raw$.group[1]==data_raw$.group)){
        data_raw$.group <- cut(data_raw$.group, breaks = sort(unique(quantile(data_raw$.group, probs = 0:num_bins/num_bins))), include.lowest = T)
      }

      # Exposure approach
      data_sum <- data_raw %>%
        dplyr::group_by(.group) %>%
        dplyr::summarise_at(.vars = c(targets_var, ".weight"), .funs = dplyr::funs(sum(as.numeric(.), na.rm=T))) %>%
        dplyr::mutate_at(.vars = targets_var, .funs = funs(./.weight))

      output$table<- DT::renderDataTable({
        DT::datatable(data = data_sum, class = "display compact", rownames = F, filter = "none",
                      options = list (
                        paging = T,
                        pageLength = 10,
                        lengthMenu = c(5, 10, 25, 50, 100),
                        lengthChange = T,
                        searching = F,
                        ordering = F,
                        info = F
                      ))
      })

      output$plot<- plotly::renderPlotly({
        if (!is.null(group_var) & !group_var==""){

          p <- plotly::plot_ly(data = data_sum, x=~.group , y=~.weight, xaxis = "x", yaxis = "y", type="bar", color=I("lightblue"), alpha = 0.5, name=weight_var)

          for (traces in targets_var) {
            p <- plotly::add_trace(p = p, data = data_sum, x=~.group , y = as.formula(paste0("~", traces)), xaxis = "x", yaxis="y2", type="scatter", mode="lines+markers", name=traces, alpha=1, inherit = F)
          }
          p <- p %>% plotly::layout(
            xaxis=list(
              title = group_var
            ),
            yaxis=list(
              side = "right",
              title = "Weight"
            ),
            yaxis2=list(
              overlaying = "y",
              side = "left",
              title = "Targets"
            ),
            margin=list(b=120, l=50, r=100)
          ) %>%
            plotly::config(collaborate = F, editable=F, showLink=F, sendData=F, displaylogo=F)

        } else plotly::plotly_empty(type="scatter", mode="lines")
      })

    })

    # Listen for 'done' events. When we're finished, we'll
    observeEvent(input$done, {
      stopApp()
    })
  }

  viewer <- shiny::browserViewer()
  # viewer <- shiny::dialogViewer(dialogName = "Explorer", width=900, height=1200)
  # viewer <- shiny::paneViewer(minHeight = 300)

  shiny::runGadget(ui, server, viewer = viewer)
}

# aggregate_stats()
