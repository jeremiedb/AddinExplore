#' RStudio Addin for Visualising Summarised Data Frame
#'
#'@import rstudioapi shiny miniUI dplyr plotly
#'
#'@export
aggregate_stats <- function() {

  objects_list<- ls(envir = .GlobalEnv)
  data_list<- objects_list[sapply(objects_list, function(x) "data.frame" %in% class(get(x, envir = .GlobalEnv)))]

  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar("Exploration"),

    miniUI::miniButtonBlock(
      shiny::selectInput("list_data", label = "Input Data", choices = data_list, multiple = F),
      shiny::selectInput("target_var", label = "Target", choices = "", multiple = F, selectize = T),
      shiny::selectInput("compare_var", label = "Compare", choices = "", multiple = F, selectize = T),
      shiny::selectInput("weight_var", label = "Weight", choices = "", multiple = F),
      shiny::selectInput("group_var", label = "Group", choices = "", multiple = F),
      shiny::numericInput("num_bins", label = "Bins #", value = 10, min = 1, max = 128, step = 1),
      border = "bottom"),

    miniTabstripPanel(
      miniTabPanel("Graph", icon = icon("area-chart"),
                   miniContentPanel(
                     plotly::plotlyOutput("plot")
                   )
      ),
      miniTabPanel("Table", icon = icon("table"),
                   miniContentPanel(
                     DT::dataTableOutput("data_DT")
                   )
      )
    ),
    shiny::verbatimTextOutput("NA_warning", placeholder = T)
  )

  server <- function(input, output, session) {

    data_input<- reactive({
      data_select<- input$list_data
      if (!data_select=="") {
        raw_data<- get(data_select, envir = .GlobalEnv)
      } else raw_data<- data.frame(xx=1:5, yy=1:5, zz=1:5)
      vars<-names(raw_data)
      var_types<-sapply(raw_data, class)
      list(data=raw_data, vars=vars, var_types=var_types)
    })

    observe({
      data_select<- input$list_data
      var_list<- data_input()$vars
      var_types<- data_input()$var_types

      num_vars<- names(var_types)[var_types %in% c("numeric", "integer")]

      shiny::updateSelectizeInput(session, inputId = "target_var", choices = num_vars)
      shiny::updateSelectizeInput(session, inputId = "compare_var", choices = num_vars)
      shiny::updateSelectInput(session, inputId = "weight_var", choices = num_vars)
      shiny::updateSelectInput(session, inputId = "group_var", choices = var_list)
    })

    ### Generate plot
    observe({

      #data_select<- input$list_data
      target_var<- input$target_var
      compare_var<- input$compare_var
      weight_var<- input$weight_var
      group_var<- input$group_var
      num_bins<- input$num_bins

      var_types<- data_input()$var_types
      num_vars<- names(var_types)[var_types %in% c("numeric", "integer")]

      data_raw<- data_input()$data

      ### Patch for no selected target
      if (is.na(target_var) | target_var==""){
        target_var<- num_vars[1]
      }

      ### Patch for no selected compare
      if (is.na(compare_var) | compare_var==""){
        compare_var<- target_var
      }

      ### Patch to assume vector of 1s if no weight is specified
      if (is.na(weight_var) | weight_var==""){
        weight_var<- "AllOnes"
        data_raw$AllOnes<-1
      }

      ### Patch for no selected group
      if (is.na(group_var) | group_var==""){
        group_var<- "TOTAL"
        data_raw$TOTAL<- "TOTAL"
      }

      ### Patch to reset variables when changing dataset
      if (!all(c(target_var, compare_var, weight_var, group_var) %in% colnames(data_raw))){
        target_var<- num_vars[1]
        compare_var<- num_vars[1]
        weight_var<- num_vars[1]
        group_var<- num_vars[1]
      }

      ### Create new variables to perform calculation on
      data_raw<- data_raw %>%
        mutate_(.target=formula(paste0("~",target_var)),
                .compare=formula(paste0("~",compare_var)),
                .weight=formula(paste0("~",weight_var)),
                .group=formula(paste0("~",group_var)))

      ### Identify NA values
      target_NA_count<- sum(is.na(data_raw$.target))
      compare_NA_count<- sum(is.na(data_raw$.compare))
      weight_NA_count<- sum(is.na(data_raw$.weight))
      group_NA_count<- sum(is.na(data_raw$.group))

      row_count_ori<- nrow(data_raw)

      ### Filter out the NAs
      data_raw<- data_raw %>% filter(!is.na(.target),
                                     !is.na(.compare),
                                     !is.na(.weight),
                                     !is.na(.group))

      row_count_filter<- nrow(data_raw)

      ### Display warning message is NAs are removed
      output$NA_warning<- shiny::renderText({
        if (!row_count_ori==row_count_filter){
          paste0(row_count_ori-row_count_filter, " obs removed because of NA\n",
                 target_var, ": ", target_NA_count, " ",
                 compare_var, ": ", compare_NA_count, " ",
                 weight_var, ": ", weight_NA_count, " ",
                 group_var, ": ", group_NA_count, " ")
        } else NULL
      })


      #### bin numeric variable
      if (group_var %in% num_vars & !is.na(num_bins) & !all(data_raw$.group[1]==data_raw$.group)){
        data_raw$.group<- cut(data_raw$.group, breaks = sort(unique(quantile(data_raw$.group, probs = 0:num_bins/num_bins))), include.lowest = T)
      }

      if (length(unique(data_raw$.group))>250){
        data_raw<- data_raw %>% mutate(.group=paste0(group_var, " has too many levels: ", length(unique(data_raw$.group))))
      }

      data_sum<- data_raw %>%
        dplyr::group_by_(.dots = ".group") %>%
        dplyr::summarise_at(.cols = c(".target", ".compare", ".weight"), .funs = dplyr::funs(sum(as.numeric(.), na.rm=T))) %>%
        dplyr::mutate(ratio_target=.target/.weight,
                      ratio_compare=.compare/.weight)

      output$data_DT<- DT::renderDataTable({
        DT::datatable(data_sum, class = "compact", filter = "none", style = "bootstrap")
      })

      output$plot<- plotly::renderPlotly({
        if (!is.null(group_var) & !group_var==""){


          plotly::plot_ly(data_sum, x=~.group , y=~.weight, type="bar", color=I("darkgray"), alpha = 0.5, name="Weight") %>%
            plotly::add_trace(data_sum, x=~.group , y=~ratio_compare, yaxis="y2", type="scatter", mode="lines+markers", color=I("red"), name="Compare", alpha=1) %>%
            plotly::add_trace(data_sum, x=~.group , y=~ratio_target, yaxis="y2", type="scatter", mode="lines+markers", color=I("navy"), name="Target", alpha=1) %>%
            plotly::layout(
              xaxis=list(
                title = group_var
              ),
              yaxis=list(
                title = weight_var
              ),
              yaxis2=list(
                overlaying = "y",
                side = "right",
                title = paste0(target_var)
              ),
              margin=list(b=120, l=50, r=100)
            ) %>%
            plotly::config(collaborate = F, editable=F, showLink=F, sendData=F, displaylogo=T)


        } else plotly::plotly_empty(type="scatter", mode="lines")
      })

    })

    # Listen for 'done' events. When we're finished, we'll
    observeEvent(input$done, {
      stopApp()
    })
  }

  viewer <- shiny::dialogViewer(dialogName = "Explorer", width=1200, height=1200)
  #viewer <- shiny::browserViewer()
  #viewer <- shiny::paneViewer(minHeight = 300)

  shiny::runGadget(ui, server, viewer = viewer)
}

# require(miniUI)
# require(plotly)
# test<- datasets::iris
# test[5,1]<- NA
# aggregate_stats()
