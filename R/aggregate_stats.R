#' RStudio Addin for Visualising Summarised Data Frame
#'
#'

aggregate_stats <- function() {

  objects_list<- ls(envir = .GlobalEnv)
  data_list<- objects_list[sapply(objects_list, function(x) "data.frame" %in% class(get(x, envir = .GlobalEnv)))]

  # Our ui will be a simple gadget page, which
  # simply displays the time in a 'UI' output.
  ui <- miniUI::miniPage(
    theme = shinythemes::shinytheme("cosmo"), title = "allo",
    miniUI::gadgetTitleBar("Exploration"),

    miniUI::miniButtonBlock(
      shiny::selectInput("list_data", label = "Input Data", choices = data_list, multiple = F),
      shiny::selectInput("target_var", label = "Target", choices = "", multiple = F),
      shiny::selectInput("expo_var", label = "Exposure", choices = "", multiple = F),
      shiny::selectInput("group_var", label = "Group", choices = "", multiple = F),
      shiny::numericInput("num_bins", label = "Bins #", value = 10, min = 1, max = 100, step = 1),
      border = "bottom"),

    shiny::fillRow(
      plotly::plotlyOutput("plot")
    )
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
      shiny::updateSelectInput(session, inputId = "expo_var", choices = num_vars)
      shiny::updateSelectInput(session, inputId = "group_var", choices = var_list)
    })

    ### Generate plot
    observe({

      #data_select<- input$list_data
      target_var<- input$target_var
      expo_var<- input$expo_var
      group_var<- input$group_var
      var_types<- data_input()$var_types
      num_vars<- names(var_types)[var_types %in% c("numeric", "integer")]
      num_bins<- input$num_bins

      data_raw<- data_input()$data

      output$plot<- plotly::renderPlotly({
        if (!is.null(group_var) & !group_var==""){

          data_raw<- data_raw %>%
            mutate_(target=formula(paste0("~",target_var)),
                    expo=formula(paste0("~",expo_var)),
                    group=formula(paste0("~",group_var)))

          # ### bin numeric variable
          if (group_var %in% num_vars & !is.na(num_bins)){
            data_raw$group<- cut(data_raw$group, breaks = sort(unique(quantile(data_raw$group, probs = 0:num_bins/num_bins))), include.lowest = T)
          }

          data_sum<- data_raw %>%
            dplyr::group_by(group) %>%
            dplyr::summarise_at(.cols = dplyr::vars(target,expo), .funs = dplyr::funs(sum(., na.rm=T))) %>%
            dplyr::mutate(ratio=target/expo)

          plotly::plot_ly(data_sum, x=~group , y=~expo, type="bar", color=I("gray"), alpha = 0.5, name=expo_var) %>%
            plotly::add_trace(data_sum, x=~group , y=~ratio, yaxis="y2", type="scatter", mode="lines+markers", color=I("navy"), name="target ratio", alpha=1) %>%
            plotly::layout(
              xaxis=list(
                title = group_var
              ),
              yaxis=list(
                title = paste0(target_var, "/", expo_var)
              ),
              yaxis2=list(
                overlaying = "y",
                side = "right",
                title = "ratio"
              ),
              margin=list(b=120)
            ) %>%
            plotly::config(collaborate = F, editable=F, showLink=F, sendData=F, displaylogo=T)


          # data_sum<- data_raw %>% dplyr::group_by_(.dots = group_var) %>%
          #   dplyr::summarise_at(.cols = c(target_var,expo_var), .funs = dplyr::funs(sum(., na.rm=T))) %>%
          #   dplyr::mutate_(ratio=paste0(target_var, "/", expo_var))
          #
          # plotly::plot_ly(data_sum, x=formula(paste0("~", group_var)) , y=formula(paste0("~", expo_var)), type="bar", color=I("gray"), alpha = 0.5, name=expo_var) %>%
          #   plotly::add_trace(data_sum, x=formula(paste0("~", group_var)) , y=~ratio, yaxis="y2", type="scatter", mode="lines+markers", color=I("navy"), name="ratio", alpha=1) %>%
          #   plotly::layout(
          #     xaxis=list(
          #       title = group_var
          #     ),
          #     yaxis=list(
          #       title = paste0(target_var, "/", expo_var)
          #     ),
          #     yaxis2=list(
          #       overlaying = "y",
          #       side = "right",
          #       title = "ratio"
          #     ),
          #     margin=list(b=120)
          #   ) %>%
          #   plotly::config(collaborate = F, editable=F, showLink=F, sendData=F, displaylogo=T)

        } else plotly::plotly_empty(type="scatter", mode="lines")
      })

    })

    # Listen for 'done' events. When we're finished, we'll
    observeEvent(input$done, {
      stopApp()
    })
  }

  viewer <- shiny::dialogViewer(dialogName = "Explorer", width=800, height=800)
  #viewer <- shiny::browserViewer()
  #viewer <- shiny::paneViewer(minHeight = 300)

  shiny::runGadget(ui, server, viewer = viewer)
}

# aggregate_stats()


# allo<- data.frame(xx=runif(10), yy=exp(runif(10)))
# bonjour<- data.frame(xx=runif(10), yy=log(runif(10)), zz=1:10)
# another<- data.frame(lettre=sample(letters[1:5], 100, replace = T),xx=runif(100), yy=exp(runif(100)))

