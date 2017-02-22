one_way_plot<- function(input_data, group, target, compare=NULL, weight=NULL){

  if (is.null(weight)){

    weighting_fun<- list(lazyeval::interp(~mean(target), target=as.name(target)), ~n())
    weighting_fun<- setNames(weighting_fun, c(target, "weight"))

    data_sum<- input_data %>%
      dplyr::group_by_(group) %>%
      dplyr::summarise_(.dots = weighting_fun)

    plot<- plotly::plot_ly(data_sum, x=as.formula(paste0("~", group)) , y=~weight, type="bar", color=I("darkgray"), alpha = 1, name="weight") %>%
      plotly::add_trace(data_sum, x=as.formula(paste0("~", group)) , y=as.formula(paste0("~", target)), yaxis="y2", type="scatter", mode="lines+markers", color=I("navy"), alpha=1, name=target) %>%
      plotly::layout(
        yaxis2=list(
          overlaying = "y",
          side = "right",
          title=target
        )
      ) %>%
      plotly::config(collaborate = F, editable=F, showLink=F, sendData=F, displaylogo=T)
    if (!is.null(compare)) plot<- plot %>% plotly::add_trace(data_sum, x=as.formula(paste0("~", group)) , y=as.formula(paste0("~", compare)), yaxis="y2", type="scatter", mode="lines+markers", color=I("pink"), alpha=1, name=compare)
  }else {

    weighting_fun<- lazyeval::interp(~ ./weight, weight = as.name(weight))

    data_sum<- input_data %>%
      dplyr::group_by_(group) %>%
      dplyr::summarise_at(.cols = c(target, compare, weight), .funs = dplyr::funs(sum(as.numeric(.), na.rm=T))) %>%
      dplyr::mutate_at(.cols = c(target, compare), .funs = dplyr::funs_(dots = weighting_fun))

    plot<- plotly::plot_ly(data_sum, x=as.formula(paste0("~", group)) , y=as.formula(paste0("~", weight)), type="bar", color=I("darkgray"), alpha = 1, name=weight) %>%
      plotly::add_trace(data_sum, x=as.formula(paste0("~", group)) , y=as.formula(paste0("~", target)), yaxis="y2", type="scatter", mode="lines+markers", color=I("navy"), alpha=1, name=target) %>%
      #if (!is.null(compare)) plotly::add_trace(data_sum, x=as.formula(paste0("~", group)) , y=as.formula(paste0("~", compare)), yaxis="y2", type="scatter", mode="lines+markers", color=I("pink"), alpha=1, name=compare) %>%
      plotly::layout(
        yaxis2=list(
          overlaying = "y",
          side = "right",
          title=target
        )
      ) %>%
      plotly::config(collaborate = F, editable=F, showLink=F, sendData=F, displaylogo=T)
    if (!is.null(compare)) plot<- plot %>% plotly::add_trace(data_sum, x=as.formula(paste0("~", group)) , y=as.formula(paste0("~", compare)), yaxis="y2", type="scatter", mode="lines+markers", color=I("pink"), alpha=1, name=compare)
  }
  return(plot)
}
