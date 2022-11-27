
#' create linechart for xts cumret data
#' @export
plotly_line_chart_xts <- function(data_xts, line_width=2){
  p <- plot_ly()
  for(i in 1:ncol(data_xts)){
    p <- p %>%
      add_trace(
        x=as.Date(index(data_xts)),
        y=coredata(data_xts[,i]),
        name=if(!is.null(colnames(data_xts)[i])){colnames(data_xts)[i]}else{paste0("trace_",i)},
        mode="lines",
        type = 'scatter',
        line = list(width=line_width))
  }
  p %>%
    layout(legend = list(orientation = 'h', xanchor = "center", x = 0.5)) %>%
    config(displayModeBar = FALSE)
}

