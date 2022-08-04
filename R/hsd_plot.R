hsd_plot <- function(dd,id){
  plot(agricolae::HSD.test(dd, id))
}
