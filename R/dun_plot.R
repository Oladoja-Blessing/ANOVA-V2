dun_plot <- function(dd,id){
  plot(agricolae::duncan.test(dd,id))
}
