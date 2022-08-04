lsd_plot <- function(dd,id){
  plot(agricolae::LSD.test(dd, id,p.adj = "bonferroni"))
}
