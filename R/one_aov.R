one_aov <- function(dc,ac,bc){
  datas <- load_data(dc)
  Treatment <- datas[,ac]
  aov(datas[,bc] ~ Treatment, data = datas)
}
