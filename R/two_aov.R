two_aov <- function(ac, bc, dc, ec){
  datas <- load_data(ac)
  Treatment <- datas[, bc]
  Block <- datas[, dc]
  aov(datas[, ec]~ Treatment + Block,data = datas)
}
