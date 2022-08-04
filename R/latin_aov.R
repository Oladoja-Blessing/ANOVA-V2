latin_aov <- latin <- function(ac, bc, dc, ec, fc){
  datas <- load_data(ac)
  Treatment <- datas[,bc]
  Row <- datas[,dc]
  Column <- datas[,ec]
  aov(datas[,fc] ~ Treatment + Row + Column
      ,data = datas)
}
