load_data <- function(df){
  req(df)
  infile <- df
  datas <- infile$datapath
  read.csv(datas)
}
