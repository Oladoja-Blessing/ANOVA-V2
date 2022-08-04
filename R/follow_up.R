follow_up <- function(anov){
  switch (anov,
          LSD = lsd_plot(),
          HSD = hsd_plot(),
          Duncan = dun_plot()
  )
}
