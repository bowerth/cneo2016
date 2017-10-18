
read_eo_sheet <- function(sheet = stop("'sheet' must be provided"),
                          file = stop("'file' must be provided"),
                          dat_empty = data.frame(NULL)) {
  dat_orig <- tryCatch(as.data.frame(read_excel(path = file, sheet = sheet)),
                  error = function(err) { data.frame(NULL) })
  dat_clean <- clean_df(dat_orig)
  dat_out <- if(nrow(dat_clean) == 0) dat_empty else dat_clean
  return(dat_out)
}


## Remove empty columns and empty values (it can happen depending on the excel contents)
## data <- rbind.data.frame(
##   c("<NA>", "<NA>", "<NA>"),
##   DATA$M)
## data <- cbind(data,
##               rep("<NA>", nrow(data)),
##               rep("<NA>", nrow(data)))
## names(data) <- c(names(data)[1:3], "", NA)
## head(data)
clean_df <- function(data = stop("'data' must be provided")) {
  if ("Date" %in% colnames(data)) {
    data_out <-
      data %>%
      subset(Date != "<NA>") %>%
      .[,colnames(.)[colnames(.)!= "" & is.na(colnames(.)) == FALSE]]
    return(data_out)
  } else {
    return(data)
  }
}


eco_format_ggplot <- function(p, y2formula="~.") {
  p_out <- p +
    expand_limits(y = c(param$y_min, param$y_max)) +
    ##
    ## Reference Line (horizontal)
    ## You can change the color and linetype like this
    ## geom_hline(yintercept = c(10,15), color = "red", linetype = "dashed")
    geom_hline(yintercept = c(0), color = "black") +
    ##
    ## Set X ticks
    scale_x_date(breaks = label_tick_seq,
                 date_labels = "%Y",
                 expand = c(0,0)) +
    ##
    ## Set Y ticks
    scale_y_continuous(position = param$YPosition,
                       breaks = seq(from = param$y_min,
                                    to = param$y_max,
                                    by = param$y_break),
                       ## Second Y axis
                       sec.axis = sec_axis(trans = as.formula(y2formula),
                                           name = param_secondAxis_Label,
                                           breaks = seq(param$secondAxis_Min,
                                                        param$secondAxis_Max,
                                                        param$secondAxis_Break)),
                       expand = c(0, 0)) +
    ##
    ## Add title, subtiles, x/y labels
    labs(x = "",
         y = param_labelY,
         title = param[[lang]]$title,
         subtitle = param[[lang]]$subtitle) +
    ## The 0.001 fix a bug on double axis display (for "DEU Labour market")
    coord_cartesian(ylim = c(param$y_min - 0.001 * abs(param$y_min),
                             param$y_max))  +
    ## Apply ECO theme
    theme_ECO(legendposition = c(param[[lang]]$legendX,
                                 param[[lang]]$legendY),
              legenddirection = param[[lang]]$legendDir,
              panelontop = param$panel_ontop,
              rotateXLabel = "NO",
              ticksOnX = "NO")+
    annotate(geom = "segment", y = param$y_min, yend = param$y_min + ticksize,
             x = major_tick_seq, xend = major_tick_seq, size = 0.1)

  return(p_out)
}
