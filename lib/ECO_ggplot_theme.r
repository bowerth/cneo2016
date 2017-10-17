## Authors: C.MEYER and C.LIOTTA
## Date: 2017-07
##
## ECO  theme to produce graph for the EO

## Load fonts
## library(extrafont)

## if (!Sys.info()[["sysname"]]=="Linux") suppressMessages(extrafont::loadfonts(device="win"))
## suppressMessages(extrafont::loadfonts(device="pdf"))
## suppressMessages(extrafont::loadfonts(device="postscript"))

## eco_default_color_values         <- c("#da2128","#58BB58"    , "#8cc841","#037bc1","#7f0506","#CCCCCC","#f47920","#a154a1","#EDF0F7"   ,"#C8c8c8",     "#000000")
## eco_default_color_names          <- c('red'    ,'light green', 'green'  ,'blue'   ,'brown'  ,'grey'   ,'orange' ,'purple' ,'light grey','middle grey', 'black')
## eco_default_line_width           <- 0.8
## eco_default_shading_color        <- "#EDF0F7"

## cat(
##   yaml::as.yaml(list(
##           eco_default_color_values = eco_default_color_values,
##           eco_default_color_names = eco_default_color_names,
##           eco_default_line_width = eco_default_line_width,
##           eco_default_shading_color = eco_default_shading_color
##         )))

## -----------------------------------------------------------------------------------------------
##
## -----------------------------------------------------------------------------------------------
StopProgramIfLibraryIsMissing <- function(lib){
  if ((lib %in%   rownames(installed.packages()))==FALSE) {
    stop(paste0("Missing library '",lib, "': You should install the library 'reshape2' (Contact your CI if you need more information)"))
  }
}

## -----------------------------------------------------------------------------------------------
##
## -----------------------------------------------------------------------------------------------
theme_ECO <- function (legendposition,legenddirection, ticksOnX="YES", rotateXLabel="NO",panelontop="TRUE") {
  if (ticksOnX=="YES"){
    opt_x_ticks <- element_line(colour = "black", size = 0.2)
  } else {
    opt_x_ticks <- element_blank()
  }
  if (rotateXLabel=="YES") {
    xTxtAngle <- 90
  }
  else {
    xTxtAngle <- 0
  }

  theme_base(base_size=10, base_family= "Arial") %+replace%
    theme(
      plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"),
      plot.background   = element_blank(),
      panel.background  = element_blank(), # backgroung of the chart
      panel.border      = element_blank(), # remove border around the graph
      panel.ontop       = panelontop,      # to see grid on top
      plot.title        = element_text(hjust  = 0.5,
                                       size   = 8,
                                       face   = "bold",
                                       vjust  = 1 ,
                                       margin = margin(0,0,0,0) ), # center the title of the grpah and define font size, and sacing with the graph below
      plot.subtitle     = element_text(hjust  = 0.5,
                                       size   = 7,
                                       vjust  = 1 ,
                                       margin = margin(0,0,5,0) ),

      ## ========================
      ## LEGEND
      ## ========================
      legend.position    = legendposition, # legende position can be defined by parameter
      legend.direction   = legenddirection, # to define vertical or horizontal legend
      legend.title       = element_blank(), # remove legend title
      legend.text        = element_text(size=6), # define font size of the legend
      legend.key         = element_rect(fill="transparent", color = 0), # define the legend left aprt (with line colors)
      legend.key.width   = unit(20,"pt"), # define width of the legend line or box
      legend.key.size    = unit(10,"pt"), # define size between series in legend
      legend.background  = element_blank(), # remobve border around the legend box
      legend.margin      = margin(0,0,0,0, "cm"), # remobve legend box margin
      legend.spacing     = unit(0, "cm"),         # reduce spacing in legend

      ## ========================
      ## AXIS
      ## ========================
      ## axis.title.y       = element_text(size=7,vjust = 1.08, margin = margin(r = -3, unit = "cm")), # put title (unit) of the Y axis on top and centered (leeft and right)
      axis.title.y       = element_text(size=7,vjust = 1.08, margin = margin(r = -1, unit = "in")), # put title (unit) of the Y axis on top and centered (leeft and right)
      ## axis.title.y       = element_text(angle = 0, vjust = 1.04, hjust = -0.3),
      ## axis.title.y.right = element_text(angle = 0, vjust = 1.04, hjust = -0.3),
      ## axis.title.y.right = element_text(size=7,vjust = 1.08, margin = margin(l = -2, unit = "cm")),
      axis.title.y.right = element_text(size=7,vjust = 1.08, margin = margin(l = -1, unit = "in")),

      axis.text.y         = element_text(size=7,margin=margin(15,10,0,0,"pt")), # define margin around the Y axis text
      axis.text.y.right   = element_text(size=7,margin=margin(15,0,0,10,"pt")),

      panel.grid.major.y  = element_line(color="#c8c8c8", size = 0.2), # define y grid size and color
      axis.ticks.y        = element_blank(), # remove tick on the y acis
      axis.line.x         = element_line(color="black", size = 0.2), # define color and siez of x axis
      axis.ticks          = opt_x_ticks,      # define color and size of x tick
      axis.ticks.length   = unit(-0.1, "cm"), # define size of the tick
      ## axis.text.x         = element_text(size=7,angle = 0,hjust=-1, margin=margin(7,0,0,150,"pt")), # rotate the X axis text
      axis.text.x         = element_text(size=7,angle = xTxtAngle, margin=margin(7,0,0,0,"pt")), # rotate the X axis text
      axis.title.x        = element_blank() # remove x title
    )
}

## -----------------------------------------------------------------------------------------------
##
## -----------------------------------------------------------------------------------------------
manage_axis_title<-function (param_labelY,param_YPosition )
{
  ## function to align the axistitltle they are calculated for 20char      _________________yty  and yty_________________

  ## PO 2017-09-18: Add trycatch to prevent exception : cannot open file 'Rplots.pdf'
  tryCatch({
    ## cat('\n  manage_axis_title...\n')
    ## pdf(paste0('c:/temp/',Sys.getenv("USERNAME"),'/temp_', Sys.getenv("USERNAME"), '.pdf'))  # This will create a temp pdf file that solve the 'cannot open file 'Rplots.pdf'' error message
    w1 <- strwidth(" " ,                  font = 9, units = "in")
    w2 <- strwidth("1234567890123456789", font = 9, units = "in")
    w3 <- strwidth(param_labelY,          font = 9, units = "in")
    if (tolower(param_YPosition) == "right") {
      param_labelY    <- paste0(strrep(" ", (w2*55/100-(w3*55/100))/(w1*55/100)),param_labelY)
    } else  {
      param_labelY    <- paste0(param_labelY,strrep(" ", (w2*55/100-(w3*55/100))/(w1*55/100)))
    }
  },
  error=function(cond){
    ## Normaly, this code should not been executed (it's just in case of, to not throw an exception)
    cat(paste0('\n Warning(manage_axis_title) - Should not come to here\n'))
    w1 <- 0.04633333
    w2 <- 1.760667
    w3 <- str_length(param_labelY)*w1    # Calcul is approximate
    if (tolower(param_YPosition) == "right") {
      param_labelY    <- paste0(strrep(" ", (w2*55/100-(w3*55/100))/(w1*55/100)),param_labelY)
    } else  {
      param_labelY    <- paste0(param_labelY,strrep(" ", (w2*55/100-(w3*55/100))/(w1*55/100)))
    }
  },
  finally = {


  })
}
