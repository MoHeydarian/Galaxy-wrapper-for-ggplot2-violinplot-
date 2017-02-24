# Setup R error handling to go to stderr
options(show.error.messages=F, error=function(){cat(geterrmessage(),file=stderr());q("no",1,F)})

# We need to not crash galaxy with an UTF8 error on German LC settings.
loc <- Sys.setlocale("LC_MESSAGES", "en_US.UTF-8")


# Import library
library("getopt")
library("reshape2")
library("ggplot2")
options(stringAsfactors = FALSE, useFancyQuotes = FALSE)
# Take in trailing command line arguments
args <- commandArgs(trailingOnly = TRUE)


# get options, using the spec as defined by the enclosed list.
# we read the options from the default: commandArgs(TRUE).
option_specification = matrix(c(
  'input', 'i', 2, 'character',
  'title', 't',2, 'character',
  'ggfill', 's', 2, 'character',
  'ggcolor', 'z', 2, 'character',
  'xlab', 'x', 2, 'character',
  'ylab', 'y', 2, 'character',
  'drawquartiles', 'a', 2, 'character',
  'transform', 'w', 2, 'character',
  'xaxismin', 'e', 2, 'integer',
  'xaxismax', 'f', 2, 'integer',
  'yaxismin', 'g', 2, 'integer',
  'yaxismax', 'h', 2, 'integer',
  'scaling', 'b', 2, 'character',
  'theme', 'l', 2, 'character',
  'dim', 'k', 2, 'character',
  'woutputdim', 'c', 2, 'integer',
  'houtputdim', 'd', 2, 'integer',
  'factor', 'n', 2, 'character',
  'factorcol', 'm', 2, 'integer',
  'colors', 'q', 2, 'character', 
  'colororder', 'r', 2, 'integer', 
  'pointcolor', 'u', 2, 'character', 
  'axistitlecust', 'v', 2, 'character',
  'axistitlecolor', '1', 2, 'character',
  'axistitlesize', '2', 2, 'integer',
  'axistitleface', '3', 2, 'character',
  'axistextcust', '4', 2, 'character',
  'axistextcolor', '5', 2, 'character',
  'axistextsize', '6', 2, 'integer',
  'axistextface', '7', 2, 'character',
  'plottitlecust', '8', 2, 'character',
  'plottitlecolor', '9', 2, 'character',
  'plottitlesize', '10', 2, 'integer',
  'plottitleface', '11', 2, 'character',
  'gridlinecust', '12', 2, 'character',
  'output', 'o', 2, 'character'
  ), byrow=TRUE, ncol=4);

# Parse options
options = getopt(option_specification);



# Print options to see what is going on
cat("\n input: ",options$input)
cat("\n title: ",options$title)
#cat("\n size: ",options$size)
cat("\n xlab: ",options$xlab)
cat("\n ylab: ",options$ylab)
cat("\n output: ",options$output)

#choosing whether to plot data as multiple groups on one plot(factoring) OR multiple groups on different plots
if(options$factor == "Multiple"){
     gg_facet = facet_wrap( ~ factor)
     gg_factor = NULL
     color_scale = NULL
          
          if(options$points == "Default"){
            gg_point = geom_point(size=1, alpha=1, gg_factor)
        } else {
            gg_point = geom_point(size=options$size, alpha=options$alpha, colour=options$pointcolor)
            cat("\n size: ",options$size)
            cat("\n alpha: ",options$alpha)
            cat("\n pointcolor: ",options$pointcolor)
        }

    names(input)[options$factorcol] <- "factor"
   
    cat("\n factor: ",options$factor)
    cat("\n factorcol: ",options$factorcol)
} else if(options$factor == "Single"){
    gg_facet = NULL
    gg_factor = aes(colour=factor(factor))

          if(options$points == "Default"){
            gg_point = geom_point(size=1, alpha=1, gg_factor)
        } else {
            gg_point = geom_point(size=options$size, alpha=options$alpha, gg_factor)
            cat("\n size: ",options$size)
            cat("\n alpha: ",options$alpha)
            cat("\n pointcolor: ",options$pointcolor)
        }

      if(options$colors == "Default"){
          color_scale = scale_colour_hue(direction=options$colororder)
      } else {
          color_scale = scale_color_brewer(palette=options$colors, direction=options$colororder)
      }

    names(input)[options$factorcol] <- "factor"
    
    cat("\n factor: ",options$factor)
    cat("\n factorcol: ",options$factorcol)
    cat("\n color_scale: ",options$colors)
    cat("\n color_order: ",options$colororder)
} else{
      gg_facet = NULL
      gg_factor = NULL
      color_scale = NULL

        if(options$points == "Default"){
            gg_point = geom_point(size=1, alpha=1, gg_factor)
        } else {
            gg_point = geom_point(size=options$size, alpha=options$alpha, colour=options$pointcolor)
            cat("\n size: ",options$size)
            cat("\n alpha: ",options$alpha)
            cat("\n pointcolor: ",options$pointcolor)
        }    
}

if(options$scaling == "Automatic"){
    gg_scaley = NULL
} else {
    gg_scaley = ylim(options$yaxismin,options$yaxismax)
    cat("\n yaxismin: ",options$yaxismin)
    cat("\n yaxismax: ",options$yaxismax)
}


integrated <- read.csv(options$input,sep='\t',header=TRUE)
input <- melt(integrated)



if(options$transform == "log2"){
    input["value"] <- log2(input["value"])
}else if(options$transform == "log2plus1"){
    input["value"] <- log2(input["value"]+1)
}else if(options$transform == "log10"){
    input["value"] <- log10(input["value"])
}else if(options$transform == "log10plus1"){
    input["value"] <- log10(input["value"]+1)
    }else{
}

if(options$drawquartiles == "none"){
    gg_quartile = NULL
} else {
    gg_quartile = c(0.25, 0.5, 0.75)
}

#axis label custization
if(options$axistitlecust == "Default"){
    gg_axistitle = theme(axis.title = element_text(color = NULL, size = NULL, face = NULL))
} else {
    gg_axistitle = theme(axis.title = element_text(color = options$axistitlecolor, size = options$axistitlesize, face = options$axistitleface))
}


#axis text(tick) custization
if(options$axistextcust == "Default"){
    gg_axistext = theme(axis.text = element_text(color = NULL, size = NULL, face = NULL))
} else {
    gg_axistext = theme(axis.text = element_text(color = options$axistextcolor, size = options$axistextsize, face = options$axistextface))
}


#plot title custimization
if(options$plottitlecust == "Default"){
    gg_plottitle = theme(plot.title = element_text(color = NULL, size = NULL, face = NULL))
} else {
    gg_plottitle = theme(plot.title = element_text(color = options$plottitlecolor, size = options$plottitlesize, face = options$plottitleface))
}

#grid line customization
if(options$gridlinecust == "Default"){
    gg_gridline = NULL
} else if(options$gridlinecust == "hidemajor"){
    gg_gridline = theme(panel.grid.major = element_blank())
} else if(options$gridlinecust == "hideminor"){
    gg_gridline = theme(panel.grid.minor = element_blank())
} else if(options$gridlinecust == "hideboth"){
    gg_gridline = theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank())
} else {
}

ggplot(input,aes(variable,value)) +geom_violin(scale = "area", colour = options$ggcolor, fill = options$ggfill, draw_quantiles = gg_quartile) +
gg_scaley+theme_bw()+xlab(options$xlab)+ylab(options$ylab)+ggtitle(options$title)

ggsave(file=options$output)
