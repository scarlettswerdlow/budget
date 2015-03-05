###########################################################################
#                                                                         #
#  Rauner budget proposal analysis II                                     #
#  Pension analysis                                                       #
#  Coded by Scarlett Swerdlow                                             #
#  scarlett.swerdlow@gmail.com                                            #
#  February 18, 2015                                                      #
#                                                                         #
###########################################################################

library( reshape2 )
library( plyr )
library( ggplot2 )

##############################
#  Read in and prepare data  #
##############################

# From the Fiscal Year 2016 Budget Book; Appropriation category added
# http://www2.illinois.gov/gov/budget/Pages/default.aspx
pensions <- read.csv( "~/budget/pensions.csv", header = TRUE )

# Melt data by appropriation
pensions_m <- melt(pensions, id = c( "X", "Approp.Code", 
                                     "Agency", "Agency.Name", 
                                     "Div", "Division.Name", 
                                     "Appropriation.Category",
                                     "Appropriation.Name", "Appropriation",
                                     "Fund.Name", "Fund", 
                                     "Fund.Category.Name",
                                     "Appropriation.Type", "Capital.Line" ) )

# Limit data to 2014 budget, 2015 budget, and 2016 proposal
pensions_m <- pensions_m[ pensions_m$variable == "FY14.Actual.Approp" |
                            pensions_m$variable == "FY15.Actual.Approp" |
                            pensions_m$variable == "FY16.Governors.Proposed", ]

pensions_m <- rename( pensions_m, replace = c( "variable" = "Year",
                                               "value" = "Amount" ) )

pensions_m$Year <- ifelse( pensions_m$Year == "FY14.Actual.Approp",
                           2014, ifelse( 
                             pensions_m$Year == "FY15.Actual.Approp",
                             2015, 2016 ) )

###################################################################
#  Calculate pension spending by system, year, and spending type  #
###################################################################

# Total appropriations by system, year, and contribution type
System <- c()
Type <- c()
Year <- c()
Approp <- c()

for ( system in unique( pensions_m$Agency.Name ) ) {
  data <- pensions_m[ pensions_m$Agency.Name == system, ]
  
  for ( year in unique( data$Year ) ) {
    dataYear <- data[ data$Year == year, ]
    
    for ( type in unique( dataYear$Appropriation.Category ) ) {
      amt <- 0
      amt <- amt + sum( dataYear$Amount[ dataYear$Appropriation.Category == type ] )
      
      System <- append( System, system )
      Year <- append( Year, year )
      Type <- append( Type, type )
      Approp <- append( Approp, amt )
    }  
  }
}

pensionsByType <- data.frame( System, Type, Year, Approp )

# Plot of state pension spending by year and system
pensionsPlt <- ggplot( pensionsByType, aes( x = factor( Year ), y = Approp/1000000000, fill = System ) ) + 
  geom_bar( stat = "identity" ) +
  ggtitle( "Illinois pension contributions, 2014-2016" ) +
  theme( axis.title.x = element_blank() ) +
  ylab( "In billions" ) +
  scale_x_discrete( "Year",
                    labels = c( "2014" = "Fiscal Year 2014\nEnacted", 
                                "2015" = "Fiscal Year 2015\nEnacted", 
                                "2016" = "Fiscal Year 2016\nProposed" ) ) +
  guides( fill = guide_legend( title=NULL ) ) +
  theme( text = element_text( size = 16 ) )

#############################################################
#  Calculate change in pension spending by year and system  #
#############################################################

p14 <- by( pensionsByType$Approp[pensionsByType$Year == 2014 ], 
           pensionsByType$System[pensionsByType$Year == 2014 ], sum )
p15 <- by( pensionsByType$Approp[pensionsByType$Year == 2015 ], 
           pensionsByType$System[pensionsByType$Year == 2015 ], sum )
p16 <- by( pensionsByType$Approp[pensionsByType$Year == 2016 ], 
           pensionsByType$System[pensionsByType$Year == 2016 ], sum )

Change <- c()

for ( i in 1:length(p16) ) {
  change <- (p16[i] - p15[i])/p15[i]
  Change <- append( Change, change )
}

pensionsChange <- data.frame( Change )

# Plot of change in state pension spending by system 
pensionsChangePlt <- ggplot( pensionsChange, aes( x = factor( row.names(pensionsChange) ), y = Change*-100, fill =  factor( row.names(pensionsChange) ) ) ) + 
  geom_bar( stat = "identity" ) +
  ggtitle( "Proposed reduction in 2016 pension contribution by retirement system" ) +
  theme( axis.title.x = element_blank() ) +
  scale_x_discrete( factor( row.names(pensionsChange) ),
                  limits = c( "Judges Retirement System",
                              "General Assembly Retirement System",
                              "State Employees' Retirement System",
                              "State Universities Retirement System",
                              "Teachers' Retirement System" ),
                  labels = c( "Judges Retirement System" = "Judges", 
                              "General Assembly Retirement System" = "General\nAssembly", 
                              "State Employees' Retirement System" = "State\nEmployees",
                              "State Universities Retirement System" = "Universities",
                              "Teachers' Retirement System" = "Teachers" ) ) +
  scale_fill_discrete( guide = FALSE ) +
  ylab( "Percent reduction from 2015 budget" ) +
  theme( text = element_text( size = 16 ) )

multiplot( pensionsPlt, pensionsChangePlt, cols=1 )

########################
#  Multiplot function  #
########################

# From ggplot2
# http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
