###########################################################################
#                                                                         #
#  Rauner budget proposal analysis I                                      #
#  Coded by Scarlett Swerdlow                                             #
#  scarlett.swerdlow@gmail.com                                            #
#  February 18, 2015                                                      #
#                                                                         #
###########################################################################

library( ggplot2 )

# From the Fiscal Year 2016 Budget Book
# http://www2.illinois.gov/gov/budget/Pages/default.aspx
budget <- read.csv( "~/budget/budget16.csv", header = TRUE )

# Agency type categorized according to Fiscal Futures Project database
# http://igpa.uillinois.edu/system/files/FF_Documentation_Jan_2015.pdf
agencies <- read.csv( "~/budget/agencies.csv", header = TRUE )

########################
#  By agency analysis  #
########################

Agency <- c()
Budget16 <- c()
Actual15 <- c()
Budget15 <- c()
Actual14 <- c()
Budget14 <- c()
Diff1615a <- c()
Diff1615b <- c()
Diff1514 <- c()
Share15 <- c()

for ( agency in unique( budget$Agency.Name ) ) {
  data <- budget[ budget$Agency.Name == agency, ]
  x16 <- sum( data$FY16.Governors.Proposed, na.rm = TRUE )
  x15a <- sum( data$FY15.Estimate.Expend, na.rm = TRUE )
  x15b <- sum( data$FY15.Actual.Approp, na.rm = TRUE )
  x14a <- sum( data$FY14.Actual.Expend, na.rm = TRUE )
  x14b <- sum( data$FY14.Actual.Approp, na.rm = TRUE )
  d1615a <- (x16-x15a)/x15a
  d1615b <- (x16-x15b)/x15b
  d1514 <- (x15a-x14a)/x14a
  s15 <- x15b/sum(budget$FY15.Actual.Approp, na.rm = TRUE)
  
  Agency <- append( Agency, agency )
  Budget16 <- append( Budget16, x16 )
  Actual15 <- append( Actual15, x15a )
  Budget15 <- append( Budget15, x15b )
  Actual14 <- append( Actual14, x14a )
  Budget14 <- append( Budget14, x14b )
  Diff1615a <- append( Diff1615a, d1615a )
  Diff1615b <- append( Diff1615b, d1615b )
  Diff1514 <- append( Diff1514, d1514 )
  Share15 <- append( Share15, s15 )
}

byAgency <- data.frame( Agency, Budget16, Actual15, Budget15, Actual14, Budget14, 
                        Diff1615a, Diff1615b, Diff1514, Share15 )
byAgency <- byAgency[ byAgency$Agency != "", ]
byAgency <- merge( byAgency, agencies )

###############################
#  By spending type analysis  #
###############################

Type <- c()
Budget16 <- c()
Actual15 <- c()
Budget15 <- c()
Actual14 <- c()
Budget14 <- c()
Diff1615a <- c()
Diff1615b <- c()
Diff1514 <- c()
Share15 <- c()

for ( type in unique( byAgency$Type ) ) {
  data <- byAgency[ byAgency$Type == type, ]
  x16 <- sum( data$Budget16, na.rm = TRUE )
  x15a <- sum( data$Actual15, na.rm = TRUE )
  x15b <- sum( data$Budget15, na.rm = TRUE )
  x14a <- sum( data$Actual14, na.rm = TRUE )
  x14b <- sum( data$Budget14, na.rm = TRUE )
  d1615a <- (x16-x15a)/x15a
  d1615b <- (x16-x15b)/x15b
  d1514 <- (x15a-x14a)/x14a
  s15 <- x15b/sum(byAgency$Budget15, na.rm = TRUE)
  
  Type <- append( Type, type )
  Budget16 <- append( Budget16, x16 )
  Actual15 <- append( Actual15, x15a )
  Budget15 <- append( Budget15, x15b )
  Actual14 <- append( Actual14, x14a )
  Budget14 <- append( Budget14, x14b )
  Diff1615a <- append( Diff1615a, d1615a )
  Diff1615b <- append( Diff1615b, d1615b )
  Diff1514 <- append( Diff1514, d1514 )
  Share15 <- append( Share15, s15 )
}

byType <- data.frame( Type, Budget16, Actual15, Budget15, Actual14, Budget14, 
                      Diff1615a, Diff1615b, Diff1514, Share15 )

changePltByType <- ggplot( byType, aes( x = reorder( factor( Type ), Diff1615b ), y = Diff1615b*100 ) ) + 
  coord_flip() +
  #ylim( -50, 50 ) +
  geom_point( colour = "#2b8cbe", aes( size = Share15 ) ) +
  theme( axis.title.y = element_blank() ) +  
  ggtitle( "Proposed changes to state spending\nin Rauner's fiscal 2016 budget" ) +
  ylab( "Percent change from 2015 budget" ) +
  theme( axis.text.y = element_text( vjust = .5, hjust = 1 ) ) +
  scale_size_continuous( name = "2015\nappropriation\nas share of\n2015 budget" ) +
  geom_hline( aes( yintercept = 0 ), width = .1, colour = "grey" ) +
  theme( text = element_text( size = 16 ) )


