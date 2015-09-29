# TODO: Add comment
# 
# Author: averma
###############################################################################

get_month_ind <- function(month, year, table){
	beg <- NULL
	end <- NULL
	col <- paste('Y', year, sep='')
	index <- table[, col]
	num <- as.numeric(month)
	beg <- index[num]
	if (num == 12){
		end <- index[num+1]
	}else{
		end <- index[num+1] - 1
	}
	range <- c(beg, end)
	return(range)
}

get_max_val <- function(year, type, table){
	val <- NULL
	col <- paste('Y', year, sep='')
	max_vals <- table[, col]
	val <- max_vals[type]
	return(val)
}

month_ini <- function(data){
	mon_ini <- c()
	year <- format(as.Date(data[[1]][1]), '%Y') 
	month <- '01'
	for (a in 1:length(data[[1]])){
		if (as.Date(data[[1]][a]) == as.Date(paste(year, '-', month, '-', '01', sep= ''))){
			mon_ini <- c(mon_ini, a)
			num <- as.numeric(month) + 1
			if (num == 13){
				break
			}
			if (num >= 10){
				month <- paste(as.character(num), sep='')
			}else{
				month <- paste('0', as.character(num), sep='')
			}
			print(a)
			print(month)
		}
	}
	return(mon_ini)
}

multiplot <- function(..., plotlist=NULL, cols=1, layout=NULL, widths=NULL, heights=NULL, title=NULL, titlefont = "", titleface = 1, titlesize = 16) {
	
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
	
	if (!is.null(title)) { # Add a narrow row at the top for the title
		layout <- rbind(rep(0,ncol(layout)),layout)
		if (is.null(heights)) {
			plotrows <- nrow(layout)-1
			rowheights <- c(0.1, rep(1,plotrows)/plotrows)
		} else {
			rowheights <- c(0.1, heights/sum(heights))
		}
	} else {
		if (is.null(heights)) {
			rowheights <- rep(1,nrow(layout))  
		} else {
			rowheights <- heights
		}
	}
	
	if (is.null(widths)) {
		colwidths <- rep(1, cols)
	} else {
		colwidths <- widths
	}
	
	if (numPlots==1) {
		
		return(plots[[1]] + labs(title=title))
		
	} else {
		# Set up the page
		grid.newpage()
		pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout), 
								widths=colwidths, 
								heights=rowheights)))
		
		# Make each plot, in the correct location
		for (i in 1:numPlots) {
			# Get the i,j matrix positions of the regions that contain this subplot
			matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
			
			print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
							layout.pos.col = matchidx$col))
		}
		
		if (!is.null(title)) {
			grid.text(title, vp = viewport(layout.pos.row = 1
							, layout.pos.col = 1:ncol(layout)), 
					gp = gpar(fontfamily = titlefont, fontface = titleface, 
							fontsize = titlesize))
		}
		
	}
	return(invisible(NULL))
}

bubble_data <- function(table, max){
	char.xy <- c()
	for (i in 1:length(table[[1]])){
		char.xy <- c(char.xy, as.character(paste(table[[9]][i], ':', table[[10]][i], sep ='')))
	}
	coord <- data.frame(table[c(9,10)])
	coord <- cbind(coord, char.xy)
	bubble1 <- as.data.frame(with(coord, table(coord[[3]])))
	latlng <- as.character(bubble1[[1]])
	lat <- c()
	lng <- c()
	for (a in 1:length(latlng)){
		temp <- strsplit(latlng[a], ':')
		lat <- c(lat, as.numeric(temp[[1]][1]))
		lng <- c(lng, as.numeric(temp[[1]][2]))
	}
	freq <- as.numeric(bubble1[[2]])
	bubble_data <- data.frame(lat, lng, freq)
	lat <- c(43.60, 43.60)
	lng <- c(-79.40, -79.40)
	freq <- c(max,1)
	apped <- data.frame(lat, lng, freq)
	bubble_data <- rbind(bubble_data, apped)
	return(bubble_data)
}

makeFootnote <- function(footnoteText = format(Sys.time(), "%d %b %Y"), size = 1, color = grey(.5)){
	require(grid)
	pushViewport(viewport())
	grid.text(label = footnoteText ,
			x = unit(1,"npc") - unit(2, "mm"),
			y = unit(2, "mm"),
			just = c("right", "bottom"),
			gp = gpar(cex = size, col = color))
	popViewport()
}


# Load in all required packages
library(ggplot2)
library(ggmap)
library(maps)
library(scales)
library(grid)

# Create vector of ticket types - number and description
Code = c(3, 6, 9, 210, 29, 'Other')
Rep = c('Park - Private Property','Park - Signed Except Permit Time','Stop/Stand - Sign Prohibited Times and Days',
		'Fail to Deposit Fee/Display Ticket','Park - Prohibited Time on Permit','Other')

# Map process
setInternet2()
basemap <- get_map(location = c(lon = -79.377906, lat = 43.649159), zoom = 13, scale = "auto", maptype = c("toner-lite"), source = "stamen", color = "bw")
ggmap(basemap)

year = '2014'
indr_merge = paste("C:/Users/averma/Desktop/Parking Data Vis/LatLong Data/",year,"_DT.csv", sep="")
indr_GP = "C:/Users/averma/Desktop/Parking Data Vis/LatLong Data/greenP_DT.csv"
out_dir = paste("C:/Users/averma/Desktop/Parking Ticket Plots/Multiplot/",year," Plot", sep='')

greenP.data <- read.csv(indr_GP, header=T)                  													# Read Green P Parking Station .csv file
parking.data_total <- read.csv(indr_merge, header=T)               												# Read Parking Ticket Data of specified year
month_index <- read.csv("C:/Users/averma/Desktop/Parking Data Vis/LatLong Data/month_index_DT.csv", header=T)	  	# Read Month Index .csv file
max_val <- read.csv("C:/Users/averma/Desktop/Parking Data Vis/LatLong Data/max_val.csv", header=T)

#color_list = c('#FF0000', '#FF7F00', '#397FDB', '#FF42A8', '#606060', '#9366B4', '#338585')
color_list = c('#FF0000', '#FF7F00', '#FFFF00', '#FF42A8', '#79CDCD', '#9366B4')
point_list = c('#800000', '#B25900', '#808000', '#802154', '#3C6666', '#4A335A')
color_list_bar = c('#79CDCD', '#FF0000',  '#FF7F00','#9366B4', '#FFFF00', '#FF42A8')
color_list_bar = c('#FF42A8', '#9366B4',  '#FF0000','#79CDCD', '#FF7F00', '#FFFF00')

# num.df is the dataframe to store every month and every code's number of tickets which can be used for bar charts
num.matrix <- matrix(c(rep(0,84)), nrow = 12, ncol = 6)
num.df <- data.frame(num.matrix)
colnames(num.df) = Code

#Loop through month 1 to 12
for (month in 5:9) {
	start <- Sys.time()
	# Initialize year, month, and load parking ticket data
	month_word <- format(as.Date(paste(year, '-', month,'-', '01', sep='')), '%B')
	range <- get_month_ind(month, year, month_index)                # Get month beginning and end index
	parking.data <- parking.data_total[range[1]:range[2],]               # Truncate full parking ticket data into specified month
	
	# Loop through all types of codes and seperate the original data base
	parking.table <- list()
	for (s in 1:length(Code)) {
		parking.table[[s]]  = subset(parking.data, parking.data$type == Code[s])
	}
	latitude <- parking.data$lat
	longitude <- parking.data$lng
	
	# Base layer
	DT_map <- ggmap(basemap, extent = 'panel', base_layer = ggplot(parking.data, aes(x=longitude, y=latitude))) + guides(colour = guide_legend(label.position = 'right', override.aes = list(alpha = 1)))
	GP_layer = geom_point(aes(x = lng, y= lat, colour = id), data = greenP.data, shape = 21, size=3, fill="#009933", colour = 'white') 
	
	# Bar Chart Dataframe
	bar <- data.frame(0,0)
	colnames(bar) <- c('Type', 'Total')
	
	# Create a new list to store every map
	all_maps = list()
	
	# Assign alpha value
	alph = 1/2.5
	
	# Start to loop through every data frame in parking table
	
	# MAP_1
	i = 1
	table = parking.table[[i]]
	num <- length(table[[i]])
	table <- bubble_data(table, get_max_val(year, i, max_val))
	map.heat_1 <- DT_map + stat_density2d(aes(x = lng, y = lat, fill = ..level.., alpha = ..level..),
					size = 0.1, bins = 11, data = table, geom = "polygon", breaks = seq(200,1500,130)) + 
			scale_alpha(range = c(.125, 1), guide = FALSE) +
			scale_fill_gradient(low = "#BFBFBF", high = color_list[i], guide = FALSE) + 
			theme(axis.line=element_blank(),axis.text.x=element_blank(),axis.text.y=element_blank(),axis.ticks=element_blank(),
					axis.title.x=element_blank(),axis.title.y=element_blank(),panel.background=element_blank(),panel.border=element_blank(),
					panel.grid.major=element_blank(),panel.grid.minor=element_blank(),plot.background=element_blank(),legend.position = "right") +
			geom_point(aes(x = lng , y = lat, size = freq), colour = point_list[i], data = table, alpha=alph) + 
			scale_size_continuous(range = c(1,10), name = 'Frequency') + GP_layer + ggtitle(paste(month_word, " ", year ," Parking Tickets: ", Rep[i], sep=''))
	num.df[month, i] = num
	
	# MAKE A PDF!
	pdf(paste("C:/Users/averma/Desktop/Parking Ticket Plots/DT Map/",year," map DT/", month_word , "_", year,"_Toronto_DT_Code_", Code[i], ".pdf", sep=''), width = 13, height = 13)
	plot(map.heat_1)
	makeFootnote(paste("Total Number of Tickets: " , as.character(num), sep=''), color = "black")
	dev.off()
	
	# MAKE A JPEG! 
	jpeg(filename = paste("C:/Users/averma/Desktop/Parking Ticket Plots/DT Map/",year," map DT/", month_word , "_", year,"_Toronto_DT_Code_", Code[i], ".jpeg", sep=''),
			width = 5000, height = 5000, pointsize = 12,quality = 100, res= 500)
	plot(map.heat_1)
	makeFootnote(paste("Total Number of Tickets: " , as.character(num), sep=''), color = "black")
	dev.off()
	
	# MAKE A NEW TITLE!
	map.heat_1 <- map.heat_1 + ggtitle(paste(Rep[i], sep='')) + theme(legend.position = 'none')
	
	# MAP_2
	i = 2
	table = parking.table[[i]]
	num <- length(table[[i]])
	table <- bubble_data(table, get_max_val(year, i, max_val))
	map.heat_2 <- DT_map + stat_density2d(aes(x = lng, y = lat, fill = ..level.., alpha = ..level..),
					size = 0.1, bins = 11, data = table, geom = "polygon", breaks = seq(200,1500,130)) + 
			scale_alpha(range = c(.125, 1), guide = FALSE) +
			scale_fill_gradient(low = "#BFBFBF", high = color_list[i], guide = FALSE) + 
			theme(axis.line=element_blank(),axis.text.x=element_blank(),axis.text.y=element_blank(),axis.ticks=element_blank(),
					axis.title.x=element_blank(),axis.title.y=element_blank(),panel.background=element_blank(),panel.border=element_blank(),
					panel.grid.major=element_blank(),panel.grid.minor=element_blank(),plot.background=element_blank(),legend.position = "right") +
			geom_point(aes(x = lng , y = lat, size = freq), colour = point_list[i], data = table, alpha=alph) + 
			scale_size_continuous(range = c(1,10), name = 'Frequency') + GP_layer + ggtitle(paste(month_word, " ", year ," Parking Tickets: ", Rep[i], sep=''))
	num.df[month, i] = num
	
	# MAKE A PDF!
	pdf(paste("C:/Users/averma/Desktop/Parking Ticket Plots/DT Map/",year," map DT/", month_word , "_", year,"_Toronto_DT_Code_", Code[i], ".pdf", sep=''), width = 13, height = 13)
	plot(map.heat_2)
	makeFootnote(paste("Total Number of Tickets: " , as.character(num), sep=''), color = "black")
	dev.off()
	
	# MAKE A JPEG! 
	jpeg(filename = paste("C:/Users/averma/Desktop/Parking Ticket Plots/DT Map/",year," map DT/", month_word , "_", year,"_Toronto_DT_Code_", Code[i], ".jpeg", sep=''),
			width = 5000, height = 5000, pointsize = 12,quality = 100, res= 500)
	plot(map.heat_2)
	makeFootnote(paste("Total Number of Tickets: " , as.character(num), sep=''), color = "black")
	dev.off()
	
	# MAKE A NEW TITLE!
	map.heat_2 <- map.heat_2 + labs(title=paste(Rep[i], sep='')) + theme(legend.position = 'none')
	
	# MAP_3
	i = 3
	table = parking.table[[i]]
	num <- length(table[[i]])
	table <- bubble_data(table, get_max_val(year, i, max_val))
	map.heat_3 <- DT_map + stat_density2d(aes(x = lng, y = lat, fill = ..level.., alpha = ..level..),
					size = 0.1, bins = 11, data = table, geom = "polygon", breaks = seq(200,1500,130)) + 
			scale_alpha(range = c(.125, 1), guide = FALSE) +
			scale_fill_gradient(low = "#BFBFBF", high = color_list[i], guide = FALSE) + 
			theme(axis.line=element_blank(),axis.text.x=element_blank(),axis.text.y=element_blank(),axis.ticks=element_blank(),
					axis.title.x=element_blank(),axis.title.y=element_blank(),panel.background=element_blank(),panel.border=element_blank(),
					panel.grid.major=element_blank(),panel.grid.minor=element_blank(),plot.background=element_blank(),legend.position = "right") +
			geom_point(aes(x = lng , y = lat, size = freq), colour = point_list[i], data = table, alpha=alph) + 
			scale_size_continuous(range = c(1,10), name = 'Frequency') + GP_layer + ggtitle(paste(month_word, " ", year ," Parking Tickets: ", Rep[i], sep=''))
	num.df[month, i] = num
	
	# MAKE A PDF!
	pdf(paste("C:/Users/averma/Desktop/Parking Ticket Plots/DT Map/",year," map DT/", month_word , "_", year,"_Toronto_DT_Code_", Code[i], ".pdf", sep=''), width = 13, height = 13)
	plot(map.heat_3)
	makeFootnote(paste("Total Number of Tickets: " , as.character(num), sep=''), color = "black")
	dev.off()
	
	# MAKE A JPEG! 
	jpeg(filename = paste("C:/Users/averma/Desktop/Parking Ticket Plots/DT Map/",year," map DT/", month_word , "_", year,"_Toronto_DT_Code_", Code[i], ".jpeg", sep=''),
			width = 5000, height = 5000, pointsize = 12,quality = 100, res= 500)
	plot(map.heat_3)
	makeFootnote(paste("Total Number of Tickets: " , as.character(num), sep=''), color = "black")
	dev.off()
	
	# MAKE A NEW TITLE!
	map.heat_3 <- map.heat_3 + labs(title=paste(Rep[i], sep='')) + theme(legend.position = 'none')
	
	# MAP_4
	i = 4
	table = parking.table[[i]]
	num <- length(table[[i]])
	table <- bubble_data(table, get_max_val(year, i, max_val))
	map.heat_4 <- DT_map + stat_density2d(aes(x = lng, y = lat, fill = ..level.., alpha = ..level..),
					size = 0.1, bins = 11, data = table, geom = "polygon", breaks = seq(200,1500,130)) + 
			scale_alpha(range = c(.125, 1), guide = FALSE) +
			scale_fill_gradient(low = "#BFBFBF", high = color_list[i], guide = FALSE) + 
			theme(axis.line=element_blank(),axis.text.x=element_blank(),axis.text.y=element_blank(),axis.ticks=element_blank(),
					axis.title.x=element_blank(),axis.title.y=element_blank(),panel.background=element_blank(),panel.border=element_blank(),
					panel.grid.major=element_blank(),panel.grid.minor=element_blank(),plot.background=element_blank(),legend.position = "right") +
			geom_point(aes(x = lng , y = lat, size = freq), colour = point_list[i], data = table, alpha=alph) + 
			scale_size_continuous(range = c(1,10), name = 'Frequency') + GP_layer + ggtitle(paste(month_word, " ", year ," Parking Tickets: ", Rep[i], sep=''))
	num.df[month, i] = num
	
	# MAKE A PDF!
	pdf(paste("C:/Users/averma/Desktop/Parking Ticket Plots/DT Map/",year," map DT/", month_word , "_", year,"_Toronto_DT_Code_", Code[i], ".pdf", sep=''), width = 13, height = 13)
	plot(map.heat_4)
	makeFootnote(paste("Total Number of Tickets: " , as.character(num), sep=''), color = "black")
	dev.off()
	
	# MAKE A JPEG! 
	jpeg(filename = paste("C:/Users/averma/Desktop/Parking Ticket Plots/DT Map/",year," map DT/", month_word , "_", year,"_Toronto_DT_Code_", Code[i], ".jpeg", sep=''),
			width = 5000, height = 5000, pointsize = 12,quality = 100, res= 500)
	plot(map.heat_4)
	makeFootnote(paste("Total Number of Tickets: " , as.character(num), sep=''), color = "black")
	dev.off()
	
	# MAKE A NEW TITLE!
	map.heat_4 <- map.heat_4 + labs(title=paste(Rep[i], sep='')) + theme(legend.position = 'none')
	
	# MAP_5
	i = 5
	table = parking.table[[i]]
	num <- length(table[[i]])
	table <- bubble_data(table, get_max_val(year, i, max_val))
	map.heat_5 <- DT_map + stat_density2d(aes(x = lng, y = lat, fill = ..level.., alpha = ..level..),
					size = 0.1, bins = 11, data = table, geom = "polygon", breaks = seq(200,1500,130)) + 
			scale_alpha(range = c(.125, 1), guide = FALSE) +
			scale_fill_gradient(low = "#BFBFBF", high = color_list[i], guide = FALSE) + 
			theme(axis.line=element_blank(),axis.text.x=element_blank(),axis.text.y=element_blank(),axis.ticks=element_blank(),
					axis.title.x=element_blank(),axis.title.y=element_blank(),panel.background=element_blank(),panel.border=element_blank(),
					panel.grid.major=element_blank(),panel.grid.minor=element_blank(),plot.background=element_blank(),legend.position = "right") +
			geom_point(aes(x = lng , y = lat, size = freq), colour = point_list[i], data = table, alpha=alph) + 
			scale_size_continuous(range = c(1,10), name = 'Frequency') + GP_layer + ggtitle(paste(month_word, " ", year ," Parking Tickets: ", Rep[i], sep=''))
	num.df[month, i] = num
	
	# MAKE A PDF!
	pdf(paste("C:/Users/averma/Desktop/Parking Ticket Plots/DT Map/",year," map DT/", month_word , "_", year,"_Toronto_DT_Code_", Code[i], ".pdf", sep=''), width = 13, height = 13)
	plot(map.heat_5)
	makeFootnote(paste("Total Number of Tickets: " , as.character(num), sep=''), color = "black")
	dev.off()
	
	# MAKE A JPEG! 
	jpeg(filename = paste("C:/Users/averma/Desktop/Parking Ticket Plots/DT Map/",year," map DT/", month_word , "_", year,"_Toronto_DT_Code_", Code[i], ".jpeg", sep=''),
			width = 5000, height = 5000, pointsize = 12,quality = 100, res= 500)
	plot(map.heat_5)
	makeFootnote(paste("Total Number of Tickets: " , as.character(num), sep=''), color = "black")
	dev.off()
	
	# MAKE A NEW TITLE!
	map.heat_5 <- map.heat_5 + labs(title=paste(Rep[i], sep='')) + theme(legend.position = 'none')
	
	# MAP_6
	i = 6
	table = parking.table[[i]]
	num <- length(table[[i]])
	table <- bubble_data(table, get_max_val(year, i, max_val))
	map.heat_6 <- DT_map + stat_density2d(aes(x = lng, y = lat, fill = ..level.., alpha = ..level..),
					size = 0.1, bins = 11, data = table, geom = "polygon", breaks = seq(200,1500,130)) + 
			scale_alpha(range = c(.125, 1), guide = FALSE) +
			scale_fill_gradient(low = "#BFBFBF", high = color_list[i], guide = FALSE) + 
			theme(axis.line=element_blank(),axis.text.x=element_blank(),axis.text.y=element_blank(),axis.ticks=element_blank(),
					axis.title.x=element_blank(),axis.title.y=element_blank(),panel.background=element_blank(),panel.border=element_blank(),
					panel.grid.major=element_blank(),panel.grid.minor=element_blank(),plot.background=element_blank(),legend.position = "right") +
			geom_point(aes(x = lng , y = lat, size = freq), colour = point_list[i], data = table, alpha=alph) + 
			scale_size_continuous(range = c(1,10), name = 'Frequency') + GP_layer + ggtitle(paste(month_word, " ", year ," Parking Tickets: ", Rep[i], sep=''))
	num.df[month, i] = num
	
	# MAKE A PDF!
	pdf(paste("C:/Users/averma/Desktop/Parking Ticket Plots/DT Map/",year," map DT/", month_word , "_", year,"_Toronto_DT_Code_", Code[i], ".pdf", sep=''), width = 13, height = 13)
	plot(map.heat_6)
	makeFootnote(paste("Total Number of Tickets: " , as.character(num), sep=''), color = "black")
	dev.off()
	
	# MAKE A JPEG! 
	jpeg(filename = paste("C:/Users/averma/Desktop/Parking Ticket Plots/DT Map/",year," map DT/", month_word , "_", year,"_Toronto_DT_Code_", Code[i], ".jpeg", sep=''),
			width = 5000, height = 5000, pointsize = 12,quality = 100, res= 500)
	plot(map.heat_6)
	makeFootnote(paste("Total Number of Tickets: " , as.character(num), sep=''), color = "black")
	dev.off()
	
	# MAKE A NEW TITLE!
	map.heat_6 <- map.heat_6 + labs(title=paste(Rep[i], sep='')) + theme(legend.position = 'none')
	
	# Plot the bar chart for this month
	month_vec = c()
	for (m in 1:12) {
		month_date <- as.character(as.Date(paste(year, '-', m,'-', '01', sep='')))
		month_vec[m] = month_date
	}
	table.df <- num.df
	# change the format of original data frame, in order to plot with ggplot
	data.mat <- matrix(rep('0', 3), nrow = 1)
	for (col in 1:length(table.df[1,])){
		for (row in 1:length(table.df[,1])) {
			data.mat<- rbind(data.mat, c(table.df[row,col], month_vec[row], Rep[col]))
		}
	}
	data.df <- as.data.frame(data.mat[2:nrow(data.mat),], stringsAsFactors = FALSE)
	data.df[[1]] <- as.numeric(data.df[[1]])
	data.df[[2]] <- as.POSIXct(data.df[[2]])
	colnames(data.df) = c('Number_of_Tickets', 'Month', 'Ticket_Type')
	
	# for loop month jan to dec and create bar chart for each month seperately
	text <- c('Fail to Deposit Fee/Display Ticket','Park - Prohibited Time on Permit','Park - Private Property',
			'Park - Signed Except Permit Time','Stop/Stand - Sign Prohibited Times and Days','Other', 'Green P Stations')
	plot_colors <- c('#FF42A8', '#79CDCD', '#FF0000', '#FF7F00', '#FFFF00', '#9366B4', '#009933')
	border_plot_colors <- c('#FF42A8', '#79CDCD', '#FF0000', '#FF7F00', '#FFFF00', '#9366B4', 'white')
	pt.symbol <- c(22,22,22,22,22,22,21)
	data.month <- subset(data.df, data.df$Month == as.POSIXct(paste(year, '-', as.character(month), '-01', sep = '')))
	data.month <- cbind(data.month, Code)
	Month_bar <- qplot(x=Code, y=Number_of_Tickets, data=data.month, geom="bar", stat="identity", position="dodge", fill = Ticket_Type) + 
			labs(title="Ticket Totals") + theme(legend.position='none') + 
			scale_fill_manual(values = color_list_bar, name = "Ticket Type", breaks = text) +
			labs(x = "Ticket Type", y = "Number of Tickets") +
			scale_y_continuous(limits = c(0,90000), breaks = seq(0,90000,10000)) +
			geom_text(aes(label=Number_of_Tickets), vjust=-0.25)
	plot.new()
	# Merge all the maps
	pdf(paste("C:/Users/averma/Desktop/Parking Ticket Plots/Multiplot/",year," Plot/", month_word , "_", year,"_Toronto.pdf", sep=''), width = 26, height = 13, onefile=FALSE)
	plot.new()
	#layout <- matrix(c(1, 3, 5, 7, 2, 4, 6,), nrow = 2, byrow = TRUE)
	multiplot(map.heat_4, map.heat_3, map.heat_5, map.heat_6, map.heat_1, Month_bar, map.heat_2, cols=4, title = paste(month_word, " ", year ," Parking Ticket Plots", sep=''))
	legend('bottomright', title = 'Parking Ticket Legend' ,legend = text, text.width = max(sapply(text, strwidth)),
			horiz = FALSE, box.col = "white", yjust = 50, bty = 'o', bg = '#E5E5E5', col = border_plot_colors, pch = pt.symbol, pt.bg = plot_colors, pt.cex = 2.25, pt.lwd = 2.25, cex = 1.15)
	par(xpd=FALSE)
	#mtext(paste(month_word, " ", year ," Parking Ticket Plots", sep=''), outer=TRUE,  cex=1.5, line=-1.5)
	dev.off()
	
	jpeg(filename = paste("C:/Users/averma/Desktop/Parking Ticket Plots/Multiplot/",year," Plot/", month_word , "_", year,"_Toronto.jpeg", sep=''),
			width = 12000, height = 6000, pointsize = 12,quality = 100, res= 500)
	plot.new()
	#layout <- matrix(c(1, 3, 5, 7, 2, 4, 6,), nrow = 2, byrow = TRUE)
	multiplot(map.heat_4, map.heat_3, map.heat_5, map.heat_6, map.heat_1, Month_bar, map.heat_2, cols=4, title = paste(month_word, " ", year ," Parking Ticket Plots", sep=''))
	legend('bottomright', title = 'Parking Ticket Legend' ,legend = text, text.width = max(sapply(text, strwidth)),
			horiz = FALSE, box.col = "white", yjust = 10, bty = 'o', bg = '#E5E5E5', col = border_plot_colors, pch = pt.symbol, pt.bg = plot_colors, pt.cex = 2.25, pt.lwd = 2.25, cex = 1.15)
	par(xpd=FALSE)
	dev.off()
	
	end <- Sys.time()
	print(end - start)
}
