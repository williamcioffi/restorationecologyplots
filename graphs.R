# restoration ecology graphs
# 26august2017
# edited 10january2018

# libraries
library(ggplot2)
library(grid)
library(gridExtra)
library(reshape2)

#raw data
a <- read.table("keptmarked.csv", header = TRUE, sep = ',', stringsAsFactors = FALSE)

################
### figure 1 ###
################

stoopidtheme <- theme_bw() + 
	theme(
		panel.grid.major 	= element_blank(),
		panel.grid.minor 	= element_blank(), 
		panel.border 		= element_blank(), 
		axis.line 			= element_line(colour = "black"), 
		text 				= element_text(size = 12),
		axis.text 			= element_text(size = 12),
		axis.title 			= element_text(size = 12)
	)
	
ylims <- ylim(0, 1)

letterlabeltextsize <- 8
barwidth <- 0.75

labels <- geom_text(aes(label = values), position = position_dodge(width = 0.9), vjust = -0.25)


#habitat

habitat <- a[, 22:24]
habitat.sum <- stack(apply(habitat, 2, sum))
habdat <- data.frame(values = habitat.sum[, 1], ind = as.character(habitat.sum[, 2]), stringsAsFactors = FALSE)
habdat[1, 2] <- "Salt Marsh"
ntot <- nrow(habitat)
habdat$perc <- habdat$values / ntot

p1 <- ggplot(data = habdat, aes(x = ind, y = perc, width = barwidth), size = 0) 
p1 <- p1 + geom_bar(colour = "black", stat = "identity") 
p1 <- p1 + stoopidtheme + ylims
p1 <- p1 + ylab("Proportion of Studies") + xlab("")
p1 <- p1 + labels + annotate("text", x = 0.66, y = 1, label = "a", size = letterlabeltextsize)
p1


#affiliation

affil <- a[, 8:10]
names(affil) <- c("University", "Government", "NGO")
affil.sum <- stack(apply(affil, 2, sum))
afdat <- data.frame(values = affil.sum[, 1], ind = affil.sum[, 2])
ntot <- nrow(affil)
afdat$perc <- afdat$values / ntot
afdat$ind <- factor(afdat$ind, levels = c("Government", "NGO", "University"))

p2 <- ggplot(data = afdat, aes(x = ind, y = perc, width = barwidth)) 
p2 <- p2 + geom_bar(colour = "black", stat = "identity") 
p2 <- p2 + stoopidtheme
p2 <- p2 + xlab("") + ylab("") #+ ylab("Proportion of Studies") #remove ylabel
p2 <- p2 + ylim(0, 1)
p2 <- p2 + labels + annotate("text", x = 0.66, y = 1, label = "b", size = letterlabeltextsize)
p2

#study type

type <- a[, 19:21]
type.sum <- stack(apply(type, 2, sum))
tydat <- data.frame(values = type.sum[, 1], ind = type.sum[, 2])
ntot <- nrow(type)
tydat$perc <- tydat$values / ntot
tydat$ind <- factor(tydat$ind, levels = c("Experimental", "Observational", "Review"))

p3 <- ggplot(data = tydat, aes(x = ind, y = perc, width = barwidth)) 
p3 <- p3 + geom_bar(colour = "black", stat = "identity") 
p3 <- p3 + stoopidtheme  + ylims
p3 <- p3 + ylab("Proportion of Studies") + xlab("")
p3 <- p3 + labels + annotate("text", x = 0.66, y = 1, label = "c", size = letterlabeltextsize)
p3

#restoration phase
phase <- a[, 15:18]
phase.sum <- stack(apply(phase, 2, sum))
phdat <- data.frame(values = phase.sum[, 1], ind = as.character(phase.sum[, 2]), stringsAsFactors = FALSE)
phdat[1, 2] <- "Site Selection"
ntot <- nrow(phase)
phdat$perc <- phdat$value / ntot

p4 <- ggplot(data = phdat, aes(x = ind, y = perc, width = barwidth)) 
p4 <- p4 + geom_bar(colour = "black", stat = "identity") 
p4 <- p4 + stoopidtheme + ylims
p4 <- p4 + xlab("") + ylab("") #+ ylab("Proportion of Studies") #remove ylabel
p4 <- p4 + labels + annotate("text", x = 0.66, y = 1, label = "d", size = letterlabeltextsize)
p4

#plot these loosers
grid.arrange(p1, p2, p3, p4, nrow = 2, ncol = 2)

################
### figure 2 ###
################

# physical, biological, or both
# bar graph
# by number 
# factors considered (not factors recommended)
# ignores other factors

physical <- a[, 28:45]
biological <- a[, 46:54]

#don't include NS?
physical[which(physical == "NS", arr.ind = TRUE)] <- 0
physical[which(physical == "S" , arr.ind = TRUE)] <- 1
physical <- apply(physical, 2, as.numeric)
phy <- apply(physical, 1, sum)
phy[which(phy > 0)] <- 1

biological[which(biological == "NS", arr.ind = TRUE)] <- 1
biological[which(biological == "S" , arr.ind = TRUE)] <- 1
biological <- apply(biological, 2, as.numeric)
bio <- apply(biological, 1, sum)
bio[which(bio > 0)] <- 1

biophy <- bio + phy
phyonly <- phy*0
bioonly <- bio*0

phyonly[which(phy == 1 & biophy == 1)] <- 1
bioonly[which(bio == 1 & biophy == 1)] <- 1
biophy [which(biophy < 2)] <- 0
biophy[which(biophy == 2)] <- 1

biophydf <- data.frame(phyonly, bioonly, biophy)
biophy.sum <- stack(apply(biophydf, 2, sum))
bpdat <- data.frame(values = biophy.sum[, 1], ind = as.character(biophy.sum[, 2]), stringsAsFactors = FALSE)
bpdat[1, 2] <- "abiotic"
bpdat[2, 2] <- "biotic"
bpdat[3, 2] <- "both"

ntot <- nrow(biophydf)
bpdat$perc <- bpdat$values / ntot

p5 <- ggplot(data = bpdat, aes(x = ind, y = perc, width = barwidth)) 
p5 <- p5 + geom_bar(colour = "black", stat = "identity") 
p5 <- p5 + stoopidtheme + ylims
p5 <- p5 + ylab("Proportion of Studies") + xlab("")
p5 <- p5 + labels + annotate("text", x = 0.66, y = 1, label = "a", size = letterlabeltextsize)
p5

################
### figure 3 ###
################

# overall top 10 factors
# includes other factors

hablabs <- 1:nrow(a)*NA
hablabs[which(a$Salt.Marsh == 1 & a$Oyster == 0 & a$Seagrass == 0)] <- "Salt Marsh"
hablabs[which(a$Salt.Marsh == 0 & a$Oyster == 1 & a$Seagrass == 0)] <- "Oyster"
hablabs[which(a$Salt.Marsh == 0 & a$Oyster == 0 & a$Seagrass == 1)] <- "Seagrass"
hablabs[which(a$Salt.Marsh == 1 & a$Oyster == 1 & a$Seagrass == 0)] <- "Salt Marsh/Oyster"
hablabs[which(a$Salt.Marsh == 1 & a$Oyster == 1 & a$Seagrass == 1)] <- "All"
hablabs[which(a$Salt.Marsh == 1 & a$Oyster == 0 & a$Seagrass == 1)] <- "Seagrass/Salt Marsh"
hablabs[which(a$Salt.Marsh == 0 & a$Oyster == 1 & a$Seagrass == 1)] <- "Oyster/Seagrass"



factors <- a[, 28:62]
factors[which(factors == "NS" | factors == "S", arr.ind = TRUE)] <- 1
factors <- apply(factors, 2, as.numeric)
factors.sum <- stack(apply(factors, 2, sum))
facdat <- data.frame(values = factors.sum[, 1], ind = as.character(factors.sum[, 2]), stringsAsFactors = FALSE)

oo <- order(facdat$values, decreasing = TRUE)
facdat <- facdat[oo, ]
facdat_10 <- facdat[1:10, ]

byhabitats <- by(factors, hablabs, apply, 2, sum)
facdat_10_byhabitat <- sapply(byhabitats, function(l) l[which(names(l) %in% facdat_10[, 2])])

oo <- order(apply(facdat_10_byhabitat, 1, sum), decreasing = TRUE)
facdat_10_byhabitat <- facdat_10_byhabitat[oo, ]

facdat_10[, 2] <- c(
					"Depth/Tidal Elevaton",
					"Tidal Flow",
					"Human Interaction",
					"Temperature",
					"Salinity",
					"Nutrients",
					"Landscape Context",
					"Recruitment",
					"Sediment Type",
					"Grazing/Predation"
				  )

rownames(facdat_10_byhabitat) <- c(
					"Depth/Tidal Elevaton",
					"Tidal Flow",
					"Human Interaction",
					"Temperature",
					"Salinity",
					"Nutrients",
					"Landscape Context",
					"Recruitment",
					"Sediment Type",
					"Grazing/Predation"
				  )
				  
facdat_10_melted <- melt(facdat_10_byhabitat)
names(facdat_10_melted) <- c("ind", "habitat", "values")

order_of_habitats <- 1:nrow(facdat_10_melted)*NA
order_of_habitats[which(facdat_10_melted$habitat == "Salt Marsh")] 			<- 7
order_of_habitats[which(facdat_10_melted$habitat == "Oyster")] 				<- 6
order_of_habitats[which(facdat_10_melted$habitat == "Seagrass")] 			<- 5
order_of_habitats[which(facdat_10_melted$habitat == "Salt Marsh/Oyster")] 	<- 4
order_of_habitats[which(facdat_10_melted$habitat == "Seagrass/Salt Marsh")] <- 3
order_of_habitats[which(facdat_10_melted$habitat == "Oyster/Seagrass")] 	<- 2
order_of_habitats[which(facdat_10_melted$habitat == "All")] 				<- 1
facdat_10_melted[, 'order_of_habitats'] <- order_of_habitats

facdat_10_melted$perc <- facdat_10_melted$values / ntot

p6 <- ggplot(data = facdat_10_melted, aes(x = reorder(ind, -perc), y = perc, fill = reorder(habitat, order_of_habitats), width = barwidth)) 
p6 <- p6 + geom_bar(colour = "black", stat = "identity") 
p6 <- p6 + stoopidtheme #+ ylims
p6 <- p6 + scale_fill_grey(start = 0, end = 0.95)
p6 <- p6 + ylab("Proportion of Studies") + xlab("")
p6 <- p6 + theme(axis.text.x = element_text(angle = 45, hjust = 1))
p6 <- p6 + theme(plot.margin = margin(5.5, 5.5, 5.5, 20, "points"))
p6 <- p6 + theme(legend.title=element_blank())
p6 <- p6 + annotate("text", x = 1:nrow(facdat_10), y = (facdat_10$values / ntot) + 3/ntot, label = as.character(facdat_10$values))
p6

################
### figure 4 ###
################

# cumulative studies over time
# line graph
# a) oyster / salt / seagrass
# b) grazers / predators
# c) competition / facilitation

# oyster / salt / seagrass

oyst.year <- sort(a$Year[which(a$Oyster 		== 1)])
salt.year <- sort(a$Year[which(a$Salt.Marsh 	== 1)])
gras.year <- sort(a$Year[which(a$Seagrass		== 1)])

oyst.year.tab <- table(oyst.year)
oyst.cumval <- 1:length(oyst.year.tab)*NA

oyst.cumval[1] <- oyst.year.tab[1]
for(i in 2:length(oyst.cumval)) {
	oyst.cumval[i] <- oyst.cumval[i - 1] + oyst.year.tab[i]
}

salt.year.tab <- table(salt.year)
salt.cumval <- 1:length(salt.year.tab)*NA

salt.cumval[1] <- salt.year.tab[1]
for(i in 2:length(salt.cumval)) {
	salt.cumval[i] <- salt.cumval[i - 1] + salt.year.tab[i]
}

gras.year.tab <- table(gras.year)
gras.cumval <- 1:length(gras.year.tab)*NA

gras.cumval[1] <- gras.year.tab[1]
for(i in 2:length(gras.cumval)) {
	gras.cumval[i] <- gras.cumval[i - 1] + gras.year.tab[i]
}

oyst.df <- data.frame(Habitat = "Oyster", count = oyst.cumval, year = unique(oyst.year))
salt.df <- data.frame(Habitat = "Salt Marsh", count = salt.cumval, year = unique(salt.year))
gras.df <- data.frame(Habitat = "Seagrass", count = gras.cumval, year = unique(gras.year))

habyeardat <- rbind(oyst.df, salt.df, gras.df)

allyears <- rep(seq(min(a$Year), max(a$Year)), 3)
allyearsdf <- data.frame(year = allyears, Habitat = c(rep("Oyster", length(allyears)/3), rep("Salt Marsh", length(allyears)/3), rep("Seagrass", length(allyears)/3)))
habyeardat2 <- merge(allyearsdf, habyeardat, all.x = TRUE)
habyeardat2$count[c(1, 3)] <- 0

uhab <- unique(habyeardat2$Habitat)
for(h in 1:length(uhab)) {
	dese <- which(habyeardat2$Habitat == uhab[h])
	for(i in 2:length(dese)) {
		if(is.na(habyeardat2 $count[dese[i]])) {
			habyeardat2$count[dese[i]] <- habyeardat2$count[dese[i - 1]]
		}
	}
}

habyeardat2$count[which(habyeardat2$count == 0)] <- NA

p7 <- ggplot(data = habyeardat2, aes(x = year, y = count, group = Habitat)) 
p7 <- p7 + geom_line(aes(linetype = Habitat), colour = "black") 
p7 <- p7 + stoopidtheme  #+ ylims
p7 <- p7 + ylab("Number of Studies") + xlab("")
p7 <- p7 + scale_x_continuous(breaks = seq(min(floor(a$Year/5)*5), max(floor(a$Year/5)*5), by = 5))
p7 <- p7 + theme(legend.title=element_blank())
p7 <- p7
p7


#grazing vs. predators

gp <- read.table("grazingpredation.csv", header = TRUE, sep = ',')

graz.year <- sort(gp$Year[which(gp$Grazing 	 == 1 & gp$Predation == 0)])
pred.year <- sort(gp$Year[which(gp$Predation == 1 & gp$Grazing == 0)])
grpr.year <- sort(gp$Year[which(gp$Predation == 1 & gp$Grazing == 1)])

graz.year.tab <- table(graz.year)
graz.cumval <- 1:length(graz.year.tab)*NA

graz.cumval[1] <- graz.year.tab[1]
for(i in 2:length(graz.cumval)) {
	graz.cumval[i] <- graz.cumval[i - 1] + graz.year.tab[i]
}

pred.year.tab <- table(pred.year)
pred.cumval <- 1:length(pred.year.tab)*NA

pred.cumval[1] <- pred.year.tab[1]
for(i in 2:length(pred.cumval)) {
	pred.cumval[i] <- pred.cumval[i - 1] + pred.year.tab[i]
}

grpr.year.tab <- table(grpr.year)
grpr.cumval <- 1:length(grpr.year.tab)*NA

grpr.cumval[1] <- grpr.year.tab[1]
for(i in 2:length(grpr.cumval)) {
	grpr.cumval[i] <- grpr.cumval[i - 1] + grpr.year.tab[i]
}

graz.df <- data.frame(type = "Grazing", count = graz.cumval, year = unique(graz.year))
pred.df <- data.frame(type = "Predation", count = pred.cumval, year = unique(pred.year))
grpr.df <- data.frame(type = "Both", count = grpr.cumval, year = unique(grpr.year))

gpdat <- rbind(graz.df, pred.df, grpr.df)

allyears <- rep(seq(min(a$Year), max(a$Year)), 3)
allyearsdf <- data.frame(year = allyears, type = c(rep("Grazing", length(allyears)/3), rep("Predation", length(allyears)/3), rep("Both", length(allyears)/3)))
gpdat2 <- merge(allyearsdf, gpdat, all.x = TRUE)
gpdat2$count[1:3] <- 0

utype <- unique(gpdat2$type)
for(t in 1:length(utype)) {
	dese <- which(gpdat2$type == utype[t])
	for(i in 2:length(dese)) {
		if(is.na(gpdat2$count[dese[i]])) {
			gpdat2$count[dese[i]] <- gpdat2$count[dese[i - 1]]
		}
	}
}
# gpdat2$count[which(gpdat2$count == 0)] <- NA

p8 <- ggplot(data = gpdat2, aes(x = year, y = count, group = type)) 
p8 <- p8 + geom_line(aes(linetype = type), colour = "black")
p8 <- p8 + stoopidtheme
p8 <- p8 + ylab("Number of Studies") + xlab("")
p8 <- p8 + scale_x_continuous(breaks = seq(min(floor(a$Year/5)*5), max(floor(a$Year/5)*5), by = 5))
p8 <- p8 + theme(legend.title = element_blank(), legend.position = c(.5, .955), legend.direction = "horizontal")
p8 <- p8 + ylim(0, 55) + annotate("text", x = 1985, y = 55, label = "a", size = letterlabeltextsize)


# competition and facilitation

comp <- a[, 50:51]
faci <- a[, 52:53]

comp[which(comp == "NS", arr.ind = TRUE)] <- 1
comp[which(comp == "S" , arr.ind = TRUE)] <- 1
faci[which(faci == "NS", arr.ind = TRUE)] <- 1
faci[which(faci == "S" , arr.ind = TRUE)] <- 1

comp <- apply(comp, 2, as.numeric)
faci <- apply(faci, 2, as.numeric)

comp <- apply(comp, 1, sum)
faci <- apply(faci, 1, sum)

comp.year <- sort(a$Year[which(comp > 0 & faci == 0)])
faci.year <- sort(a$Year[which(faci > 0 & comp == 0)])
cofa.year <- sort(a$Year[which(faci > 0 & comp > 0)])


comp.year.tab <- table(comp.year)
comp.cumval <- 1:length(comp.year.tab)*NA
comp.cumval[1] <- comp.year.tab[1]
for(i in 2:length(comp.cumval)) {
	comp.cumval[i] <- comp.cumval[i - 1] + comp.year.tab[i]
}

faci.year.tab <- table(faci.year)
faci.cumval <- 1:length(faci.year.tab)*NA
faci.cumval[1] <- faci.year.tab[1]
for(i in 2:length(faci.cumval)) {
	faci.cumval[i] <- faci.cumval[i - 1] + faci.year.tab[i]
}


cofa.year.tab <- table(cofa.year)
cofa.cumval <- 1:length(cofa.year.tab)*NA
cofa.cumval[1] <- cofa.year.tab[1]
for(i in 2:length(cofa.cumval)) {
	cofa.cumval[i] <- cofa.cumval[i - 1] + cofa.year.tab[i]
}


comp.df <- data.frame(type = "Competition", count = comp.cumval, year = unique(comp.year))
faci.df <- data.frame(type = "Facilitation", count = faci.cumval, year = unique(faci.year))
cofa.df <- data.frame(type = "Both", count = cofa.cumval, year = unique(cofa.year))


compfacidat <- rbind(comp.df, faci.df, cofa.df)

allyears <- rep(seq(min(a$Year), max(a$Year)), 3)
allyearsdf <- data.frame(year = allyears, type = c(rep("Competition", length(allyears)/3), rep("Facilitation", length(allyears)/3), rep("Both", length(allyears)/3)))
compfacidat2 <- merge(allyearsdf, compfacidat, all.x = TRUE)
compfacidat2$count[1:3] <- 0

utype <- unique(compfacidat2$type)
for(t in 1:length(utype)) {
	dese <- which(compfacidat2$type == utype[t])
	for(i in 2:length(dese)) {
		if(is.na(compfacidat2$count[dese[i]])) {
			compfacidat2$count[dese[i]] <- compfacidat2$count[dese[i - 1]]
		}
	}
}

p9 <- ggplot(data = compfacidat2, aes(x = year, y = count, group = type)) 
p9 <- p9 + geom_line(aes(linetype = type), colour = "black")
p9 <- p9 + stoopidtheme
p9 <- p9 + ylab("Number of Studies") + xlab("")
p9 <- p9 + scale_x_continuous(breaks = seq(min(floor(a$Year/5)*5), max(floor(a$Year/5)*5), by = 5))
p9 <- p9 + theme(legend.title = element_blank(), legend.position = c(.5, .955), legend.direction = "horizontal")
p9 <- p9 + ylim(0, 55) + annotate("text", x = 1985, y = 55, label = "b", size = letterlabeltextsize)



grid.arrange(p8, p9, nrow = 1, ncol = 2)





### biotic and abiotic over time

# ignores other factors

#biophy
#phyonly
#bioonly


biophy.year  <- sort(a$Year[which(biophy 	== 1)])
phyonly.year <- sort(a$Year[which(phyonly 	== 1)])
bioonly.year <- sort(a$Year[which(bioonly	== 1)])

biophy.year.tab <- table(biophy.year)
biophy.cumval <- 1:length(biophy.year.tab)*NA

biophy.cumval[1] <- biophy.year.tab[1]
for(i in 2:length(biophy.cumval)) {
	biophy.cumval[i] <- biophy.cumval[i - 1] + biophy.year.tab[i]
}

phyonly.year.tab <- table(phyonly.year)
phyonly.cumval <- 1:length(phyonly.year.tab)*NA

phyonly.cumval[1] <- phyonly.year.tab[1]
for(i in 2:length(phyonly.cumval)) {
	phyonly.cumval[i] <- phyonly.cumval[i - 1] + phyonly.year.tab[i]
}

bioonly.year.tab <- table(bioonly.year)
bioonly.cumval <- 1:length(bioonly.year.tab)*NA

bioonly.cumval[1] <- bioonly.year.tab[1]
for(i in 2:length(bioonly.cumval)) {
	bioonly.cumval[i] <- bioonly.cumval[i - 1] + bioonly.year.tab[i]
}

biophy.df <- data.frame(type = "Both", count = biophy.cumval, year = unique(biophy.year))
phyonly.df <- data.frame(type = "Abiotic", count = phyonly.cumval, year = unique(phyonly.year))
bioonly.df <- data.frame(type = "Biotic", count = bioonly.cumval, year = unique(bioonly.year))


biophydat <- rbind(biophy.df, phyonly.df, bioonly.df)

allyears <- rep(seq(min(a$Year), max(a$Year)), 3)
allyearsdf <- data.frame(year = allyears, type = c(rep("Both", length(allyears)/3), rep("Abiotic", length(allyears)/3), rep("Biotic", length(allyears)/3)))
biophydat2 <- merge(allyearsdf, biophydat, all.x = TRUE)
biophydat2$count[c(1, 3)] <- 0

utype <- unique(biophydat2$type)
for(t in 1:length(utype)) {
	dese <- which(biophydat2$type == utype[t])
	for(i in 2:length(dese)) {
		if(is.na(biophydat2$count[dese[i]])) {
			biophydat2$count[dese[i]] <- biophydat2$count[dese[i - 1]]
		}
	}
}



p10 <- ggplot(data = biophydat2, aes(x = year, y = count, group = type)) 
p10 <- p10 + geom_line(aes(linetype = type), colour = "black")
p10 <- p10 + stoopidtheme
p10 <- p10 + ylab("Number of Studies") + xlab("")
p10 <- p10 + scale_x_continuous(breaks = seq(min(floor(a$Year/5)*5), max(floor(a$Year/5)*5), by = 5))
p10 <- p10 + theme(legend.title = element_blank(), legend.position = c(.5, .955), legend.direction = "horizontal")
p10 <- p10 + annotate("text", x = 1985, y = 200, label = "b", size = letterlabeltextsize)
p10

grid.arrange(p5, p10, nrow = 1, ncol = 2)
