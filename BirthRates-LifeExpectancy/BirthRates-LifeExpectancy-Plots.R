library(ggplot2)
library(grid)

# Import Data
df.life <- read.csv("sp.dyn.le00.in_Indicator_en_csv_v2.csv")[,c("Country.Code", "X2010", "X2013", "X2014", "X2015")]
colnames(df.life) <- c("Code", "LifeExp2010", "LifeExp2013", "LifeExp2014", "LifeExp2015")
df.birth <- read.csv("sp.dyn.cbrt.in_Indicator_en_csv_v2.csv")[,c("Country.Code", "X2010", "X2013", "X2014", "X2015")]
colnames(df.birth) <- c("Code", "Birthrate2010", "Birthrate2013", "Birthrate2014", "Birthrate2015")
df.gdp <- read.csv("ny.gdp.pcap.pp.kd_Indicator_en_csv_v2.csv")[,c("Country.Code", "X2010", "X2013", "X2014", "X2015")]
colnames(df.gdp) <- c("Code", "GDP2010.PC", "GDP2013.PC", "GDP2014.PC", "GDP2015.PC")
df.meta <- read.csv("Metadata_Country_ny.gdp.pcap.pp.kd_Indicator_en_csv_v2.csv")[,1:4]
colnames(df.meta) <- c("Country", "Code", "Region", "IncomeGroup")

df.complete <- merge(df.life, df.birth, by=c("Code"))
df.complete <- merge(df.complete, df.gdp, by=c("Code"))
df.complete <- merge(df.complete, df.meta, by=c("Code"))

df.complete[df.complete$Region == "","Region"] <- NA

# Select only complete cases for 2013 and only countries
df.complete <- df.complete[complete.cases(df.complete[,c("LifeExp2013", "Birthrate2013", "GDP2013.PC", "Region")]),]

# Using crude birth rates:
r <- cor(df.complete$Birthrate2013, df.complete$LifeExp2013)
ggplot(data=df.complete, aes(x=Birthrate2013, y=LifeExp2013)) +
  stat_smooth(method="lm", se=FALSE) +
  geom_point(aes(color=Region)) + 
  geom_text(aes(label=Code, color=Region), hjust=-0.2, vjust=0, alpha=.2) +
  scale_x_continuous("Crude Birth Rate (2013)") +
  scale_y_continuous("Life Expectancy at Birth (2013)") +
  annotate("text", x=15, y=50, label=paste("r = ", round(r, 2)), show_guide=F, size=4)
grid.text(label="Source: World Bank (2015), http://data.worldbank.org",
          x = unit(1, "npc") - unit(2, "mm"),
          y = unit(2, "mm"),
          just=c("right", "bottom"),
          gp=gpar(cex=.7, col=grey(.5)))

r <- cor(df.complete$Birthrate2013, df.complete$GDP2013.PC)
ggplot(data=df.complete, aes(x=Birthrate2013, y=GDP2013.PC)) + 
  stat_smooth(method="lm", se=FALSE) +
  geom_point(aes(color=Region)) + 
  geom_text(aes(label=Code, color=Region), hjust=-0.2, vjust=0, alpha=.2) +
  scale_x_continuous("Crude Birth Rate (2013)") +
  scale_y_log10("Gross Domestic Product per Capita PPP (2013, Log-scaled, US Dollar)") +
  annotate("text", x=15, y=1000, label=paste("r = ", round(r, 2)), show_guide=F, size=4)
grid.text(label="Source: World Bank (2015), http://data.worldbank.org",
          x = unit(1, "npc") - unit(2, "mm"),
          y = unit(2, "mm"),
          just=c("right", "bottom"),
          gp=gpar(cex=.7, col=grey(.5)))

r <- cor(df.complete$LifeExp2013, df.complete$GDP2013.PC)
ggplot(data=df.complete, aes(x=LifeExp2013, y=GDP2013.PC)) +
  stat_smooth(method="lm", se=FALSE) +
  geom_point(aes(color=Region)) +
  geom_text(aes(label=Code, color=Region), hjust=-0.2, vjust=0, alpha=.2) +
  scale_x_continuous("Life Expectancy at Birth (2013)") +
  scale_y_log10("Gross Domestic Product per Capita PPP (2013, Log-scaled, US Dollar)") +
  annotate("text", x=80, y=1000, label=paste("r = ", round(r, 2)), show_guide=F, size=4)
grid.text(label="Source: World Bank (2015), http://data.worldbank.org",
          x = unit(1, "npc") - unit(2, "mm"),
          y = unit(2, "mm"),
          just=c("right", "bottom"),
          gp=gpar(cex=.7, col=grey(.5)))

# Calculate residuals for Birthrate ~ GDP and Life Expectancy ~ GDP
m <- lm(Birthrate2013 ~ GDP2013.PC, data=df.complete)
res.birth <- residuals(m)
m <- lm(LifeExp2013 ~ GDP2013.PC, data=df.complete)
res.life <- residuals(m)
res.data <- data.frame(res.birth, res.life, df.complete$Region, df.complete$Code, df.complete$Country)
colnames(res.data) <- c("res.birth", "res.life", "Region", "Code", "Country")

r <- cor(res.data$res.birth, res.data$res.life)
ggplot(data=res.data, aes(x=res.birth, y=res.life)) +
  stat_smooth(method="lm", se=FALSE) +
  geom_point(aes(color=Region)) +
  geom_text(aes(label=Country, color=Region), hjust=-0.2, vjust=0, alpha=.2) +
  scale_x_continuous("Crude Birth Rate (2013, corrected for GDP per Capita)") +
  scale_y_continuous("Life Expectancy at Birth (2013, corrected for GDP per Capita)") +
  annotate("text", label=paste("r_partial = ", round(r, 2)), x=-10, y=-20, show_guide=F, size=4)
grid.text(label="Source: World Bank (2015), http://data.worldbank.org",
          x = unit(1, "npc") - unit(2, "mm"),
          y = unit(2, "mm"),
          just=c("right", "bottom"),
          gp=gpar(cex=.7, col=grey(.5)))