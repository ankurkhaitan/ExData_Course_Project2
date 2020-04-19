<h3><a name = "Project2">Project 2 - Analysis of fine particulate matter (PM2.5)</a></h3>

Fine particulate matter (PM2.5) is an ambient air pollutant for which there is strong evidence that it is harmful to human health. In the United States, 
the Environmental Protection Agency (EPA) is tasked with setting national ambient air quality standards for fine PM and for tracking the emissions of 
this pollutant into the atmosphere. Approximatly every 3 years, the EPA releases its database on emissions of PM2.5. This database is known as the 
National Emissions Inventory (NEI). You can read more information about the NEI at the [EPA National Emissions Inventory](http://www.epa.gov/ttn/chief/eiinformation.html) web site.

The data for this assignment are available from the course web site as a single zip file:<br>
<ul>
  <li><b>Dataset:</b> [PM Emission Data](https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip) [29Mb]</li><br>
  <li><b>Description</b></li>
  PM2.5 Emissions Data <b>(summarySCC_PM25.rds)</b>: This file contains a data frame with all of the PM2.5 emissions data for 1999, 2002, 2005, and 2008. 
  For each year, the table contains number of tons of PM2.5 emitted from a specific type of source for the entire year. Here are the first few rows.<br>

  Source Classification Code Table <b>(Source_Classification_Code.rds)</b>: This table provides a mapping from the SCC digit strings in the Emissions table to 
  the actual name of the PM2.5 source. The sources are categorized in a few different ways from more general to more specific and you may choose to explore whatever 
  categories you think are most useful. For example, source “10100101” is known as “Ext Comb /Electric Gen /Anthracite Coal /Pulverized Coal”.

</ul>

Parameter description summarySCC_pm25:
<ul>
<li><b>fips:</b> A five-digit number (represented as a string) indicating the U.S. county
<li><b>SCC:</b> The name of the source as indicated by a digit string (see source code classification table)
<li><b>Pollutant:</b> A string indicating the pollutant
<li><b>Emissions:</b> Amount of PM2.5 emitted, in tons
<li><b>type:</b> The type of source (point, non-point, on-road, or non-road)
<li><b>year:</b> The year of emissions recorded
</ul>

The following questions will be answered
<ol>
<li>Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? Using the base plotting system, make a plot showing the total PM2.5 emission from all sources for each of the years 1999, 2002, 2005, and 2008.</li>
<li>Have total emissions from PM2.5 decreased in the Baltimore City, Maryland (fips == "24510") from 1999 to 2008? Use the base plotting system to make a plot answering this question.</li>
<li>Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable, which of these four sources have seen decreases in emissions from 1999–2008 for Baltimore City? Which have seen increases in emissions from 1999–2008? Use the ggplot2 plotting system to make a plot answer this question.</li>
<li>Across the United States, how have emissions from coal combustion-related sources changed from 1999–2008?</li>
<li>How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City?</li>
<li>Compare emissions from motor vehicle sources in Baltimore City with emissions from motor vehicle sources in Los Angeles County, California (fips == "06037"). Which city has seen greater changes over time in motor vehicle emissions?</li>
</ol>

##################ALL EXPLORATORY DATA ANALYSIS PLOTS####################


<h3>Plot1</h3>
Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? Using the base plotting system, make a plot showing the total PM2.5 emission from 
all sources for each of the years 1999, 2002, 2005, and 2008.<br>


<h3>Plot2</h3>
Have total emissions from PM2.5 decreased in the Baltimore City, Maryland (fips == "24510") from 1999 to 2008? Use the base plotting system to make a plot 


<h3>Plot3</h3>
Have total emissions from PM2.5 decreased in the Baltimore City, Maryland (fips == "24510") from 1999 to 2008? Use the base plotting system to make a plot 


<h3>Plot4</h3>
Have total emissions from PM2.5 decreased in the Baltimore City, Maryland (fips == "24510") from 1999 to 2008? Use the base plotting system to make a plot 


<h3>Plot5</h3>
Have total emissions from PM2.5 decreased in the Baltimore City, Maryland (fips == "24510") from 1999 to 2008? Use the base plotting system to make a plot 


<h3>Plot6</h3>
Have total emissions from PM2.5 decreased in the Baltimore City, Maryland (fips == "24510") from 1999 to 2008? Use the base plotting system to make a plot 



