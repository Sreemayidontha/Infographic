


#############################
df <- read.csv("C:/Users/Sreemayi Dontha/Downloads/archive (3)/Significant_Earthquakes.csv")
# Filter out rows with missing values in depth, longitude, and latitude columns
df <- df[complete.cases(df$depth) & complete.cases(df$longitude) & complete.cases(df$latitude), ]

# Convert "time" column to POSIXct format and extract year
df$time <- as.POSIXct(df$time, format = "%Y-%m-%dT%H:%M:%OSZ")
df$year <- as.numeric(format(df$time, "%Y"))
high_loss=df[df$year ==2023, ]
high_loss=high_loss[complete.cases(high_loss$year), ]
#View(high_loss)
library(leaflet)

# Assuming your dataframe is named df and contains earthquake data for the year 2023
# Adjust the column names as per your dataset


# Define color palette for magnitude
color_palette <- colorNumeric(palette = "YlOrRd", domain = high_loss$mag)

# Create leaflet map
leaflet(high_loss) %>%
  addTiles() %>%
  addCircleMarkers(
    ~longitude, ~latitude,
    radius = ~mag,  # Use magnitude directly as radius
    color = ~color_palette(mag),
    fillOpacity = 0.3,
    stroke = FALSE,
    label = ~as.character(mag)
  ) %>%
  addLegend(
    pal = color_palette,
    values = ~mag,
    title = "Magnitude",
    position = "bottomright"
  )

##############

# Load required libraries
library(dplyr)
library(ggplot2)

# Convert timestamp to POSIXct format
high_loss$time <- as.POSIXct(high_loss$time, format = "%Y-%m-%d %H:%M:%S")

# Extract year and month from timestamp
high_loss$year_month <- as.Date(high_loss$time)



# Group data by year-month and count number of earthquakes in each month
earthquake_freq <- high_loss %>%
  group_by(year_month) %>%
  summarise(num_earthquakes = n())

# Convert year_month to Date format with specific format
earthquake_freq$year_month <- as.Date(earthquake_freq$year_month, format = "%Y-%m")

# Create time series plot
ggplot(earthquake_freq, aes(x = year_month, y = num_earthquakes)) +
  geom_line() +
  geom_point() +
  geom_line(color = "skyblue", size = 1.2, linetype = "dashed") +  # Customize line appearance
  geom_point(color = "brown", size = 1, shape = 16) +
  labs(x = "Year-Month", y = "Number of Earthquakes", title = "Earthquake Frequency Over Time") +
  theme_minimal()

##############################

countries <- c("Afghanistan", "Albania", "Algeria", "Andorra", "Angola", "Antigua and Barbuda", "Argentina", "Armenia", 
               "Australia", "Austria", "Azerbaijan", "Bahamas", "Bahrain", "Bangladesh", "Barbados", "Belarus", 
               "Belgium", "Belize", "Benin", "Bhutan", "Bolivia", "Bosnia and Herzegovina", "Botswana", "Brazil", 
               "Brunei", "Bulgaria", "Burkina Faso", "Burundi", "Cabo Verde", "Cambodia", "Cameroon", "Canada", 
               "Central African Republic", "Chad", "Chile", "China", "Colombia", "Comoros", "Congo", "Costa Rica", 
               "Croatia", "Cuba", "Cyprus", "Czechia", "Denmark", "Djibouti", "Dominica", "Dominican Republic", 
               "Ecuador", "Egypt", "El Salvador", "Equatorial Guinea", "Eritrea", "Estonia", "Eswatini", "Ethiopia", 
               "Fiji", "Finland", "France", "Gabon", "Gambia", "Georgia", "Germany", "Ghana", "Greece", "Grenada", 
               "Guatemala", "Guinea", "Guinea-Bissau", "Guyana", "Haiti", "Honduras", "Hungary", "Iceland", "India", 
               "Indonesia", "Iran", "Iraq", "Ireland", "Israel", "Italy", "Jamaica", "Japan", "Jordan", "Kazakhstan", 
               "Kenya", "Kiribati", "Kosovo", "Kuwait", "Kyrgyzstan", "Laos", "Latvia", "Lebanon", "Lesotho", "Liberia", 
               "Libya", "Liechtenstein", "Lithuania", "Luxembourg", "Madagascar", "Malawi", "Malaysia", "Maldives", 
               "Mali", "Malta", "Marshall Islands", "Mauritania", "Mauritius", "Mexico", "Micronesia", "Moldova", 
               "Monaco", "Mongolia", "Montenegro", "Morocco", "Mozambique", "Myanmar", "Namibia", "Nauru", "Nepal", 
               "Netherlands", "New Zealand", "Nicaragua", "Niger", "Nigeria", "North Korea", "North Macedonia", "Norway", 
               "Oman", "Pakistan", "Palau", "Palestine", "Panama", "Papua New Guinea", "Paraguay", "Peru", "Philippines", 
               "Poland", "Portugal", "Qatar", "Romania", "Russia", "Rwanda", "Saint Kitts and Nevis", "Saint Lucia", 
               "Saint Vincent and the Grenadines", "Samoa", "San Marino", "Sao Tome and Principe", "Saudi Arabia", 
               "Senegal", "Serbia", "Seychelles", "Sierra Leone", "Singapore", "Slovakia", "Slovenia", "Solomon Islands", 
               "Somalia", "South Africa", "South Korea", "South Sudan", "Spain", "Sri Lanka", "Sudan", "Suriname", 
               "Sweden", "Switzerland", "Syria", "Taiwan", "Tajikistan", "Tanzania", "Thailand", "Timor-Leste", "Togo", 
               "Tonga", "Trinidad and Tobago", "Tunisia", "Turkey", "Turkmenistan", "Tuvalu", "Uganda", "Ukraine", 
               "United Arab Emirates", "United Kingdom", "United States", "Uruguay", "Uzbekistan", "Vanuatu", "Vatican City", 
               "Venezuela", "Vietnam", "Yemen", "Zambia", "Zimbabwe","Pacific","Mid-Atlantic","Pacific-Antarctic")

# Print the first few countries


# Initialize vectors to store matched and unmatched places
matched <- vector("list", length = nrow(high_loss))
unmatched <- vector("list", length = nrow(high_loss))

# Loop through each row in the dataframe
for (i in seq_len(nrow(high_loss))) {
  # Split the words in the "place" column
  place_words <- strsplit(high_loss$place[i], "\\s+")[[1]]
  
  # Initialize vectors to store matched and unmatched countries
  matched_countries <- character(0)
  unmatched_place <- character(0)
  
  # Loop through each word in the "place" column
  for (word in place_words) {
    # Check if the word matches any country name
    if (word %in% countries) {
      matched_countries <- c(matched_countries, word)
    } else {
      unmatched_place <- c(unmatched_place, word)
    }
  }
  
  # Combine matched and unmatched countries into strings
  matched[i] <- toString(matched_countries)
  unmatched[i] <- toString(unmatched_place)
}

# Create new columns in the dataframe for matched and unmatched places


# Print the updated dataframe
print(matched)
print(unmatched)
#############################
# Replace empty values with "Other"
matched <- lapply(matched, function(x) ifelse(x == "", "Other", x))
matched <- matched[matched != "Other"]
matched <- unlist(matched)
word_freq <- table(matched)

# Convert the frequencies to a data frame
word_freq_df <- as.data.frame(word_freq)
colnames(word_freq_df) <- c("Word", "Frequency")

# Create the treemap
treemap(word_freq_df, index = "Word", vSize = "Frequency", 
        title = "Frequency of Countries", 
        fontsize.title = 16, fontsize.labels = 9,margin = c(40, 10, 40, 10))


###########################################################


library(treemap)

# Define Pacific Ring of Fire countries
pacific_ring <- c("Chile", "Peru", "Ecuador", "Colombia", "Panama", "Costa Rica", 
                  "Nicaragua", "El Salvador", "Guatemala", "Mexico", "United States",
                  "Canada", "Russia", "Japan", "Philippines", "Indonesia", 
                  "Papua New Guinea", "New Zealand", "Australia", "Tonga", "Fiji", 
                  "Solomon Islands", "Vanuatu", "Taiwan")

# Create a new column indicating whether the country is in the Pacific Ring of Fire
matched_filtered$in_pacific_ring <- matched_filtered$Word %in% pacific_ring

# Define colors for tiles based on whether the country is in the Pacific Ring of Fire
colors <- ifelse(matched_filtered$in_pacific_ring, "#EEA18C", "#3498DB") 

# Create treemap with color-coded tiles
treemap(matched_filtered, index = "Word", vSize = "Frequency", 
        title = "Frequency of Earthquakes by Country",
        palette = colors, border.col = "black")



##############################################3

# Create circular barplot
par(mar = c(1, 1, 1, 1))  # Set margins to make space for the circular plot
circos.par(cell.padding = c(0, 0, 0, 0))  # Set cell padding to 0

# Initialize circular plot
circos.initialize(factors = word_freq_df$Word, x = 1, xlim = c(0, max(word_freq_df$Frequency)))

# Generate positions for bars
pos <- 1:nrow(word_freq_df)

# Create circular barplot
circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
  circos.axis(h = "top", labels.cex = 0.6)
  circos.barplot(height = word_freq_df$Frequency, col = "skyblue", border = "white", 
                 y = word_freq_df$Frequency, stack = TRUE)
}, bg.border = NA)
#####################################3

# Uniform color
barplot(height=word_freq_df$Frequency, names=word_freq_df$Word, 
        col=rgb(0.8,0.1,0.1,0.6),
        xlab="categories", 
        ylab="values", 
        main="My title", 
        
)

###################################



# Filter frequencies less than 10
word_freq_df_filtered <- word_freq_df[word_freq_df$Frequency >= 15, ]
barplot(word_freq_df_filtered$Frequency,word_freq_df_filtered$Word, 
        col=rgb(0.8,0.1,0.1,0.6),
        xlab="categories", 
        ylab="values", 
        main="My title", 
        
)




############################################

# Filter high_loss table for places in the Pacific Ring of Fire
pacific_ring_data <- high_loss[high_loss$place %in% pacific_ring, ]

# Convert year_date to Date format if it's not already
pacific_ring_data$year_month <- as.Date(pacific_ring_data$year_month)

# Group data by year_date and count number of earthquakes in each month
earthquake_freq_pacific <- pacific_ring_data %>%
  group_by(year_month) %>%
  summarise(num_earthquakes = n())

# Create a sequence of months for the x-axis breaks
months_sequence <- seq(min(earthquake_freq_pacific$year_date), 
                       max(earthquake_freq_pacific$year_date), by = "month")

# Create time series plot
ggplot(earthquake_freq_pacific, aes(x = year_date, y = num_earthquakes)) +
  geom_line() +
  geom_point() +
  scale_x_date(breaks = months_sequence, date_labels = "%b %Y") +  # Set x-axis breaks to months
  labs(x = "Date", y = "Number of Earthquakes", 
       title = "Earthquake Frequency in Pacific Ring of Fire (2023)") +
  theme_minimal()




# Convert year_month to Date format with specific format
earthquake_freq$year_month <- as.Date(earthquake_freq$year_month, format = "%Y-%m")

# Create time series plot
ggplot(earthquake_freq, aes(x = year_month, y = num_earthquakes)) +
  geom_line() +
  geom_point() +
  geom_line(color = "skyblue", size = 1.2, linetype = "dashed") +  # Customize line appearance
  geom_point(color = "brown", size = 1, shape = 16) +
  labs(x = "Year-Month", y = "Number of Earthquakes", title = "Earthquake Frequency Over Time") +
  theme_minimal()