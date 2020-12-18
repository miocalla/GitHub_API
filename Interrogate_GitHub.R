#Source: Michael Galarnyk - "Accessing Data from Github API using R"

#install.packages("jsonlite")
library(jsonlite)
#install.packages("httpuv")
library(httpuv)
#install.packages("httr")
library(httr)

# Can be github, linkedin etc depending on application
oauth_endpoints("github")

# Change based on what you 
my_app <- oauth_app(appname = "Access_GitHub",
                   key = "405a5a528e18cb7fd282",
                   secret = "7ed344909e30a8bc2ff23179248cb821f7ee236a")

# Get OAuth credentials
github_token <- oauth2.0_token(oauth_endpoints("github"), my_app)

# Use API
my_token <- config(token = github_token)
following_data <- GET("https://api.github.com/users/miocalla/repos", my_token)

# Take action on http error
stop_for_status(following_data)

# Extract content from a request
following_data_content = content(following_data)

# Convert to a data.frame
data_frame = jsonlite::fromJSON(jsonlite::toJSON(following_data_content))

# Subset data.frame
data_frame[data_frame$full_name == "miocalla/datasharing", "created_at"] 

# Retrieve usernames 
id = dataFrame$login
user_ids = c(id)

# Create empty vectors and data frame
all_users = c()
all_users_df = data.frame(
  username = integer(),
  following = integer(),
  followers = integer(),
  repositories = integer(),
  date_created = integer()
)

# Loop through the list of usernames in order to find users to add to the list
for (i in 1:length(user_ids)) 
{

  # Retrieve an individual users following list
  following_url = paste("https://api.github.com/users/", user_ids[i], "/following", sep = "")
  following = GET(following_url, my_token)
  following_content = content(following)
  
  # Skip user if they don't follow anybody
  if (length(following_content) == 0) 
  {
    next
  }
  
  # Add followings to a data frame and retrieve usernames
  following_df = jsonlite::fromJSON(jsonlite::toJSON(following_content))
  following_login = following_df$login
  
}