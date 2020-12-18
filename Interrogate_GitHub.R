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

## My GitHub Data
# Retrieve my data
my_data = fromJSON("https://api.github.com/users/miocalla")

# Display the number of followers
my_data$followers

# Gives user names of all my followers
followers = fromJSON("https://api.github.com/users/miocalla/followers")
followers$login

# Display the number of users I am following
my_data$following

# Gives user names of all the users I am following
following = fromJSON("https://api.github.com/users/miocalla/following")
following$login

# Display the number of repositories I have
my_data$public_repos

# Gives the name and creation date for my repositories
repositories = fromJSON("https://api.github.com/users/miocalla/repos")
repositories$name
repositories$created_at

# Retrieve usernames 
id = data_frame$login
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
  
  # Add followings to data frame and get usernames
  following_df = jsonlite::fromJSON(jsonlite::toJSON(following_content))
  following_login = following_df$login
  
  # Loop through all of the 'following' users
  for (j in 1:length(following_login)) {
    
    # Check to see that the user is not already in the list of users
    if (is.element(following_login[j], all_users) == FALSE) {
      
      #Add user to list of users
      all_users[length(all_users) + 1] = following_login[j]
      
      # Get data on each user
      following_url2 = paste("https://api.github.com/users/", following_login[j], sep = "")
      following2 = GET(following_url2, myToken)
      following_content2 = content(following2)
      following_df2 = jsonlite::fromJSON(jsonlite::toJSON(following_content2))
      
      # Get each users following
      following_number = following_df2$following
      
      # Get each users followers
      followers_number = following_df2$followers
      
      # Get number of repositories for each user 
      repos_number = following_df2$public_repos
      
      # Get year which each user joined Github
      year_created = substr(following_df2$created_at, start = 1, stop = 4)
      
      # Add users data to a new row in data frame
      all_users[nrow(all_users) + 1, ] = c(following_login[j], following_number, followers_number, repos_number, year_created)
    }
    next
  }
  
  #Stop when there are more than 200 users
  if(length(all_users) > 200) {
    break
  }
  next
}
