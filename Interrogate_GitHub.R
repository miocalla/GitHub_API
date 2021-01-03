#Source: Michael Galarnyk - "Accessing Data from Github API using R"

#install.packages("jsonlite")
library(jsonlite)
#install.packages("httpuv")
library(httpuv)
#install.packages("httr")
library(httr)
#install.packages("plotly")
library(plotly)
#install.packages("devtools")
library(devtools)

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

# I decided to use the GitHub account of Fabien Potencier for this task as my account is relatively new
# From research I conducted, I learned that Potencier is one of the most active developers on GitHub with with 11.1k followers,
# Began to interrogate Fabien Potencier's account to produce graphs, by first looking at his followers
myData = GET("https://api.github.com/users/fabpot/followers?per_page=100;", my_token)
stop_for_status(myData)
extract = content(myData)
data_frame = jsonlite::fromJSON(jsonlite::toJSON(extract)) #converts into data frame
data_frame$login

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

  # Retrieve user's following list
  following_url = paste("https://api.github.com/users/", user_ids[i], "/following", sep = "")
  following = GET(following_url, my_token)
  following_content = content(following)
  
  # Skip if user doesn't follow anyone
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
      
      # Add user to list of users
      all_users[length(all_users) + 1] = following_login[j]
      
      # Get data on each user
      following_url2 = paste("https://api.github.com/users/", following_login[j], sep = "")
      following2 = GET(following_url2, my_token)
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
      all_users_df[nrow(all_users_df) + 1, ] = c(following_login[j], following_number, followers_number, repos_number, year_created)
    }
    next
  }
  
  # Stop when there are more than 200 users
  if(length(all_users) > 200) {
    break
  }
  next
}

Sys.setenv("plotly_username"="miocalla")
Sys.setenv("plotly_api_key"="w0tRKclIUvGiEcXpKoeN")

# Plot 1
# plot followers vs repositories coloured by year
plot_one = plot_ly(data = all_users_df, x = ~repositories, y = ~followers, 
                text = ~paste("Followers: ", followers, "<br>Repositories: ", 
                              repositories, "<br>Date Created:", date_created), color = ~date_created)
plot_one

# Send to plotly
api_create(plot_one, filename = "Followers vs Repositories")

# Plot 2
# Graphs following v followers coloured by year
plot_two = plot_ly(data = all_users_df, x = ~following, y = ~followers, text = ~paste("Followers: ", followers, "<br>Following: ", following), color = ~date_created)
plot_two

#send to plotly
api_create(plot_two, filename = "Following vs Followers")

# Plot 3

#now attempting to graph the 10 most popular languages used by the 250 users.
languages = c()

for (i in 1:length(user_ids)) {
  
  RepositoriesUrl = paste("https://api.github.com/users/", user_ids[i], "/repos", sep = "")
  Repositories = GET(RepositoriesUrl, my_token)
  RepositoriesContent = content(Repositories)
  RepositoriesDF = jsonlite::fromJSON(jsonlite::toJSON(RepositoriesContent))
  RepositoriesNames = RepositoriesDF$name
  
  #Loop through all the repositories of an individual user
  for (j in 1: length(RepositoriesNames)) {
    
    #Find all repositories and save in data frame
    RepositoriesUrl2 = paste("https://api.github.com/repos/", user_ids[i], "/", RepositoriesNames[j], sep = "")
    Repositories2 = GET(RepositoriesUrl2, my_token)
    RepositoriesContent2 = content(Repositories2)
    RepositoriesDF2 = jsonlite::fromJSON(jsonlite::toJSON(RepositoriesContent2))
    language = RepositoriesDF2$language
    
    #Removes repositories containing no specific languages
    if (length(language) != 0 && language != "<NA>") {
      languages[length(languages)+1] = language
    }
    next
  }
  next
}

# Puts 10 most popular languages in table 
all_languages = sort(table(languages), increasing=TRUE)
top_ten = all_languages[(length(all_languages)-9):length(all_languages)]

# Converts to dataframe
language_df = as.data.frame(top_ten)

# Plot the data frame of languages
plot_three = plot_ly(data = language_df, x = language_df$languages, y = language_df$Freq, type = "bar")
plot_three

api_create(plot_three, filename = "Top 10 Most Popular Languages")
