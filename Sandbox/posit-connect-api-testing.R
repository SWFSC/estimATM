library(connectapi)
client <- connect(
  server = CONNECT_SERVER,
  api_key = CONNECT_API_KEY
)

# get data
users <- get_users(client)
groups <- get_groups(client)
usage_shiny <- get_usage_shiny(client)
usage_static <- get_usage_static(client)
some_content <- get_content(client)

# get all content
all_content <- get_content(client, limit = Inf)
