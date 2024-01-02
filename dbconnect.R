## azure database connection
# Install and load the required packages

library(DBI)
library(odbc)

# Define connection parameters
server <- "tycl.database.windows.net"
database <- "tyclaronium"
user <- "tycl"
password <- "xazfek-Bivxo2-cudroz"
driver <- "/opt/homebrew/Cellar/msodbcsql18/18.3.2.1/lib/libmsodbcsql.18.dylib"  # Adjust the driver based on your configuration

# Create a connection string
conn_str <- paste0(
  "DRIVER=", driver,
  ";SERVER=", server,
  ";DATABASE=", database,
  ";UID=", user,
  ";PWD=", password,
  ";Encrypt=yes;TrustServerCertificate=no;Connection Timeout=30;"
)

# Establish the connection
con <- dbConnect(
  odbc::odbc(),
  .connection_string = conn_str
)

# Example query
query <- "SELECT * FROM product_sales
"
sales_dat <- dbGetQuery(con, query)
# add more queries as necessary eg: products
productsdb = dbGetQuery(con, "SELECT * FROM Product")
# Close the connection when done
dbDisconnect(con)

