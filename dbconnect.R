# Install and load the required packages
library(DBI)
library(odbc)

# Define connection parameters for local Docker SQL Server
server <- "127.0.0.1,1433"  # Assuming SQL Server is running on the default port 1433
database <- "aroniumdb"
user <- "sa"
password <- "Avrutd12$"
driver <- "/opt/homebrew/Cellar/msodbcsql18/18.3.2.1/lib/libmsodbcsql.18.dylib"

# Create a connection string
conn_str <- paste0(
  "DRIVER=", driver,
  ";SERVER=", server,
  ";DATABASE=", database,
  ";UID=", user,
  ";PWD=", password,
  ";Encrypt=no;TrustServerCertificate=no;Connection Timeout=300;"
)

# Establish the connection
con <- dbConnect(
  odbc::odbc(),
  .connection_string = conn_str
)

# Example query
query <- "SELECT * FROM product_sales"
sales_dat <- dbGetQuery(con, query)

# Additional queries as needed
productsdb <- dbGetQuery(con, "SELECT * FROM Product")

# Close the connection when done
dbDisconnect(con)
