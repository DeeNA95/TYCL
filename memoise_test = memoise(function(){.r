library(shiny)
library(DBI)
library(odbc)
library(memoise)

sqlserver <- "tycl.database.windows.net"
    database <- "backup"
    user <- "tycl"
    password <- "xazfek-Bivxo2-cudroz"
    driver <- "/opt/homebrew/Cellar/msodbcsql18/18.3.2.1/lib/libmsodbcsql.18.dylib"
    # Create a connection string
    conn_str <- paste0(
      "DRIVER=", driver,
      ";SERVER=", sqlserver,
      ";DATABASE=", database,
      ";UID=", user,
      ";PWD=", password,
      ";Encrypt=yes;TrustServerCertificate=no;Connection Timeout=300;"
    )
    
    # Establish the connection
    con <- dbConnect(
      odbc::odbc(),
      .connection_string = conn_str
    )
    
    # Example query
    query <- "SELECT * FROM product_sales"
    data_2 <- dbGetQuery(con, query)
    
    # add more queries as necessary, e.g., products
    productsdb <- dbGetQuery(con, "SELECT * FROM Product")
    
    # Close the connection when done
    dbDisconnect(con)



ui2 = fluidPage('plot', dataTableOutput('r'))#tableOutput('gg'))


server2 = function(input, output, session){
  # Memoize the SQL query function
 
  
  output$r = renderDataTable(data_2)
  #dat = memoise_test()
  #output$gg = renderTable(dat)
}

shinyApp(ui2, server2)
