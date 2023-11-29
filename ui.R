source('preprocessing.R')
source('sidebar.R')

ui <- dashboardPage(skin = 'purple',
                    
                    dashboardHeader(title = 'V1'),
                    sidebar,
                    {
                      dashboardBody(tabsetPanel(
                        id = 'tabs',
                        tabPanel(
                          'Year On Year',
                          value = 'YOY',
                          
                          box(
                            tableOutput('yoydata'),
                            title = 'Month On Month',
                            status = 'primary',
                            solidHeader = T,
                            width = 12,
                            height = 150
                          ),
                          
                          box(
                            plotlyOutput('yoy', height = '550px'),
                            title = 'YoY',
                            solidHeader = T,
                            status = 'primary',
                            collapsible = T,
                            width = 12
                          )
                          
                        ),
                        
                        tabPanel(
                          'Products',
                          value = 'PRODUCTS',
                          box(
                            plotlyOutput('plo', height = '800px'),
                            title = 'Top 10',
                            status = 'primary',
                            solidHeader = T,
                            width = 12,
                            collapsible = T
                          )
                        ),
                        tabPanel(
                          'Distribution Of Sales By Product Group',
                          value = 'DISTRIBUTION',
                          box(
                            plotlyOutput('pie', height = '800px'),
                            title = 'Distribution Of Sales By Product Group',
                            status = 'primary',
                            solidHeader = T,
                            width = 12,
                            collapsible = T
                          )
                        ),
                        tabPanel(
                          'Top Products',
                          value = 'TOP_PRODUCTS',
                          box(plotlyOutput('topProductGraph',height = '550px'), width = 12),
                          box('Sales',tableOutput('topProductsTableTotal'),
                              'Quantity',tableOutput('topProductsTableQuantity'),width = 12)
                        )
                        
                        ,
                        tabPanel('Data', dataTableOutput(outputId = 'pout'), value = 'DATA')
                        
                        
                      ))
                    },
                    tags$head(tags$style(
                      HTML(
                        "
      .main-sidebar {
        position: fixed;
        height: 100%;
      }
      .content-wrapper {
        margin-left: 250px; /* Adjust this value based on your sidebar width */
      }
    "
                      )
                    )))
