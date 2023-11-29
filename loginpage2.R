loginpage2 = div(class = "pull-right", shinyauthr::logoutUI(id = "logout")),
# add login panel UI function
shinyauthr::loginUI(id = "login")