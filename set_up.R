install.packages("rsconnect")
library('rsconnect')
rsconnect::setAccountInfo(name='deena95', token='46CC9EDE8976A1A6C3E411924ACE4845', secret='xkno4b7TbAHXmGvFnWapg28n/GPCDqjnZSQqyT+t')

rsconnect::deployApp('https://github.com/DeeNA95/TYCL')
