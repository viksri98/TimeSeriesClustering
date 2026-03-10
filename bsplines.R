library(splines)
library(tidyverse)

n.knots <- 6
x <- seq(0,1,length.out=100)
knots <- seq(0,1,length.out=n.knots-2)

B <- bs(x, knots=knots, degree=3)[,1:n.knots]

B%>%
  as.data.frame()%>%
  mutate(rw=row_number())%>%
  gather(key='key',value='value',`1`:`6`)%>%
  ggplot(aes(x=rw,y=value,col=key))+geom_line(aes(group=key))+
  labs(title='B-Spline Basis with 6 knots and 3rd-Degree Polynomials')
