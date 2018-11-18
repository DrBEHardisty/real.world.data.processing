## Lab March 8
## Using Loess Curves to Visualize Time Series

i <- 1 + seq(0, lengthco2-1)%%12

mo <- month.abb[1]

mo <- ordered(mo, levels=month.abb)

resdat <- (res~t1mo, data=resdat) + layer(panel.loess(x, y, degree=2, span=2/3, col="red"))
resdat <- data.frame(res=residual(tr), mo=mo, t=t)

xyplot(res~t1mo, data=resdat) + layer(panel.loess(x,y, degree=2, span=2/3, col="red"))
xyplot(st1(co2, s.window=12))
stl(lynx, s.window=9)
xyplot(stl(lx, s.window=9))
