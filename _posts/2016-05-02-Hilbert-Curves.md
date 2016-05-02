---
layout: post
title: 'Hilbert Curves'
series: test 
date: 2016-05-02 10:42
categories: ['blogging', 'jekyll']
published: true 

---

Introduction

The Hilbert cuves was an approach of filling space curves with simple lines. It was discovered by German mathematician *David Hilbert* in 1891 to fill entire plain with a curve, and it is a variant of Peano curves named after Giuseppe Peano in 1890. The Hausdorff dimension is 2. The Euclidean length of \(\H_n\) is \(2^n - 1/(2^n)\), i.e., it grows exponentially with \(n\).

The following codes come from <https://aschinchon.wordpress.com/2016/02/01/going-bananas-with-hilbert/>

``` r
library(reshape2)
library(dplyr)
library(ggplot2)

opt=theme(legend.position="none",
          panel.background = element_rect(fill="white"),
          panel.grid=element_blank(),
          axis.ticks=element_blank(),
          axis.title=element_blank(),
          axis.text=element_blank())

hilbert = function(m,n,r) {
  for (i in 1:n)
  {
    tmp=cbind(t(m), m+nrow(m)^2)
    m=rbind(tmp, (2*nrow(m))^r-tmp[nrow(m):1,]+1)
  }
  melt(m) %>% plyr::rename(c("Var1" = "x", "Var2" = "y", "value"="order")) %>% arrange(order)
}
# Original
ggplot(hilbert(m=matrix(1), n=1, r=2), aes(x, y)) + geom_path()+ opt
ggplot(hilbert(m=matrix(1), n=2, r=2), aes(x, y)) + geom_path()+ opt
ggplot(hilbert(m=matrix(1), n=3, r=2), aes(x, y)) + geom_path()+ opt
ggplot(hilbert(m=matrix(1), n=4, r=2), aes(x, y)) + geom_path()+ opt
ggplot(hilbert(m=matrix(1), n=5, r=2), aes(x, y)) + geom_path()+ opt
ggplot(hilbert(m=matrix(1), n=6, r=2), aes(x, y)) + geom_path()+ opt
# Changing order
ggplot(hilbert(m=matrix(.5), n=5, r=2), aes(x, y)) + geom_path()+ opt
ggplot(hilbert(m=matrix(0), n=5, r=2), aes(x, y)) + geom_path()+ opt
ggplot(hilbert(m=matrix(tan(1)), n=5, r=2), aes(x, y)) + geom_path()+ opt
ggplot(hilbert(m=matrix(3), n=5, r=2), aes(x, y)) + geom_path()+ opt
ggplot(hilbert(m=matrix(-1), n=5, r=2), aes(x, y)) + geom_path()+ opt
ggplot(hilbert(m=matrix(log(.1)), n=5, r=2), aes(x, y)) + geom_path()+ opt
ggplot(hilbert(m=matrix(-15), n=5, r=2), aes(x, y)) + geom_path()+ opt
ggplot(hilbert(m=matrix(-0.001), n=5, r=2), aes(x, y)) + geom_path()+ opt
# Polygons
ggplot(hilbert(m=matrix(log(1)), n=4, r=2), aes(x, y)) + geom_polygon()+ opt
ggplot(hilbert(m=matrix(.5), n=4, r=2), aes(x, y)) + geom_polygon()+ opt
ggplot(hilbert(m=matrix(tan(1)), n=5, r=2), aes(x, y)) + geom_polygon()+ opt
ggplot(hilbert(m=matrix(-15), n=4, r=2), aes(x, y)) + geom_polygon()+ opt
ggplot(hilbert(m=matrix(-25), n=4, r=2), aes(x, y)) + geom_polygon()+ opt
ggplot(hilbert(m=matrix(0), n=4, r=2), aes(x, y)) + geom_polygon()+ opt
ggplot(hilbert(m=matrix(1000000), n=4, r=2), aes(x, y)) + geom_polygon()+ opt
ggplot(hilbert(m=matrix(-1), n=4, r=2), aes(x, y)) + geom_polygon()+ opt
ggplot(hilbert(m=matrix(-.00001), n=4, r=2), aes(x, y)) + geom_polygon()+ opt
# Changing exponent
gplot(hilbert(m=matrix(log(1)), n=4, r=-1), aes(x, y)) + geom_polygon()+ opt
ggplot(hilbert(m=matrix(.5), n=4, r=-2), aes(x, y)) + geom_polygon()+ opt
ggplot(hilbert(m=matrix(tan(1)), n=4, r=6), aes(x, y)) + geom_polygon()+ opt
ggplot(hilbert(m=matrix(-15), n=3, r=sin(2)), aes(x, y)) + geom_polygon()+ opt
ggplot(hilbert(m=matrix(-25), n=4, r=-.0001), aes(x, y)) + geom_polygon()+ opt
ggplot(hilbert(m=matrix(0), n=4, r=200), aes(x, y)) + geom_polygon()+ opt
ggplot(hilbert(m=matrix(1000000), n=3, r=.5), aes(x, y)) + geom_polygon()+ opt
ggplot(hilbert(m=matrix(-1), n=4, r=sqrt(2)), aes(x, y)) + geom_polygon()+ opt
ggplot(hilbert(m=matrix(-.00001), n=4, r=52), aes(x, y)) + geom_polygon()+ opt
# Polar coordinates
ggplot(hilbert(m=matrix(1), n=4, r=2), aes(x, y)) + geom_polygon()+ coord_polar()+opt
ggplot(hilbert(m=matrix(-1), n=5, r=2), aes(x, y)) + geom_polygon()+ coord_polar()+opt
ggplot(hilbert(m=matrix(.1), n=2, r=.5), aes(x, y)) + geom_polygon()+ coord_polar()+opt
ggplot(hilbert(m=matrix(1000000), n=2, r=.1), aes(x, y)) + geom_polygon()+ coord_polar()+opt
ggplot(hilbert(m=matrix(.25), n=3, r=3), aes(x, y)) + geom_polygon()+ coord_polar()+opt
ggplot(hilbert(m=matrix(tan(1)), n=5, r=1), aes(x, y)) + geom_polygon()+ coord_polar()+opt
ggplot(hilbert(m=matrix(1), n=4, r=1), aes(x, y)) + geom_polygon()+ coord_polar()+opt
ggplot(hilbert(m=matrix(log(1)), n=3, r=sin(2)), aes(x, y)) + geom_polygon()+ coord_polar()+opt
ggplot(hilbert(m=matrix(-.0001), n=4, r=25), aes(x, y)) + geom_polygon()+ coord_polar()+opt
```
