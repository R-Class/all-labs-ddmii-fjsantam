Lab 02 - Random Walking Animation
================
Francisco Santamarina
February 16, 2017

Load the necessary packages and dataset.

``` r
library( animation )
```

#### Animation

``` r
saveGIF({
    random.walk <- 0
    for( i in 1:100)
    {
      random.walk[i] <- cumsum(rnorm(100))
      plot( random.walk, type="l", col="darkred", axes=F, 
            xlab="", ylab="", xlim = c(0,100), ylim = c(-3,3), 
            main="Random Walk" )
      abline( h=0, lty=2, col="gray" )
    }
},

movie.name = "Random_Walk_Animation.gif",
interval = .1,
ani.width = 800,
ani.height = 400
)
```

![](https://github.com/R-Class/all-labs-ddmii-fjsantam/blob/master/Lab%2002/Random_Walk_Animation.gif)
