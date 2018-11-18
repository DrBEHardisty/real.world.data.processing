## Code for Project #2
## B.E. Hardisty

## Call lattice and the relevant data set:
library(lattice)
ToothGrowth

## Now you're ready to start plotting things:
ToothLength <- xyplot(supp ~ len | dose, data=ToothGrowth)

## By assigning a name to your plot you can easily modify it via "update"
## I henceforth label the x and y axes and give it a title:
update(ToothLength, xlab = "Final Length of Tooth", ylab = "Supplement Type Given", 
main = "Effects of Vitamin C vs Orange Juice on Tooth Length")

## Now try to some density plotting:
densityplot(supp ~ len | dose, data=ToothGrowth)
## I think there's a pattern here;
## if we look at tooth length by dosage, irrespective of what type of supplement our guinea pigs ingested:
densityplot(~ len, data=ToothGrowth, groups=dose)

## We also can look at the aggregate behavior and we note an additive shift:
Teeth1 <- qq(supp ~ len, data=ToothGrowth, distribution=qunif)
pr <- seq(0.25, 0.75, 0.02)
Teeth2 <- qq(supp ~ len, data=ToothGrowth, f.value=pr)
TeethData <- update(Teeth2, main="Tooth Growth by Supplement Type Taken")
TeethData

## Let's call this graphic "Tooth2" and add a title:
Tooth2 <- qqmath(len ~ dose | supp, data=ToothGrowth)
Tooth2
update(Tooth2, 
main="Effects of Vitamin C Delivery System on Tooth Growth in a Small Mammal")

## Final set of graphs, qqplots, holding dose constant:
Tooth3 <- qq(supp ~ len | dose, data=ToothGrowth)
Tooth3
update(Tooth3, xlab="Orange Juice", ylab="Vitamin C", 
main="Effects of VC Delivery System when Dosage is Held Constant")
