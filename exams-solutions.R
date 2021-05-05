
> #Ispit od 2020 g.
> #1 grupa
> #Prva zadacha
> #Reshenie:
>
> #X~Pois(10)
> X<-rpois(100,10)
> X
  [1] 13 11 10 10 12  5  7 11  9 10  6 11  9  8 11  5  9 14  9  9 10 15  7 10 10
 [26] 10  5 14 10 10 10  9 11  7 11 21  8  7  9 16 10 15 14 15  9  7 12 13 17  9
 [51]  7  9  9 10  8  7 15 10  7 10 11  9 12  8  8  8 16 10  9  4  9 18  8  7 17
 [76]  6 16 11 12 10 17  5 11 13 16  2  8  7  8  4 12  7  9 10 14  8  9 10 14 12
>
> log.l<- function(lambda=10){
+ r=dpois(X,lambda);
+ -sum(log(r));}
> library(stats4)
> mle(log.l)

Call:
mle(minuslogl = log.l)

Coefficients:
  lambda 
10.17005 
> #P{X=10}
> teov<- dpois(15,10)
> teov
[1] 0.03471807
> #2 zad
> Y<- c(1.13,1.83, 13.14, 41.22, 27.19, 7.13, 6.94, 24.22, 31.77, 1.03, 7.80, 26.90, 2.27, 3.21, 19.01, 5.98, 6.57, 9.47, 1.29, 6.99, 28.12, 6.13, 13.91, 21.37, 12.23, 7.29, 13.93, 40.32, 0.17, 7.23, 4.44, 3.59, 0.16, 22.27, 7.32, 4.82, 6.04, 3.80, 3.23, 6.04)
> #Y~Exp(lambda)
> #a)
> min(Y);max(Y);mean(Y);sd(Y);sum(Y);var(Y);range(Y);median(Y);IQR(Y);
[1] 0.16
[1] 41.22
[1] 11.4375
[1] 10.99639
[1] 457.5
[1] 120.9207
[1]  0.16 41.22
[1] 7.06
[1] 11.4525
> quantile(Y)
     0%     25%     50%     75%    100% 
 0.1600  3.7475  7.0600 15.2000 41.2200 
> hist(Y)
> summary(Y)
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  0.160   3.748   7.060  11.438  15.200  41.220 
> #b)
> log.l<- function(lambda=10){
+ r=dexp(Y,lambda);
+ -sum(log(r));}
> library(stats4)
> mle(log.l)

Call:
mle(minuslogl = log.l)

Coefficients:
    lambda 
0.08743111 
There were 22 warnings (use warnings() to see them)
> lambda=0.087
> #bidejki e apsolutno neprekinata kolmogorov test
> #h0: Y ima exp r-ba
> #ha: Y nema exp r-ba
> ks.test(Y,"pexp",lambda)

        One-sample Kolmogorov-Smirnov test

data:  Y
D = 0.13233, p-value = 0.4854
alternative hypothesis: two-sided

Warning message:
In ks.test(Y, "pexp", lambda) :
  ties should not be present for the Kolmogorov-Smirnov test
> #p-val = 0.485 > 0.05 znaci h0 se prifakja Y ima exp r-ba
> #v) 
> #h0: mu=20
> #ha: mu!=20
> t.test(Y,alt="two.sided",mu=20)

        One Sample t-test

data:  Y
t = -4.9247, df = 39, p-value = 1.588e-05
alternative hypothesis: true mean is not equal to 20
95 percent confidence interval:
  7.920682 14.954318
sample estimates:
mean of x 
  11.4375 

> #p-val < od 0.05 znaci ha se prifakja, dodeka pak prosecnoto vreme na cekanje ne e 20 min
> #3 zad
> pod<- matrix(c(7,16,5,80,2,2,byrow=T)
+ )
> dimnames(pod)<- list(c("<50%",">50%"),c(">200","0-199"))
Error in dimnames(pod) <- list(c("<50%", ">50%"), c(">200", "0-199")) : 
  length of 'dimnames' [1] not equal to array extent
> pod
     [,1]
[1,]    7
[2,]   16
[3,]    5
[4,]   80
[5,]    2
[6,]    2
[7,]    1
> pod<- matrix(c(7,16,5,80),2,2,byrow=T))
Error: unexpected ')' in "pod<- matrix(c(7,16,5,80),2,2,byrow=T))"
> pod<- matrix(c(7,16,5,80),2,2,byrow=T)
> pod
     [,1] [,2]
[1,]    7   16
[2,]    5   80
> dimnames(pod)<- list(c("<50%",">50%"),c(">200","0-199"))
> pod
     >200 0-199
<50%    7    16
>50%    5    80
> #h0: nezavisni se 
> #ha: ne se nezavisni
> chisq.test(pod,correct=F)

        Pearson's Chi-squared test

data:  pod
X-squared = 11.049, df = 1, p-value = 0.0008875

Warning message:
In chisq.test(pod, correct = F) :
  Chi-squared approximation may be incorrect
> chisq.test(pod, correct=FALSE)

        Pearson's Chi-squared test

data:  pod
X-squared = 11.049, df = 1, p-value = 0.0008875

Warning message:
In chisq.test(pod, correct = FALSE) :
  Chi-squared approximation may be incorrect
> pod <- matrix(c(7,16,5,8), 2,2, byrow=T)
> pod <- matrix(c(7,16,5,8), 2,2, byrow=T)
> 
> dimnames(pod) <- list(c("pod 50", "nad 50"), c(">200", "0-199"))
> chisq.test(pod, correct=FALSE)

        Pearson's Chi-squared test

data:  pod
X-squared = 0.2408, df = 1, p-value = 0.6236

Warning message:
In chisq.test(pod, correct = FALSE) :
  Chi-squared approximation may be incorrect
> pod
       >200 0-199
pod 50    7    16
nad 50    5     8
> pod <- matrix(c(16,7,8,5), 2,2, byrow=T)
> pod
     [,1] [,2]
[1,]   16    7
[2,]    8    5
> dimnames(pod) <- list(c("pod 50", "nad 50"), c("0-199", ">200"))
> pod\
Error: unexpected input in "pod\"
> pod
       0-199 >200
pod 50    16    7
nad 50     8    5
> chisq.test(pod, correct=FALSE)

        Pearson's Chi-squared test

data:  pod
X-squared = 0.2408, df = 1, p-value = 0.6236

Warning message:
In chisq.test(pod, correct = FALSE) :
  Chi-squared approximation may be incorrect
> #p-val -0.62 > 0.05 znaci se prifakja h0 nezavisni se
> #2020 2ra grupa
> #1 zad
> #X~Exp(lambda), EX=15, EX=1/lambda
> lambda=1/15
> X<-rexp(100,lambda)
> X
  [1]  6.0755654 24.9800218  4.3703586  1.0333777  4.9611032  0.5969317
  [7] 17.6040111  8.1925481  1.7563430  3.6393320  2.5821449  9.7609417
 [13] 50.5387942  3.6462853 24.5092983 67.1265451 17.0004794 16.8955496
 [19] 12.4585358  5.0887531 23.8998642 19.0847957 30.2862450 50.5362103
 [25] 15.4591438  1.7761755 10.2557135  2.0147809  5.6747306 22.3404950
 [31] 35.9516377  2.6879796 27.0353892  2.0379836  5.7059835 12.4520112
 [37] 16.3872960  0.7091262  8.4730650 17.9898745 19.9261413  3.3184506
 [43] 12.3037081  9.8129715  0.4555230  3.7582590 39.0639321  2.1750856
 [49]  9.0620686  2.9876517  8.5635557  3.3847631  0.4768197 42.7244624
 [55] 30.3709145 15.4882218 72.5464648  4.7141676 21.3644048 15.6233027
 [61] 50.8671331 45.5407766 19.9847630  4.1947324  0.7777357 11.0590809
 [67]  2.8101368  9.1905808  8.3650213 19.5012768  9.9969422 15.4915086
 [73] 15.4242111  9.3077512  8.9012102  3.2701209  7.4068681 39.7015854
 [79]  7.2751890  8.2592606 52.1324740  3.2279043 21.0624327 26.1441654
 [85] 27.8435094 31.4339018 11.9427924  1.7964296 23.6822395 16.2108466
 [91]  4.5756460  1.7280837 24.4468792 16.9886112  0.8945220  5.5978918
 [97] 20.8423632  6.1974804  0.4090732  0.1187772
> #ocena 
> EX= 1/mean(X)
> EX
[1] 0.06638792
> #ili
> log.l<- function(lambda){
+ r=dexp(X,lambda);
+ -sum(log(r));}
> library(stats4)
> mle(log.l)
Error in optim(start, f, method = method, hessian = TRUE, ...) : 
  non-finite value supplied by optim
> log.l <- function(lambda = 10){
+ 
+ r = dexp(Y, lambda);
+ 
+ -sum(log(r));
+ 
+ }
> library(stats4)
> mle(log.l)

Call:
mle(minuslogl = log.l)

Coefficients:
    lambda 
0.08743111 
There were 22 warnings (use warnings() to see them)
> log.l <- function(lambda = 1/15){
+ 
+ r = dexp(Y, lambda);
+ 
+ -sum(log(r));
+ 
+ }
> library(stats4)
> mle(log.l)

Call:
mle(minuslogl = log.l)

Coefficients:
    lambda 
0.08743373 
> log.l <- function(lambda = 1/15){
+ 
+ r = dexp(X, lambda);
+ 
+ -sum(log(r));
+ 
+ }
> library(stats4)
> mle(log.l)

Call:
mle(minuslogl = log.l)

Coefficients:
    lambda 
0.06638928 
Warning messages:
1: In dexp(X, lambda) : NaNs produced
2: In dexp(X, lambda) : NaNs produced
3: In dexp(X, lambda) : NaNs produced
> #znaci za exp moze so toa 1/mean(X)ili log.l isto se dobiva
> #P{X>=10}=1-P{X<10}
> teov<- 1-dexp(10,1/15)
> teov
[1] 0.9657722
> #2 zadaca
> #Y~Pois(lambda)
> #a)
> min(Y);max(Y);mean(Y);sd(Y);range(Y);median(Y);sum(Y);var(Y);IQR(Y);
[1] 0.16
[1] 41.22
[1] 11.4375
[1] 10.99639
[1]  0.16 41.22
[1] 7.06
[1] 457.5
[1] 120.9207
[1] 11.4525
> quantile(Y)
     0%     25%     50%     75%    100% 
 0.1600  3.7475  7.0600 15.2000 41.2200 
> Y<- c(7, 8, 11, 8, 12, 13, 14, 7, 10, 12, 6, 11, 7, 9, 6, 3, 8, 12, 10, 4, 11, 10, 7, 8, 14, 13, 7, 9, 11, 9, 8, 16, 12, 12, 5, 13, 9, 10, 11, 9)
> min(Y);max(Y);mean(Y);sd(Y);range(Y);median(Y);sum(Y);var(Y);IQR(Y);
[1] 3
[1] 16
[1] 9.55
[1] 2.881862
[1]  3 16
[1] 9.5
[1] 382
[1] 8.305128
[1] 4.25
> hist(Y)
> summary(Y)
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   3.00    7.75    9.50    9.55   12.00   16.00 
> quantile(Y)
   0%   25%   50%   75%  100% 
 3.00  7.75  9.50 12.00 16.00 
> #b)
> log.l<- function(lambda=10){
+ r=dpois(Y,lambda);
+ -sum(log(r));}
> library(stats4)
> mle(log.l)

Call:
mle(minuslogl = log.l)

Coefficients:
  lambda 
9.550007 
> lambda=9.55
> #Pois raspredelba e od diskreten tip, zatoa ideme so ova
> #h0: Y ima pois
> #ha: Y nema pois
> prob<- c(dpois(Y,lambda))
> chisq.test(Y,prob)

        Pearson's Chi-squared test

data:  Y and prob
X-squared = 480, df = 144, p-value < 2.2e-16

Warning message:
In chisq.test(Y, prob) : Chi-squared approximation may be incorrect
> #p-val e < od 0.05 znaci Y nema pois r-ba
> #v)
> #h0: mu=15
> #ha: mu>15
> t.test(Y,alt"greater",mu=15)
Error: unexpected string constant in "t.test(Y,alt"greater""
> t.test(Y,alt="greater",mu=15)

        One Sample t-test

data:  Y
t = -11.961, df = 39, p-value = 1
alternative hypothesis: true mean is greater than 15 
95 percent confidence interval:
 8.782266      Inf
sample estimates:
mean of x 
     9.55 

> #p-val e 1 > 0.05, znaci h0 se prifakja mu =15
>
>
> #3 zad ista ko u grupa 1
> #2020 juli
> #2 zad
> # X-broj na frlanja na kocka se dodeka ne se pojavi 6ka X~Geom.
> X<-rgeom(10,1/6)
> X
 [1] 4 1 1 0 0 3 0 0 3 4
> log.l <- function( p = 1/6){
+ 
+ r=dgeom(X, p);
+ 
+ -sum(log(r));
+ 
+ }
> library(stats4);
> mle(log.l)

Call:
mle(minuslogl = log.l)

Coefficients:
        p 
0.3846155 
Warning messages:
1: In dgeom(X, p) : NaNs produced
2: In dgeom(X, p) : NaNs produced
3: In dgeom(X, p) : NaNs produced
> #3 zad
> X = c(56, 47, 49, 37, 38, 60, 50, 43, 43, 59, 50, 56, 54, 58)
> Y = c(53, 21, 32, 49, 45, 38, 44, 33, 32, 43, 53, 46, 36, 48, 39, 35, 37, 36, 39, 45)
> #Testiranje na ednakvost na disprerzii
> #Se pravi so var.test
> #h0: S1=S2
> #ha: S1<S2
> var.test(X,Y,alt="less")

        F test to compare two variances

data:  X and Y
F = 0.91589, num df = 13, denom df = 19, p-value = 0.4454
alternative hypothesis: true ratio of variances is less than 1
95 percent confidence interval:
 0.000000 2.263048
sample estimates:
ratio of variances 
          0.915891 

> #p-val = 0.445> 0.05, znaci h0 se prifakjaa vo ovaa situacija
> #h0: mu1=mu2
> #h0: mu1>mu2
> t.test(X,Y,alt="greater",var.equal=T)

        Two Sample t-test

data:  X and Y
t = 3.596, df = 32, p-value = 0.0005365
alternative hypothesis: true difference in means is greater than 0
95 percent confidence interval:
 5.183765      Inf
sample estimates:
mean of x mean of y 
     50.0      40.2 

> #p-val=0.00.. < 0.05 taka da ha se prifakja mu1 e pogolemo
> #4 zad
> frekv<-c(234,28,98,151,422,67)
> f1<- summary(as.factor(frekv))["234"]/1000
> probs<-c(0.25,0.04,0.1,0.15,0.4,0.06)
> chisq.test(frekv,probs)

        Pearson's Chi-squared test

data:  frekv and probs
X-squared = 30, df = 25, p-value = 0.2243

> #p-val=0.22 >0.05, znaci gi imaa soodvetnite vrednosti
> #2020 sep
> #1 zad
> #a)
> X<- sample(c(3,5,15),1000,replace=T,probs=c(0.25, 0.4, 0.35))
Error in sample(c(3, 5, 15), 1000, replace = T, probs = c(0.25, 0.4, 0.35)) : 
  unused argument (probs = c(0.25, 0.4, 0.35))
> X<- sample(c(3,5,15),1000,replace=T,prob=c(0.25, 0.4, 0.35))
> X
   [1]  3  5 15  5  5  5  5  5  3 15  3 15 15  3  5  3  5  3  5  3 15 15 15  5
  [25]  5  3  5 15  5 15  3  5  5  5  3  5  5  5  5  5  3  3  3  5 15 15  5  3
  [49]  5 15 15 15  5 15  5  5  5  5 15 15 15  5  5 15  3  5  3  5  5  5  5  5
  [73]  3  5 15  3  5  3  3 15  3  3  5  3  3 15  3  5 15  3  5 15  5 15 15  5
  [97]  5  3  5  5 15  3  5 15 15  3  5  5 15 15  5 15  5 15  5 15 15  5 15  5
 [121] 15  3  5  5  5 15 15  3 15  3  3  3  5  3  3  5  5 15  5  3  5  5 15 15
 [145] 15  5  3  5  5  5  5  5 15  5  3  5  5  5  3  3  5  3  5  5 15 15  5  3
 [169]  5  3  5 15  3  3  5 15 15 15  5  3  3  5 15 15  5  3  3 15  5  5  5 15
 [193] 15  5  5 15 15  5  5 15  3  3  5  3 15  3 15 15  5  5  3  3  5  5  5 15
 [217] 15  3  3  5 15 15  3  3  5  5  5  5  5 15  5  5  3 15  5  5  3 15  5  3
 [241]  5 15 15  5 15  3  3  3  3  3  5  5  5  3 15 15  5  3  3  5  5 15  5 15
 [265] 15  3  5 15  5  3 15 15 15  5  3 15  3  3  3  5 15  3  5  5 15  3 15 15
 [289] 15  5 15  3  5 15  3 15 15  3  5  5  3  5  5 15 15 15  3 15  5  5  5  5
 [313]  5  5 15 15  3  5 15 15  5  3  3 15  5  5  5 15  5  5  3 15 15  3  5 15
 [337]  5  5 15  3  3  3  5  5  5  5 15 15 15  5  3  3  5 15  3 15  3  5  3  5
 [361] 15  5  5  5  3  3  5  5  5 15 15 15  5  5  5  5  3  5  3  5  5  5  5  5
 [385]  3  3 15  5  5 15 15  5  3 15 15  5 15 15  5  5 15  3  5  3 15  3  3 15
 [409]  3 15  5  5  3 15  3  5  3  3 15 15  5 15 15  3 15  5  5  5  5  3  3  5
 [433]  5  3 15 15  5 15 15  5  5 15  5  3  5  5  3  3 15 15  5  3  3  5 15  5
 [457]  5  5 15  3  5 15  5 15  3  5  5  3 15 15  3  5  5  3 15  5 15 15  3 15
 [481]  3  3 15  3  5 15  5 15 15  5  5  5  5 15 15  5  3  5  5  5  5  5  3  3
 [505]  5  5 15  5  5  3 15  5  5 15  5  5 15  5  5  5  5  3 15 15 15  5  5  3
 [529] 15  5  5  5 15 15  5 15  5  5  3 15  3  3  5 15  5  5  5  5 15  3  3  5
 [553] 15  5  5  3  5  5  5  3 15 15  3  5  3  5 15  5 15  5 15  5  3 15 15 15
 [577] 15 15 15  3 15 15 15  5  3 15  5  5  5  5  5 15  3  5  5  3  3 15 15 15
 [601]  5 15  5  5 15 15  5 15 15  3 15 15  3 15  3 15 15  3 15  3  5  5 15  5
 [625] 15 15  5  5  5  3 15 15  5  5  3  5 15  3  5 15  5 15  5  5  5  3 15 15
 [649]  5  3  5  3  5  5  5  5 15  3  5  5  5  5 15  3  3  5 15 15 15 15  3  3
 [673]  5 15  3  5 15  3 15 15  5  3  5 15  3  5  3 15 15  3 15  5  5  3  5 15
 [697]  3  3  5  3 15  5  5  5  5  3 15  3  5 15  5  5  3  3  3  5  5  5  5  3
 [721]  5  5 15  3 15  5  5 15  3  5 15  5 15  3 15  5 15 15  5 15  5  5  5 15
 [745]  3 15  5  5  5 15  5  3  3 15  3  3  5  5  5  5  5 15  3 15  3  5  5  5
 [769]  5  3  5  5  3  3 15  5  3 15  3 15  5  5 15  5 15  3 15  5 15  5  5  5
 [793]  3  5 15  5  5  5  5  5 15 15 15  5  3  5  5  3  5  5  5  5  3  5 15 15
 [817]  5 15  3 15 15 15  3 15  5 15  5  5 15 15 15  5 15 15  3 15  5 15 15  3
 [841] 15 15 15 15  5  3  3 15 15  5  5  3 15 15  3 15  5 15 15  5  3  3 15  5
 [865]  5  5 15 15  5 15 15  5 15 15  3  5 15  3  5  5  3  5  5 15 15  3  5  5
 [889]  5  5  5  5  5  3 15 15 15  5 15 15  3 15  3 15  5  5  3 15  5  3  3  5
 [913]  3  3  5 15  3 15  5  5  5  3 15  5 15 15 15  3  5  3  3  5 15 15 15 15
 [937]  5  3  3  5 15  5 15  5 15  3  5 15 15  5  3  3  5  5  5  5 15 15  5  5
 [961]  5 15  3  5 15  3 15  3  5  3  5 15  5  3 15 15  5  5 15 15  5  3  5 15
 [985]  5  5  5  5  5  5 15 15  5  5  3 15 15 15  3  3
> #b)
> min(x), max(x), quantile(x), median(x), m(x), hist(x)
> #v)
> frek<-summary(as.factor(X))["3"]/1000
> frek
    3 
0.245 
> #2 zad
> Y <- c(5, 7 , 5, 8 , 9, 7, 5, 6 , 4 , 8)
> #Y~NB(k=3,p=0.5) p e 0.5, se zema proizvona vrednost, kje zemam neshto blizu do prosekot sega na slucajnata promenliva, 0.5 primer
> log.l <- function(p=0.5){
+ k=3;
+ r = dnbinom(Y, k, p);
+ -sum(log(r));}
> library(stats4);
> mle(log.l)

Call:
mle(minuslogl = log.l)

Coefficients:
        p 
0.3191493 

> log.l <- function(p=0.5){
+ r = dnbinom(Y-k, k, p);
+ -sum(log(r));}
> library(stats4);
> mle(log.l)
>
> #3 zad
> before <- c(121, 125, 130, 145, 160, 180, 145, 184, 178, 169, 178 ,179 ,154,
+ 
+ 155, 165)
> after <- c(125, 132, 135, 169, 132, 188, 198, 200, 202,
+ 
+ 145, 154, 178, 141, 122, 131, 128, 118, 145, 185, 158
+ )
> #h0: prosecniot broj na novozarazeni e ist i kaj before i kaj after
> #ha: procecniot broj na novozarazeni na before e pomal od prosecniot broj
> t.test(before,after, alt="less")

        Welch Two Sample t-test

data:  before and after
t = 0.42713, df = 32.999, p-value = 0.664
alternative hypothesis: true difference in means is less than 0
95 percent confidence interval:
     -Inf 17.69842
sample estimates:
mean of x mean of y 
 157.8667  154.3000 

> t.test(before,after, alt="less",var.equal=T)

        Two Sample t-test

data:  before and after
t = 0.40916, df = 33, p-value = 0.6575
alternative hypothesis: true difference in means is less than 0
95 percent confidence interval:
     -Inf 18.31922
sample estimates:
mean of x mean of y 
 157.8667  154.3000 

> #u svaki slucaj p-val > 0.05 znaci h0 
> #4 zad
> pod<- matrix(c(65,55,85,80,135,145,115,120),4,2,byrow=T)
> pod
     [,1] [,2]
[1,]   65   55
[2,]   85   80
[3,]  135  145
[4,]  115  120
> dimnames(pod)<-list(c("mat", "bio", "fiz", "hem"), c("m", "z"))
> pod
      m   z
mat  65  55
bio  85  80
fiz 135 145
hem 115 120
> #h0:X i Y se nezavisni 
> #ha:X i Y se zavisni
> chisq.test(pod, correct=FALSE)

        Pearson's Chi-squared test

data:  pod
X-squared = 1.4484, df = 3, p-value = 0.6942

> #p-val > 0.05 znaci h0
> #2017 na courses sto go ima fileot
> #1 zad
> #1b 10c vk =11, se dodeka= geo r-ba
> #X~Geo(1/11)
> #a)
> X<- rgeo(50,1/11)
Error in rgeo(50, 1/11) : could not find function "rgeo"
> X<- rgeom(50,1/11)
> X
 [1]  2 10 15 14  4  3  1 19  7 21 27  5  3  5  0  2  3 15  3  3  8  4 10 27 20
[26] 23 10 10  7  6  3  2 12  5  4  4  1  2 18 11 23  2 33  7  3  1  4  4  8 22
> #b)
> n<- length(X)
> n
[1] 50
> #z alfa/2 toa e 0.05/2=0.025 1-0.025=0.975 spored tablici 1.96
> mean(X)+1.96*sd(X)/sqrt(n)
[1] 11.40686
> mean(X)-1.96*sd(X)/sqrt(n)
[1] 6.833142
> #znaci intervalot e (6.8 , 11.4)
> #EX spored geo e 1/p
> p=1/11
> EX<=1/p
[1] TRUE
> EX<-1/p
> EX
[1] 11
> #h0: mu=5
> #ha: mu!=5
> t.test(X,alt="two.sided")

        One Sample t-test

data:  X
t = 7.8165, df = 49, p-value = 3.649e-10
alternative hypothesis: true mean is not equal to 0
95 percent confidence interval:
  6.775299 11.464701
sample estimates:
mean of x 
     9.12 
> #p-value mnogu malo < 0.05, znaci ha se zima 
> #2 zad
> X<-c(0.053, 0.246, 0.528, 0.233, 0.161, 1.749,0.690, 0.097, 0.839, 0.122, 0.093, 0.525, 0.952, 1.601,1.059,
> n<-length(X)
> n
[1] 40
> #prvo so MMO
> EX<- 1/mean(X)
> EX
[1] 1.932022
> #sea so MPO
> log.l<- function(lambda=10){
+ r=dexp(X,lambda);
+ -sum(log(r));}
> library(stats4)
> mle(log.l)

Call:
mle(minuslogl = log.l)

Coefficients:
  lambda 
1.932015 
Warning messages:

> lambda=1.932
> #h0: X ima exp
> #ha: X nema exp 
> ks.test(X,"pexp",lambda)

        One-sample Kolmogorov-Smirnov test

data:  X
D = 0.12114, p-value = 0.6001
alternative hypothesis: two-sided

Warning message:
In ks.test(X, "pexp", lambda) :
  ties should not be present for the Kolmogorov-Smirnov test
> #p-val 0.6> 0.05 znaci h0 se prifakja
> #3 zad
> pod<- matrix(c(82,74,48,96),2,2,byrow=T)
> pod
     [,1] [,2]
[1,]   82   74
[2,]   48   96
> dimnames(pod)<-list(c("primanja nad","primanja pod"),c("so visoko","bez visoko"))
> pod
             so visoko bez visoko
primanja nad        82         74
primanja pod        48         96
> #test za zavisnosti
> #h0: ne se zavisni
> #ha: se azavisni
> chisq.test(pod,correct=F)

        Pearson's Chi-squared test

data:  pod
X-squared = 11.277, df = 1, p-value = 0.0007846

> #p-val 0.0007< 0.05, znaci se zavisni
> 
