##This code will not work as you do not have the classified data I used. It is only for demonstration of what the code looks like

# devtools::install_github("GENmatic",'rdelbel')
# devtools::install_github("reportRx",'rdelbel')

require(GENmatic)
require(survival)
require(Rserve)
Rserve(args="--no-save")
#setwd("./GENmatic")

#old way
cstime=Sys.time()
system("plink --noweb --bfile thinned --covar covar.txt --pheno pheno.txt --R Rplinkcoxph.R --out coxph")
ctime=Sys.time()-cstime
ctime

#new way
fstime=Sys.time()
GENfit(c(2,1),c("Siteone", "Sitetwo"),"Hist",bfilename="thinned")
ftime=Sys.time()-fstime
ftime

as.numeric(ctime)/as.numeric(ftime)
#Approx 13x faster
#When the number of snps gets large it is around 15-17x faster (less proportion of time spend in overhead)

#Are the results the same? Of course!
coxphresults=read.table("coxph.auto.R")
head(coxphresults)
fastresults=read.table("thinned.auto.R")
head(fastresults)
all.equal(coxphresults,fastresults)

