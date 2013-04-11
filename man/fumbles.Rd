\docType{data}
\name{fumbles}
\alias{fumbles}
\alias{fusion1}
\alias{fusion2}
\alias{golfballs}
\alias{pheno}
\title{NCAA football fumbles}
\format{A data frame with 120 observations on the following 7 variables.
\itemize{ \item{team}{NCAA football team} \item{rank}{rank
based on fumbles per game through games on November 26, 2010}
\item{W}{number of wins through games on November 26, 2010}
\item{L}{number of losses through games on November 26, 2010}
\item{week1}{number of fumbles on November 6, 2010}
\item{week2}{number of fumbles on November 13, 2010}
\item{week3}{number of fumbles on November 20, 2010} }}
\source{
  \url{http://www.teamrankings.com/college-football/stat/fumbles-per-game}

  Similar to the data presented in

  Laura J. Scott, Karen L. Mohlke, Lori L. Bonnycastle,
  Cristen J. Willer, Yun Li, William L. Duren, Michael R.
  Erdos, Heather M. Stringham, Pe- ter S. Chines, Anne U.
  Jackson, Ludmila Prokunina-Olsson, Chia-Jen J. Ding, Amy
  J. Swift, Narisu Narisu, Tianle Hu, Randall Pruim, Rui
  Xiao, Xiao- Yi Y. Li, Karen N. Conneely, Nancy L. Riebow,
  Andrew G. Sprau, Maurine Tong, Peggy P. White, Kurt N.
  Hetrick, Michael W. Barnhart, Craig W. Bark, Janet L.
  Goldstein, Lee Watkins, Fang Xiang, Jouko Saramies,
  Thomas A.  Buchanan, Richard M. Watanabe, Timo T. Valle,
  Leena Kinnunen, Goncalo R.  Abecasis, Elizabeth W. Pugh,
  Kimberly F. Doheny, Richard N. Bergman, Jaakko
  Tuomilehto, Francis S. Collins, and Michael Boehnke, A
  genome-wide association study of type 2 diabetes in Finns
  detects multiple susceptibility vari- ants,
  \emph{Science} (2007).

  Data collected by Allan Rossman in Carlisle, PA.
}
\description{
  This data frame gives the number of fumbles by each NCAA
  FBS team for the first three weeks in November, 2010.

  Phenotype and genotype data from the Finland United
  States Investigation of NIDDM (type 2) Diabetes (FUSION)
  study.

  Allan Rossman used to live on a golf course in a spot
  where dozens of balls would come into his yard every
  week.  He collected the balls and eventually tallied up
  the numbers on the first 5000 golf balls he collected. Of
  these 486 bore the number 1, 2, 3, or 4.  The remaining
  14 golf balls were omitted from the data.
}
\details{
  The fumble counts listed here are total fumbles, not
  fumbles lost.  Some of these fumbles were recovered by
  the team that fumbled.
}
\examples{
data(fumbles)
m <- max(fumbles$week1)
table(factor(fumbles$week1,levels=0:m))
favstats(fumbles$week1)
# compare with Poisson distribution
signif( cbind(
		  fumbles=0:m,
		  observedCount=table(factor(fumbles$week1,levels=0:m)),
		  modelCount= 120* dpois(0:m,mean(fumbles$week1)),
		  observedPct=table(factor(fumbles$week1,levels=0:m))/120,
		  modelPct= dpois(0:m,mean(fumbles$week1))
	) ,3)
showFumbles <- function(x,lambda=mean(x),...) {
	mx <- max(x)
    result <- histogram(~x,type="density", xlim=c(-.5,(mx+2.5)),
		xlab='number of fumbles',
        panel=function(x,y,...){
            panel.histogram(x,alpha=0.8,breaks=seq(-0.5,(mx+2.5),by=1,...))
            panel.points(0:(mx+2),dpois(0:(mx+2),lambda),pch=19,alpha=0.8)
        }
    )
    print(result)
    return(result)
}
showFumbles(fumbles$week1)
showFumbles(fumbles$week2)
showFumbles(fumbles$week3)
data(pheno); data(fusion1); data(fusion2)
fusion1m <- merge(fusion1, pheno, by="id", all.x=FALSE, all.y=FALSE)
xtabs(~t2d + genotype, fusion1m)
xtabs(~t2d + Gdose, fusion1m)
chisq.test( xtabs( ~t2d + genotype, fusion1m ) )
f1.glm <- glm( factor(t2d) ~ Gdose, fusion1m, family=binomial)
summary(f1.glm)
data(golfballs)
golfballs/sum(golfballs)
chisq.test(golfballs, p=rep(.25,4))
}
\keyword{datasets}

