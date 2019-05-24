P.x <- function(p,m,n,x,sens,spec) {
  P.pool <- 1-(1-p)^m
  P.pool2 <- P.pool*sens + (1-P.pool)*(1-spec)
  return(dbinom(x,n,P.pool2))
}

P.ci <- function(m,n,x,sens,spec,ci) {
  p <- seq(0,1,0.001)
  P <- sapply(p,function(i) P.x(i,m,n,x,sens,spec))
  Pln2 <- -2*log(P)
  Pdev <- Pln2-min(Pln2)
  cscrit=qchisq(ci,1)
  Pci <- which(Pdev<cscrit)
  Plo <- p[min(Pci)]
  Phi <- p[max(Pci)]
  Ppe1 <- p[which.min(Pdev)]
  Ppe2 <- rev(p)[which.min(rev(Pdev))]
  Ppe <- 0.5*(Ppe1+Ppe2)
  return(c(lo=Plo,pe=Ppe,hi=Phi))
}

P.cis <- function(m,n,sens=1,spec=1,ci=0.95) {
  x <- 0:n
  cis <- sapply(x,function(i) {
    setProgress(value=i/n)
    P.ci(m,n,i,sens,spec,ci)
    })
  A<-cbind(x,t(cis))
  return(as.data.frame(A))
}

OutOfRange <- function(x,min,max,name) {
  if (x>max) showModal(modalDialog(title="Error",paste(name,"is too high.")))
  else if (x<min) showModal(modalDialog(title="Error",paste(name,"is too low.")))
  else return(F)
  return(T)
}

DrawGraph <- function() {
  cis <- D$cis
  plot.new()
  plot(cis$x,cis$hi,type="l",col="red",xlab="number of positive pools",ylab="sample prevalence",ylim=c(0,1),panel.first=grid())
  lines(cis$x,cis$pe,type="o",col="blue")
  lines(cis$x,cis$lo,type="l",col="red")
}

RunModel <- function() {
  D$err <- F
  if (OutOfRange(input$sens,0.5,1,"Sensitivity")) {D$err<-T; return()}
  if (OutOfRange(input$spec,0.5,1,"Specificity")) {D$err<-T; return()}
  if (OutOfRange(input$m,0,250,"Size of each pool")) {D$err<-T; return()}
  if (OutOfRange(input$n,0,250,"Number of pools")) {D$err<-T; return()}
  if (OutOfRange(input$ci,0,0.998,"Confidence interval")) {D$err<-T; return()}
  D$cis <- P.cis(
    input$m, input$n,
    input$sens, input$spec,
    input$ci
  )
}



