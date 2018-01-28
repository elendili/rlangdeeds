#graphics.off()
plot.new()
n=7
x=seq(-n,n,0.1)
y=x^2

# set graph parameters: axis labels always horisontal, change number of tickmarks, reduce whitepace around
par(las=1,lab=c(n*4,n^2,0), tcl=-0.5, xaxs="i",yaxs="i",oma=c(0,0,0,0), omd=c(0,0,0,0), omi=c(0,0,0,0), mfrow=c(1,1), mgp=c(0,1,0), pch=1);

# plot with parabola, no axes, with lines
plot(x,y,type="l",axes=FALSE, ann=FALSE,mar=c(0,0,0,0),asp=0.4);





# asix x with pos zero
axis(side=1,pos=0,cex.axis=0.5,col.axis="blue");
# asix y with pos zero
axis(side=2,pos=0,cex.axis=0.5,col.axis="blue");
# draw grid on plot
grid()

genLineFun =function(a,b){ a=-abs(a); b=abs(b); fslope=function(x){ m=(b^2-a^2)/(b-a); return(m*x-a*b);  }; return (fslope); }
# genLineFun(4,5)(0)
# [1] 20


# lines(x, genLineFun(2,4)(x),col="red")
# lines

library(numbers);
prs=1:(n); 
#prs =Filter({function(i)i>0}, prs*isPrime(prs))
for(i in prs) {
	for(j in prs) {
		ucol=rainbow(n^2)[i*j]
		ulwd=3/n
#      lines(x, genLineFun(j,i)(x), col=ucol,lwd=ulwd);
      segments(-i,i^2,j,j^2,col=ucol,lwd=ulwd);
      segments(-i,i^2,-i,0,col=ucol,lwd=ulwd);
      segments(j,j^2,j,0,col=ucol,lwd=ulwd);
   }
}
# redraw plot
# replot()
