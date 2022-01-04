#
library(ggplot2); theme_set(theme_bw())
library(latex2exp)# LaTeX expressions in ggplot

setwd("~/Papers/intfee/intfee_coding")

### set parameters to generate positive IF
bch = 8
bmh = 6
beta_c = 12
beta_m = 11
nc = 1
nm = 1
# verify that Assumption 2 is satisfied
beta_c+beta_m > bch+bmh
abs(beta_c-beta_m) < bch+bmh

### simulations for subsection 5.1: Card organization interchange fee
# computing maximum mu still makes assumption 2 valid (used in the appendix to prove concavity and that xhat and yhat are between 0 and 1)
bch - bmh > beta_c - beta_m # Assumption 2 =>
(mu_max = (bch - bmh -beta_c + beta_m)/beta_m)

### Total welfare (paper equation (8) as expanded on Derive file (as function of phi, lambda, mu)
#
# Range of interchange fees to draw on horizontal axis
phi.vec=seq(0,0.6,0.01)
#
## case of lambda=mu=0
lam=0; mu=0; delta=0.05
# hence, phi_bar (card org set interchange fee)
(phi_bar_lam0 = 0.5*(bch-bmh-beta_c+beta_m*(1-mu)))
#
xhat_lam0.vec = (bch-phi.vec)/beta_c
yhat_lam0.vec = (-phi.vec^2 + bch*bmh + bmh*beta_c*(lam-1) + phi.vec*(bch-bmh-beta_c))/(beta_m*(bch-beta_c-phi.vec))
#
# consumer welfare (lam0)
wc_lam0.vec = 0.5*nc*nc*(xhat_lam0.vec^2*beta_c*(yhat_lam0.vec+mu-1) + 2*xhat_lam0.vec*phi.vec*(yhat_lam0.vec+mu-1) -yhat_lam0.vec*(beta_c+2*phi.vec) + beta_c*(1-delta^2*mu) -2*delta*mu*phi.vec + 2*phi.vec) -bch*nc*nm*(xhat_lam0.vec*(yhat_lam0.vec+mu-1) + yhat_lam0.vec*(lam-1))
#
# merchant welfare (lam0)
wm_lam0.vec = 0.5*nc*nm*(xhat_lam0.vec*(yhat_lam0.vec^2*beta_m-2*yhat_lam0.vec*phi.vec-beta_m*(mu^2-2*mu+1) -2*mu*phi.vec + 2*phi.vec) - yhat_lam0.vec^2*beta_m + 2*yhat_lam0.vec*phi.vec + beta_m*(delta*mu*(mu-2)+1) + 2*delta*mu*phi.vec - 2*phi.vec) - bmh*nc*nm*(xhat_lam0.vec*(yhat_lam0.vec+mu-1)+yhat_lam0.vec*(lam-1))
#
w_lam0.vec = wc_lam0.vec + wm_lam0.vec
#
## case of lambda=0.005, mu=0
lam=0.005; mu=0; delta=0.05
# hence, phi_bar (card org set interchange fee) [no change because phi_bar is not a function of lambda]
(phi_bar_lam1 = 0.5*(bch-bmh-beta_c+beta_m*(1-mu)))
#
xhat_lam1.vec = (bch-phi.vec)/beta_c
yhat_lam1.vec = (-phi.vec^2 + bch*bmh + bmh*beta_c*(lam-1) + phi.vec*(bch-bmh-beta_c))/(beta_m*(bch-beta_c-phi.vec))
#
# consumer welfare (lam1)
wc_lam1.vec = 0.5*nc*nc*(xhat_lam1.vec^2*beta_c*(yhat_lam1.vec+mu-1) + 2*xhat_lam1.vec*phi.vec*(yhat_lam1.vec+mu-1) -yhat_lam1.vec*(beta_c+2*phi.vec) + beta_c*(1-delta^2*mu) -2*delta*mu*phi.vec + 2*phi.vec) -bch*nc*nm*(xhat_lam1.vec*(yhat_lam1.vec+mu-1) + yhat_lam1.vec*(lam-1))
#
# merchant welfare (lam0)
wm_lam1.vec = 0.5*nc*nm*(xhat_lam1.vec*(yhat_lam1.vec^2*beta_m-2*yhat_lam1.vec*phi.vec-beta_m*(mu^2-2*mu+1) -2*mu*phi.vec + 2*phi.vec) - yhat_lam1.vec^2*beta_m + 2*yhat_lam1.vec*phi.vec + beta_m*(delta*mu*(mu-2)+1) + 2*delta*mu*phi.vec - 2*phi.vec) - bmh*nc*nm*(xhat_lam1.vec*(yhat_lam1.vec+mu-1)+yhat_lam1.vec*(lam-1))
#
w_lam1.vec = wc_lam1.vec + wm_lam1.vec

## case of lambda=0.01, mu=0.005
lam=0.005; mu=0.05; delta=0.05
# hence, phi_bar (card org set interchange fee) [no change because phi_bar is not a function of lambda]
(phi_bar_lam1_mu1 = 0.5*(bch-bmh-beta_c+beta_m*(1-mu)))
#
xhat_lam1_mu1.vec = (bch-phi.vec)/beta_c
yhat_lam1_mu1.vec = (-phi.vec^2 + bch*bmh + bmh*beta_c*(lam-1) + phi.vec*(bch-bmh-beta_c))/(beta_m*(bch-beta_c-phi.vec))
#
# consumer welfare (lam1 mu1)
wc_lam1_mu1.vec = 0.5*nc*nc*(xhat_lam1_mu1.vec^2*beta_c*(yhat_lam1_mu1.vec+mu-1) + 2*xhat_lam1_mu1.vec*phi.vec*(yhat_lam1_mu1.vec+mu-1) -yhat_lam1_mu1.vec*(beta_c+2*phi.vec) + beta_c*(1-delta^2*mu) -2*delta*mu*phi.vec + 2*phi.vec) -bch*nc*nm*(xhat_lam1_mu1.vec*(yhat_lam1_mu1.vec+mu-1) + yhat_lam1_mu1.vec*(lam-1))
#
# merchant welfare (lam1 mu1)
wm_lam1_mu1.vec = 0.5*nc*nm*(xhat_lam1_mu1.vec*(yhat_lam1_mu1.vec^2*beta_m-2*yhat_lam1_mu1.vec*phi.vec-beta_m*(mu^2-2*mu+1) -2*mu*phi.vec + 2*phi.vec) - yhat_lam1_mu1.vec^2*beta_m + 2*yhat_lam1_mu1.vec*phi.vec + beta_m*(delta*mu*(mu-2)+1) + 2*delta*mu*phi.vec - 2*phi.vec) - bmh*nc*nm*(xhat_lam1_mu1.vec*(yhat_lam1_mu1.vec+mu-1)+yhat_lam1_mu1.vec*(lam-1))
#
w_lam1_mu1.vec = wc_lam1_mu1.vec + wm_lam1_mu1.vec


### combining all welare into a single data frame
w.df = data.frame(phi.vec, w_lam0.vec, w_lam1.vec, w_lam1_mu1.vec)
# find interchange fees phi that maximize w (to be plotted as dotted vertical lines)
(phi_maxw_lam0 = phi.vec[which.max(w_lam0.vec)])
(phi_maxw_lam1 = phi.vec[which.max(w_lam1.vec)])
(phi_maxw_lam1_mu1 = phi.vec[which.max(w_lam1_mu1.vec)])
# I used device 5x7 
ggplot(w.df) + geom_line(aes(x=phi.vec, y=w_lam0.vec)) + geom_line(aes(x=phi.vec, y=w_lam1.vec), color="red") + geom_line(aes(x=phi.vec, y=w_lam1_mu1.vec, linetype="longdashed"), color="blue") +geom_vline(xintercept = phi_bar_lam1, color="black", size=1.1) +geom_vline(xintercept = phi_bar_lam1_mu1, color="blue", size=1.1) + xlab(TeX("Interchage fee $\\phi$")) + ylab("Total welfare") + scale_x_continuous(breaks = seq(0, 0.6, 0.05))  + theme(axis.text.x = element_text(size=rel(1.5), color = "black"))+ theme(axis.text.y = element_text(size=rel(1.5), color = "black")) + theme(axis.title.x = element_text(size = rel(1.5))) + theme(axis.title.y = element_text(size = rel(1.5))) + annotate("text", x=phi_bar_lam0+0.009, y=14.67, label= TeX("$\\bar{\\phi} = 0.50$", output="character") , size = 5, parse=T) + annotate("text", x=phi_bar_lam1_mu1 +0.013, y=14.67, label= TeX("$\\bar{\\phi} = 0.225$", output="character") , size = 5, parse=T)  + theme(legend.position="none") + geom_vline(xintercept = phi_maxw_lam1, linetype="dotted", size=1.1, color="red") + geom_vline(xintercept = phi_maxw_lam1_mu1, linetype="dotted", size=1.1, color="blue") + annotate("text", x=phi_maxw_lam0, y=max(w_lam0.vec), label= TeX("$\\bullet$", output="character") , size = 8, parse=T) + annotate("text", x=phi_maxw_lam1, y=max(w_lam1.vec), label= TeX("$\\bullet$", output="character") , size = 8, color="red", parse=T) + annotate("text", x=phi_maxw_lam1_mu1, y=max(w_lam1_mu1.vec), label= TeX("$\\bullet$", output="character") , size = 8, color="blue", parse=T) + annotate("text", x=0.325, y=14.6868, label= TeX("$W(\\lambda = \\mu = 0)$", output="character"), size = 5, color="black", parse=T) + annotate("text", x=0.325, y=14.653, label= TeX("$W(\\lambda = 0.005 , \\mu = 0)$", output="character"), size = 5, color="red", parse=T)  + annotate("text", x=0.327, y=14.643, label= TeX("$W(\\lambda = 0.005 , \\mu = 0.05)$", output="character"), size = 5, color="blue", parse=T) + annotate("text", x=0.5*(phi_maxw_lam1 + phi_bar_lam1), y=14.658, label= "2(b)", size = 5, color="red", parse=T) + annotate("text", x=0.5*(phi_maxw_lam1_mu1 + phi_bar_lam1_mu1), y=14.6495, label= "2(c)", size = 5, color="blue", parse=T) + geom_segment(x = phi_maxw_lam1, y = 14.657, xend = phi_bar_lam1, yend = 14.657, arrow = arrow(length = unit(0.02, "npc"), ends = "both"), color="red") + geom_segment(x = phi_maxw_lam1_mu1, y = 14.6485, xend = phi_bar_lam1_mu1, yend = 14.6485, arrow = arrow(length = unit(0.02, "npc"), ends = "both"), color="blue") + annotate("text", x=0.55, y=14.685, label= "2(a)", size = 5, color="black", parse=T) + geom_segment(x = 0.53, y = 14.685, xend = phi_maxw_lam0+0.005, yend = max(w_lam0.vec)-0.001, arrow = arrow(length = unit(0.02, "npc")))

