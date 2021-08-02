# intfee_2021_7_29.R [contains (1) simulations for subsection 4.2, and (2) charts for section 2 (facts)]
#
library(ggplot2); theme_set(theme_bw())
library(latex2exp)# LaTeX expressions in ggplot

setwd("~/Papers/intfee/intfee_coding")

### simulations for subsection 4.2
## set parameters to generate positive IF
bch = 8
bmh = 6
beta_c = 12
beta_m = 11
bch - bmh > beta_c - beta_m # Result 1 => phi_star > 0
(phi_star_no_lam = (bch - bmh + beta_m - beta_c)/2)
#
# check whether Assumption 1 is satisfied
beta_c + beta_m - bch - bmh # > 0 ?
beta_c < bch + bmh +beta_m
beta_m < bch + bmh +beta_c

#
#lam=0 # to test whether phi_star_lambda == phi_star_no_lambda = optimal IF as function of lambda
(lam = seq(0, 0.02, 0.0001)) # x-axis
(phi_star_lam = (bch^2 + 2*bch*(bmh*lam + beta_c*(lam-1)) + bmh^2*(lam+1)*(lam-1) +2* bmh*beta_m +beta_c^2 - beta_m^2)/(2*(bch + bmh*(1+lam) -beta_c - beta_m) ))
# xhat and yhat under optimal phi
(xhat_star_lam = (bch - phi_star_lam)/beta_c)
(yhat_star_lam = (bmh*(1-lam) + phi_star_lam)/ beta_m)
#
# card organization fee (=phi_star_lambda @ lam=0)
(phi_net_lam = (bch - bmh*(1-lam)+beta_m - beta_c)/2)
# xhat and yhat under card org set phi net
(xhat_net_lam = (bch - phi_net_lam)/beta_c)
(yhat_net_lam = (bmh*(1-lam) + phi_net_lam)/ beta_m)

# create data frame
phi.df = data.frame(lambda = lam, phi_star = phi_star_lam, phi_net = phi_net_lam, xhat_star = xhat_star_lam, yhat_star = yhat_star_lam, xhat_net = xhat_net_lam, yhat_net = yhat_net_lam)
str(phi.df)
#
# phi star versus phi card org plot
ggplot(phi.df) + geom_line(aes(x=lambda, y=phi_star)) +  geom_line(aes(x=lambda, y=phi_net)) + geom_line(aes(x=lambda, y=phi_star)) + xlab(expression(lambda)) + ylab("Positive interchange fees") + scale_x_continuous(breaks = seq(0, 0.2, 0.005)) + scale_y_continuous(breaks = seq(0.1, 0.8, 0.05)) + theme(axis.text.x = element_text(size=rel(1.5)))+ theme(axis.text.y = element_text(size=rel(1.5))) + theme(axis.title.x = element_text(size = rel(1.5))) + theme(axis.title.y = element_text(size = rel(1.5))) + annotate("text", x=0.015, y=0.316, label= TeX("$\\phi^* (optimal)$", output="character"), size = 5, parse=T) + annotate("text", x=0.014, y=0.515, label= TeX("$\\phi_n (card \\phantom{i} organization)$", output="character"), size = 5, parse=T) 

# plotting xhat and yhat under optimum and card org
ggplot(phi.df) + geom_line(aes(x=lambda, y=xhat_star_lam), linetype = "solid") + geom_line(aes(x=lambda, y=yhat_star_lam), linetype = "solid")+ geom_line(aes(x=lambda, y=xhat_net_lam), linetype = "dashed") + geom_line(aes(x=lambda, y=yhat_net_lam), linetype = "dashed") + annotate("text", x=0.005, y=0.639, label= TeX("$\\hat{x} (optimal)$", output="character"), size = 5, parse=T) + annotate("text", x=0.016, y=0.567, label= TeX("$\\hat{y} (optimal)$", output="character"), size = 5, parse=T) + annotate("text", x=0.0061, y=0.617, label= TeX("$\\hat{x} (card \\phantom{i} organization)$", output="character"), size = 5, parse=T) + annotate("text", x=0.016, y=0.592, label= TeX("$\\hat{y} (card \\phantom{i} organization)$", output="character"), size = 5, parse=T)+ xlab(expression(lambda)) + ylab(TeX("$Optimal \\phantom{y} vs \\phantom{y} card: \\phantom{y} \\hat{x}\\phantom{y} and\\phantom{y} \\hat{y}$")) + scale_x_continuous(breaks = seq(0, 0.2, 0.005)) + theme(axis.text.x = element_text(size=rel(1.5)))+ theme(axis.text.y = element_text(size=rel(1.5))) + theme(axis.title.x = element_text(size = rel(1.5))) + theme(axis.title.y = element_text(size = rel(1.5)))

#+ scale_y_continuous(breaks = seq(0.55, 0.655, 0.0))

### start figure interchange fee for negative fees
## set parameters to generate positive IF
bch_neg = 6
bmh_neg = 8
beta_c_neg = 11
beta_m_neg = 12
bch_neg - bmh_neg < beta_c_neg - beta_m_neg # Result 1 => phi_star < 0
(phi_star_no_lam_neg = (bch_neg - bmh_neg + beta_m_neg - beta_c_neg)/2)
#
# check whether Assumption 1 is satisfied
beta_c_neg + beta_m_neg - bch_neg - bmh_neg # > 0 ?
beta_c_neg < bch_neg + bmh_neg +beta_m_neg
beta_m_neg < bch_neg + bmh_neg +beta_c_neg

#
(lam_neg = seq(0, 0.02, 0.0001)) # x-axis
(phi_star_lam_neg = (bch_neg^2 + 2*bch_neg*(bmh_neg*lam_neg + beta_c_neg*(lam_neg-1)) + bmh_neg^2*(lam_neg+1)*(lam_neg-1) +2* bmh_neg*beta_m_neg +beta_c_neg^2 - beta_m_neg^2)/(2*(bch_neg + bmh_neg*(1+lam_neg) -beta_c_neg - beta_m_neg) ))
# xhat and yhat under optimal phi
(xhat_star_lam_neg = (bch_neg - phi_star_lam_neg)/beta_c_neg)
(yhat_star_lam_neg = (bmh_neg*(1-lam_neg) + phi_star_lam_neg)/ beta_m_neg)
#
# card organization fee (=phi_star_lambda @ lam=0)
(phi_net_lam_neg = (bch_neg - bmh_neg*(1-lam_neg)+beta_m_neg - beta_c_neg)/2)
# xhat and yhat under card org set phi net
(xhat_net_lam_neg = (bch_neg - phi_net_lam_neg)/beta_c_neg)
(yhat_net_lam_neg = (bmh_neg*(1-lam_neg) + phi_net_lam_neg)/ beta_m_neg)

# create data frame (negative IF)
phi_neg.df = data.frame(lambda_neg = lam_neg, phi_star_neg = phi_star_lam_neg, phi_net_neg = phi_net_lam_neg, xhat_star_neg = xhat_star_lam_neg, yhat_star_neg = yhat_star_lam_neg,  xhat_net_neg = xhat_net_lam_neg, yhat_net_neg = yhat_net_lam_neg)
str(phi_neg.df)

# phi star versus phi card org plot (negative IF)
ggplot(phi_neg.df) + geom_line(aes(x=lambda_neg, y=phi_star_neg)) +  geom_line(aes(x=lambda_neg, y=phi_net_neg)) + geom_line(aes(x=lambda_neg, y=phi_star_neg)) + xlab(expression(lambda)) + ylab("Negative interchange fees") + scale_x_continuous(breaks = seq(0, 0.2, 0.005))  + theme(axis.text.x = element_text(size=rel(1.5)))+ theme(axis.text.y = element_text(size=rel(1.5))) + theme(axis.title.x = element_text(size = rel(1.5))) + theme(axis.title.y = element_text(size = rel(1.5))) + annotate("text", x=0.011, y=-0.605, label= TeX("$\\phi^* (optimal)$", output="character"), size = 5, parse=T) + annotate("text", x=0.014, y=-0.475, label= TeX("$\\phi_n (card \\phantom{i} organization)$", output="character"), size = 5, parse=T) + scale_y_continuous(breaks = seq(-0.3, -0.9, -0.05))


## plotting xhat_neg and yhat_neg under optimum and card org
ggplot(phi_neg.df) + geom_line(aes(x=lambda_neg, y=xhat_star_lam_neg), linetype = "solid") + geom_line(aes(x=lambda_neg, y=yhat_star_lam_neg), linetype = "solid")+ geom_line(aes(x=lambda_neg, y=xhat_net_lam_neg), linetype = "dashed") + geom_line(aes(x=lambda_neg, y=yhat_net_lam_neg), linetype = "dashed") + annotate("text", x=0.008, y=0.616, label= TeX("$\\hat{y} (optimal)$", output="character"), size = 5, parse=T) + annotate("text", x=0.005, y=0.601, label= TeX("$\\hat{x} (optimal)$", output="character"), size = 5, parse=T) + annotate("text", x=0.010, y=0.625, label= TeX("$\\hat{y} (card \\phantom{i} organization)$", output="character"), size = 5, parse=T) + annotate("text", x=0.008, y=0.591, label= TeX("$\\hat{x} (card \\phantom{i} organization)$", output="character"), size = 5, parse=T)+ xlab(expression(lambda)) + ylab(TeX("$Optimal \\phantom{y} vs \\phantom{y} card: \\phantom{y} \\hat{y}\\phantom{y} and\\phantom{y} \\hat{y}$")) + scale_x_continuous(breaks = seq(0, 0.2, 0.005)) + theme(axis.text.x = element_text(size=rel(1.5)))+ theme(axis.text.y = element_text(size=rel(1.5))) + theme(axis.title.x = element_text(size = rel(1.5))) + theme(axis.title.y = element_text(size = rel(1.5)))+ scale_y_continuous(breaks = seq(0.57, 0.63, 0.005))

### Start constructing data frame on interchange fees EU & US

(region_card_type = c("EU (all)", "US (MC)", "US (MC prem)", "US (Visa)", "US (Visa prem)"))
#
(in_person = c(0.3, 1.502, 2.125, 1.185, 1.65))
#
(e_commerce = c(0.3, 1.78, 2.5, 1.8, 2.4))
#
(grocery = c(0.3, 1.315, 1.575, 1.325, 1.7))
#
(gas = c(0.3, 1.9, 2, 1.15, 1.15))
# 
# make credit data frame
(credit1.df = data.frame(Region_brand =region_card_type, In_person = in_person, E_commerce = e_commerce, Grocery = grocery, Gas = gas))
#
# make it a LaTeX Table
library(xtable)
#(digitm = matrix(c(rep(rep(0,4), 2), rep(rep(2,4), 4), rep(rep(1,4), 3), rep(rep(0,4), 6), rep(rep(1,4), 2), rep(rep(0,4),1), rep(rep(1,4), 6), rep(rep(2,4), 3), rep(rep(1,4), 1)), nrow = 28, ncol = 4, byrow = T))
#
print(xtable(credit1.df, digits = 2), include.rownames = F, hline.after = c(0))
