# intfee_2021_8_16.R fixing an error, now yhat dpends on \lambda (figures for intfee_8.tex).
# intfee_2021_8_10.R plotting only the difference in IF and xhat and yhat all as functions of mu.
# intfee_2021_8_8.R model2 revised with 2 parameters: mu and lambda
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
bch - bmh > beta_c - beta_m # Result 1 =>
# compute maximum mu to satisfy SOC in model 2
(mu_max = (bch - bmh -beta_c + beta_m)/beta_m)

# phi_star = phi_net when mu=lambda=0 (section 3, model 1)
(phi_star_model1 = (bch - bmh + beta_m - beta_c)/2)
#
# check whether Assumption 1 is satisfied
beta_c + beta_m - bch - bmh # > 0 ?
beta_c < bch + bmh +beta_m
beta_m < bch + bmh +beta_c

# values under single mu and lambda
lam = 0.01# setting to 0 yields model1 phi_star
mu = 0.05# setting to 0 yields model1 phi_star
(phi_star_model2 = (bch^2 + 2*bch*(bmh*lam +beta_c*(lam-1)) +bmh^2*(lam+1)*(lam-1) +2*bmh*beta_m*(1-mu) +beta_c^2 -beta_m^2*(mu^2-2*mu +1))/(2*(bch +bmh*(lam+1) -beta_c -beta_m*(1-mu)))) #
# xhat and yhat under optimal phi
(xhat_star_model2 = (bch - phi_star_model2)/beta_c)
(yhat_star_model2 = (bmh*(1-lam) + phi_star_model2)/ beta_m)
#
# card organization fee (=phi_star_lambda @ lam=0)
(phi_net_model2 = (bch - bmh*(1-lam) -beta_c +beta_m*(1-mu))/2)
# xhat and yhat under card org set phi net
(xhat_net_model2 = (bch - phi_net_model2)/beta_c)
(yhat_net_model2 = (bmh*(1-lam) + phi_net_model2)/ beta_m)

## define the above as vectors of mu (given lambda)
#
#(mu.vec = seq(0, 0.01, 0.0001)) # x-axis
(mu.vec = seq(0, 0.04, 0.0002)) # x-axis
#
#(lam.vec = seq(0, 0.02, 0.0001))#

(lam.vec = rep(0.01, length(mu.vec)))# hold constant!!!

(phi_star_model2.vec = (bch^2 + 2*bch*(bmh*lam.vec +beta_c*(lam.vec-1)) +bmh^2*(lam.vec+1)*(lam.vec-1) +2*bmh*beta_m*(1-mu.vec) +beta_c^2 -beta_m^2*(mu.vec^2-2*mu.vec +1))/(2*(bch +bmh*(lam.vec+1) -beta_c -beta_m*(1-mu.vec)))) # 
# xhat and yhat under optimal phi
(xhat_star_model2.vec = (bch - phi_star_model2.vec)/beta_c)
(yhat_star_model2.vec = (bmh*(1-lam.vec) + phi_star_model2.vec)/ beta_m)

# card organization set fee (vector)
(phi_net_model2.vec = (bch - bmh*(1-lam.vec) -beta_c +beta_m*(1-mu.vec))/2)
# xhat and yhat under card org set phi net
(xhat_net_model2.vec = (bch - phi_net_model2.vec)/beta_c)
(yhat_net_model2.vec = (bmh*(1-lam.vec) + phi_net_model2.vec)/ beta_m)

# phi_net.vec - phi_star.vec (gap)
(gap_model2.vec = phi_net_model2 - phi_star_model2.vec)

# create data frame from the above vectors
#phi_model2.df = data.frame(lam.vec, mu.vec, phi_star_model2.vec, phi_net_model2.vec, gap_model2.vec, xhat_star_model2.vec, yhat_star_model2.vec, xhat_net_model2.vec, yhat_net_model2.vec = yhat_net_model2.vec)
#str(phi_model2.df)
#
# phi star versus phi card org plot
#ggplot(phi_model2.df) + geom_line(aes(x=mu.vec, y=phi_star_model2.vec)) +  geom_line(aes(x=mu.vec, y=phi_net_model2.vec)) + geom_line(aes(x=mu.vec, y=phi_star_model2.vec)) + xlab(expression(mu)) + ylab("Positive interchange fees") + scale_x_continuous(breaks = seq(0, 0.2, 0.005)) + scale_y_continuous(breaks = seq(0.1, 0.8, 0.05)) + theme(axis.text.x = element_text(size=rel(1.5)))+ theme(axis.text.y = element_text(size=rel(1.5))) + theme(axis.title.x = element_text(size = rel(1.5))) + theme(axis.title.y = element_text(size = rel(1.5))) + annotate("text", x=0.015, y=0.316, label= TeX("$\\phi^* (optimal)$", output="character"), size = 5, parse=T) + annotate("text", x=0.014, y=0.515, label= TeX("$\\phi_n (card \\phantom{i} organization)$", output="character"), size = 5, parse=T) 

# plotting xhat and yhat under optimum and card org
#ggplot(phi.df) + geom_line(aes(x=lambda, y=xhat_star_lam), linetype = "solid") + geom_line(aes(x=lambda, y=yhat_star_lam), linetype = "solid")+ geom_line(aes(x=lambda, y=xhat_net_lam), linetype = "dashed") + geom_line(aes(x=lambda, y=yhat_net_lam), linetype = "dashed") + annotate("text", x=0.005, y=0.639, label= TeX("$\\hat{x} (optimal)$", output="character"), size = 5, parse=T) + annotate("text", x=0.016, y=0.567, label= TeX("$\\hat{y} (optimal)$", output="character"), size = 5, parse=T) + annotate("text", x=0.0061, y=0.617, label= TeX("$\\hat{x} (card \\phantom{i} organization)$", output="character"), size = 5, parse=T) + annotate("text", x=0.016, y=0.592, label= TeX("$\\hat{y} (card \\phantom{i} organization)$", output="character"), size = 5, parse=T)+ xlab(expression(lambda)) + ylab(TeX("$Optimal \\phantom{y} vs \\phantom{y} card: \\phantom{y} \\hat{x}\\phantom{y} and\\phantom{y} \\hat{y}$")) + scale_x_continuous(breaks = seq(0, 0.2, 0.005)) + theme(axis.text.x = element_text(size=rel(1.5)))+ theme(axis.text.y = element_text(size=rel(1.5))) + theme(axis.title.x = element_text(size = rel(1.5))) + theme(axis.title.y = element_text(size = rel(1.5)))

#+ scale_y_continuous(breaks = seq(0.55, 0.655, 0.0))

## Start graph of gap as function of mu for 3 values of (lam.vec = seq(0, 0.02, 0.0001))
lam_0.vec = rep(0.00, length(lam.vec))# hold constant
lam_1.vec = rep(0.003, length(lam.vec))# hold constant
lam_2.vec = rep(0.005, length(lam.vec))# hold constant
# 
mu.vec = seq(0, 0.04, 0.0002) # x-axis
#(mu.vec = seq(0, 0.04, 0.0002)) # x-axis
#
#(phi_net_model2.vec = (bch - bmh -beta_c +beta_m*(1-mu.vec))/2)# not a function of lambda
#
#gap_model2_1.vec = phi_net_model2.vec - phi_star_model2_1.vec
#
#gap_model2_2.vec = phi_net_model2.vec - phi_star_model2_2.vec
# use equation (16) directly from paper
gap_model2_0.vec = (beta_c*lam_0.vec* (bch + bmh)/(beta_c + beta_m*(1-mu.vec) - bch - bmh*(lam_0.vec+1)))
#
gap_model2_1.vec = (beta_c*lam_1.vec* (bch + bmh)/(beta_c + beta_m*(1-mu.vec) - bch - bmh*(lam_1.vec+1)))
#
gap_model2_2.vec = (beta_c*lam_2.vec* (bch + bmh)/(beta_c + beta_m*(1-mu.vec) - bch - bmh*(lam_2.vec+1)))
#
# create data frame from the above vectors
gap_model2.df = data.frame(lam_0.vec, lam_1.vec, lam_2.vec, mu.vec, gap_model2_1.vec, gap_model2_2.vec)
str(gap_model2.df)
# graph it (2 curves)
ggplot(gap_model2.df)+ geom_line(aes(x=mu.vec, y=gap_model2_0.vec), linetype = "solid") + geom_line(aes(x=mu.vec, y=gap_model2_1.vec), linetype = "solid") + geom_line(aes(x=mu.vec, y=gap_model2_2.vec), linetype = "solid") + xlab(expression(mu)) + ylab("Interchange fee differences") + scale_x_continuous(breaks = seq(0, 0.2, 0.01))  + theme(axis.text.x = element_text(size=rel(1.5)))+ theme(axis.text.y = element_text(size=rel(1.5))) + theme(axis.title.x = element_text(size = rel(1.5))) + theme(axis.title.y = element_text(size = rel(1.5))) + annotate("text", x=0.02, y=0.008, label= TeX("$(\\bar{\\phi} - \\phi^*) \\phantom{xx} (given \\phantom{i}\\lambda = 0)$", output="character"), size = 5, parse=T) + annotate("text", x=0.012, y=0.048, label= TeX("$(\\bar{\\phi} - \\phi^*) \\phantom{xx} (given \\phantom{i}\\lambda = 0.003)$", output="character"), size = 5, parse=T) + annotate("text", x=0.025, y=0.089, label= TeX("$(\\bar{\\phi}-\\phi^*) \\phantom{xx} (given \\phantom{i}\\lambda = 0.005)$", output="character"), size = 5, parse=T) 

#+ scale_y_continuous(breaks = seq(0.1, 0.8, 0.05))

## plotting xhat and yhat under optimum and card org using lam_1.vec
#
# (xhat_star_model2.vec = (bch - phi_star_model2.vec)/beta_c)
# (yhat_star_model2.vec = (bmh + phi_star_model2.vec)/ beta_m)
# #
# (xhat_net_model2.vec = (bch - phi_net_model2.vec)/beta_c)
# (yhat_net_model2.vec = (bmh + phi_net_model2.vec)/ beta_m)
# Construct a dataframe from the above
xy.df = data.frame(xhat_star_model2.vec, yhat_star_model2.vec, xhat_net_model2.vec, yhat_net_model2.vec)
str(xy.df)
#
ggplot(xy.df) + geom_line(aes(x=mu.vec, y=xhat_star_model2.vec), linetype = "solid") + geom_line(aes(x=mu.vec, y=yhat_star_model2.vec), linetype = "solid")+ geom_line(aes(x=mu.vec, y=xhat_net_model2.vec), linetype = "dashed") + geom_line(aes(x=mu.vec, y=yhat_net_model2.vec), linetype = "dashed") + annotate("text", x=0.005, y=0.65, label= TeX("$\\hat{x} \\phantom{i} (optimal)$", output="character"), size = 5, parse=T) + annotate("text", x=0.005, y=0.56, label= TeX("$\\hat{y} \\phantom{i} (optimal)$", output="character"), size = 5, parse=T) + annotate("text", x=0.02, y=0.62, label= TeX("$\\hat{x} \\phantom{i} (card \\phantom{i} organization)$", output="character"), size = 5, parse=T) + annotate("text", x=0.02, y=0.59, label= TeX("$\\hat{y} \\phantom{i} (card \\phantom{i} organization)$", output="character"), size = 5, parse=T)+ xlab(expression(mu)) + ylab(TeX("$Optimal \\phantom{y} vs \\phantom{y} card: \\phantom{y} \\hat{x}\\phantom{y} and\\phantom{y} \\hat{y}$"))  + scale_x_continuous(breaks = seq(0, 0.2, 0.01)) + theme(axis.text.x = element_text(size=rel(1.5)))+ theme(axis.text.y = element_text(size=rel(1.5))) + theme(axis.title.x = element_text(size = rel(1.5))) + theme(axis.title.y = element_text(size = rel(1.5)))

#+ scale_y_continuous(breaks = seq(0.55, 0.655, 0.0))

### intfee_2021_11_20.R Change to differences in x and y instead of x and y (right panel of Figure 3 in paper) => NOT USED!
names(xy.df)
xy.df$xdiff.vec = xy.df$xhat_net_model2.vec - xy.df$xhat_star_model2.vec
#
xy.df$ydiff.vec = xy.df$yhat_net_model2.vec - xy.df$yhat_star_model2.vec
#
ggplot(xy.df) + geom_line(aes(x=mu.vec, y=xdiff.vec), linetype = "solid") + geom_line(aes(x=mu.vec, y=ydiff.vec), linetype = "dashed") 





### start figure interchange fee for negative fees
## set parameters to generate positive IF
# bch_neg = 6
# bmh_neg = 8
# beta_c_neg = 11
# beta_m_neg = 12
# bch_neg - bmh_neg < beta_c_neg - beta_m_neg # Result 1 => phi_star < 0
# (phi_star_no_lam_neg = (bch_neg - bmh_neg + beta_m_neg - beta_c_neg)/2)
# #
# # check whether Assumption 1 is satisfied
# beta_c_neg + beta_m_neg - bch_neg - bmh_neg # > 0 ?
# beta_c_neg < bch_neg + bmh_neg +beta_m_neg
# beta_m_neg < bch_neg + bmh_neg +beta_c_neg
# 
# #
# (lam_neg = seq(0, 0.02, 0.0001)) # x-axis
# (phi_star_lam_neg = (bch_neg^2 + 2*bch_neg*(bmh_neg*lam_neg + beta_c_neg*(lam_neg-1)) + bmh_neg^2*(lam_neg+1)*(lam_neg-1) +2* bmh_neg*beta_m_neg +beta_c_neg^2 - beta_m_neg^2)/(2*(bch_neg + bmh_neg*(1+lam_neg) -beta_c_neg - beta_m_neg) ))
# # xhat and yhat under optimal phi
# (xhat_star_lam_neg = (bch_neg - phi_star_lam_neg)/beta_c_neg)
# (yhat_star_lam_neg = (bmh_neg*(1-lam_neg) + phi_star_lam_neg)/ beta_m_neg)
# #
# # card organization fee (=phi_star_lambda @ lam=0)
# (phi_net_lam_neg = (bch_neg - bmh_neg*(1-lam_neg)+beta_m_neg - beta_c_neg)/2)
# # xhat and yhat under card org set phi net
# (xhat_net_lam_neg = (bch_neg - phi_net_lam_neg)/beta_c_neg)
# (yhat_net_lam_neg = (bmh_neg*(1-lam_neg) + phi_net_lam_neg)/ beta_m_neg)
# 
# # create data frame (negative IF)
# phi_neg.df = data.frame(lambda_neg = lam_neg, phi_star_neg = phi_star_lam_neg, phi_net_neg = phi_net_lam_neg, xhat_star_neg = xhat_star_lam_neg, yhat_star_neg = yhat_star_lam_neg,  xhat_net_neg = xhat_net_lam_neg, yhat_net_neg = yhat_net_lam_neg)
# str(phi_neg.df)
# 
# # phi star versus phi card org plot (negative IF)
# ggplot(phi_neg.df) + geom_line(aes(x=lambda_neg, y=phi_star_neg)) +  geom_line(aes(x=lambda_neg, y=phi_net_neg)) + geom_line(aes(x=lambda_neg, y=phi_star_neg)) + xlab(expression(lambda)) + ylab("Negative interchange fees") + scale_x_continuous(breaks = seq(0, 0.2, 0.005))  + theme(axis.text.x = element_text(size=rel(1.5)))+ theme(axis.text.y = element_text(size=rel(1.5))) + theme(axis.title.x = element_text(size = rel(1.5))) + theme(axis.title.y = element_text(size = rel(1.5))) + annotate("text", x=0.011, y=-0.605, label= TeX("$\\phi^* (optimal)$", output="character"), size = 5, parse=T) + annotate("text", x=0.014, y=-0.475, label= TeX("$\\phi_n (card \\phantom{i} organization)$", output="character"), size = 5, parse=T) + scale_y_continuous(breaks = seq(-0.3, -0.9, -0.05))
# 
# 
# ## plotting xhat_neg and yhat_neg under optimum and card org
# ggplot(phi_neg.df) + geom_line(aes(x=lambda_neg, y=xhat_star_lam_neg), linetype = "solid") + geom_line(aes(x=lambda_neg, y=yhat_star_lam_neg), linetype = "solid")+ geom_line(aes(x=lambda_neg, y=xhat_net_lam_neg), linetype = "dashed") + geom_line(aes(x=lambda_neg, y=yhat_net_lam_neg), linetype = "dashed") + annotate("text", x=0.008, y=0.616, label= TeX("$\\hat{y} (optimal)$", output="character"), size = 5, parse=T) + annotate("text", x=0.005, y=0.601, label= TeX("$\\hat{x} (optimal)$", output="character"), size = 5, parse=T) + annotate("text", x=0.010, y=0.625, label= TeX("$\\hat{y} (card \\phantom{i} organization)$", output="character"), size = 5, parse=T) + annotate("text", x=0.008, y=0.591, label= TeX("$\\hat{x} (card \\phantom{i} organization)$", output="character"), size = 5, parse=T)+ xlab(expression(lambda)) + ylab(TeX("$Optimal \\phantom{y} vs \\phantom{y} card: \\phantom{y} \\hat{y}\\phantom{y} and\\phantom{y} \\hat{y}$")) + scale_x_continuous(breaks = seq(0, 0.2, 0.005)) + theme(axis.text.x = element_text(size=rel(1.5)))+ theme(axis.text.y = element_text(size=rel(1.5))) + theme(axis.title.x = element_text(size = rel(1.5))) + theme(axis.title.y = element_text(size = rel(1.5)))+ scale_y_continuous(breaks = seq(0.57, 0.63, 0.005))
# 
# ### Start constructing data frame on interchange fees EU & US
# 
# (region_card_type = c("EU (all)", "US (MC)", "US (MC prem)", "US (Visa)", "US (Visa prem)"))
# #
# (in_person = c(0.3, 1.502, 2.125, 1.185, 1.65))
# #
# (e_commerce = c(0.3, 1.78, 2.5, 1.8, 2.4))
# #
# (grocery = c(0.3, 1.315, 1.575, 1.325, 1.7))
# #
# (gas = c(0.3, 1.9, 2, 1.15, 1.15))
# # 
# # make credit data frame
# (credit1.df = data.frame(Region_brand =region_card_type, In_person = in_person, E_commerce = e_commerce, Grocery = grocery, Gas = gas))
# #
# # make it a LaTeX Table
# library(xtable)
# #(digitm = matrix(c(rep(rep(0,4), 2), rep(rep(2,4), 4), rep(rep(1,4), 3), rep(rep(0,4), 6), rep(rep(1,4), 2), rep(rep(0,4),1), rep(rep(1,4), 6), rep(rep(2,4), 3), rep(rep(1,4), 1)), nrow = 28, ncol = 4, byrow = T))
# #
# print(xtable(credit1.df, digits = 2), include.rownames = F, hline.after = c(0))
