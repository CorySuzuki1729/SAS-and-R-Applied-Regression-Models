data assignment;
    input grade gender$ mathscore propassign @@;
    female=(gender='F');
    cards;
    5 M 87.5 0.62 7 F 68.4 0.00 8 F 100.0 0.95 6 M 84.5 0.50
    7 F 76.1 1.00 5 M 87.3 0.32 6 M 66.9 0.80 5 F 91.5 1.00
    7 M 90.6 0.45 6 M 85.9 0.27 7 M 97.1 0.55 6 F 104.9 0.85
    8 F 98.2 1.00 5 F 103.8 0.95 5 F 80.2 0.97 8 M 56.1 0.00
    7 M 68.1 0.00 7 M 76.3 0.70 6 M 97.3 0.30 8 F 66.3 1.00
    7 M 79.0 0.00 6 M 73.9 0.46 8 M 77.3 0.22 7 M 80.2 0.72
    7 M 77.1 1.00 8 F 103.1 1.00 7 M 104.8 1.00 7 F 55.7 0.00
    7 M 69.4 0.67 5 F 83.6 0.68 8 F 81.4 1.00 7 M 84.7 0.59
    6 F 92.1 0.51 5 M 97.8 0.65 7 M 88.2 0.00 8 F 69.5 0.96
    6 M 94.8 0.21 7 M 84.7 0.35 5 M 67.7 0.00 7 F 100.3 1.00
    8 M 104.2 0.92 7 F 77.2 0.20 7 M 70.9 0.73 6 M 96.7 0.62
    ;

proc nlmixed;
    parms b0=0.1 b1=0.1 g0=0.1 g1=0.1 z0=0.1 z1=0.1 phi=0.1;
    mu = exp(b0+b1*female)/(1+exp(b0+b1*female));
    nu = exp(g0+g1*mathscore);
    tau = exp(z0+z1*grade);
    pi0=nu/(1+nu+tau);
    pi1=tau/(1+nu+tau);
    if (propassign=0) then loglikelihood=log(pi0);
    if (propassign=1) then loglikelihood=log(pi1);
    if (propassign>0 and propassign<1) then
    loglikelihood=log(1-pi0-pi1)+lgamma(phi)-lgamma(mu*phi)-
    lgamma((1-mu)*phi)+(mu*phi-1)*log(propassign)+
    ((1-mu)*phi-1)*log(1-propassign);
    model propassign~general(loglikelihood);
run;

proc nlmixed;
    parms b0=0.1 g0=0.1 z0=0.1 phi=0.1;
    mu = exp(b0)/(1+exp(b0));
    nu = exp(g0);
    tau = exp(z0);
    pi0 = nu/(1+nu+tau);
    pi1 = tau/(1+nu+tau);
    if (propassign=0) then loglikelihood=log(pi0);
    if (propassign=1) then loglikelihood=log(pi1);
    if (propassign>0 and propassign<1) then
    loglikelihood = log(1-pi0-pi1)+lgamma(phi)-lgamma(mu*phi)-
    lgamma((1-mu)*phi)+(mu*phi-1)*log(propassign)+
    ((1-mu)*phi-1)*log(1-propassign);
    model propassign~general(loglikelihood);
run;

data deviance_test;
    deviance=73.3-47.8;
    pvalue = 1-probchi(deviance, 3);
run;

proc print;
run;

data prediction;
    input grade mathscore female;
    cards;
    8 101 0
    ;

data assignment;
    set assignment prediction;
run;

proc nlmixed;
    parms b0=0.1 b1=0.1 g0=0.1 g1=0.1 z0=0.1 z1=0.1 phi=0.1;
    mu = exp(b0+b1*female)/(1+exp(b0+b1*female));
    nu = exp(g0+g1*mathscore);
    tau = exp(z0+z1*grade);
    pi0=nu/(1+nu+tau);
    pi1=tau/(1+nu+tau);
    if (propassign=0) then loglikelihood=log(pi0);
    if (propassign=1) then loglikelihood=log(pi1);
    if (propassign>0 and propassign<1) then
    loglikelihood=log(1-pi0-pi1)+lgamma(phi)-lgamma(mu*phi)-
    lgamma((1-mu)*phi)+(mu*phi-1)*log(propassign)+
    ((1-mu)*phi-1)*log(1-propassign);
    model propassign~general(loglikelihood);
    predict (tau+mu)/(1+nu+tau) out=outdata;
run;

proc print data=outdata (firstobs=45 obs=45);
    var Pred;
run;