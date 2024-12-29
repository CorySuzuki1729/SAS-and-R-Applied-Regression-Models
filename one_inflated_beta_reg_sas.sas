data medadherence;
    input age gender$ depression diabetes nmeds pdc@@;
    female=(gender='F');
    cards;
    62 M 0 1 2 1.00 73 M 1 0 5 0.96 56 M 0 0 5 0.08
    58 M 0 0 4 0.32 56 F 0 0 3 1.00 62 F 0 0 4 0.86
    31 F 0 1 5 0.62 59 M 0 0 5 0.23 64 M 0 1 3 0.56
    67 F 0 0 4 0.91 59 F 0 0 3 0.39 90 M 1 0 4 0.86
    83 M 0 1 2 1.00 70 F 0 0 2 0.41 58 M 1 0 6 0.36
    61 F 0 0 2 0.85 70 F 1 1 4 0.87 56 M 0 0 3 0.28
    67 F 0 0 3 0.80 60 F 0 0 3 0.16 67 M 0 1 3 1.00
    67 M 1 0 3 0.24 71 M 1 0 2 0.64 67 F 1 1 2 1.00
    46 M 0 0 3 0.48 62 F 0 0 6 0.32 63 M 0 1 1 0.27
    67 F 0 1 4 0.92 83 M 0 0 2 0.83 62 M 0 1 4 0.47
    71 F 0 1 3 1.00 49 F 1 0 5 0.26 66 M 0 0 1 1.00
    61 M 0 0 4 0.51 66 F 0 0 4 0.66 71 M 0 0 5 0.82
    58 F 0 0 3 0.40 70 F 0 1 4 0.88 83 M 0 1 2 1.00
    64 M 0 0 5 0.39
    ;

proc nlmixed;
    parms b0=0.1 b1=0.1 b2=0.1 g0=0.1 g1=0.1 g2=0.1 g3=0.1 phi=0.1;
    pi1 = exp(b0+b1*diabetes+b2*nmeds)/
    (1+exp(b0+b1*diabetes+b2*nmeds));
    mu = exp(g0+g1*age+g2*depression+g3*female)/
    (1+exp(g0+g1*age+g2*depression+g3*female));
    if (pdc=1) then loglikelihood=log(pi1);
    else loglikelihood =log(1-pi1)+lgamma(phi)-lgamma(mu*phi)-
    lgamma((1-mu)*phi)+(mu*phi-1)*log(pdc)+
    ((1-mu)*phi-1)*log(1-pdc);
    model pdc~general(loglikelihood);
run;

proc nlmixed;
    parms b0=0.1 g0=0.1 phi=0.1;
    pi1 = exp(b0)/(1+exp(b0));
    mu = exp(g0)/(1+exp(g0));
    if (pdc=1) then loglikelihood=log(pi1);
    else loglikelihood =log(1-pi1)+lgamma(phi)-lgamma(mu*phi)-
    lgamma((1-mu)*phi-1)+(mu*phi-1)*log(pdc)+
    ((1-mu)*phi-1)*log(1-pdc);
    model pdc~general(loglikelihood);
run;

data deviance_test;
    deviance=36.4-6.4;
    pvalue=1-probchi(deviance, 5);
run;

proc print;
run;

data prediction;
    input age depression diabetes nmeds female;
    cards;
    77 0 1 3 1
    ;

data medadherence;
    set medadherence prediction;
run;

proc nlmixed;
    parms b0=0.1 b1=0.1 b2=0.1 g0=0.1 g1=0.1 g2=0.1 g3=0.1 phi=0.1;
    pi1 = exp(b0+b1*diabetes+b2*nmeds)/
    (1+exp(b0+b1*diabetes+b2*nmeds));
    mu = exp(g0+g1*age+g2*depression+g3*female)/
    (1+exp(g0+g1*age+g2*depression+g3*female));
    if (pdc=1) then loglikelihood=log(pi1);
    else loglikelihood =log(1-pi1)+lgamma(phi)-lgamma(mu*phi)-
    lgamma((1-mu)*phi)+(mu*phi-1)*log(pdc)+
    ((1-mu)*phi-1)*log(1-pdc);
    model pdc~general(loglikelihood);
    predict pi1+(1-pi1)*mu out=outdata;
run;

proc print data=outdata (firstobs=41 obs=41);
    var Pred;
run;
