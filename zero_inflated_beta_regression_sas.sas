data transport;
    input ntrips nbiked status$ gender$ parking distance @@;
    propbiked=nbiked/ntrips;
    faculty=(status='faculty');
    staff=(status='staff');
    male=(gender='M');
    cards;
    26 6 student F 6 7 13 0 faculty M 9 15 17 0 faculty M 9 31
    15 0 faculty M 9 9 17 0 student M 6 34 20 13 staff F 6 3
    17 0 student F 9 17 14 0 faculty F 9 10 26 7 student M 6 3
    8 5 faculty F 12 3 17 0 student F 12 22 18 0 student M 12 8
    15 0 staff F 12 4 17 7 faculty F 12 5 20 15 staff M 9 6
    8 0 student F 6 5 26 9 student M 9 3 12 11 faculty F 6 12
    13 5 student M 9 7 8 6 faculty M 9 5 9 0 student M 9 7
    12 2 faculty F 9 8 11 0 student F 6 13 8 1 staff F 9 16
    14 0 faculty M 12 35 9 2 staff F 12 8 19 12 student M 6 5
    20 0 faculty M 12 60 28 11 staff M 9 1 10 0 faculty F 6 30
    14 4 staff M 12 4 8 5 student F 6 2 9 0 staff F 12 16
    12 0 faculty M 12 15 12 0 student F 9 25 21 19 faculty M 9 3
    23 22 faculty F 6 14 27 22 faculty M 9 6 14 3 student F 6 12
    15 9 faculty M 12 3 8 2 student F 9 6 15 5 faculty F 12 6
    12 10 student F 6 3 8 5 faculty M 9 4 14 4 student F 6 2 
    15 0 student M 6 10 12 7 student F 6 6 23 7 staff M 12 2
    18 13 student F 6 2 9 0 student M 6 16 10 9 faculty M 9 6
    8 1 staff M 12 7 12 2 student F 6 12 14 0 staff M 12 7
    8 7 student F 12 12 16 14 student M 6 2 11 4 faculty F 12 5
    10 0 faculty F 12 22 16 0 staff M 12 4 10 0 faculty M 12 11
    ;

proc nlmixed;
    parms b0=0.1 b1=0.1 b2=0.1 g0=0.1 g1=0.1 g2=0.1 g3=0.1 phi=0.1;
    pi0 = exp(b0+b1*male+b2*distance)/(1+exp(b0+b1*male+b2*distance));
    mu = exp(g0+g1*faculty+g2*staff+g3*parking)/(1+exp(g0+g1*faculty+g2*staff+g3*parking));
    if (propbiked=0) then loglikelihood=log(pi0);
    else loglikelihood =log(1-pi0)+lgamma(phi)-lgamma(mu*phi)-
    lgamma((1-mu)*phi)+(mu*phi-1)*log(propbiked)+
    ((1-mu)*phi-1)*log(1-propbiked);
    model propbiked~general(loglikelihood);
run;

proc nlmixed;
    parms b0=0.1 g0=0.1 phi=0.1;
    pi0 = exp(b0)/(1+exp(b0));
    mu = exp(g0)/(1+exp(g0));
    if (propbiked=0) then loglikelihood=log(pi0);
    else loglikelihood =log(1-pi0)+lgamma(phi)-lgamma(mu*phi)-
    lgamma((1-mu)*phi)+(mu*phi-1)*log(propbiked)+
    ((1-mu)*phi-1)*log(1-propbiked);
    model propbiked~general(loglikelihood);
run;

data deviance_test;
    deviance=77.5-33.3;
    pvalue=1-probchi(deviance, 5);
run;

proc print;
run;

data prediction;
    input parking distance faculty staff male;
    cards;
    6 3 0 0 0
    ;

data transport;
    set transport prediction;
run;

proc nlmixed;
    parms b0=0.1 b1=0.1 b2=0.1 g0=0.1 g1=0.1 g2=0.1 g3=0.1 phi=0.1;
    pi0 = exp(b0+b1*male+b2*distance)/(1+exp(b0+b1*male+b2*distance));
    mu = exp(g0+g1*faculty+g2*staff+g3*parking)/(1+exp(g0+g1*faculty+g2*staff+g3*parking));
    if (propbiked=0) then loglikelihood=log(pi0);
    else loglikelihood =log(1-pi0)+lgamma(phi)-lgamma(mu*phi)-
    lgamma((1-mu)*phi)+(mu*phi-1)*log(propbiked)+
    ((1-mu)*phi-1)*log(1-propbiked);
    model propbiked~general(loglikelihood);
    predict (1-pi0)*mu out=outdata;
run;

proc print data=outdata (firstobs=61 obs=61);
    var Pred;
run;

