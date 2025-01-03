data oral_hygiene;
    length choice$ 9.;
    input ID gender$ age nteeth choice$ @@;
    cards;
    1 F 48 7 sensitive 2 M 30 5 cavity
    3 M 34 6 tartar 4 M 50 8 sensitive
    5 M 68 4 tartar 6 F 22 1 cavity
    7 M 53 8 sensitive 8 F 38 2 cavity
    9 M 36 7 sensitive 10 F 25 1 cavity
    11 F 32 7 sensitive 12 F 54 2 tartar
    13 M 32 8 sensitive 14 M 26 3 cavity
    15 F 35 2 cavity 16 F 33 8 cavity
    17 F 52 4 tartar 18 F 43 9 sensitive
    19 M 58 2 tartar 20 F 43 3 cavity
    21 F 60 6 tartar 22 M 28 3 tartar
    23 M 70 10 sensitive 24 M 41 2 tartar
    25 F 43 5 cavity 26 M 18 1 cavity
    27 M 66 12 sensitive 28 M 34 2 sensitive
    ;

proc logistic;
    class gender(ref='M')/param=ref;
    model choice=gender age nteeth/link=glogit;
run;

data deviance_test;
    deviance=61.229-25.370;
    pvalue=1-probchi(deviance, 3);
run;

proc print;
run;

data prediction;
    input gender$ age nteeth;
    cards;
    M 49 7
    ;

data oral_hygiene;
    set oral_hygiene prediction;
run;

proc logistic;
    class gender(ref='M')/param=ref;
    model choice=gender age nteeth/link=glogit;
    output out=outdata p=pred_prob;
run;

proc print data=outdata (firstobs=85 obs=87);
    var _level_ pred_prob;
run;

