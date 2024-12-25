data smoking;
    input gender$ health$ age cigarettes @@;
    cards;
    M good 34 3 F excellent 48 1 M excellent 26 0 M good 39 0
    F good 27 1 M good 28 5 F good 44 1 M excellent 30 0
    F excellent 26 0 F good 38 2 F good 40 1 F excellent 31 0
    M good 27 3 F excellent 34 1 F good 36 2 F excellent 34 2
    F excellent 39 0 F good 42 1 F good 48 4 M good 32 5
    M good 47 2 M good 29 3 M excellent 38 0 F good 50 4
    M good 30 3 M good 38 2 M good 31 6 F excellent 33 0
    F good 28 0 F good 42 3 M excellent 28 0 M good 31 2
    F excellent 31 0 F excellent 42 0 F good 44 4 F good 39 1
    M good 40 6 M good 39 3 M excellent 25 0 F good 45 2
    ;

proc genmod;
    class gender(ref='F') health(ref='good');
    model cigarettes=gender age/dist=zip;
    zeromodel health;
run;

proc genmod;
    model cigarettes=/dist=zip;
    zeromodel;
run;

data deviance_test;
    deviance=-2*(-71.3892-(-57.0406));
    pvalue=1-probchi(deviance, 3);
run;

proc print;
run;

data prediction;
    input gender$ health$ age;
    cards;
    M good 50
    ;

data smoking;
    set smoking prediction;
run;

proc genmod;
    class gender(ref='F') health(ref='good');
    model cigarettes=gender age/dist=zip;
    zeromodel health;
    output out=outdata p=pred_cig;
run;

proc print data=outdata(firstobs=41 obs=41);
    var pred_cig;
run;