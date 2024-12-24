data hospital_stay;
    input days gender$ age illness$ @@;
    cards;

    1 F 31 yes 0 F 28 no 0 M 52 yes
    1 M 72 yes 0 F 29 no 0 F 30 no
    1 M 74 no 2 M 30 yes 2 F 72 no
    1 M 58 no 2 F 28 no 2 F 65 no
    2 M 65 no 1 M 52 no 4 M 51 no
    2 F 63 no 0 F 31 no 1 F 47 yes
    1 M 49 no 2 M 71 yes 2 M 48 no
    2 F 47 no 0 F 31 no 3 M 44 yes
    3 M 44 no 3 M 54 yes 4 F 72 yes
    4 M 56 yes 3 F 73 yes 1 F 46 no
    3 M 58 no 4 M 70 yes 2 M 36 no
    1 M 50 no 1 M 59 no 0 M 52 no
    6 M 68 yes 2 F 41 no 1 M 31 yes
    1 M 69 no 3 M 73 no 3 F 77 yes
    2 F 54 no 4 M 69 yes 5 M 68 yes
    ;

proc genmod;
    class gender(ref='F') illness(ref='no');
    model days=gender age illness/dist=poisson link=log;
run;

proc genmod data=hospital_stay;
    model days=/dist=poisson link=log;
run;

data deviance_test;
    deviance=-2*(-77.0978-(-68.2139));
    pvalue=1-probchi(deviance, 3);
run;

proc print;
run;

data prediction;
    input gender$ age illness$;
    cards;
    M 55 no
    ;

data hospital_stay;
    set hospital_stay prediction;
run;

proc genmod;
    class gender(ref='F') illness(ref='no');
    model days=gender age illness
    /dist=poisson link=log;
    output out=outdata p=pred_days;
run;

proc print data=outdata(firstobs=46 obs=46);
    var pred_days;
run;