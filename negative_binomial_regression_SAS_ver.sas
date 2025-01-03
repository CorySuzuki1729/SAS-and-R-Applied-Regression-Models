data swim;
    input gender$ age firsttime$ sets @@;
    cards;
    M 38 yes 20 M 26 no 0 M 21 yes 8
    M 19 yes 13 M 18 yes 28 M 20 yes 2
    F 26 yes 8 M 21 no 14 F 20 no 0
    F 18 yes 3 M 25 yes 6 F 42 yes 1
    F 24 yes 7 M 58 yes 27 M 19 yes 10
    F 32 no 17 F 46 no 12 M 21 yes 4
    F 26 no 3 M 22 no 35 F 19 yes 2
    F 56 yes 11 F 41 no 15 M 25 no 1
    M 25 yes 9 M 21 no 8 M 19 yes 11
    M 37 no 34 F 22 yes 8 F 23 yes 5
    ;

proc genmod;
    class gender(ref='F') firsttime(ref='yes');
    model sets=gender age firsttime/dist=negbin;
run;

proc genmod;
    model sets=/dist=negbin;
run;

data deviance_test;
    deviance=-2*(-102.1982-(-97.8206));
    pvalue = 1-probchi(deviance,3);
run;

proc print;
run;

data prediction;
    input gender$ age firsttime$;
    cards;
    F 20 yes
    ;

data swim;
    set swim prediction;
run;

proc genmod;
    class gender(ref='F') firsttime(ref='yes');
    model sets=gender age firsttime/dist=negbin;
    output out=outdata p=pred_sets;
run;

proc print data=outdata(firstobs=31 obs=31);
    var pred_sets;
run;