data creditcards;
    input ndelinqaccounts age gender$ income$ numemplyears @@;
    cards;
    12 53 M Low 8 0 26 F Low 4 16 49 M Low 8 0 23 M Low 5
    7 28 F High 6 2 44 M Low 2 11 49 F Low 7 4 43 M Low 4
    0 25 M Low 0 0 28 M High 5 4 25 M High 1 5 40 M High 6
    1 37 M Low 3 0 48 M High 1 7 36 F Low 5 0 48 M Low 4
    13 56 M Low 10 0 22 M Low 0 7 36 F High 4 3 35 M Low 1
    7 35 F High 0 0 42 F Low 0 1 56 M Low 4 5 22 M Low 0
    0 38 F Low 7 4 52 M High 5 1 30 F Low 6 0 27 M High 1
    0 32 F High 3 4 46 F Low 1 2 32 M Low 2 7 26 F Low 6
    0 23 M Low 5 3 30 M High 2 0 25 M Low 3
    ;

proc fmm;
    class gender income;
    model ndelinqaccounts=gender income
    numemplyears/dist=truncnegbin;
    model+/dist=constant;
    probmodel age;
run;

proc fmm;
    model ndelinqaccounts=/dist=truncnegbin;
    model+/dist=constant;
    probmodel;
run;

data deviance_test;
    deviance=161.4-144.5;
    pvalue = 1-probchi(deviance,4);
run;

proc print;
run;

data prediction;
    input age gender$ income$ numemplyears;
    cards;
    45 M High 0
    ;

data creditcards;
    set creditcards prediction;
run;

proc fmm;
    class gender income;
    model ndelinqaccounts=gender income
    numemplyears/dist=truncnegbin;
    model +/dist=constant;
    probmodel age;
    output out=outdata pred=p_ndelinqaccounts;
run;

proc print data=outdata(firstobs=36 obs=36);
    var p_ndelinqaccounts;
run;