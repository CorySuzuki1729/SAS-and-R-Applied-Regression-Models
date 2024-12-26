data hotel;
    input ncomplaints floor member$ days@@;
    cards;
    2 6 no 3 1 7 no 12 3 6 no 7 3 7 yes 3
    3 8 yes 2 9 3 no 4 1 8 no 4 2 6 yes 5
    6 5 no 8 2 8 no 11 1 2 yes 5 2 3 no 3
    2 4 no 8 2 3 no 6 7 4 yes 4 4 5 yes 5
    8 2 yes 3 3 5 no 8 4 5 yes 4 4 2 no 3
    1 2 yes 4 1 7 no 3 6 3 no 2 3 3 no 2
    12 2 yes 4 2 6 no 1 1 6 no 3 5 3 no 8
    ;

proc format;
    value $memberfmt 'no'='ref' 'yes'='member';
run;

proc fmm;
    class member;
    model ncomplaints=floor member days/dist=truncnegbin;
    format member $memberfmt.;
run;

proc fmm;
    model ncomplaints=/dist=truncnegbin;
run;

data deviance_test;
    deviance=118.1-108.6;
    pvalue=1-probchi(deviance,3);
run;

proc print;
run;

data prediction;
    input floor member$ days;
    cards;
    4 yes 2
    ;

data hotel;
    set hotel prediction;
run;

proc fmm;
    class member;
    model ncomplaints=floor member days/dist=truncnegbin;
    output out=outdata pred=p_ncomplaints;
run;

proc print data=outdata(firstobs=29 obs=29);
    var p_ncomplaints;
run;