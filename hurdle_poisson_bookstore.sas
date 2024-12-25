data bookstore;
    input ntextbooks renting aid$ @@;
    cards;
    0 3 no 0 2 no 3 0 yes 0 0 no 0 1 no 1 0 no
    0 3 no 2 0 yes 4 0 no 0 2 no 4 1 no 7 0 yes
    3 2 yes 0 4 no 1 2 no 0 0 no 0 5 no 1 0 no
    0 2 no 3 0 no 0 3 no 6 1 yes 2 1 yes 1 0 no
    6 0 yes 2 0 no 0 3 no 4 0 yes 0 1 no 0 2 no
    3 0 no 3 2 no 3 0 yes 2 0 yes 0 3 no 2 1 no
    3 0 yes 1 3 no 3 0 yes 0 2 no
    ;

proc format;
    value $aidfmt 'no'='ref' 'yes'='aid';
run;

proc fmm;
    class aid;
    model ntextbooks=aid/dist=truncpoisson;
    model+/dist=constant;
    probmodel renting;
    format aid $aidfmt.;
run;

proc fmm;
    model ntextbooks=/dist=truncpoisson;
    model+/dist=constant;
    probmodel;
run;

data deviance_test;
    deviance=139.1-115.7;
    pvalue=1-probchi(deviance, 2);
run;

proc print;
run;

data prediction;
    input renting aid$;
    cards;
    0 no
    ;

data bookstore;
    set bookstore prediction;
run;

proc fmm;
    class aid;
    model ntextbooks=aid/dist=truncpoisson;
    model+/dist=constant;
    probmodel renting;
    output out=outdata pred=p_ntextbooks;
    format aid $aidfmt.;
run;

proc print data=outdata(firstobs=41 obs=41);
    var p_ntextbooks;
run;