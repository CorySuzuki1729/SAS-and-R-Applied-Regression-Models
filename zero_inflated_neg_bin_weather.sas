data weather;
    input elevation mintemp snowyears @@;
    cards;
    131.1 72.6 0 81.7 68.7 0 602 65.4 2
    1338.1 58.8 50 18.3 67.0 2 310.3 64.6 1
    18.3 67.5 2 182.9 67.9 0 1444.4 54.5 44
    1278.6 57.0 27 1389.9 57.7 49 1295.4 59.9 21
    712.6 62.2 7 256 69.2 0 27.4 65.4 0
    974.8 56.3 44 271.3 70.7 0 381.9 63.9 26
    71 68.9 0 1088.1 66.8 12 3.7 70.9 0
    28 73.1 0 520.6 65.4 6 2139.7 60.1 24
    974.8 59.1 44 18.3 67.6 0 18 67.7 0
    101.5 70.3 2 44.2 68.2 0 327.7 68.0 6
    125 68.0 0 999.7 62.1 50 146.3 67.4 0
    77.4 66.6 1 146.3 68.7 0 240.8 69.9 0
    391.7 65.4 0 222.5 66.9 0 1165.9 63.1 22
    132.6 65.8 0 483.4 65.0 1 712.6 64.7 7
    847.6 61.7 45 725.4 66.5 16 36.6 65.9 0
    81.7 68.6 0 6.1 69.1 0 73.8 67.4 0
    1516.4 55.5 50 1160.7 61.5 50 111.3 67.9 1
    21 67.6 1 12.2 66.2 0 83.8 66.2 0
    1160.7 62.5 50 576.4 67.2 0 398.4 64.2 4
    1431 59.6 41 41.5 67.9 7 1179.6 64.0 49
    ;

proc genmod;
    model snowyears=mintemp/dist=zinb;
    zeromodel elevation;
run;

proc genmod;
    model snowyears=/dist=zinb;
    zeromodel;
run;

data deviance_test;
    deviance=-2*(-178.2204-(-151.5915));
    pvalue=1-probchi(deviance,2);
run;

proc print;
run;

data prediction;
    input elevation mintemp;
    cards;
    1165.9 63.1
    ;

data weather;
    set weather prediction;
run;

proc genmod;
    model snowyears=mintemp/dist=zinb;
    zeromodel elevation;
    output out=outdata p=p_snowyears;
run;

proc print data=outdata(firstobs=61 obs=61);
    var p_snowyears;
run;