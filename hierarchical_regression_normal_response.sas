data dyads;
input family individual	relation$ depression1	
depression2 depression3 qol1 qol2 qol3 @@;
cards;
1  1 M 1 1 1 4.0 4.1 4.9  1  2 D 1 1 0 2.5 3.2 4.2
2  1 M 1 1 1 2.6 2.8 4.1  2  2 D 1 1 1 2.8 3.1 4.2
3  1 M 1 1 1 2.5 3.8 4.0  3  2 D 1 1 1 2.4 5.1 3.3
4  1 M 1 0 0 2.1 3.3 4.6  4  2 D 1 0 0 3.7 3.1 4.4
5  1 M 1 0 0 2.9 4.2 3.4  5  2 D 1 0 0 2.4 2.6 2.7
6  1 M 1 1 0 3.3 4.2 4.7  6  2 D 1 1 0 2.7 4.0 4.1
7  1 M 1 1 0 3.7 4.3 3.8  7  2 D 1 1 0 2.8 3.2 3.6
8  1 M 1 0 0 3.5 4.1 4.3  8  2 D 1 1 0 1.6 2.6 3.5
9  1 M 1 0 0 4.0 4.4 3.6  9  2 D 1 0 1 1.8 2.5 3.1
10 1 M 1 1 1 3.0 3.7 4.3  10 2 D 1 1 1 2.2 2.0 3.3
11 1 M 1 1 1 4.3 5.0 3.7  11 2 D 1 1 1 3.3 2.5 3.2
12 1 M 1 1 1 3.5 5.4 4.7  12 2 D 1 1 1 3.5 3.6 4.2
13 1 M 1 0 0 4.1 4.5 3.2  13 2 D 1 0 0 3.7 4.2 3.5
14 1 M 1 1 0 5.0 4.2 3.6  14 2 D 1 0 0 3.3 4.6 3.0
15 1 M 1 1 0 1.8 2.2 2.3  15 2 D 1 0 0 2.4 3.5 3.6
16 1 M 1 0 0 3.1 2.5 3.9  16 2 D 1 1 0 2.0 2.9 2.4
17 1 M 1 1 0 3.4 5.5 4.7  17 2 D 1 0 1 3.2 4.3 3.7
18 1 M 1 0 0 3.4 5.3 4.1  18 2 D 1 1 0 1.8 3.4 3.1
19 1 M 1 0 0 3.5 3.3 5.1  19 2 D 1 0 0 2.8 4.3 3.4
20 1 M 1 0 0 3.5 3.3 5.1  20 2 D 1 0 0 3.2 4.9 3.6
21 1 M 1 0 0 2.9 2.7 3.5  21 2 D 1 0 0 4.3 3.7 2.5
22 1 M 1 0 0 4.8 4.3 4.3  22 2 D 1 0 0 4.5 3.8 3.3
23 1 M 1 1 0 3.6 3.9 3.7  23 2 D 1 0 0 3.7 3.7 3.5
23 3 D 1 1 0 2.5 1.8 2.3  24 1 M 1 1 0 5.0 4.4 4.2
24 2 D 1 1 0 4.9 3.2 2.5  24 3 D 1 0 0 2.7 3.1 4.1
24 4 D 1 0 1 3.0 3.5 3.6
;

data longform;
    set dyads;
    array d[3] depression1-depression3;
    array q[3] qol1-qol3;
    do visit=1 to 3;
    depression=d[visit];
    qol_score=q[visit];
    output;
    end;
    keep family individual relation depression visit qol_score;
run;

proc univariate;
    var qol_score;
    histogram/normal;
run;

proc mixed covtest;
    class family individual relation(ref='D') depression;
    model qol_score=relation depression visit/solution;
    random intercept visit/subject=family type=un;
    random intercept visit/subject=individual(family) type=un;
run;

data prediction;
    input family individual relation$ depression visit;
    cards;
    25 1 M 0 3
    ;

data longform;
    set longform prediction;
run;

proc mixed covtest;
    class family individual relation(ref='D') depression;
    model qol_score=relation depression visit/solution
    outpm=outdata;
    random intercept visit/subject=family type=un;
    random intercept visit/subject=individual(family) type=un;
run;

proc print data=outdata (firstobs=154 obs=154);
    var Pred;
run;