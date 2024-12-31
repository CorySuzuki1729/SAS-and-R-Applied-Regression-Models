data cholesterol;
    input id gender$ age LDL0 LDL6 LDL9 LDL24 @@;
    cards;
    1 M 50 73 71 80 85 2 F 72 174 164 139 112
    3 M 46 85 86 82 90 4 F 71 172 150 139 127
    5 F 75 186 177 153 145 6 F 68 184 169 153 138
    7 F 63 196 188 163 155 8 M 73 137 137 132 104
    9 M 59 135 120 106 106 10 M 60 111 110 100 76
    11 F 59 127 126 106 99 12 M 46 88 87 84 80
    13 F 67 176 150 156 153 14 F 52 155 135 128 120
    15 M 65 142 117 114 97 16 F 75 158 143 145 135
    17 F 57 148 131 138 102 18 M 58 125 111 118 124
    19 M 48 76 65 94 98 20 M 47 116 108 94 107
    21 F 53 191 185 162 113 22 F 73 167 165 162 140
    23 M 62 109 104 93 94 24 F 77 167 164 155 155
    25 M 55 103 94 75 78 26 F 74 122 126 105 111
    27 F 79 203 204 178 145
    ;

data longform;
    set cholesterol;
    array m[4] (0 6 9 24);
    array c[4] LDL0 LDL6 LDL9 LDL24;
    do i=1 to 4;
    month=m[i];
    LDL=c[i];
    output;
    end;
    keep id gender age month LDL;
run;

proc univariate data=longform;
    var LDL;
    histogram LDL/normal;
run;

/*unstructured covariance matrix of error terms*/
proc mixed covtest;
    class gender;
    model LDL=gender age month/solution;
    random intercept month/subject=id type=un;
    repeated/subject=id type=un r;
run;

/*Toeplitz covariance matrix of error terms*/
proc mixed covtest;
    class gender;
    model LDL=gender age month/solution;
    random intercept month/subject=id type=un;
    repeated / subject=id type=toep r;
run;

/*spatial power covariance matrix of error terms*/
proc mixed covtest;
    class gender;
    model LDL=gender age month/solution;
    random intercept month/subject=id type=un;
    repeated / subject=id type=sp(pow)(month) r;
run;

/*autoregressive covariance matrix of error terms*/
proc mixed covtest;
    class gender;
    model LDL=gender age month/solution;
    random intercept month/subject=id type=un;
    repeated /subject=id type=ar(1) r;
run;

/*compound symmetric covariance matrix of error terms*/
proc mixed covtest;
    class gender;
    model LDL=gender age month/solution;
    random intercept month/subject=id type=un;
    repeated /subject=id type=cs r;
run;

data prediction;
    input id gender$ age month;
    cards;
    28 F 48 3
    ;

data longform;
    set longform prediction;
run;

proc mixed covtest;
    class gender;
    model LDL=gender age month/solution outpm=outdata;
    random intercept month/subject=id type=un;
    repeated /subject=id type=ar(1) r;
run;

proc print data=outdata (firstobs=109 obs=109);
    var Pred;
run;