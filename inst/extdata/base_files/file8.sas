proc datasets library=work kill;run;
/**********************************************************************
Creates case study for NMA combining IPD and AD
Author:Anna Wiksten
Date:19May2015
**********************************************************************/
/*set path for  folder to save figures*/
%let fp=H:\data\PhD\project4\manuscript;
/*Set prefix to be added to result figures*/
%let pgm=case1;
/*set seed for simulation*/
*%let seed=474685678;
/*set seed for simulation*/
*%let seed=474685678;
%let seed=45668;
/*Between and within study standard deviation*/
%let sigmau=0.1;*between;
%let sigmae=1.6;*within;
/*Give treatment effects, intercept(theta) and slope(beta)*/
data effects;
input trtn int slope;
cards;
1	0	0.66
2	0.66	0
3	0.5151	-0.00241
4	0.2	0.2
5	-0.2356	0.4
6	0.3192	0.211
7	0		0
run;
/*Studies in the network*/
data studies;
input study ipd xmean xstd A B C D E F xP;
label 	study="Study ID(must be unique)"
		ipd="0=AD,1=IPD"
		xmean="Mean of  covariate distribution"
		xmean="Standard deviation of  covariate distribution";
cards;
1	0	0.79	0.42	0	219	0	0	0	0	104
2	0	1.02	0.44	0	122	0	0	56	0	0
3	0	0.76	0.37	0	370	0	0	0	0	183
4	0	1.26	0.44	0	854	427	0	0	0	425
5	0	1.07	0.50	0	1028	45	0	0	542	419
6	0	1.07	0.48	0	330	0	0	327	0	334
7	0	1.39	0.56	0	211	0	0	0	0	205
8	0	0.87	0.30	0	1707	0	0	0	1714	0
9	0	1.09	0.43	0	760	0	0	0	776	0
10	0	1.02	0.48	0	161	0	0	0	0	155
11	0	1.10	0.50	0	150	0	0	0	0	155
12	0	1.04	0.48	0	473	0	471	0	478	229
13	0	1.05	0.46	0	260	0	258	0	0	260
14	0	0.95	0.48	0	251	0	249	0	0	247
15	1	1.11	0.50	523	0	0	0	0	266	267
16	1	1.06	0.46	550	0	0	0	0	0	265
17	1	0.77	0.37	301	0	0	0	0	0	154
18	1	1.04	0.49	325	0	0	0	0	326	0
19	1	1.06	0.50	222	0	0	0	0	0	218
20	1	1.34	0.47	216	0	0	0	0	0	215
21	1	0.85	0.30	702	0	0	707	0	689	0
run;
proc transpose data=studies out=s1;
by study ipd xmean xstd notsorted;
var a b c d e f xP;
run;

data s2;
set s1;
where col1 ne 0;
trt2=_name_;
if trt2="A" then trtn=1	;
if trt2="B" then trtn=2;
if trt2="C" then trtn=3;	
if trt2="D" then trtn=4;	
if trt2="E" then trtn=5;	
if trt2="F" then trtn=6	;
if trt2="xP" then trtn=7;
proc sort;by trtn;
run;

data s3;
merge s2 effects;
by trtn;
proc sort; by study trtn;
run;
/*Simulate ipd data*/
data s4;
call streaminit(&seed.);
set s3;
by study trtn;
if first.study or first.trtn then do;
u=rand("NORMAL",0,&sigmau.);
do i=1 to col1;
e=rand("NORMAL",0,&sigmae.);
x=rand("NORMAL",xmean,xstd);
y=int+slope*x+u+e;output;
end; 
end;
run; 

data ipd0;
set s4;
keep ipd study trtn  y x;
proc sort;by ipd study;
run;

/*Create aggregate data for all studies*/
ods output lsmeans=ad0;
proc mixed data=ipd0;
by ipd study;
class trtn;
model y=trtn ;
lsmeans trtn;
run;
/*Mean covariate values*/
proc means data=ipd0;
by ipd study;
var x;
output out=x(keep=ipd study x) mean=x;
run;

data ad1;
merge ad0 x;
by ipd study;
y=estimate;
keep ipd study trtn y x stderr;
run;
proc sort;by ipd study trtn;
run;
/*Create case data, different case number for each study*treatment combination*/
data case;
set ad0;
case=_n_;
keep ipd study trtn case;
run;
/*Final AD*/
data ad2;
merge case ad1 ;
by ipd study trtn;
case2=case; if ipd=1 then case2=1000;
x1=x-1;
run;




/*Add case and mean x values to IPD*/
data ipd1;
merge case ipd0 ;
by ipd study trtn;
keep case ipd study trtn y x;
run;
data ipd2;
merge ipd1 x(rename=(x=meanx));
by ipd study;
case2=case; if ipd=1 then case2=1000;
centx=x-meanx;
x1=x-1;
run;

/*Create combined AD and IPD*/
data ipd_ad0;
set ad2(where=(ipd=0)) ipd2(where=(ipd=1)) ;
run;

data ipd_ad1;
merge ipd_ad0 x(rename=(x=meanx));
by ipd study;
centx=x-meanx;
x1=x-1;
run;

/*IPD analysis*/
ods output estimates=est_i;
proc mixed  data=ipd2;
	title 'IPD and AD';
	class study trtn ;
	model y=  study trtn x1*study x1*trtn/ ddf=(10000,10000,10000,10000) noint s cl;
	random study*trtn;
	*repeated/group=case;/*heterogeneus residual variances*/
estimate "0.5" trtn 1 -1 x1*trtn -0.5 0.5/alpha=0.05;
estimate "1" trtn 1 -1 x1*trtn 0 0/alpha=0.05;
estimate "1.5" trtn 1 -1 x1*trtn 0.5 -0.5/alpha=0.05;
estimate "2" trtn 1 -1 x1*trtn 1 -1/alpha=0.05;
estimate "2.5" trtn 1 -1 x1*trtn 1.5 -1.5/alpha=0.05;
estimate "3" trtn 1 -1 x1*trtn 2 -2/alpha=0.05;
estimate "3.5" trtn 1 -1 x1*trtn 2.5 -2.5/alpha=0.05;
run;
/*IPD and AD analysis*/
/*Create covariance parameter data*/
data u;
study=0;stderr=sqrt(0.000165);case=0;
run;
data parms_ip_ad;
set u ad2;
est=stderr**2;
if ipd=1 then est=0.05;
case2=case; if ipd=1 then case2=1000;
keep study trtn est case2;
proc sort nodupkey;by case2;
run;

data firstipdcase;
set ad2(where=(ipd=1));
if _n_=1;
call symput('firstipdcase',case);
run;

ods output estimates=est_ia;
proc mixed  data=ipd_ad0;
	title 'IPD and AD';
	class study trtn ;
	model y=  study trtn x1*study x1*trtn/ddf=(10000,10000,10000,10000)  noint s cl;
	random trtn/subject=study;
	repeated/group=case2;/*heterogeneus residual variances*/
	parms/eqcons=2 to &firstipdcase. parmsdata=parms_ip_ad;
estimate "0.5" trtn 1 -1 x1*trtn -0.5 0.5/alpha=0.05;
estimate "1" trtn 1 -1 x1*trtn 0 0/alpha=0.05;
estimate "1.5" trtn 1 -1 x1*trtn 0.5 -0.5/alpha=0.05;
estimate "2" trtn 1 -1 x1*trtn 1 -1/alpha=0.05;
estimate "2.5" trtn 1 -1 x1*trtn 1.5 -1.5/alpha=0.05;
estimate "3" trtn 1 -1 x1*trtn 2 -2/alpha=0.05;
estimate "3.5" trtn 1 -1 x1*trtn 2.5 -2.5/alpha=0.05;
run;
/*AD analysis*/
/*Create covariance parameter data*/
data parms_ad;
set u ad0;
est=stderr**2;
keep study trtn est;
run;

ods output estimates=est_a;
proc mixed  data=ad2;
	title 'IPD and AD';
	class study trtn ;
	model y=  study trtn  x1*trtn/  ddf=(10000,10000,10000) noint s cl;
	random study*trtn;
	repeated/group=case;/*heterogeneus residual variances*/
	parms/eqcons=2 to 10000 parmsdata=parms_ad;
estimate "0.5" trtn 1 -1 x1*trtn -0.5 0.5/alpha=0.05;
estimate "1" trtn 1 -1 x1*trtn 0 0/alpha=0.05;
estimate "1.5" trtn 1 -1 x1*trtn 0.5 -0.5/alpha=0.05;
estimate "2" trtn 1 -1 x1*trtn 1 -1/alpha=0.05;
estimate "2.5" trtn 1 -1 x1*trtn 1.5 -1.5/alpha=0.05;
estimate "3" trtn 1 -1 x1*trtn 2 -2/alpha=0.05;
estimate "3.5" trtn 1 -1 x1*trtn 2.5 -2.5/alpha=0.05;
run;

/*Combine estimates for figure*/
libname library "&fp";
proc format library=library;
value dt 1="IPD"
2="AD and IPD"
3="AD";
run;

data est;
set est_i(in=a) est_ia(in=b) est_a(in=c);
if a then dat=1;
if b then dat=2;
if c then dat=3;
if a then x=label*1-0.05;
if b then x=label*1;
if c then x=label*1+0.05;
format dat dt.;
run;

   %modstyle(name=markstyle, parent=statistical, type=CLM,
             markers= circlefilled squarefilled trianglefilled,
			   linestyles=1 1 1,
colors=red green blue);


ods graphics on /
      width=10cm
      height=8cm
      imagefmt=pdf
      /*Nimi vaihtoon*/
      imagename="&pgm._resultsx0"
      reset=index
      border=off;
/*polku vaihtoon, image_dpi maarittaa kuvan tarkkuuden*/
ods listing   style=markstyle  gpath ="&fp" image_dpi=300;

title;
proc sgplot data=est;
scatter x=x y=estimate/  transparency=1;
*vector x=x y=lower/XORIGIN=x yORIGIN= upper   group=dat NOARROWHEADS;
refline 0/axis=y;
XAXIS label="Covariate value" values=(0 to 4 by 0.5);
YAXIS label="Comparison treatment A vs B";;
keylegend "d"  /ACROSS=3 position=bottom noborder title="" ;
lineparm x=1 y=0 slope=1;
yaxis min=-1 max=7;
run;

ods graphics off;


ods graphics on /
      width=10cm
      height=8cm
      imagefmt=pdf
      /*Nimi vaihtoon*/
      imagename="&pgm._resultsx1"
      reset=index
      border=off;
/*polku vaihtoon, image_dpi maarittaa kuvan tarkkuuden*/
ods listing   style=markstyle  gpath ="&fp" image_dpi=300;

title;
proc sgplot data=est;
where dat in (1);
scatter x=x y=estimate/  group=dat name="d";
vector x=x y=lower/XORIGIN=x yORIGIN= upper   group=dat NOARROWHEADS;
refline 0/axis=y;
XAXIS label="Covariate value" values=(0 to 4 by 0.5);
YAXIS label="Comparison treatment A vs B";;
keylegend "d"  /ACROSS=3 position=bottom noborder title="" ;
lineparm x=1 y=0 slope=1;
yaxis min=-1 max=7;
run;

ods graphics off;

ods graphics on /
      width=10cm
      height=8cm
      imagefmt=pdf
      /*Nimi vaihtoon*/
      imagename="&pgm._resultsx2"
      reset=index
      border=off;
/*polku vaihtoon, image_dpi maarittaa kuvan tarkkuuden*/
ods listing   style=markstyle  gpath ="&fp" image_dpi=300;

title;
proc sgplot data=est;
where dat in (1 2);
scatter x=x y=estimate/  group=dat name="d";
vector x=x y=lower/XORIGIN=x yORIGIN= upper   group=dat NOARROWHEADS;
refline 0/axis=y;
XAXIS label="Covariate value" values=(0 to 4 by 0.5);
YAXIS label="Comparison treatment A vs B";;
keylegend "d"  /ACROSS=3 position=bottom noborder title="" ;
lineparm x=1 y=0 slope=1;
yaxis min=-1 max=7;
run;

ods graphics off;

ods graphics on /
      width=10cm
      height=8cm
      imagefmt=pdf
      /*Nimi vaihtoon*/
      imagename="&pgm._resultsx3"
      reset=index
      border=off;
/*polku vaihtoon, image_dpi maarittaa kuvan tarkkuuden*/
ods listing   style=markstyle  gpath ="&fp" image_dpi=300;

title "Model 2: Estimated treatment difference A vs B ";
proc sgplot data=est;
scatter x=x y=estimate/  group=dat name="d";
vector x=x y=lower/XORIGIN=x yORIGIN= upper   group=dat NOARROWHEADS;
refline 0/axis=y;
XAXIS label="Covariate value" values=(0 to 4 by 0.5);
YAXIS label="Comparison treatment A vs B";;
keylegend "d"  /ACROSS=3 position=bottom noborder title="" ;
lineparm x=1 y=0 slope=1;
yaxis min=-1 max=7;
run;

ods graphics off;


   %modstyle(name=markstyle, parent=statistical, type=CLM,
             markers= circlefilled squarefilled trianglefilled,
			   linestyles=1 1 1,
colors=black black black);

ods graphics on /
      width=10cm
      height=8cm
      imagefmt=pdf
      /*Nimi vaihtoon*/
      imagename="&resultsPaper"
      reset=index
      border=off;
/*polku vaihtoon, image_dpi maarittaa kuvan tarkkuuden*/
ods listing   style=markstyle  gpath ="&fp" image_dpi=300;

title;
proc sgplot data=est;
scatter x=x y=estimate/  group=dat name="d";
vector x=x y=lower/XORIGIN=x yORIGIN= upper   group=dat NOARROWHEADS;
refline 0/axis=y;
XAXIS label="Covariate value" values=(0 to 4 by 0.5);
YAXIS label="Comparison treatment A vs B";;
keylegend "d"  /ACROSS=3 position=bottom noborder title="" ;
lineparm x=1 y=0 slope=0.66;
yaxis min=-1 max=7;
run;
ods graphics off;


proc format library=library;
value dt 3="IPD"
2="IPD-AD"
1="AD";
run;

data est;
set est_a(in=c) est_ia(in=b)  est_i(in=a);
if a then dat=3;
if b then dat=2;
if c then dat=1;
if a then x=label*1;
if b then x=label*1;
if c then x=label*1;
format dat dt.;
run;

   %modstyle(name=markstyle, parent=statistical, type=CLM,
             markers= circlefilled squarefilled trianglefilled,
			   linestyles=34 4 1,
colors=black black black,FILLCOLORS=lightgrey darkgrey grey  );

ods graphics on /
      width=10cm
      height=8cm
      imagefmt=pdf
      /*Nimi vaihtoon*/
      imagename="&resultsPaperBands"
      reset=index
      border=off;
/*polku vaihtoon, image_dpi maarittaa kuvan tarkkuuden*/
ods listing   style=markstyle  gpath ="&fp" image_dpi=300;

title;
proc sgplot data=est;
  band x=x lower=lower upper=upper /group=dat
       legendlabel="95% CLI" name="band1" TRANSPARENCY=0;
refline 0/axis=y;
series x=x y=estimate/  group=dat name="d" LINEATTRS= (THICKNESS=2);
XAXIS label="Covariate value" values=(0.5 to 3.5 by 0.5);

keylegend "d" "band1"  /ACROSS=3 position=bottom noborder title="" ;
yaxis min=-1 max=7;
YAXIS label="Comparison treatment A vs B";;
run;
ods graphics off;
