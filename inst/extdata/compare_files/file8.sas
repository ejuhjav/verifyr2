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
	estimate "ap" trtn 1 0 0 0 0 0 -1/alpha=0.05;
	estimate "bp" trtn 0 1 0 0 0 0 -1;
	estimate "cp" trtn 0 0 1 0 0 0 -1;
	estimate "dp" trtn 0 0 0 1 0 0 -1;
	estimate "ep" trtn 0 0 0 0 1 0 -1;
	estimate "fp" trtn 0 0 0 0 0 1 -1;
	estimate "ba" trtn 1 -1;
	estimate "bap" x1*trtn 1 0 0 0 0 0 -1/alpha=0.05;
	estimate "bbp" x1*trtn 0 1 0 0 0 0 -1;
	estimate "bcp" x1*trtn 0 0 1 0 0 0 -1;
	estimate "bdp" x1*trtn 0 0 0 1 0 0 -1;
	estimate "bep" x1*trtn 0 0 0 0 1 0 -1;
	estimate "bfp" x1*trtn 0 0 0 0 0 1 -1;
	estimate "bba" x1*trtn 1 -1;
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
	estimate "ap" trtn 1 0 0 0 0 0 -1/alpha=0.05;
	estimate "bp" trtn 0 1 0 0 0 0 -1;
	estimate "cp" trtn 0 0 1 0 0 0 -1;
	estimate "dp" trtn 0 0 0 1 0 0 -1;
	estimate "ep" trtn 0 0 0 0 1 0 -1;
	estimate "fp" trtn 0 0 0 0 0 1 -1;
	estimate "ba" trtn 1 -1;
	estimate "bap" x1*trtn 1 0 0 0 0 0 -1/alpha=0.05;
	estimate "bbp" x1*trtn 0 1 0 0 0 0 -1;
	estimate "bcp" x1*trtn 0 0 1 0 0 0 -1;
	estimate "bdp" x1*trtn 0 0 0 1 0 0 -1;
	estimate "bep" x1*trtn 0 0 0 0 1 0 -1;
	estimate "bfp" x1*trtn 0 0 0 0 0 1 -1;
	estimate "bba" x1*trtn 1 -1;
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
	estimate "ap" trtn 1 0 0 0 0 0 -1/alpha=0.05;
	estimate "bp" trtn 0 1 0 0 0 0 -1;
	estimate "cp" trtn 0 0 1 0 0 0 -1;
	estimate "dp" trtn 0 0 0 1 0 0 -1;
	estimate "ep" trtn 0 0 0 0 1 0 -1;
	estimate "fp" trtn 0 0 0 0 0 1 -1;
	estimate "ba" trtn 1 -1;
	estimate "bap" x1*trtn 1 0 0 0 0 0 -1/alpha=0.05;
	estimate "bbp" x1*trtn 0 1 0 0 0 0 -1;
	estimate "bcp" x1*trtn 0 0 1 0 0 0 -1;
	estimate "bdp" x1*trtn 0 0 0 1 0 0 -1;
	estimate "bep" x1*trtn 0 0 0 0 1 0 -1;
	estimate "bfp" x1*trtn 0 0 0 0 0 1 -1;
	estimate "bba" x1*trtn 1 -1;
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


options ls=256 ps=100 nocenter;

data aaa;
set est_i(in=a) est_ia(in=b) est_a(in=c);
if a then d=1;
if b then d=2;
if c then d=3;
e=round(estimate,0.001); 
l=round(lower,0.001); 
u=round(upper,0.001); 
ci=compress(e||"("||l||","||u||")");
x=(d-1)*14;
obsn=_n_;
parid=obsn-x;
run;
proc sort data=aaa;by parid;
run;

proc transpose data=aaa out=a1 prefix=d;
by parid;
id d ;
var ci;
run;
data a2;
retain d1 a1 d2 a2 d3 a3;
set a1;
a1="&";
a2="&";
a3="&";
run;
proc print; run;




\begin{table}[h]
\begin{tabular}{ L{3.0cm}  L{1.6cm}  C{3.6cm}  C{3.6cm} C{3.6cm}  }
	&	&	\multicolumn{3}{c}{Estimate and  95\% CI}\\ \cline{3-5}
	Model&Parameter&IPD& IPD and AD & AD\\ \hline
Models \ref{eq:m3ipd} and \ref{eq:m2ipd}&$\theta_A$&	 0.658(0.501,0.815)      &     0.656(0.492,0.82)        &     0.662(0.476,0.849) 	 	 	\\
 REML			&$\theta_B$&							 0.542(0.423,0.661)      &     0.541(0.412,0.671)       &     0.543(0.406,0.68) 	 	 	\\
				&$\theta_C$&							 0.62(0.319,0.922)       &     0.81(0.093,1.526)        &     0.807(0.077,1.538) 	 	 	\\
				&$\theta_D$&							 0.539(0.356,0.722)      &     0.539(0.347,0.731)       &     0.531(0.322,0.74) 	 	 	\\
				&$\theta_E$&							 0.243(-0.067,0.554)     &     0.062(-1.028,1.152)      &     0.063(-1.056,1.182) 	 	 	\\
				&$\theta_F$&							 0.431(0.279,0.583)      &     0.432(0.272,0.592)       &     0.441(0.265,0.618) 	 	 	\\
				&$\theta_A-\theta_B$&					 0.116(-0.063,0.295)     &     0.115(-0.074,0.304)      &     0.12(-0.087,0.327) 	 	 	\\
				&$\beta_A$&								 0.608(0.397,0.818)      &     0.66(0.412,0.908)        &     0.821(-0.262,1.905) 	 	 	\\
				&$\beta_B$&								 -0.023(-0.176,0.129)    &     -0.124(-0.832,0.584)     &     -0.158(-0.925,0.609) 	 	 	\\
				&$\beta_C$&								 -0.168(-0.554,0.217)    &     -1.064(-4.064,1.936)     &     -1.069(-4.155,2.017) 	 	 	\\
				&$\beta_D$&								 0.253(0.01,0.496)       &     0.341(-0.193,0.874)      &     -0.012(-2.376,2.352) 	 	 	\\
				&$\beta_E$&								 -0.121(-0.542,0.299)    &     3.096(-16.113,22.305)    &     3.073(-16.731,22.877) 	 	 	\\
				&$\beta_F$&								 0.077(-0.109,0.263)     &     0.206(-0.144,0.556)      &     -0.003(-1.578,1.572) 	 	 	\\
				&$\beta_A-\beta_B$&						 0.631(0.398,0.864)      &     0.784(0.045,1.524)       &     0.98(-0.274,2.233)	 	 	\\
				&$\sigma_u^2$&&&\\

	&&&&
\end{tabular}
\caption{Proportion of simulations where the ratio of the lengths of  fixed effect and \hk\  confidence intervals is greater than given value} \label{tab:simres1}
\end{table}


  0.658(0.501,0.815)      &     0.656(0.492,0.82)        &     0.662(0.476,0.849)
  0.542(0.423,0.661)      &     0.541(0.412,0.671)       &     0.543(0.406,0.68)
  0.62(0.319,0.922)       &     0.81(0.093,1.526)        &     0.807(0.077,1.538)
  0.539(0.356,0.722)      &     0.539(0.347,0.731)       &     0.531(0.322,0.74)
  0.243(-0.067,0.554)     &     0.062(-1.028,1.152)      &     0.063(-1.056,1.182)
  0.431(0.279,0.583)      &     0.432(0.272,0.592)       &     0.441(0.265,0.618)
  0.116(-0.063,0.295)     &     0.115(-0.074,0.304)      &     0.12(-0.087,0.327)
  0.608(0.397,0.818)      &     0.66(0.412,0.908)        &     0.821(-0.262,1.905)
  -0.023(-0.176,0.129)    &     -0.124(-0.832,0.584)     &     -0.158(-0.925,0.609)
  -0.168(-0.554,0.217)    &     -1.064(-4.064,1.936)     &     -1.069(-4.155,2.017)
  0.253(0.01,0.496)       &     0.341(-0.193,0.874)      &     -0.012(-2.376,2.352)
  -0.121(-0.542,0.299)    &     3.096(-16.113,22.305)    &     3.073(-16.731,22.877)
  0.077(-0.109,0.263)     &     0.206(-0.144,0.556)      &     -0.003(-1.578,1.572)
  0.631(0.398,0.864)      &     0.784(0.045,1.524)       &     0.98(-0.274,2.233)
