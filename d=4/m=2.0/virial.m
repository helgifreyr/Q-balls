<<NumericalMath`ListIntegrate`
Off[General::spell1]
Remove["Global`*"];
Unprotect[In,Out];
Clear[In,Out];

conf=ReadList["res.txt",{Number,Number ,Number ,Number ,Number,Number  }]

nr=conf[[1]][[4]];
w= conf[[1]][[5]];
c1= conf[[1]][[1]];
c2= conf[[1]][[2]];
c3= conf[[1]][[3]]; 

gr=ReadList["gridx.dat",{Number}];
lgr=Length[gr];
nx=lgr;

Print["nx = ",nx];

listar=Table[gr[[k]][[1]],{k,1,lgr}] ;
listalogr=Table[Log[10,gr[[k]][[1]]],{k,1,lgr}];

unghi0=ReadList["gridy.dat",{Number}];

ny=Length[unghi0];

Print["ny = ",ny];

unghi=Table[unghi0[[k]][[1]],{k,1,ny}]; 
asa1=2;
asa2=IntegerPart[ny/2];
asa3=ny-1;
ntot=nx*ny;

q=ReadList["T44.dat",{Number,Number,Number}];
lungq=Length[q];
diference=lungq-ntot;

Print["It must be zero! ",diference];

T441=Table[q[[k]][[1]],{k,1,lungq}];

T442=Table[q[[k]][[2]],{k,1,lungq}];

T443=Table[q[[k]][[3]],{k,1,lungq}];

Do[
T44u1[k]=Table[T441[[i]],{i,(k-1)*nx+1,k*nx}];
T44u2[k]=Table[T442[[i]],{i,(k-1)*nx+1,k*nx}];
T44u3[k]=Table[T443[[i]],{i,(k-1)*nx+1,k*nx}];
,{k,1,ny}]

ni=2;

t1=Table[{i,T44u1[asa1][[i]]+T44u2[asa1][[i]]+T44u3[asa1][[i]]},{i,ni,nx-1}];
t2=Table[{i,T44u1[asa2][[i]]+T44u2[asa2][[i]]+T44u3[asa2][[i]]},{i,ni,nx-1}];
t3=Table[{i,T44u1[asa3][[i]]+T44u2[asa3][[i]]+T44u3[asa3][[i]]},{i,ni,nx-1}];	

g1=ListPlot[t1,PlotRange->All,DisplayFunction->Identity, Frame->True,Axes->None ,PlotJoined->True];
g2=ListPlot[t2,PlotRange->All,DisplayFunction->Identity, Frame->True ,PlotJoined->True, PlotStyle->{RGBColor[1,0,0]}];
g3=ListPlot[t3,PlotRange->All,DisplayFunction->Identity, Frame->True ,PlotJoined->True, PlotStyle->{RGBColor[1,1,0]}];

t1=Table[{ listar[[i]],T44u1[asa1][[i]]+T44u2[asa1][[i]]+T44u3[asa1][[i]]},{i,ni,nx-1}];
t2=Table[{ listar[[i]],T44u1[asa2][[i]]+T44u2[asa2][[i]]+T44u3[asa2][[i]]},{i,ni,nx-1}];
t3=Table[{ listar[[i]],T44u1[asa3][[i]]+T44u2[asa3][[i]]+T44u3[asa3][[i]]},{i,ni,nx-1}];	

g1=ListPlot[t1,PlotRange->All,DisplayFunction->Identity, Frame->True,Axes->None ,PlotJoined->True];
g2=ListPlot[t2,PlotRange->All,DisplayFunction->Identity, Frame->True ,PlotJoined->True, PlotStyle->{RGBColor[1,0,0]}];
g3=ListPlot[t3,PlotRange->All,DisplayFunction->Identity, Frame->True ,PlotJoined->True, PlotStyle->{RGBColor[1,1,0]}];

Do[
  Mio1[k]=Table[{listar[[i]],listar[[i]]^2T44u1[k][[i]]},{i,ni,nx-1}];
  Mio2[k]=Table[{listar[[i]],listar[[i]]^2T44u2[k][[i]]},{i,ni,nx-1}];
  Mio3[k]=Table[{listar[[i]],listar[[i]]^2T44u3[k][[i]]},{i,ni,nx-1}];
		,{k,1,ny-1}];

Do[
  Ma1[k]=ListIntegrate[Mio1[k],2]//N;
  Ma2[k]=ListIntegrate[Mio2[k],2]//N;
  Ma3[k]=ListIntegrate[Mio3[k],2]//N;
  ,{k,1,ny-1}];

Ma1[ny]=Ma1[ny-1];
Ma2[ny]=Ma2[ny-1];
Ma3[ny]=Ma3[ny-1];

Minn1=Table[{unghi[[k]],Sin[unghi[[k]]] Ma1[k]},{k,1,ny}];
Minn2=Table[{unghi[[k]], Sin[unghi[[k]]] Ma2[k]},{k,1,ny}];
Minn3=Table[{unghi[[k]], Sin[unghi[[k]]] Ma3[k]},{k,1,ny}];

T=ListIntegrate[Minn1,2]//N;
Q=ListIntegrate[Minn2,2]//N;
V=ListIntegrate[Minn3,2]//N;

Print["T = ",T];
Print["Q = ",Q];
Print["V = ",V];

Etot=T+Q+V;
Print["Energy = ",Etot];

Print["Error Virial = ",Abs[1-T/3/(Q-V)]]
wMQ=Table[{w, Etot,Q}]
stmp=OpenAppend["w-M-Q.txt"];
Write[stmp,wMQ];
Close[stmp];
