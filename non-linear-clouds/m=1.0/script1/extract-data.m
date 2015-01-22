(* ::Package:: *)

(* ::Input:: *)
<<NumericalMath`ListIntegrate`


(* ::Input:: *)
(*SetDirectory["/home/h/skoli/pt/projects/Q-balls/non-linear-clouds/w=0.67/rh=0.205"];*)
(**)
(**)
Off[General::spell1]
Remove["Global`*"];
Unprotect[In,Out];
Clear[In,Out];
(**)
gr=ReadList["gridx.dat",{Number}];
(**)
Directory[]
(**)
lgr=Length[gr];
nx=lgr;
Print["nx = ",nx];
(**)
listar=Table[gr[[k]][[1]],{k,1,lgr}] ;
listalogr=Table[Log[10,gr[[k]][[1]]],{k,1,lgr}];
(**)
 unghi0=ReadList["gridy.dat",{Number}];
ny=Length[unghi0];
Print["ny = ",ny];
unghi=Table[unghi0[[k]][[1]],{k,1,ny}]; 
(**)
(*(*unghi=Table[(k-1)*Pi/2/(ny-1),{k,1,ny}];*)*)
(**)
ntot=nx*ny;
(**)
a=ReadList["functf.dat",{Number  }];
(**)
lung1=Length[a];
(*(*Datele sunt salvate direct cu indexarea globala*)*)
X=Table[a[[k]][[1]],{k,1,lung1}]; 
(*  *)
(*(*Se construiesc ny liste pt. marimi de interes la unghiuri fixate *)*)
(*(*foarte util in reprezentari grafice *)*)
(**)
Do[
  k=Table[X[[i]],{i,(k-1)*nx+1,k*nx}]
  ,{k,1,ny}];
(**)
(* *)
(* (* Marimi la fixed r  *)*)
(**)
Do[
 Xr[i]=Table[Xu[k][[i]],{k,1,ny}]; 
,{i,1,nx}] 
(**)
as1=2;
as2=IntegerPart[ny/2];
as3=ny-1;
(**)
sa1=3;
sa2=IntegerPart[nx/2];
sa3=nx-1;
(**)
(**)
(*Print["rmax = ",gr[[nx]][[1]]];*)
(**)
t1=Table[{i,Xu[as1][[i]]},{i,1,lgr}];
t2=Table[{i,Xu[as2][[i]]},{i,1,lgr}];
t3=Table[{i,Xu[as3][[i]]},{i,1,lgr}];
(**)
(**)
(*g1=ListPlot[t1,PlotRange->All,DisplayFunction->Identity, Frame->True,Axes->None];*)
(*g2=ListPlot[t2,PlotRange->All,DisplayFunction->Identity, Frame->True];*)
(*g3=ListPlot[t3,PlotRange->All,DisplayFunction->Identity,  Frame->True];*)
(**)
(*Print[" FUNCTION X"]*)
(*Show[g1,g2,g3,DisplayFunction->$DisplayFunction,Prolog->AbsolutePointSize[4]];*)
(**)
t1=Table[{Log[10,listar[[i]]],Xu[as1][[i]]},{i,2,lgr}];
t2=Table[{Log[10,listar[[i]]] ,Xu[as2][[i]]},{i,2,lgr}];
t3=Table[{Log[10,listar[[i]]],Xu[as3][[i]]},{i,2,lgr}];
(**)
(*g1=ListPlot[t1,PlotRange->All,DisplayFunction->Identity,  Frame->True ,Axes->None];*)
(*g2=ListPlot[t2,PlotRange->All,DisplayFunction->Identity, Frame->True ];*)
(**)
(*g3=ListPlot[t3,PlotRange->All,DisplayFunction->Identity,  Frame->True ];*)
(*Show[g1,g2,g3,DisplayFunction->$DisplayFunction,Prolog->AbsolutePointSize[4]];*)
(**)
maxZ=Max[X]
(**)
(* *)
(**)
(*(*c1,c2,c3,nr,w,rh,ct*)*)
conf=ReadList["res.txt",{Number,Number ,Number ,Number ,Number,Number ,Number  }]
(**)
nr=conf[[1]][[4]];
w= conf[[1]][[5]];
c1= conf[[1]][[1]];
c2= conf[[1]][[2]];
c3= conf[[1]][[3]]; 
rh=conf[[1]][[6]];
ct=conf[[1]][[7]];
(**)
(**)
gr=ReadList["gridx.dat",{Number}];
(**)
Directory[]
(**)
lgr=Length[gr];
nx=lgr;
(**)
(*Print["nx = ",nx];*)
(**)
listar=Table[gr[[k]][[1]],{k,1,lgr}] ;
listalogr=Table[Log[10,gr[[k]][[1]]],{k,1,lgr}];
(**)
 unghi0=ReadList["gridy.dat",{Number}];
ny=Length[unghi0];
(*Print["ny = ",ny];*)
unghi=Table[unghi0[[k]][[1]],{k,1,ny}]; 
(**)
asa1=2;
asa2=IntegerPart[ny/2];
asa3=ny-1;
(**)
(*(*unghi=Table[(k-1)*Pi/2/(ny-1),{k,1,ny}];*)*)
(**)
ntot=nx*ny;
(* *)
(*(* ordinea este: T44, T34, sqr T44, sqr T34 *) *)
q=ReadList["T44.dat",{Number,Number,Number,Number}];
lungq=Length[q];
(**)
diference=lungq-ntot;
Print["It must be zero! ",diference];
(*(*Datele sunt salvate direct cu indexarea globala*)*)
(* *)
T44=Table[q[[k]][[1]],{k,1,lungq}];
T34=Table[q[[k]][[2]],{k,1,lungq}];
T44s=Table[q[[k]][[3]],{k,1,lungq}];
T34s=Table[q[[k]][[4]],{k,1,lungq}];
(**)
(*(*Se construiesc ny liste pt. marimi de interes la unghiuri fixate *)*)
(*(*foarte util in reprezentari grafice *)*)
(**)
Do[
T44u[k]=-Table[T44[[i]],{i,(k-1)*nx+1,k*nx}];
T34u[k]=Table[T34[[i]],{i,(k-1)*nx+1,k*nx}];
T44su[k]=Table[T44s[[i]],{i,(k-1)*nx+1,k*nx}];
T34su[k]=Table[T34s[[i]],{i,(k-1)*nx+1,k*nx}];
(*	Print[T44u[k]];*)
,{k,1,ny}]
(**)
(*(*  Do[*)
(*ListPlot[T44u2[i],PlotRange->All]*)
(*,{i,2,ny-1}]  *)
(**)*)
(**)
ni=2;
(**)
(*Print["energy density profiles"];*)
(*t1=Table[{i,T44u[asa1][[i]]},{i,ni,nx-1}];*)
(*t2=Table[{i,T44u[asa2][[i]]},{i,ni,nx-1}];*)
(*t3=Table[{i,T44u[asa3][[i]]},{i,ni,nx-1}];	*)
(* *)
(*g1=ListPlot[t1,PlotRange->All,DisplayFunction->Identity, Frame->True,Axes->None ,PlotJoined->True];*)
(*g2=ListPlot[t2,PlotRange->All,DisplayFunction->Identity, Frame->True ,PlotJoined->True, PlotStyle->{RGBColor[1,0,0]}];*)
(*g3=ListPlot[t3,PlotRange->All,DisplayFunction->Identity, Frame->True ,PlotJoined->True, PlotStyle->{RGBColor[1,1,0]}];*)
(*(* Print["black: theta=0 "];*)*)
(* Show[g1,g2,g3,DisplayFunction->$DisplayFunction];*)
(**)
(*Print["J density profiles"];*)
(*t1=Table[{ i,T34u[asa1][[i]]},{i,ni,nx-1}];*)
(*t2=Table[{i,T34u[asa2][[i]]},{i,ni,nx-1}];*)
(*t3=Table[{ i,T34u[asa3][[i]]},{i,ni,nx-1}];	*)
(* *)
(*g1=ListPlot[t1,PlotRange->All,DisplayFunction->Identity, Frame->True,Axes->None ,PlotJoined->True];*)
(*g2=ListPlot[t2,PlotRange->All,DisplayFunction->Identity, Frame->True ,PlotJoined->True, PlotStyle->{RGBColor[1,0,0]}];*)
(*g3=ListPlot[t3,PlotRange->All,DisplayFunction->Identity, Frame->True ,PlotJoined->True, PlotStyle->{RGBColor[1,1,0]}];*)
(*(* Print["black: theta=0 "];*)*)
(* Show[g1,g2,g3,DisplayFunction->$DisplayFunction];*)
(**)
(**)
(**)
(*(*Se construiesc ny liste pt. integralele marimilor de interes la unghiuri fixate *)*)
Do[
  Mio1[k]=Table[{listar[[i]],T44su[k][[i]]},{i,ni,nx-1}];
  Mio2[k]=Table[{listar[[i]], T34su[k][[i]]},{i,ni,nx-1}];
		,{k,1,ny-1}];
(**)
(*(*Se construiesc ny liste pt. integralele marimilor de interes la unghiuri fixate *)*)
Do[
Ma1[k]=ListIntegrate[Mio1[k],2]//N;
Ma2[k]=ListIntegrate[Mio2[k],2]//N;
,{k,1,ny-1}];
(**)
(*(* Ma1[1]=Ma1[2];*)
(*Ma2[1]=Ma2[2];*)
(* *)*)
(* *)
Ma1[ny]=Ma1[ny-1];
Ma2[ny]=Ma2[ny-1];
(* *)
(**)
Minn1=Table[{unghi[[k]], Ma1[k]},{k,1,ny}];
Minn2=Table[{unghi[[k]],  Ma2[k]},{k,1,ny}];
(**)
M=-ListIntegrate[Minn1,2]//N;
J=ListIntegrate[Minn2,2]//N;
(* *)
Print["Mass = ",M];
Print["J = ",J];
(**)
d1 = ReadList["sup.dat", {Number, Number }]
Zm = d1[[1]][[1]];
rm = d1[[1]][[2]]; 
Table[{c1,c2,c3,nr,w,rh,ct, M,J,maxZ,Zm,rm}]
(**)


(* ::Input:: *)
asa= Table[{c1,c2,c3,nr,w,rh,ct, M,J,maxZ,Zm,rm}]
(**)


(* ::Input:: *)
(*SetDirectory["/home/h/skoli/pt/projects/Q-balls/non-linear-clouds/w=0.67/rh=0.205"];*)
stmp=OpenAppend["tmp.txt"];
Write[stmp,asa];
Close[stmp] ;
(* *)
