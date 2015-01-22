(* ::Package:: *)

(* ::Input:: *)
(*(*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% *)
(**)
(*here we find the parameter ct for  given (rh,OmegaH) *)
(*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% *)*)


(* ::Input:: *)
Remove["Global`*"];
Unprotect[In,Out];
Clear[In,Out];
Off[General::spell1];
(**)
(*(**)
(*Mass=1/2 ( rh-2ct);*)
(*J=1/2 Sqrt[ct (ct-rh)] (rh-2 ct);*)
(*AH=4 \[Pi] (rh-ct) (rh-2ct);*)
(*TH=rh/(4 \[Pi] (rh-ct) (rh-2ct));*)
(*OmegaH=Sqrt[ct (ct-rh)]/((rh-ct) (rh-2ct));*)
(*S=AH/4;*)
(**)*)
(**)
OmegaH[rh_,ct_]:=Sqrt[ct (ct-rh)]/(2 ct^2-3 ct rh+rh^2);
rhw = ReadList["rhw.txt",Number];
rh = rhw[[1]]
w = rhw[[2]]
nr = 2.0;
(* *)
(*(*rh=0.205; (* this parameter is increased *)*)
(* *)
(*w=0.67; (* this is the angular horizon velocity we keep fixed *) *)*)
(**)
(**)
asa=NSolve[nr OmegaH[rh,x]==w,x]
(**)
ct=asa[[3]][[1]][[2]]
(**)
(*(* attention!*)
(**)
(*for some critical (maximal rh), the root 2 and root 3 become equal!*)
(*after that, for larger values of rh, we get imaginary numbers;*)
(**)
(*then we have a second branch decreasing in rh and we should take instead:*)
(**)
(*ct=asa[[2]][[1]][[2]]*)
(**)*)
(**)
(**)
(**)


(* ::Input:: *)
(**)


(* ::Input:: *)
(*(* SetDirectory["/home/h/skoli/pt/projects/Q-balls/non-linear-clouds/w=0.67/rh=0.210"] *)*)
(**)
DeleteFile["rhct.txt"];
(**)
(*(* dat1=TableForm[{{rh,ct}}];*)*)
OutputForm[FortranForm[rh]] >>> "rhct.txt";	
OutputForm[FortranForm[ct]] >>> "rhct.txt";	
