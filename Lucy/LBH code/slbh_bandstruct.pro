pro slbh_bandstruct,nmax,j_hi,j_low,sym,mag_dip_flag,hlf
;
; Set up for the 8 possible branches.  BR defines the change
; in J from the upper state, i.e. J''=J'+BR.
;
br = intarr(8)
;
; Change in J for P,Q,R magnetic dipole branches
;
br(0)=1 & br(1)=0 & br(2)=-1
;
; Change in J for O,P,Q,R,S electric quadrupole branches
;
br(3)=2 & br(4)=1 & br(5)=0 & br(6)=-1 & br(7)=-2
;
; Create arrays corresponding to transitions
; Note that lowest rotational level of upper state is J=1
;
J = indgen(nmax)+1
j_hi  = j#replicate(1,8)
j_low = j_hi + replicate(1,nmax)#br
;
; This is for the population of each lambda state.  Either a 2 or a 1
; for each J'.  As you increase J' each of the 8 branches alternate
; the symmetry of their upper state.
;
sym = intarr(8,nmax)
sym1 = (indgen(8)/2. eq indgen(8)/2)+1
for i=0,nmax-1 do sym(0,i) = shift(sym1,i)
sym = transpose(sym)
;
; Generate flags describing transition type.
; Required later to use correct band transition rate
;
mag_dip_flag = replicate(1,nmax)#[1,1,1,0,0,0,0,0]
;
; Generate Honl-London factors 
; Later these should be divided by (2*J+1) to normalize
; Note J means J' (upper state)
;
hlf = fltarr(nmax,8)
hlf(0,0) = J                          ;P,Q,R dipole
hlf(0,1) = (2*J+1)
hlf(0,2) = (J+1)
hlf(0,3) = 2.*J*(J+2)/(2*J+3)         ;O,P,Q,R,S quadrupole
hlf(0,4) = (J+2)
hlf(0,5) = 3.*(2*J+1)/(2*J-1)/(2*J+3)
hlf(0,6) = (J-1)
hlf(0,7) = 2.*(J-1)*(J+1)/(2*J-1)
;
; Now select only values corresponding to valid transitions
;
select = where(j_low ge 0 and j_low le nmax-1)
j_hi = j_hi(select)
j_low = j_low(select)
sym = sym(select)
mag_dip_flag = mag_dip_flag(select)
hlf = hlf(select)
;
; Finished
;
return
end
