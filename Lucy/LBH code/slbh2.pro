function slbh2,temperature,wavelength,print=print,verbose=verbose,$
               population_in=uspw_in,population_out=uspw,hash_out=hash_out,$
               list_out=list_out
;
; This function returns a synthetic LBH spectrum appropriate for
; photoelectron impact excitation in the thermosphere.  It reads
; the thermospheric temperature to determine the rotational pop-
; ulation distribution.  It assigns the vibrational population
; distribution according to the Franck-Condon factors from the
; ground state.  The output spectrum is binned onto the wavelength
; scale supplied as a parameter.
;
;---------------------------------------------------------------
;
; Calculate the energy levels: electronic (t), vibrational (g),
; and rotational (f) components.  G and F components stored separately
; allows calculation of band origins.  G and F stored as 2-D arrays
; allows addressing according to v and j values.
;
nmax = 40
nv = 7                ;a-state vibration levels
nvv = 20              ;X-state vibration levels
slbh_enlev,nmax,nv,nvv,at,ag_vj,af_vj,xt,xg_vj,xf_vj,bv,bvv
;
; Read in the Franck-Condon factors, compute the band orgins,
; and generate the band transition probabilities for mag dipole
; and elec quadr transitions
;
slbh_fc,at,ag_vj,xt,xg_vj,nu_vv,q_vv,ad_vv,aq_vv
;
; Set up two handy vectors.  One pointing to the upper state rotational
; lines and one pointing to lower state rotational lines of all allowed
; combinations of magnetic dipole and electric quadrupole transitions.
; Special case required for v=6 due to predissociation.
;
slbh_bandstruct,nmax-3,j_hi_05,j_low_05,symm_05,mag_dip_flag_05,hlf_05
slbh_bandstruct,13,j_hi_6,j_low_6,symm_6,mag_dip_flag_6,hlf_6
;
; Define handy constants for population distribution
;
hc_kt = 1./(0.695030*temperature)    ;in cm-1 compatible units
;
; Handy constants for doing wavelength binning
; Assumes an evenly-spaced wavelength grid
;
;print,wavelength
dw = wavelength[1]-wavelength[0]
;print,'here'
w0 = wavelength[0]
nw = n_elements(wavelength)
;
; Make a temporary array with an extra bin on the short wavelength side,
; and an extra bin on the long wavelength side.  These extra bins will
; collect all intensities outside the specified wavelength range, and
; are not stored into the final output array.
;
wavelength = [wavelength[0]-dw,wavelength,wavelength[nw-1]+dw]
tempout = fltarr(nw+2)
out = fltarr(nw,7)
wllist = list()
intlist = list()
;
; Calculate weights:  initial population weights, based on temperature
;                     lead to upper state population weights, after
;                     calculating transitions according to FC factors
;
if ~isa(uspw_in) then begin
  ipw = exp(-hc_kt*xg_vj(indgen(nvv),0)) & ipw = ipw/total(ipw)
  uspw = q_vv[0:nv-1,0:nvv-1]#ipw
  ;
  ; Otherwise use specified weights
  ;
endif else begin
  uspw = fltarr(7)
  uspw[0] = uspw_in
endelse
;
; Consider all initial ground state vibrational populations
; (Currently, no variables depend upon initial ground state; don't loop)
;
if keyword_set(print) then openw,lun,'lbh.txt',/get_lun
for vigs=0,0 do begin
    j_hi = j_hi_05
    j_low= j_low_05
    symm = symm_05
    mag_dip_flag = mag_dip_flag_05
    hlf = hlf_05
;
; Cycle through all progressions from the upper states
;
    for v=0,nv-1 do begin
        if keyword_set(verbose) then print,"$('v=',i1,' progression')",v
        wllistvv = list()
        intlistvv = list()
;
; Special case due to v'=6 predissociation
        if v eq 6 then begin
            j_hi = j_hi_6
            j_low= j_low_6
            symm = symm_6
            mag_dip_flag = mag_dip_flag_6
            hlf = hlf_6
        endif
        nlines = n_elements(j_hi)
;
; Each lower state in the progression
;
        for vv=0,nvv-1 do begin
            wl = 1e8/(nu_vv[v,vv]+af_vj[v,j_hi]-xf_vj[vv,j_low])
            wlbin = (fix((wl-w0)/dw+.5+1)<(nw+1))>0
            wllistvv.add, reform(wl)
;
; Here is a tricky bit:  The N2 is assumed to be in rotational equilibrium with kinetic
; temperature while in the ground state (X) where it spends most of its time.  N2 does not
; remain in the a-state long enough to thermalize, so it retains the rotational J-distribution
; of the ground state
;
; The rotational state population should be multiplied by (2*J'+1)
; multiplicity, but is not
            popj = bvv[vv]*hc_kt/3.*exp(-hc_kt*xf_vj[vv,j_hi])*symm
;            popj = bv[v]*hc_kt/3.*exp(-hc_kt*af_vj[v,j_hi])*symm   ;ignore a-state J distribution
;
; probability should be divided by (2*J'+1) multiplicity, but is not.
; Note that the appropriate band transition probability is selected.
            aq_ad = [aq_vv[v,vv],ad_vv[v,vv]]
            transprob = aq_ad[mag_dip_flag]*hlf

;
; the following is complete except for v' population distribution
            intens = popj*transprob
;
; Printing transition designations. Recall that j_low - j_hi = delta
            if keyword_set(print) then begin
                id = strarr(nlines)
                for l=0,nlines-1 do begin
                    case j_hi[l]-j_low[l] of
                       2 : id[l] = 'O'
                       1 : id[l] = 'P'
                       0 : id[l] = 'Q'
                       -1: id[l] = 'R'
                       -2: id[l] = 'S'
                    endcase
                    id[l] = id[l] + '(' + strtrim(j_low[l],2) + ')'
                    printf,lun,"$(f8.3,2x,e12.5,2x,i1,'-',i2,2x,i2,'-',i2,2x,a10)",wl[l],transprob[l],v,vv,j_hi[l],j_low[l],id[l]
                endfor
            endif
;
; rebin
            tempout[*] = 0.
            for i=0,nlines-1 do tempout[wlbin[i]]=tempout[wlbin[i]]+intens[i]
            out[0,v] = out[*,v] + tempout[1:nw]
            intlistvv.add,reform(intens)*uspw[v]
        endfor
        wllist.add,wllistvv
        intlist.add,intlistvv
        if keyword_set(verbose) then print,"$(f5.1,' usec lifetime')",1e6/total(out[*,v])
        if keyword_set(verbose) then print,"$('Rotational population total=',f6.3)",total(popj)
    endfor
endfor
if keyword_set(print) then free_lun,lun
wavelength = wavelength[1:nw]
;
; Apply the upper state population weights to the transition
; probabilities to generate the final spectrum
;
for i=0,nv-1 do out[0,i] = out[*,i]*uspw[i]
if keyword_set(verbose) then print,'Upper state population weights:'
if keyword_set(verbose) then print,"$(7f9.5)",uspw
;
; If hash argument present the create output hash
;
if arg_present(hash_out) then begin
  wlout = wllist
  intout = intlist
  if ~keyword_set(list_out) then begin
    wave = list()
    intens = list()
    for v=0, n_elements(wllist)-1 do foreach vv, wllist[v] do wave.add, vv, /EXTRACT
    for v=0, n_elements(intlist)-1 do foreach vv, intlist[v] do intens.add, vv, /EXTRACT
    wlarr = wave.ToArray()
    intarr = intens.ToArray()
    wlout = wlarr
    intout = intarr
  endif
  hash_out = hash('wavelength', wlout, 'intensity', intout, /fold_case)
endif
;
; Finished
;
return,out
end
