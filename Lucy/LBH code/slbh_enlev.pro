pro slbh_enlev,nmax,nv,nvv,a_t,a_g,a_f,x_t,x_g,x_f,bv,bvv
;
; Computes the electronic, vibrational, and rotational energy levels
; of the a, X states of N2.  Also computes rotataional constants.
;  NMAX   no. upper and lower rotational levels
;  NV     no. upper vibrational levels
;  NVV    no. lower vibrational levels
;
; a  1 Pi g  state
;
aT0   = 68951.20             ;electronic
awe   =  1694.208            ;vibrational harmonic
awexe =    13.9491           ;vibrational anharmonic
aweye =      .007935
aweze =      .0002914
abe   =     1.6169           ;rotational
aae   =      .01793
age   =     -.0000293
ade   =      .00000589       ;centrifugal
;
; X  1 Sigma g  state
;
xT0   =     0.0              ;electronic
xwe   =  2359.13             ;vibrational harmonic
xwexe =    14.295            ;vibrational anharmonic
xweye =      .004160
xweze =     -.0004374
xbe   =     1.998241         ;rotational
xae   =      .017318
xge   =     -.000033
xde   =      .00000576       ;centrigugal
;
; Define handy vectors:
;    v'+1/2, v"+1/2, j'(=j"), and vectors of 1's to make matrices
;
v12 = findgen(NV)+.5    & i12 = replicate(1,NV)
vv12 = findgen(NVV)+.5  & ii12 = replicate(1,NVV)
j = long(indgen(NMAX))   & i = replicate(1,NMAX)
jj1 = j*(j+1)
;
; a-state
;
A_T = aT0 - awe/2 + awexe/4 $
      - aweye/8 - aweze/16               ;electronic energy
A_G=(    awe*V12   - awexe*V12^2 $
     + aweye*V12^3 + aweze*V12^4 ) # i   ;vibrational energy

BV = abe - aae*V12 + age*V12^2           ;rotational constants
DV = aDE*i12                             ;centrifugal constants
A_F = BV#JJ1-DV#(JJ1^2)                  ;rotational energy
;
; X-state
;
X_t = xt0 - xwe/2 + xwexe/4 $
      - xweye/8 - xweze/16               ;electronic energy
X_G=(    xwe*VV12   - xwexe*VV12^2 $
     + xweye*VV12^3 + xweze*VV12^4 ) # i ;vibrational energy
BVV = XBE - XAE*VV12 + XGE*VV12          ;rotational constants
DVV = XDE*ii12                           ;centrifugal constants
X_F = BVV#JJ1-DVV#(JJ1^2)
;
; finished
;
return
END
