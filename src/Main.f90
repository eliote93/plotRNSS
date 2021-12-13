PROGRAM plotRNSS
! ASSUME : Only One Fuel Pin Type

USE mdat, ONLY : fdir

IMPLICIT NONE

fdir = 'C:\Users\user\Documents\MATLAB\'

CALL readinp
CALL readobj(1)
CALL readobj(2)
CALL chkobj

CALL setgeo

CALL plotpow

CALL calpowerr_3D
CALL calpowerr_int

CALL printout
CALL editinfo
CALL editgrid
CALL editout

END PROGRAM plotRNSS