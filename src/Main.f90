PROGRAM plotRNSS
! ASSUME : Only One Fuel Pin Type

USE mdat, ONLY : fdir, nerr

IMPLICIT NONE

INTEGER :: ierr

fdir = 'C:\Users\user\Documents\MATLAB\'

CALL readinp
CALL readobj(1)
CALL readobj(2)
CALL normpow
CALL chkobj

CALL setgeo

CALL plotpow

CALL calpowerr_3D
CALL calpowerr_int

DO ierr = 1, nerr
  CALL printout(ierr)
END DO

CALL editinfo
CALL editgrid

DO ierr = 1, nerr
  CALL editout(ierr)
END DO

END PROGRAM plotRNSS