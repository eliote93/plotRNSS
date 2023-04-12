PROGRAM plotRNSS
! ASSUME : Only One Fuel Pin Type

USE mdat, ONLY : fdir, nerr

IMPLICIT NONE

INTEGER :: ierr

fdir = 'C:\Users\rpl 6\Documents\MATLAB\'

CALL default
CALL openinp
CALL readinp
CALL readbench_rad
CALL readbench_ax
CALL fininp
CALL readobj(1)
CALL readobj(2)
CALL setmcpow1d2d
CALL adjmcpowlv
CALL chkobj

CALL setgeo
CALL setdfrm

CALL plotpow

CALL calpowerr_3D
CALL calpowerr_int

CALL editinfo
CALL editgrid

DO ierr = 1, nerr
  CALL printout(ierr)
  CALL editout(ierr)
  CALL editerr(ierr)
END DO

END PROGRAM plotRNSS