! --------------------------------------------------------------------------------------------------
SUBROUTINE readbench_rad(oneline)

USE allocs
USE mdat,  ONLY : l3d, xstr2d, ystr2d, nsize2d, gcf2d, gca2d, xstr1d, ystr1d, nsize1d, gcf1d, gca1d, nz, hgt

IMPLICIT NONE

CHARACTER*512 :: oneline
CHARACTER*20 :: cn, tmp
! ------------------------------------------------

READ (oneline, *) cn, tmp
CALL toupper(tmp)

IF (tmp(1:7) .EQ. 'BCN_ERR') THEN
  xstr2d  =  47
  ystr2d  = -55
  nsize2d =  20
  
  gcf2d = [500, 100, 1070, 800]
  gca2d = [0.11, 0.0035, 0.76, 1.05]
ELSE IF (tmp(1:7) .EQ. 'BCN_POW') THEN
  xstr2d  =  47
  ystr2d  = -55
  nsize2d =  25
  
  gcf2d = [500, 100, 1070, 800]
  gca2d = [0.11, 0.0035, 0.75, 1.05]
ELSE IF (tmp(1:9) .EQ. 'PGSFR_ERR') THEN
  xstr2d  =  47
  ystr2d  = -55
  nsize2d =  25
  
  gcf2d = [500, 100, 1070, 800]
  gca2d = [0.11, 0.0035, 0.75, 1.05]
ELSE IF (tmp(1:9) .EQ. 'PGSFR_POW') THEN
  xstr2d  =  30
  ystr2d  = -35
  nsize2d =  23
  
  gcf2d = [500, 100, 1070, 810]
  gca2d = [0.11, 0.0035, 0.75, 1.05]
ELSE IF (tmp(1:13) .EQ. 'V4_CHAO95_ERR') THEN
  xstr2d  =  100
  ystr2d  = -120
  nsize2d =  20
  
  gcf2d = [500, 100, 1150, 890]
  gca2d = [0.13, 0.108, 0.727, 0.91]
ELSE IF (tmp(1:13) .EQ. 'V4_CHAO95_POW') THEN
  xstr2d  =  100
  ystr2d  = -120
  nsize2d =  25
  
  gcf2d = [500, 100, 1100, 850]
  gca2d = [0.13, 0.112, 0.727, 0.89]
ELSE IF (tmp(1:10) .EQ. 'SNR300_ERR') THEN
  xstr2d  =  50
  ystr2d  = -95
  nsize2d =  25
  
  gcf2d = [500, 100, 1080, 890]
  gca2d = [0.11, 0.128, 0.77, 0.85]
ELSE IF (tmp(1:10) .EQ. 'SNR300_POW') THEN
  xstr2d  =  50
  ystr2d  = -85
  nsize2d =  30
  
  gcf2d = [500, 100, 1060, 890]
  gca2d = [0.11, 0.128, 0.77, 0.85]
ELSE
  CALL terminate("WRONG BENCH")
END IF
! ------------------------------------------------

END SUBROUTINE readbench_rad
! --------------------------------------------------------------------------------------------------
SUBROUTINE readbench_ax(oneline)

USE allocs
USE mdat,  ONLY : l3d, xstr2d, ystr2d, nsize2d, gcf2d, gca2d, xstr1d, ystr1d, nsize1d, gcf1d, gca1d, nz, hgt

IMPLICIT NONE

CHARACTER*512 :: oneline
CHARACTER*20 :: cn, tmp
! ------------------------------------------------

READ (oneline, *) cn, tmp
CALL toupper(tmp)

xstr1d  = 10
nsize1d = 30
gcf1d   = [500, 100, 1050, 730]

IF (tmp(1:3) .EQ. 'BCN') THEN
  nz = 20
  CALL dmalloc(hgt, nz)
  hgt(1:nz) = 4.
  
  IF (tmp(5:7) .EQ. 'ERR') THEN
    ystr1d =  0.
    gca1d  = [0.125, 0.17, 0.85, 0.8]
  ELSE IF (tmp(5:7) .EQ. 'POW') THEN
    ystr1d = 0.2
    gca1d  = [0.11, 0.16, 0.86, 0.82]
  END IF
ELSE IF (tmp(1:5) .EQ. 'PGSFR') THEN
  nz = 13
  CALL dmalloc(hgt, nz)
  hgt(1:10) = 7.252
  hgt(11)   = 8.
  hgt(12)   = 8.867
  hgt(13)   = 8.433
  
  IF (tmp(7:9) .EQ. 'ERR') THEN
    ystr1d =  0.
    gca1d  = [0.125, 0.17, 0.85, 0.8]
  ELSE IF (tmp(7:9) .EQ. 'POW') THEN
    ystr1d = 0.2
    gca1d  = [0.11, 0.16, 0.86, 0.82]
  END IF
ELSE
  CALL terminate("WRONG BENCH")
END IF
! ------------------------------------------------

END SUBROUTINE readbench_ax
! --------------------------------------------------------------------------------------------------