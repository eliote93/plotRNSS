! --------------------------------------------------------------------------------------------------
SUBROUTINE readbench_rad()

USE mdat, ONLY : lbnch, cbnch, lerr, xstr2d, ystr2d, nsize2d, gcf2d, gca2d, xstr1d, ystr1d, nsize1d, gcf1d, gca1d, nz, hgt

IMPLICIT NONE
! ------------------------------------------------

IF (.NOT. lbnch) RETURN

IF (cbnch(1:3) .EQ. 'BCN') THEN
  IF (lerr) THEN
    xstr2d  =  47
    ystr2d  = -55
    nsize2d =  20
    
    gcf2d = [500, 100, 1070, 800]
    gca2d = [0.11, 0.0035, 0.76, 1.05]
  ELSE
    xstr2d  =  47
    ystr2d  = -55
    nsize2d =  25
    
    gcf2d = [500, 100, 1070, 800]
    gca2d = [0.11, 0.0035, 0.75, 1.05]
  END IF
ELSE IF (cbnch(1:5) .EQ. 'PGSFR') THEN
  IF (lerr) THEN
    xstr2d  =  47
    ystr2d  = -55
    nsize2d =  25
    
    gcf2d = [500, 100, 1070, 800]
    gca2d = [0.11, 0.0035, 0.75, 1.05]
  ELSE
    xstr2d  =  30
    ystr2d  = -35
    nsize2d =  23
    
    gcf2d = [500, 100, 1070, 810]
    gca2d = [0.11, 0.0035, 0.75, 1.05]
  END IF
ELSE IF (cbnch(1:9) .EQ. 'V4_CHAO95') THEN
  IF (lerr) THEN
    xstr2d  =  100
    ystr2d  = -120
    nsize2d =  20
    
    gcf2d = [500, 100, 1150, 890]
    gca2d = [0.13, 0.108, 0.727, 0.91]
  ELSE
    xstr2d  =  100
    ystr2d  = -120
    nsize2d =  25
    
    gcf2d = [500, 100, 1100, 850]
    gca2d = [0.13, 0.112, 0.727, 0.89]
  END IF
ELSE IF (cbnch(1:2) .EQ. 'S3') THEN
  IF (lerr) THEN
    xstr2d  =  50
    ystr2d  = -95
    nsize2d =  25
    
    ! Max. 2 %
    !gcf2d = [500,  80, 1090, 900]
    !gca2d = [0.11, 0.125, 0.755, 0.86]
    
    ! Max. 5 %
    gcf2d = [500,  80, 1075, 920]
    gca2d = [0.11, 0.128, 0.785, 0.86]
    
    ! Max. 10 %
    !gcf2d = [500,  80, 1100, 920]
    !gca2d = [0.11, 0.125, 0.77, 0.86]
  ELSE
    xstr2d  =  50
    ystr2d  = -95
    nsize2d =  30
    
    ! Max. 3 %
    gcf2d = [500, 100, 1060, 890]
    gca2d = [0.11, 0.134, 0.765, 0.85]
  END IF
ELSE
  CALL terminate("WRONG BENCH")
END IF
! ------------------------------------------------

END SUBROUTINE readbench_rad
! --------------------------------------------------------------------------------------------------
SUBROUTINE readbench_ax()

USE allocs
USE mdat, ONLY : lbnch, cbnch, l3d, lerr, xstr2d, ystr2d, nsize2d, gcf2d, gca2d, xstr1d, ystr1d, nsize1d, gcf1d, gca1d, nz, hgt

IMPLICIT NONE
! ------------------------------------------------

IF (.NOT. lbnch) RETURN
IF (.NOT. l3d)   RETURN

xstr1d  = 10
nsize1d = 30
gcf1d   = [500, 100, 1050, 730]

IF (cbnch(1:3) .EQ. 'BCN') THEN
  nz = 20
  CALL dmalloc(hgt, nz)
  hgt(1:nz) = 4.
  
  IF (lerr) THEN
    ystr1d =  0.
    gca1d  = [0.125, 0.17, 0.85, 0.8]
  ELSE
    ystr1d = 0.2
    gca1d  = [0.11, 0.16, 0.86, 0.82]
  END IF
ELSE IF (cbnch(1:5) .EQ. 'PGSFR') THEN
  nz = 13
  CALL dmalloc(hgt, nz)
  hgt(1:10) = 7.252
  hgt(11)   = 8.
  hgt(12)   = 8.867
  hgt(13)   = 8.433
  
  IF (lerr) THEN
    ystr1d =  0.
    gca1d  = [0.125, 0.17, 0.85, 0.8]
  ELSE
    ystr1d = 0.2
    gca1d  = [0.11, 0.16, 0.86, 0.82]
  END IF
ELSE
  CALL terminate("WRONG BENCH")
END IF
! ------------------------------------------------

END SUBROUTINE readbench_ax
! --------------------------------------------------------------------------------------------------