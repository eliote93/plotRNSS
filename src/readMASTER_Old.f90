SUBROUTINE readMASTER_Old(iobj, fn)

USE allocs
USE param, ONLY : TRUE, DALLR, DOT, BLANK, oneline, probe, io1, SQ3
USE mdat,  ONLY : nxa, nya, nsfc, nxy, nz, ndat, powxy, powax, pow3d, aoF2F

IMPLICIT NONE

INTEGER :: iobj
CHARACTER(*), INTENT(IN) :: fn
! ------------------------------------------------
CHARACTER*100 :: gn

INTEGER :: Lgh, fndndata, itmp, mya, mxy, mxy1, mxy2, iz, indev
REAL :: rtmp
! ------------------------------------------------

indev = io1
gn    = 'MASTER\' // trim(fn) // '.out'
mya   = 0

CALL openfile(indev, TRUE, gn)
! ------------------------------------------------
!            01. READ : 1st
! ------------------------------------------------
DO
  READ (indev, '(A1000)', END = 1000) oneline
  
  IF (oneline(2:9) .EQ. '%GEN_GEO') THEN
    READ (indev, '(A1000)', END = 1000) oneline
    READ(oneline, *) aoF2F(iobj)
    aoF2F(iobj) = aoF2F(iobj)*SQ3
  END IF
  
  Lgh = len_trim(oneline)
  
  IF (Lgh .LT. 57) CYCLE
  
  SELECT CASE (oneline(24:57))
  CASE ('MAXIMUM PIN AND AXIALLY AVERAGED P') ! 2-D
    DO itmp = 1, 21
      READ (indev, *)
    END DO
    
    DO
      READ (indev, '(A1000)', END = 1000) oneline
      
      IF (fndndata(oneline) .EQ. 0) EXIT
      
      mya = mya + 1
      
      nxa(mya, iobj) = fndndata(oneline) - 1
      
      DO itmp = 1, 3
        READ (indev, *)
      END DO
    END DO
        
  CASE ('LAYER AVERAGED POWER DISTRIBUTIONS') ! 1-D
    DO itmp = 1, 12
      READ (indev, *)
    END DO
    
    DO iz = 1, nz
      READ (indev, '(A1000)', END = 1000) oneline
      
      READ (oneline, *) itmp, rtmp, powax(nz - iz + 1, iobj) ! NOTICE : Reverse
    END DO
  END SELECT
END DO

1000 CONTINUE

IF (mod(mya, 2) .NE. 1) CALL terminate("EVEN MASTER # of 2-D ASY. (y)")

IF (mod(nxa((mya+1)/2, iobj), 2) .NE. 1) CALL terminate("EVEN MASTER # of 2-D ASY. (x)") ! Zero Asy. Power .OR. Asymmetric

nya (iobj) = mya
nxy (iobj) = sum(nxa(1:mya, iobj))
ndat(iobj) = nxy(iobj)*nz
nsfc(iobj) = (mya+1)/2

REWIND (indev)
! ------------------------------------------------
!            02. READ : 2nd
! ------------------------------------------------
DO
  READ (indev, '(A1000)', END = 2000) oneline
  
  Lgh = len_trim(oneline)
  
  IF (Lgh .LT. 57) CYCLE
  
  SELECT CASE (oneline(24:57))
  CASE ('MAXIMUM PIN AND AXIALLY AVERAGED P') ! 2-D
    DO itmp = 1, 21
      READ (indev, *)
    END DO
    
    mya  = 0
    mxy1 = 1
    
    DO
      READ (indev, '(A1000)', END = 2000) oneline
      
      IF (fndndata(oneline) .EQ. 0) EXIT
      
      mya  = mya + 1
      mxy2 = mxy1 + nxa(mya, iobj) - 1
      
      READ (oneline, *) itmp, powxy(mxy1:mxy2, iobj)
      
      mxy1 = mxy2 + 1
      
      DO itmp = 1, 3
        READ (indev, *)
      END DO
    END DO
  
  CASE ('MAXIMUM PIN AND NODAL POWER DISTRI') ! 3-D
    DO itmp = 1, 15
      READ (indev, *)
    END DO
    
    DO iz = nz, 1, -1 ! NOTICE : Reverse
      mya  = 0
      mxy1 = 1
      
      DO itmp = 1, 4
        READ (indev, *)
      END DO
      
      DO
        READ (indev, '(A1000)', END = 2000) oneline
        
        IF (fndndata(oneline) .EQ. 0) EXIT
        
        mya  = mya + 1
        mxy2 = mxy1 + nxa(mya, iobj) - 1
        
        READ (oneline, *) itmp, pow3d(mxy1:mxy2, iz, iobj)
        
        mxy1 = mxy2 + 1
        
        DO itmp = 1, 3
          READ (indev, *)
        END DO
      END DO
    END DO
  END SELECT
END DO

2000 CONTINUE

CLOSE (indev) ! 1
! ------------------------------------------------

END SUBROUTINE readMASTER_Old