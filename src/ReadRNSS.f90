SUBROUTINE readRNSS(iobj, fn)

USE allocs
USE param, ONLY : TRUE, DALLR, DOT, BLANK, oneline, probe, io1
USE mdat,  ONLY : nxa, nya, nsfc, nxy, nz, ndat, izp, powxy, powax, pow3d, aoF2F, keff

IMPLICIT NONE

INTEGER :: iobj
CHARACTER(*), INTENT(IN) :: fn
! ------------------------------------------------
CHARACTER*100 :: gn

INTEGER :: Lgh, fndndata, mz, itmp, mya, mxy, mxy1, mxy2, iz, indev, ist, ied, idat, jdat
LOGICAL :: chknum
! ------------------------------------------------

indev = io1
gn    = 'RNSS\' // trim(fn) // '.out'
mya   = 0
mz    = 0

izp(:, :, iobj) = 0

CALL openfile(indev, TRUE, gn)
! ------------------------------------------------
!            01. READ : 1st
! ------------------------------------------------
DO
  READ (indev, '(A1000)', END = 1000) oneline
  
  IF (oneline( 4:11) .EQ. 'grid_hex') READ(oneline(16:), *) aoF2F(iobj)
  IF (oneline(11:15) .EQ. 'K-eff')    READ(oneline(18:), *) keff(iobj)
  
  !IF (probe .EQ. DOT)   EXIT ! ECHO : inp
  IF (probe .NE. DALLR) CYCLE ! READ : Only Output Card
  
  Lgh = len_trim(oneline)
  
  SELECT CASE (oneline(6:Lgh))
  CASE ('Assembly Radial 2-D Power Distribution')
    READ (indev, *)
    READ (indev, *)
    READ (indev, *)
    
    DO
      READ (indev, '(A1000)', END = 1000) oneline
      
      IF (fndndata(oneline) .EQ. 0) EXIT
      
      mya = mya + 1
      
      nxa(mya, iobj) = fndndata(oneline) - 1
      
      ! izp
      DO idat = 11, len_trim(oneline)
        IF (oneline(idat:idat) .EQ. BLANK) CYCLE
        
        ist = idat
        
        DO jdat = ist+1, len_trim(oneline)
          IF (oneline(jdat:jdat) .EQ. BLANK) EXIT
        END DO
        
        ied = jdat
        
        EXIT
      END DO
      
      DO idat = 2, nxa(mya, iobj)
        DO jdat = ied+1, len_trim(oneline)
          IF (oneline(jdat:jdat) .NE. BLANK) EXIT
        END DO
        
        IF (jdat-ist .GT. 8) THEN
          izp(0, mya, iobj) = izp(0, mya, iobj) + 1
          izp(izp(0, mya, iobj), mya, iobj) = idat
        END IF
        
        ist = jdat
        
        DO jdat = ist+1, len_trim(oneline)
          IF (oneline(jdat:jdat) .EQ. BLANK) EXIT
        END DO
        
        ied = jdat
      END DO
    END DO
    
  CASE ('Axial 1-D Power Distribution')
    READ (indev, *)
    
    DO
      READ (indev, '(A1000)', END = 1000) oneline
      
      IF (fndndata(oneline) .EQ. 0) EXIT
      
      mz = mz + 1
      
      READ (oneline, *) itmp, powax(mz, iobj)
    END DO
    
    IF (mz .NE. nz) THEN
      CALL terminate("RNSS # of PLANES")
    END IF
  END SELECT
END DO

1000 CONTINUE

IF (mod(mya, 2) .NE. 1) CALL terminate("EVEN RNSS # of 2-D ASY. (y)")

nya (iobj) = mya
nxy (iobj) = sum(nxa(1:mya, iobj))
nsfc(iobj) = (mya+1)/2
ndat(iobj) = nxy(iobj) * nz

IF (mod(nxa(nsfc(iobj), iobj) + izp(0, nsfc(iobj), iobj), 2) .NE. 1) CALL terminate("EVEN MASTER # of 2-D ASY. (x)") ! Asymmetric Zero Power at Periphery

REWIND (indev)
! ------------------------------------------------
!            02. READ : 2nd
! ------------------------------------------------
DO
  READ (indev, '(A1000)', END = 2000) oneline
  
  !IF (probe .EQ. DOT)   EXIT ! ECHO : inp
  IF (probe .NE. DALLR) CYCLE ! READ : Only Output Card
  
  Lgh = len_trim(oneline)
  
  SELECT CASE (oneline(6:Lgh))
  CASE ('Assembly Radial 2-D Power Distribution')
    READ (indev, *)
    READ (indev, *)
    READ (indev, *)
    
    mya  = 0
    mxy1 = 1
    
    DO
      READ (indev, '(A1000)', END = 2000) oneline
      
      IF (fndndata(oneline) .EQ. 0) EXIT
      
      mya  = mya + 1
      mxy2 = mxy1 + nxa(mya, iobj) - 1
      
      READ (oneline, *) itmp, powxy(mxy1:mxy2, iobj)
      
      mxy1 = mxy2 + 1
    END DO
    
  CASE ('Power Distribution in All Region')
    DO
      READ (indev, '(A1000)') oneline
      
      IF (.NOT. chknum(oneline)) CYCLE
      
      BACKSPACE (indev)
      EXIT
    END DO
    
    DO
      READ (indev, '(A1000)', END = 2000) oneline
      
      IF (fndndata(oneline) .EQ. 0) EXIT
      
      READ (oneline, *) mxy, pow3d(mxy, 1:nz, iobj)
    END DO
  END SELECT
END DO

2000 CONTINUE

CLOSE (indev) ! 1
! ------------------------------------------------

END SUBROUTINE readRNSS