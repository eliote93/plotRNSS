SUBROUTINE readRNSS(iobj, fn)
! RNSS Cards in use : 'GRID_HEX', 'K-EFF', 'VTX. DEFORMATION', 'Asy. 2-D Power', 'Ax. 1-D Power', 'Power in All Region'

USE allocs
USE param, ONLY : TRUE, DALLR, DOT, BLANK, oneline, probe, io1
USE mdat,  ONLY : nxa, nya, nsfc, nxy, nz, ndat, izp, powxy, powax, pow3d, aoF2F, keff, ndfrm, odfrm

IMPLICIT NONE

INTEGER :: iobj
CHARACTER(*), INTENT(IN) :: fn
! ------------------------------------------------
CHARACTER*2 :: dumc
CHARACTER*100 :: gn

INTEGER :: Lgh1, Lgh2, fndndata, mz, itmp, mya, mxy, mxy1, mxy2, iz, indev, idat, idfrm, nasy, nzp, ii
LOGICAL :: chknum, ifnumeric
! ------------------------------------------------

indev = io1
gn    = 'RNSS\' // trim(fn) // '.out'
mya   = 0
mz    = 0

izp(:, :, iobj) = 0

CALL openfile(indev, TRUE, gn)
! ------------------------------------------------
!            READ : 1st
! ------------------------------------------------
DO
  READ (indev, '(A1000)', END = 1000) oneline
  IF (oneline .EQ. BLANK) CYCLE
  
  IF (oneline( 4:11) .EQ. 'grid_hex') READ(oneline(16:), *) aoF2F(iobj)
  IF (oneline(11:15) .EQ. 'K-eff')    READ(oneline(18:), *) keff(iobj)
  IF (oneline(14:29) .EQ. 'Vtx. Deformation') THEN
    DO
      READ (indev, '(A1000)', END = 1000) oneline
      IF (probe   .NE. BLANK)      EXIT
      IF (oneline .EQ. BLANK)      EXIT
      IF (.NOT.ifnumeric(oneline)) CYCLE
      
      ndfrm(iobj) = ndfrm(iobj) + 1
      idfrm = ndfrm(iobj)
      READ (oneline, *) odfrm(idfrm, iobj)%ixy, odfrm(idfrm, iobj)%iz, dumc, odfrm(idfrm, iobj)%dx, odfrm(idfrm, iobj)%dy
      SELECT CASE (dumc)
      CASE ('SW'); odfrm(idfrm, iobj)%irdir = 1
      CASE ('NW'); odfrm(idfrm, iobj)%irdir = 2
      CASE ('NN'); odfrm(idfrm, iobj)%irdir = 3
      CASE ('NE'); odfrm(idfrm, iobj)%irdir = 4
      CASE ('SE'); odfrm(idfrm, iobj)%irdir = 5
      CASE ('SS'); odfrm(idfrm, iobj)%irdir = 6
      END SELECT
    END DO
  END IF
END DO

1000 CONTINUE

REWIND (indev)
! ------------------------------------------------
!            READ : 2nd
! ------------------------------------------------
DO
  READ (indev, '(A1000)', END = 2000) oneline
  !IF (probe .EQ. DOT)   EXIT ! ECHO : inp
  IF (probe .NE. DALLR) CYCLE ! READ : Only Output Card
  
  Lgh1 = len_trim(oneline)
  
  SELECT CASE (oneline(6:Lgh1))
  CASE ('Assembly Radial 2-D Power Distribution')
    READ (indev, *)
    READ (indev, *)
    READ (indev, *)
    
    DO
      READ (indev, '(A1000)', END = 2000) oneline
      IF (fndndata(oneline) .EQ. 0) EXIT
      
      Lgh2 = len_trim(oneline)
      mya = mya + 1
      nxa(mya, iobj) = fndndata(oneline) - 1
      
      ! SET : izp
      idat = 0
      DO ii = 8, Lgh2
        IF (oneline(ii-1:ii-1).EQ.BLANK .AND. oneline(ii:ii).NE.BLANK) THEN ! Non-Zero
          idat = idat + 1
          
          IF (idat .EQ. 1) CYCLE
          IF (oneline(ii-4:ii-4).EQ.BLANK) THEN
            nzp = izp(0, mya, iobj) + 1
            izp(  0, mya, iobj) = nzp
            izp(nzp, mya, iobj) = idat
          END IF
        END IF
      END DO
    END DO
    
  CASE ('Axial 1-D Power Distribution')
    READ (indev, *)
    
    DO
      READ (indev, '(A1000)', END = 2000) oneline
      IF (fndndata(oneline) .EQ. 0) EXIT
      
      mz = mz + 1
      READ (oneline, *) itmp, powax(mz, iobj)
    END DO
    
    IF (mz .NE. nz) THEN
      CALL terminate("RNSS # of PLANES")
    END IF
  END SELECT
END DO

2000 CONTINUE

IF (mod(mya, 2) .NE. 1) CALL terminate("EVEN RNSS # of 2-D ASY. (y)")

nya (iobj) = mya
nxy (iobj) = sum(nxa(1:mya, iobj))
nsfc(iobj) = (mya+1)/2
ndat(iobj) = nxy(iobj)*nz

nasy = nxa(   nsfc(iobj), iobj) ! # of Asy. at Cnt. y
nzp  = izp(0, nsfc(iobj), iobj) ! # of Zero Power at Cnt. y
IF (mod(nasy + nzp, 2) .NE. 1) CALL terminate("EVEN RNSS # of 2-D ASY. (x)") ! Asymmetric Zero Power at Periphery

REWIND (indev)
! ------------------------------------------------
!            READ : 3rd
! ------------------------------------------------
DO
  READ (indev, '(A1000)', END = 3000) oneline
  !IF (probe .EQ. DOT)   EXIT ! ECHO : inp
  IF (probe .NE. DALLR) CYCLE ! READ : Only Output Card
  
  Lgh1 = len_trim(oneline)
  SELECT CASE (oneline(6:Lgh1))
  CASE ('Assembly Radial 2-D Power Distribution')
    READ (indev, *)
    READ (indev, *)
    READ (indev, *)
    
    mya  = 0
    mxy1 = 1
    
    DO
      READ (indev, '(A1000)', END = 3000) oneline
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
      READ (indev, '(A1000)', END = 3000) oneline
      IF (fndndata(oneline) .EQ. 0) EXIT
      
      READ (oneline, *) mxy, pow3d(mxy, 1:nz, iobj)
    END DO
  END SELECT
END DO

3000 CONTINUE

CLOSE (indev) ! 1
! ------------------------------------------------

END SUBROUTINE readRNSS