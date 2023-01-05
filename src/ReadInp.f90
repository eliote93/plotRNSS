! --------------------------------------------------------------------------------------------------
SUBROUTINE openinp()

USE param, ONLY : io1
USE mdat,  ONLY : inpfn

IMPLICIT NONE

INTEGER :: indev, narg, ichar1st
LOGICAL :: lext
! ------------------------------------------------

narg = iargc()
IF (narg .EQ. 1) THEN ! User Inputted
  CALL getarg(narg, inpfn)
  inpfn = trim(inpfn)
  
  ichar1st = ichar(inpfn(1:1))
  IF (ichar1st.EQ.0 .OR. ichar1st.EQ.32) CALL terminate("WRONG USER INPUT")
ELSE ! Default
  inpfn = 'plotRNSS.inp'
END IF

INQUIRE (FILE = inpfn, EXIST = lext)
IF (.NOT. lext) CALL terminate("FILE DOES NOT EXIST - " // inpfn)
OPEN (io1, FILE = inpfn)
! ------------------------------------------------

END SUBROUTINE openinp
! --------------------------------------------------------------------------------------------------
SUBROUTINE readinp()

USE allocs
USE param, ONLY : DOT, BANG, BLANK, SLASH, TRUE, FALSE, ONE, oneline, probe, io1, ZERO
USE mdat,  ONLY : l3d, l02, objcn, objfn, lerr, plotobj, xstr2d, ystr2d, nsize2d, xstr1d, ystr1d, nsize1d, gcf2d, gca2d, gcf1d, gca1d, nz, hgt, avghgt, xylmax, zlmax, aoF2F, nerr, iedterr, lbnch, cbnch, nMC

IMPLICIT NONE

CHARACTER*12 :: cn, tmp2

INTEGER :: lgh, fndndata, ndat, idat, nchr, indev
INTEGER :: ipos(2)

CHARACTER*30, DIMENSION(100) :: tmp1
! ------------------------------------------------

indev = io1

DO
  READ (indev, '(A1000)', END = 1000) oneline
  IF (probe .EQ. DOT)   EXIT
  IF (probe .EQ. BANG)  CYCLE
  IF (probe .EQ. BLANK) CYCLE
  
  READ (oneline, '(A12)') cn
  CALL toupper(cn)
  
  lgh = len(oneline)
  
  SELECT CASE (cn)
  CASE ('ID_01')
    READ  (oneline, *) cn, objcn(1)
    CALL toupper(objcn(1))
    IF (objcn(1) .EQ. 'MC') READ  (oneline, *) cn, objcn(1), nMC(1)
    
    CALL fndchr(oneline, ipos, nchr, SLASH)
    objfn(1) = oneline(ipos(1)+1:lgh)
    CALL rmvfnblnk(objfn(1))
    
  CASE ('ID_02')
    l02 = TRUE
    
    READ  (oneline, *) cn, objcn(2)
    CALL toupper(objcn(2))
    IF (objcn(2) .EQ. 'MC') READ  (oneline, *) cn, objcn(2), nMC(2)
    
    CALL fndchr(oneline, ipos, nchr, SLASH)
    objfn(2) = oneline(ipos(1)+1:lgh)
    CALL rmvfnblnk(objfn(2))
    
  CASE ('PLOT_ERR')
    READ (oneline, *) cn, lerr, plotobj
    
  CASE ('EDIT_ERR')
    READ (oneline, *) cn, iedterr
    
  CASE ('BENCH')
    READ (oneline, *) cn, cbnch
    CALL toupper(cbnch)
    lbnch = TRUE
        
  CASE ('TPOS_1D')
    READ (oneline, *) cn, xstr1d, ystr1d
    
  CASE ('TPOS_2D')
    READ (oneline, *) cn, xstr2d, ystr2d
  
  CASE ('TSIZE_1D')
    READ (oneline, *) cn, nsize1D
    
  CASE ('TSIZE_2D')
    READ (oneline, *) cn, nsize2D
    
  CASE ('GCF_1D')
    READ (oneline, *) cn, gcf1d(1:4)
    
  CASE ('GCF_2D')
    READ (oneline, *) cn, gcf2d(1:4)
    
  CASE ('GCA_1D')
    READ (oneline, *) cn, gca1d(1:4)
    
  CASE ('GCA_2D')
    READ (oneline, *) cn, gca2d(1:4)
    
  CASE ('HGT')
    ndat = fndndata(oneline)-1
    tmp1 = BLANK
    READ (oneline, *, END = 500) cn, tmp1
    500 CONTINUE
    
    DO idat = 1, 100
      READ (tmp1(idat), '(A)') tmp2
      
      IF (tmp2(1:1).EQ.BLANK .OR. tmp2(1:1).EQ.BANG) EXIT
    END DO
    
    nz = idat-1
    CALL dmalloc(hgt, nz)
    READ (oneline, *) cn, hgt(1:nz)
    
  CASE ('XYLMAX')
    READ (oneline, *) cn, xylmax
    
  CASE ('ZLMAX')
    READ (oneline, *) cn, zlmax
        
  CASE DEFAULT
    CALL terminate("READ INP")
  END SELECT
END DO

1000 CONTINUE
! ------------------------------------------------
IF (probe .NE. DOT) CALL terminate("INPUT MUST END WITH DOT")

CLOSE (indev) ! 1
! ------------------------------------------------

END SUBROUTINE readinp
! --------------------------------------------------------------------------------------------------
SUBROUTINE fininp()

USE allocs
USE param, ONLY : MP, ZERO
USE mdat,  ONLY : l02, lerr, l3d, plotobj, nz, nerr, hgt, avghgt, iedterr, nMC, xylmin, xylmax, zlmin, zlmax

IMPLICIT NONE
! ------------------------------------------------

! CHK : plot mod
IF (.NOT.l02  .AND. plotobj.EQ.2) CALL terminate("WRONG PLOTTING OBJECT")
IF (.NOT.lerr .AND. iedterr.EQ.1) CALL terminate("EDIT ERR.")

IF (nMC(1).NE.0 .AND. (nMC(1).LT.1 .OR. nMC(1).GT.9)) CALL terminate("MC. #")
IF (nMC(2).NE.0 .AND. (nMC(2).LT.1 .OR. nMC(2).GT.9)) CALL terminate("MC. #")

! Basic
l3d = nz .GT. 1
IF (.NOT. associated(hgt)) CALL dmalloc1(hgt, 1)
avghgt = sum(hgt(1:nz)) / nz

nerr = 1 ! ABS
IF (lerr) nerr = 2 ! ABS, REL
IF (iedterr .GT. 0) nerr = 1 ! ABS

IF (lerr) THEN
  xylmin = -xylmax
  zlmin  = -zlmax
ELSE
  xylmin = ZERO
  zlmin  = ZERO
END IF
! ------------------------------------------------

END SUBROUTINE fininp
! --------------------------------------------------------------------------------------------------