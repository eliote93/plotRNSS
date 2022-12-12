! --------------------------------------------------------------------------------------------------
SUBROUTINE printout(ierr)

USE param, ONLY : ERRABS, ERRREL
USE mdat, ONLY : l3d, lerr, xyztotmax, xyztotrms, xyztotpf, xymax, xyrms, xyzpf, axmax, axrms, axpf

IMPLICIT NONE

INTEGER :: ierr
CHARACTER*4 :: ctmp
! ------------------------------------------------

SELECT CASE (ierr)
CASE (ERRABS); ctmp = 'Abs.'
CASE (ERRREL); ctmp = 'Rel.'
END SELECT

IF (lerr) THEN
  IF (l3d) THEN
    WRITE (*, '(A36, F7.2, X, A3)') '3-D Power ' // ctmp // ' Error Max. : ', xyztotmax(ierr), '(%)'
    WRITE (*, '(A36, F7.2, X, A3)') '3-D Power ' // ctmp // ' Error RMS  : ', xyztotrms(ierr), '(%)'
  END IF
  
  WRITE (*, '(A36, F7.2, X, A3)') '2-D Power ' // ctmp // ' Error Max. : ', xymax(ierr), '(%)'
  WRITE (*, '(A36, F7.2, X, A3)') '2-D Power ' // ctmp // ' Error RMS  : ', xyrms(ierr), '(%)'
  
  IF (l3d) THEN
    WRITE (*, '(A36, F7.2, X, A3)') '1-D Power ' // ctmp // ' Error Max. : ', axmax(ierr), '(%)'
    WRITE (*, '(A36, F7.2, X, A3)') '1-D Power ' // ctmp // ' Error RMS  : ', axrms(ierr), '(%)'
  END IF
ELSE
  IF (l3d) WRITE (*, '(A27, F7.2)') '3-D Power Peaking Factor : ', xyztotpf
           WRITE (*, '(A27, F7.2)') '2-D Power Peaking Factor : ', xyzpf(0)
  IF (l3d) WRITE (*, '(A27, F7.2)') '1-D Power Peaking Factor : ', axpf
END IF

WRITE (*,*)
! ------------------------------------------------

END SUBROUTINE printout
! --------------------------------------------------------------------------------------------------
SUBROUTINE editinfo()

USE param, ONLY : FALSE, DOT, io1, io2, oneline, probe, ERRABS
USE mdat,  ONLY : l3d, objfn, objcn, plotobj, nz, nxy, lerr, xylim, zlim, xstr2d, ystr2d, nsize2d, gcf2d, gca2d, xstr1d, ystr1d, nsize1d, gcf1d, gca1d, powerr

IMPLICIT NONE

INTEGER :: indev, jndev, nn
CHARACTER*100 :: locfn
! ------------------------------------------------

! Echo
jndev = io1
OPEN (jndev, FILE = 'plotRNSS.inp')

indev = io2
WRITE (locfn, '(A, A5)') trim(objfn(plotobj)), '.info'
CALL openfile(indev, FALSE, locfn)

WRITE (indev, '(A6/)') "$ Echo"
DO
  READ  (jndev, '(A1000)') oneline
  WRITE (indev, '(A)') trim(oneline)
  
  IF (probe .NE. DOT) CYCLE
  
  BACKSPACE (indev)
  EXIT
END DO

WRITE (indev, *)
CLOSE (jndev) ! 1

! Info. : Rad.
WRITE (indev, '(A6/)') "$ Rad."

IF (l3d) THEN
  nn = nz + 1
ELSE
  nn = 1
END IF

IF (xylim .LT. 0) THEN
  IF (lerr) THEN
    xylim = max(maxval(powerr(1:nxy(plotobj), 1:nz, ERRABS)), abs(minval(powerr(1:nxy(plotobj), 1:nz, ERRABS)))) ! Fixed
  ELSE
    xylim = maxval(powerr(1:nxy(plotobj), 1:nz, 1))
  END IF
END IF

WRITE (indev, '(A9, X, 2I6, A25)') "# of Dat.", nxy(plotobj), nn,        " ! Asy., Img."
WRITE (indev, '(A9, X, 2L6, A27)') "LOGICAL",   lerr, l3d,               " ! err, rel, 3d"
WRITE (indev, '(A9, X, 3I6, A19)') "String",    xstr2d, ystr2d, nsize2d, " ! x, y, size"
WRITE (indev, '(A9, X, 4I6)')      "GCF",       gcf2D(1:4)
WRITE (indev, '(A9, X, 4F6.3)')    "GCA",       gca2D(1:4)
WRITE (indev, '(A9, X, 2F6.1)')    "yMax",      xylim

IF (lerr) THEN
  WRITE (indev, '(A9, X, A31)')    "Label",     "Normalized Asy. Power Error (%)"
ELSE
  WRITE (indev, '(A9, X, A21)')    "Label",     "Normalized Asy. Power"
END IF

! Info. : Ax.
IF (l3d) THEN
  WRITE (indev, '(/A5/)') "$ Ax."
  
  nn = 1 ! Manually Inputted
  
  WRITE (indev, '(A9, X, 2I6, A25)')          "# of Dat.", nz, nn,                  " ! Pln., Img."
  WRITE (indev, '(A9, X, I6, F6.3, I6, A19)') "String",    xstr1d, ystr1d, nsize1d, " ! x, y, size"
  WRITE (indev, '(A9, X, 4I6)')               "GCF",       gcf1D(1:4)
  WRITE (indev, '(A9, X, 4F6.3)')             "GCA",       gca1D(1:4)
  WRITE (indev, '(A9, X, F6.3)')              "yMax",      zlim
  
  IF (lerr) THEN
    WRITE (indev, '(A9, X, A30)')    "Label",     "Normalized Pln. Power Eror (%)"
  ELSE
    WRITE (indev, '(A9, X, A21)')    "Label",     "Normalized Pln. Power"
  END IF
END IF

WRITE (indev, '(A1)') DOT
CLOSE (indev) ! 2
! ------------------------------------------------

END SUBROUTINE editinfo
! --------------------------------------------------------------------------------------------------
SUBROUTINE editgrid()

USE param, ONLY : FALSE, HALF, ONE, ZERO, SQ3, DOT, io2
USE mdat,  ONLY : l3d, nxy, nz, objfn, plotobj, aoF2F, hgt, cntxy, lptb, optb

IMPLICIT NONE

INTEGER :: indev, ixy, iz, iptb, ipt, irdir
LOGICAL :: chksamepts
REAL :: aoPch
REAL, DIMENSION(6) :: x0, y0, x1, y1
CHARACTER*100 :: locfn
! ------------------------------------------------

indev = io2
WRITE (locfn, '(A, A5)') trim(objfn(plotobj)), '.grid'
CALL openfile(indev, FALSE, locfn)

! Rad.
WRITE (indev, '(A6)') "$ Rad."
WRITE (indev, '(5X, 2A78)') "x", "y"
WRITE (indev, '(5X, 12A13)') (("SW", "NW", "NN", "NE", "SE", "SS"), ixy = 1, 2)

x0 = [-HALF, -HALF, ZERO,  HALF,  HALF, ZERO]
y0 = [-HALF,  HALF,  ONE,  HALF, -HALF, -ONE]

aoPch = aoF2F(1) / SQ3
x0    = x0*aoF2F(1)
y0    = y0*aoPch

DO ixy = 1, nxy(plotobj)
  x1 = x0 + cntxy(1, ixy)
  y1 = y0 + cntxy(2, ixy)
  
  IF (lptb(ixy)) THEN
    DO iptb = 1, optb(ixy)%nptb
      irdir = optb(ixy)%irdir(iptb)
      x1(irdir) = x1(irdir) + optb(ixy)%dx(iptb)
      y1(irdir) = y1(irdir) + optb(ixy)%dy(iptb)
    END DO
  END IF
  
  WRITE (indev, '(I4, X, 12ES13.5)') ixy, x1, y1
END DO

! Ax.
IF (l3d) THEN
  WRITE (indev, '(/A5)') "$ Ax."
  WRITE (indev, '(5X, A13)') "Thk."
  
  DO iz = 1, nz
    WRITE (indev, '(I4, X, ES13.5)') iz, hgt(iz)
  END DO
END IF

WRITE (indev, '(A1)') DOT
CLOSE (indev) ! 2
! ------------------------------------------------

END SUBROUTINE editgrid
! --------------------------------------------------------------------------------------------------
SUBROUTINE editout(ierr)

USE param, ONLY : FALSE, MP, DOT, BLANK, ERRABS, ERRREL, io2
USE mdat,  ONLY : l3d, objfn, objcn, plotobj, nz, nxy, lerr, xyzpf, xyzmax, xyzrms, axpf, axmax, axrms, powerr, powax

IMPLICIT NONE

INTEGER :: indev, ixy, iz, istz, istxy, iedxy, iquo, refobj, ierr
INTEGER, PARAMETER :: NLGH = 100
CHARACTER*100 :: locfn
! ------------------------------------------------

indev = io2

! Rad.
IF (lerr) THEN
  SELECT CASE (ierr)
  CASE (ERRABS); WRITE (locfn, '(A, A11)') trim(objfn(plotobj)), '_abs_xy.out'
  CASE (ERRREL); WRITE (locfn, '(A, A11)') trim(objfn(plotobj)), '_rel_xy.out'
  END SELECT
ELSE
  WRITE (locfn, '(A, A7)') trim(objfn(plotobj)), '_xy.out'
END IF

CALL openfile(indev, FALSE, locfn)

IF (l3d) THEN
  istz = 0
ELSE
  istz = 1
END IF

IF (lerr) THEN
  WRITE (indev, '(A10, 2A7, 1000I13)') "Legend", "Max.", "RMS", (ixy, ixy = 1, NLGH)
ELSE
  WRITE (indev, '(A10, A14, 1000I13)') "Legend", "P.F.",        (ixy, ixy = 1, NLGH)
END IF

DO iz = istz, nz
  IF (lerr) THEN
    WRITE (indev, '(I10,    2F7.2)') iz, xyzmax(iz, ierr), xyzrms(iz, ierr)
  ELSE
    WRITE (indev, '(I10, 7X, F7.2)') iz, xyzpf(iz)
  END IF
  
  iquo = 0
  
  DO
    iquo  = iquo + 1
    istxy = NLGH*(iquo-1) + 1
    iedxy = min(NLGH*iquo, nxy(plotobj))
    
    IF (istxy .GT. nxy(plotobj)) EXIT
    
    WRITE (indev, '(24X, 1000ES13.5)') (powerr(ixy, iz, ierr), ixy = istxy, iedxy)
  END DO
END DO

WRITE (indev, '(A1)') DOT
CLOSE (indev) ! 2

! Ax.
IF (l3d) THEN
  IF (lerr) THEN
    SELECT CASE (ierr)
    CASE (ERRABS); WRITE (locfn, '(A, A10)') trim(objfn(plotobj)), '_abs_z.out'
    CASE (ERRREL); WRITE (locfn, '(A, A10)') trim(objfn(plotobj)), '_rel_z.out'
    END SELECT
  ELSE
    WRITE (locfn, '(A, A6)') trim(objfn(plotobj)), '_z.out'
  END IF
  
  CALL openfile(indev, FALSE, locfn)
  
  IF (lerr) THEN
    WRITE (indev, '(A10, 2A13, 1000I13)') "Legend", "Max.", "RMS", (iz, iz = 1, nz)
    WRITE (indev, '(A10, 1000ES13.5)') objcn(plotobj), axmax(ierr), axrms(ierr), (powerr(0, iz, ierr), iz = 1, nz)
  ELSE
    WRITE (indev, '(A10,  A13, 1000I13)') "Legend", "P.F.",        (iz, iz = 1, nz)
    WRITE (indev, '(A10, 1000ES13.5)') objcn(plotobj), axpf,                  (powax(iz, plotobj),  iz = 1, nz)
    
    !IF (lrel) THEN
    !  refobj = MP(plotobj)
    !  
    !  WRITE (indev, '(A10, A13, 1000ES13.5)') objcn(refobj), BLANK,        (powax(iz, refobj),  iz = 1, nz)
    !END IF
  END IF
  
  WRITE (indev, '(A1)') DOT
  CLOSE (indev) ! 2
END IF
! ------------------------------------------------

 END SUBROUTINE editout
! --------------------------------------------------------------------------------------------------
SUBROUTINE editerr(ierr)

USE param, ONLY : TRUE, FALSE, io1, io2, ERRABS, ERRREL, DALLR, BLANK, oneline, probe
USE mdat,  ONLY : lerr, l3d, iedterr, objfn, plotobj, aoF2F, nxy, nz, drho, powerr

IMPLICIT NONE

INTEGER :: ierr, indev, jndev, iz, ixy, fndndata, ii, jj, Lgh, nnz
INTEGER, DIMENSION(100) :: inz
CHARACTER*100 :: locfn, gn
CHARACTER*1000 :: tmpline
! ------------------------------------------------

IF (iedterr .EQ. 0) RETURN
IF (.NOT. lerr) RETURN

indev = io2
SELECT CASE (ierr)
CASE (ERRABS); WRITE (locfn, '(A, A8)') 'RNSS\' // trim(objfn(plotobj)), '_abs.out'
CASE (ERRREL); WRITE (locfn, '(A, A8)') 'RNSS\' // trim(objfn(plotobj)), '_rel.out'
END SELECT

CALL openfile(indev, FALSE, locfn)

WRITE (indev, '(3X,  A8, X, F)') 'grid_hex', aoF2F(plotobj)
WRITE (indev, '(10X, A5, X, I)') 'K-eff',    drho

! Pow. Err. : 2-D
IF (.NOT. l3d) THEN
  DO ixy = 1, nxy(plotobj)
    powerr(ixy, 0, ierr) = powerr(ixy, 1, ierr)
  END DO
END IF

WRITE (indev, '(/A)') '$  1 Assembly Radial 2-D Power Distribution'
jndev = io1
gn    = 'RNSS\' // trim(objfn(plotobj)) // '.out'
CALL openfile(jndev, TRUE, gn)

DO
  READ (jndev, '(A1000)', END = 1000) oneline
  IF (probe .NE. DALLR) CYCLE ! READ : Only Output Card
  
  Lgh = len_trim(oneline)
  IF (oneline(6:Lgh) .NE. 'Assembly Radial 2-D Power Distribution') CYCLE
  
  READ  (jndev, *)
  WRITE (indev, *)
  READ  (jndev, '(A1000)') oneline
  WRITE (indev, '(A)') trim(oneline)
  READ  (jndev, *)
  WRITE (indev, *)
  
  ixy = 0
  DO
    READ (jndev, '(A1000)', END = 1000) oneline
    IF (fndndata(oneline) .EQ. 0) EXIT
    
    Lgh = len_trim(oneline)
    nnz = 0
    tmpline = BLANK
    WRITE (tmpline, '(A7)') oneline(1:7)
    
    DO ii = 9, Lgh
      IF (oneline(ii-1:ii-1).EQ.BLANK .AND. oneline(ii:ii).NE.BLANK) THEN
        nnz = nnz + 1 ! Not-Zero
        inz(nnz) = ii
      END IF
    END DO
    
    DO ii = 1, nnz
      ixy = ixy + 1
      jj = inz(ii)
      WRITE (tmpline(jj-2:jj+5), '(F7.3)') powerr(ixy, 0, ierr)
    END DO
    
    WRITE (indev, '(A)') tmpline
  END DO
END DO

1000 CONTINUE

CLOSE (jndev) ! 1

! Pow. Err. : 1-D
WRITE (indev, '(/A/)') '$  2 Axial 1-D Power Distribution'

DO iz = 1, nz
  WRITE (indev, '(I7, F10.4)') iz, powerr(0, iz, ierr)
END DO

! Pow. Err. : 3-D
WRITE (indev, '(/A/)') '$  4 Power Distribution in All Region'
WRITE (indev, '(X, A6)') 'Assem.'

DO ixy = 1, nxy(plotobj)
  WRITE (indev, '(I7, 50(1PE15.6))') ixy, (powerr(ixy, iz, ierr), iz = 1, nz)
END DO

CLOSE (indev) ! 2
! ------------------------------------------------

END SUBROUTINE editerr
! --------------------------------------------------------------------------------------------------