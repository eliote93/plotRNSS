! --------------------------------------------------------------------------------------------------
SUBROUTINE printout()

USE mdat, ONLY : l3d, lerr, errtotmax, errtotrms, powtotpf, errplnmax, errplnrms, powplnpf, erraxmax, erraxrms, powaxpf

IMPLICIT NONE
! ------------------------------------------------

IF (l3d) THEN
  IF (lerr) THEN
    WRITE (*, '(A31, F7.2, X, A3)') '3-D Power Error Max. : ', errtotmax, '(%)'
    WRITE (*, '(A31, F7.2, X, A3)') '3-D Power Error RMS  : ', errtotrms, '(%)'
  ELSE
    WRITE (*, '(A27, F7.2)') '3-D Power Peaking Factor : ', powtotpf
  END IF
END IF

IF (lerr) THEN
  WRITE (*, '(A31, F7.2, X, A3)') '2-D Power Error Max. : ', errplnmax(0), '(%)'
  WRITE (*, '(A31, F7.2, X, A3)') '2-D Power Error RMS  : ', errplnrms(0), '(%)'
ELSE
  WRITE (*, '(A27, F7.2)') '2-D Power Peaking Factor : ', powplnpf(0)
END IF

IF (l3d) THEN
  IF (lerr) THEN
    WRITE (*, '(A31, F7.2, X, A3)') '1-D Power Error Max. : ', erraxmax, '(%)'
    WRITE (*, '(A31, F7.2, X, A3)') '1-D Power Error RMS  : ', erraxrms, '(%)'
  ELSE
    WRITE (*, '(A27, F7.2)') '1-D Power Peaking Factor : ', powaxpf
  END IF
END IF
! ------------------------------------------------

END SUBROUTINE printout
! --------------------------------------------------------------------------------------------------
SUBROUTINE editinfo()

USE param, ONLY : FALSE, DOT, io1, io2, oneline, probe
USE mdat,  ONLY : l3d, objfn, objcn, plotobj, nz, nxy, lerr, lrel, xylim, zlim, xstr2d, ystr2d, nsize2d, gcf2d, gca2d, xstr1d, ystr1d, nsize1d, gcf1d, gca1d

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
  READ  (jndev, '(A512)') oneline
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

WRITE (indev, '(A9, X, 2I6, A25)') "# of Dat.", nxy(plotobj), nn,        " ! Asy., Img."
WRITE (indev, '(A9, X, 3L6, A21)') "LOGICAL",   lerr, lrel, l3d,         " ! err, rel, 3d"
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
USE mdat,  ONLY : l3d, nxy, nz, objfn, plotobj, aoF2F, hgt, cntxy, nptb, ptbpt

IMPLICIT NONE

INTEGER :: indev, ixy, iz, iptb, ipt
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
WRITE (indev, '(5X, 12A13)') (("NW", "SW", "SS", "SE", "NE", "NN"), ixy = 1, 2)

x0 = [-HALF, -HALF, ZERO,  HALF, HALF, ZERO]
y0 = [ HALF, -HALF, -ONE, -HALF, HALF,  ONE]

aoPch = aoF2F / SQ3
x0    = x0 * aoF2F
y0    = y0 * aoPch

DO ixy = 1, nxy(plotobj)
  x1 = x0 + cntxy(1, ixy)
  y1 = y0 + cntxy(2, ixy)
  
  DO ipt = 1, 6
    DO iptb = 1, nptb
      IF (.NOT. chksamepts(x1(ipt), y1(ipt), ptbpt(1, iptb), ptbpt(2, iptb))) CYCLE
      
      x1(ipt) = ptbpt(3, iptb)
      y1(ipt) = ptbpt(4, iptb)
    END DO
  END DO
  
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
SUBROUTINE editout()

USE param, ONLY : FALSE, MP, DOT, BLANK, io2
USE mdat,  ONLY : l3d, objfn, objcn, plotobj, nz, nxy, lerr, lrel, powplnpf, errplnmax, errplnrms, powaxpf, erraxmax, erraxrms, powerr, powax

IMPLICIT NONE

INTEGER :: indev, ixy, iz, istz, istxy, iedxy, iquo, refobj
INTEGER, PARAMETER :: NLGH = 100
CHARACTER*100 :: locfn
! ------------------------------------------------

indev = io2
WRITE (locfn, '(A, A4)') trim(objfn(plotobj)), '.out'
CALL openfile(indev, FALSE, locfn)

! Rad.
WRITE (indev, '(A6/)') "$ Rad."

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
    WRITE (indev, '(I10,    2F7.2)') iz, errplnmax(iz), errplnrms(iz)
  ELSE
    WRITE (indev, '(I10, 7X, F7.2)') iz, powplnpf(iz)
  END IF
  
  iquo = 0
  
  DO
    iquo  = iquo + 1
    istxy = NLGH*(iquo-1) + 1
    iedxy = min(NLGH*iquo, nxy(plotobj))
    
    IF (istxy .GT. nxy(plotobj)) EXIT
    
    WRITE (indev, '(24X, 1000ES13.5)') (powerr(ixy, iz), ixy = istxy, iedxy)
  END DO
END DO

! Ax.
IF (l3d) THEN
  WRITE (indev, '(/A5/)') "$ Ax."
  
  IF (lerr) THEN
    WRITE (indev, '(A10, 2A13, 1000I13)') "Legend", "Max.", "RMS", (iz, iz = 1, nz)
    WRITE (indev, '(A10, 1000ES13.5)') objcn(plotobj), erraxmax, erraxrms, (powerr(0, iz), iz = 1, nz)
  ELSE
    WRITE (indev, '(A10,  A13, 1000I13)') "Legend", "P.F.",        (iz, iz = 1, nz)
    WRITE (indev, '(A10, 1000ES13.5)') objcn(plotobj), powaxpf,            (powax(iz, plotobj), iz = 1, nz)
    
    IF (lrel) THEN
      refobj = MP(plotobj)
      
      WRITE (indev, '(A10, A13, 1000ES13.5)') objcn(refobj), BLANK,        (powax(iz, refobj),  iz = 1, nz)
    END IF
  END IF
END IF

WRITE (indev, '(A1)') DOT
CLOSE (indev) ! 2
! ------------------------------------------------

END SUBROUTINE editout
! --------------------------------------------------------------------------------------------------