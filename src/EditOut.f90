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
USE mdat,  ONLY : l3d, objfn, objcn, plotobj, nz, nxy, lerr, lrel, zlim, xstr2d, ystr2d, nsize2d, gcf2d, gca2d, xstr1d, ystr1d, nsize1d, gcf1d, gca1d

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
  WRITE (indev, '(A512)') oneline
  
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
WRITE (indev, *)

! Info. : Ax.
WRITE (indev, '(A5/)') "$ Ax."

IF (.NOT.lerr .AND. lrel) THEN
  nn = 2
ELSE
  nn = 1 ! Manually Inputted
END IF

WRITE (indev, '(A9, X, 2I6, A25)')          "# of Dat.", nz, nn,                  " ! Pln., Img."
WRITE (indev, '(A9, X, 3L6, A21)')          "LOGICAL",   lerr, lrel, l3d,         " ! err, rel, 3d"
WRITE (indev, '(A9, X, I6, F6.3, I6, A19)') "String",    xstr1d, ystr1d, nsize1d, " ! x, y, size"
WRITE (indev, '(A9, X, 4I6)')               "GCF",       gcf1D(1:4)
WRITE (indev, '(A9, X, 4F6.3)')             "GCA",       gca1D(1:4)
WRITE (indev, '(A9, X, F6.3)')              "ZLIM",      zlim
WRITE (indev, '(A1)') DOT

CLOSE (indev) ! 2
! ------------------------------------------------

END SUBROUTINE editinfo
! --------------------------------------------------------------------------------------------------
SUBROUTINE editgrid()

USE param, ONLY : FALSE, HALF, ONE, ZERO, SQ3, io2
USE mdat,  ONLY : nxy, nz, objfn, plotobj, aoF2F, hgt, cntxy

IMPLICIT NONE

INTEGER :: indev, ixy, iz
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
  
  WRITE (indev, '(I4, X, 12ES13.5)') ixy, x1, y1
END DO

WRITE (indev, *)

! Ax.
WRITE (indev, '(A5)') "$ Ax."
WRITE (indev, '(5X, A13)') "Thk."

DO iz = 1, nz
  WRITE (indev, '(I4, X, ES13.5)') iz, hgt(iz)
END DO

CLOSE (indev) ! 2
! ------------------------------------------------

END SUBROUTINE editgrid
! --------------------------------------------------------------------------------------------------
SUBROUTINE editout()

USE param, ONLY : FALSE, MP, io2
USE mdat,  ONLY : l3d, objfn, objcn, plotobj, nz, nxy, lerr, lrel, powplnpf, errplnmax, errplnrms, powaxpf, erraxmax, erraxrms, powerr, powax

IMPLICIT NONE

INTEGER :: indev, ixy, iz, ist
CHARACTER*100 :: locfn
! ------------------------------------------------

indev = io2
WRITE (locfn, '(A, A4)') trim(objfn(plotobj)), '.out'
CALL openfile(indev, FALSE, locfn)

! Rad.
WRITE (indev, '(A6/)') "$ Rad."

IF (l3d) THEN
  ist = 0
ELSE
  ist = 1
END IF

WRITE (indev, '(A6, X, 1000I13)') "Legend", (iz, iz = ist, nz)

IF (lerr) THEN
  WRITE (indev, '(A6, X, 1000ES13.5)') "Max.", errplnmax(ist:nz)
  WRITE (indev, '(A6, X, 1000ES13.5)') "RMS",  errplnrms(ist:nz)
ELSE
  WRITE (indev, '(A6, X, 1000ES13.5)') "P.F.", powplnpf (ist:nz)
END IF

DO ixy = 1, nxy(plotobj)
  WRITE (indev, '(I6, X, 1000ES13.5)') ixy, powerr(ixy, ist:nz)
END DO

! Ax.
IF (.NOT. l3d) RETURN

WRITE (indev, *)
WRITE (indev, '(A5/)') "$ Ax."

IF (lerr) THEN
  WRITE (indev, '(A6, X, ES13.5)') "Max.", erraxmax
  WRITE (indev, '(A6, X, ES13.5)') "RMS",  erraxrms
ELSE
  WRITE (indev, '(A6, X, ES13.5)') "P.F.",   powaxpf
  
  IF (lrel) THEN
    WRITE (indev, '(A6, X, 2A13)') "Legend", objcn(plotobj), objcn(MP(plotobj))
  ELSE
    WRITE (indev, '(A6, X,  A13)') "Legend", objcn(plotobj)
  END IF
END IF

DO iz = 1, nz
  IF (.NOT.lerr .AND. lrel) THEN
    WRITE (indev, '(I6, X, 2ES13.5)') iz, powax(iz, plotobj), powax(iz, MP(plotobj))
  ELSE
    WRITE (indev, '(I6, X,  ES13.5)') iz, powerr(0, iz)
  END IF
END DO

CLOSE (indev) ! 2
! ------------------------------------------------

END SUBROUTINE editout
! --------------------------------------------------------------------------------------------------