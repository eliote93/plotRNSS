SUBROUTINE editout

USE param, ONLY : FALSE, DOT, MP
USE mdat,  ONLY : l3d, objfn, objcn, plotobj, nz, indev, nxy, lerr, lrel, powpf, errmax, errrms, xypf, xymax, xyrms, axpf, axmax, axrms, zlim, hgt, powerr, powax, &
                  xstr2d, ystr2d, nsize2d, gcf2d, gca2d, xstr1d, ystr1d, nsize1d, gcf1d, gca1d

IMPLICIT NONE

CHARACTER*100 :: fniz

INTEGER :: iz
! ------------------------------------------------

IF (.NOT. l3d) WRITE (fniz, '(A, A4)') trim(objfn(plotobj)), '.inp'

DO iz = 1, nz
  IF (l3d) THEN
    IF (iz .LT. 10) WRITE (fniz, '(A, A6, I1, A4)') trim(objfn(plotobj)), ' PLN 0', iz, '.inp'
    IF (iz .GE. 10) WRITE (fniz, '(A, A5, I2, A4)') trim(objfn(plotobj)), ' PLN ',  iz, '.inp'
  END IF
  
  CALL openfile(indev, FALSE, fniz)
  CALL echoinp(indev)
  
  WRITE (indev, '(I5, 3L2)') nxy(plotobj), lerr, lrel, l3d
  WRITE (indev, '(3ES13.5)') powpf(iz), errmax(iz), errrms(iz)
  WRITE (indev, '(3I5)')     xstr2d, ystr2d, nsize2d
  WRITE (indev, '(4I5)')     gcf2D(1:4)
  WRITE (indev, '(4F6.3)')   gca2D(1:4)
  
  CALL editrad(iz)
  
  WRITE (indev, '(A1)') DOT
  
  CLOSE (indev)
END DO
! ------------------------------------------------
IF (.NOT. l3d) RETURN

! 2-D
WRITE (fniz, '(A, A11)') trim(objfn(plotobj)), ' PLN 00.inp'

CALL openfile(indev, FALSE, fniz)
CALL echoinp(indev)

WRITE (indev, '(I5, 3L2)') nxy(plotobj), lerr, lrel, l3d
WRITE (indev, '(3ES13.5)') xypf, xymax, xyrms
WRITE (indev, '(3I5)')     xstr2d, ystr2d, nsize2d
WRITE (indev, '(4I5)')     gcf2D(1:4)
WRITE (indev, '(4F6.3)')   gca2D(1:4)

IF (lerr) THEN
  WRITE (*, '(A31, F5.2, X, A3)') '2-D Power Error Max. : ', xymax, '(%)'
  WRITE (*, '(A31, F5.2, X, A3)') '2-D Power Error RMS  : ', xyrms, '(%)'
END IF

CALL editrad(0)

! 1-D
WRITE (indev, '(I5, 3L2, 2(X, A2))') nz, lerr, lrel, l3d, objcn(1), objcn(2)
WRITE (indev, '(4ES13.5)')           axpf, axmax, axrms, zlim
WRITE (indev, '(I5, F7.2, I5)')      xstr1d, ystr1d, nsize1d
WRITE (indev, '(4I5)')               gcf1D(1:4)
WRITE (indev, '(4F6.3)')             gca1D(1:4)

IF (.NOT.lerr .AND. lrel) THEN
  DO iz = 1, nz
    WRITE (indev, '(100ES13.5)') hgt(iz), powax(iz, plotobj), powax(iz, MP(plotobj))
  END DO
ELSE
  DO iz = 1, nz
    WRITE (indev, '(100ES13.5)') hgt(iz), powerr(0, iz)
  END DO
END IF

IF (lerr) THEN
  WRITE (*, '(A31, F5.2, X, A3)') '1-D Power Error Max. : ', axmax, '(%)'
  WRITE (*, '(A31, F5.2, X, A3)') '1-D Power Error RMS  : ', axrms, '(%)'
END IF

WRITE (indev, '(A1)') DOT

CLOSE (indev)
! ------------------------------------------------

END SUBROUTINE editout
! --------------------------------------------------------------------------------------------------
SUBROUTINE editrad(iz)

USE param, ONLY : HALF, ONE, ZERO, SQ3
USE mdat, ONLY : aoF2F, lerr, indev, nxy, powerr, plotobj, cntxy

IMPLICIT NONE

INTEGER :: ixy, iz
REAL :: aoPch, x0(6), y0(6), x1(6), y1(6)
! ------------------------------------------------

!OPEN (43, FILE = 'V09', STATUS = 'OLD')
!
!DO ixy = 1, nxy(plotobj)
!  READ (43, *) powerr(ixy, iz)
!END DO
!
!CLOSE (43)

x0 = [-HALF, -HALF, ZERO,  HALF, HALF, ZERO]
y0 = [ HALF, -HALF, -ONE, -HALF, HALF,  ONE]

aoPch = aoF2F / SQ3
x0    = x0 * aoF2F
y0    = y0 * aoPch

DO ixy = 1, nxy(plotobj)
  x1 = x0 + cntxy(1, ixy)
  y1 = y0 + cntxy(2, ixy)

  WRITE (indev, '(13ES13.5)') powerr(ixy, iz), x1, y1
END DO
! ------------------------------------------------

END SUBROUTINE editrad