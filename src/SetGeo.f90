! --------------------------------------------------------------------------------------------------
SUBROUTINE setgeo

USE param, ONLY : ZERO, SQ3, HALF, FILE1
USE mdat,  ONLY : aoF2F, asy2Dto1D, nxa, nya, nsfc, cntxy, izp

IMPLICIT NONE

INTEGER :: iax, jax, iay, ixy, icx, icy, mxa, nx, ny, idat, ist, mx
REAL :: aoPch, xx, yy, dx, dy
! ------------------------------------------------

asy2Dto1D = 0
cntxy     = ZERO

! SET : 1D to 2D map
ixy = 0

DO iay = 1, nya(FILE1)
  nx = nya(FILE1) -   abs(nsfc(FILE1) - iay) ! Full Hex.
  mx = nxa(iay, FILE1) + izp(0, iay, FILE1)  ! Inputted
  
  jax = max(1, iay - nsfc(FILE1) + 1) + (nx - mx)/2 - 1
  
  DO iax = 1, nxa(iay, FILE1)
    jax = jax + 1
    
    DO idat = 1, izp(0, iay, FILE1)
      IF (iax .EQ. izp(idat, iay, FILE1)) jax = jax + 1
    END DO
    
    ixy = ixy + 1
    
    asy2Dto1D(jax, iay) = ixy
  END DO
END DO

! SET : Cnt.
aoPch = aoF2F(1) / SQ3

dx = 0.5*aoF2F(1)
dy = 1.5*aoPch

DO iay = 1, nya(FILE1)
  DO iax = 1, nya(FILE1)
    ixy = asy2Dto1D(iax, iay)
    
    IF (ixy .LT. 1) CYCLE
    
    cntxy(1, ixy) = aof2f(1)*(iax - nsfc(FILE1)) - dx*(iay - nsfc(FILE1))
    cntxy(2, ixy) =     - dy*(iay - nsfc(FILE1))
  END DO
END DO
! ------------------------------------------------

END SUBROUTINE setgeo
! --------------------------------------------------------------------------------------------------
SUBROUTINE setdfrm()
! ASSUME : 2-D

USE param, ONLY : TRUE, FILE1, ZERO, HALF, ONE, SQ3
USE mdat,  ONLY : ldfrm, ndfrm, nxy, nya, plotobj, inghasy, odfrm, aoF2F, lptb, optb, cntxy

IMPLICIT NONE

INTEGER, PARAMETER, DIMENSION(0:10) :: IDX = [6, 1, 2, 3, 4, 5, 6, 1, 2, 3, 4]

INTEGER :: idfrm, ixy, jxy, iz, irdir, ist, ied
LOGICAL :: chksamepts
REAL :: aoPch, dx, dy, cntx, cnty, tx, ty
REAL, DIMENSION(6) :: x0, y0
! ------------------------------------------------

IF (.NOT. ldfrm) RETURN

! SET : Asy. Ngh.
x0 = [-ONE, -HALF, HALF,  ONE, HALF, -HALF] ! WW to Clock-wise
y0 = [ZERO,   ONE,  ONE, ZERO, -ONE,  -ONE]

aoPch = aoF2F(1) / SQ3
x0    = x0*aoF2F(1)
y0    = y0*aoPch*1.5

inghasy = 0

DO ixy = 1, nxy(FILE1)
  cntx = cntxy(1, ixy)
  cnty = cntxy(2, ixy)
  
  ist = max(ixy - nya(FILE1), 1)
  ied = min(ixy + nya(FILE1), nxy(FILE1))
  
  DO irdir = 1, 6
    tx = cntx + x0(irdir)
    ty = cnty + y0(irdir)
    
    DO jxy = ist, ied
      IF (.NOT. chksamepts(tx, ty, cntxy(1, jxy), cntxy(2, jxy))) CYCLE
      inghasy(irdir, ixy) = jxy
      EXIT
    END DO
  END DO
END DO

! MOVE : Vtx.
DO idfrm = 1, ndfrm(plotobj)
  ixy   = odfrm(idfrm, plotobj)%ixy
  iz    = odfrm(idfrm, plotobj)%iz
  irdir = odfrm(idfrm, plotobj)%irdir
  dx    = odfrm(idfrm, plotobj)%dx
  dy    = odfrm(idfrm, plotobj)%dy
  
  lptb(ixy) = TRUE
  optb(ixy)%nptb = optb(ixy)%nptb + 1
  optb(ixy)%irdir(optb(ixy)%nptb) = irdir
  optb(ixy)%dx   (optb(ixy)%nptb) = dx
  optb(ixy)%dy   (optb(ixy)%nptb) = dy
  
  jxy = inghasy(irdir, ixy)
  IF (jxy .GT. 0) THEN
    lptb(jxy) = TRUE
    optb(jxy)%nptb = optb(jxy)%nptb + 1
    optb(jxy)%irdir(optb(jxy)%nptb) = IDX(irdir + 4)
    optb(jxy)%dx   (optb(jxy)%nptb) = dx
    optb(jxy)%dy   (optb(jxy)%nptb) = dy
  END IF
  
  jxy = inghasy(IDX(irdir-1), ixy)
  IF (jxy .GT. 0) THEN
    lptb(jxy) = TRUE
    optb(jxy)%nptb = optb(jxy)%nptb + 1
    optb(jxy)%irdir(optb(jxy)%nptb) = IDX(irdir + 2)
    optb(jxy)%dx   (optb(jxy)%nptb) = dx
    optb(jxy)%dy   (optb(jxy)%nptb) = dy
  END IF
END DO
! ------------------------------------------------

END SUBROUTINE setdfrm