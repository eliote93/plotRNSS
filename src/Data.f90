MODULE mdat

USE param, ONLY : FALSE

IMPLICIT NONE

CHARACTER*2   :: objcn(2)
CHARACTER*100 :: fdir, objfn(2)

LOGICAL :: l02, lerr, lrel, l3d
! ------------------------------------------------
INTEGER :: xstr2d, ystr2d, nsize2d, xstr1d, nsize1d
INTEGER :: gcf2d(4), gcf1d(4)
INTEGER :: plotobj, nz

INTEGER, DIMENSION(2) :: nya, nxy, ndat ! (ifile)

INTEGER, DIMENSION(100, 2)   :: nxa
INTEGER, DIMENSION(100, 100) :: asy2Dto1D ! (ix, iy)

INTEGER, DIMENSION(0:10, 100, 2) :: izp = FALSE ! (ii, iy, iobj), Zero Asy. Power, ii = 0 : # of NNZ, 1 ~ : Location of ZP
! ------------------------------------------------
REAL :: aoF2F, avghgt, xylim, zlim, errtotmax, errtotrms, powtotpf, erraxmax, erraxrms, powaxpf
REAL :: ystr1d, gca2d(4), gca1d(4)

REAL, DIMENSION(     100, 2) :: powax ! (iz, iobj)
REAL, DIMENSION(     500, 2) :: powxy ! (ixy, iobj)
REAL, DIMENSION(500, 100, 2) :: pow3d ! (ixy, iz, iobj)

REAL, DIMENSION(2, 500) :: cntxy ! (x/y, ixy)

REAL, POINTER, DIMENSION(:) :: errplnmax ! (iz)
REAL, POINTER, DIMENSION(:) :: errplnrms ! (iz)
REAL, POINTER, DIMENSION(:) :: powplnpf  ! (iz)
REAL, POINTER, DIMENSION(:) :: hgt       ! (iz)

REAL, POINTER, DIMENSION(:,:) :: powerr ! (ixy, iz)
REAL, POINTER, DIMENSION(:,:) :: axpow  ! (iz, iobj)
! ------------------------------------------------

END MODULE mdat