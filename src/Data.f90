MODULE mdat

USE param, ONLY : FALSE

IMPLICIT NONE

INTEGER, PARAMETER :: FNXY = 600 ! Max. # of Rad. Asy.
INTEGER, PARAMETER :: FNX  = 100 ! Max. # of Rad. Ring
INTEGER, PARAMETER :: FNZ  = 100 ! Max. # of Pln.
INTEGER, PARAMETER :: FNV  =  10 ! Max. # of Void. Asy. = Zero Pw.
INTEGER, PARAMETER :: FNP  =  10 ! Max. # of Perturbed Pt.
! ------------------------------------------------
CHARACTER*2   :: objcn(2)
CHARACTER*100 :: inpfn, fdir, objfn(2)

LOGICAL :: l02, lerr, l3d, ldfrm
LOGICAL, DIMENSION(FNXY) :: lptb
! ------------------------------------------------
INTEGER :: xstr2d, ystr2d, nsize2d, xstr1d, nsize1d, drho, plotobj, nz, nerr, iedterr

INTEGER, DIMENSION(2) :: nya, nxy, ndat, nsfc, ndfrm ! (iobj)
INTEGER, DIMENSION(4) :: gcf2d, gcf1d

INTEGER, DIMENSION(FNX, 2)   :: nxa
INTEGER, DIMENSION(FNX, FNX) :: asy2Dto1D ! (ix, iy)

INTEGER, DIMENSION(0:FNV, FNX, 2) :: izp = FALSE ! (ii, iy, iobj), ii = 0 : # of Void Asy., 1 ~ : Location of Void Asy.
INTEGER, DIMENSION(6, FNXY) :: inghasy
! ------------------------------------------------
REAL :: avghgt, xylim, zlim, xyztotpf, axpf, ystr1d

REAL, DIMENSION(2) :: aoF2F, xyztotmax, xyztotrms, xymax, xyrms, axmax, axrms, keff
REAL, DIMENSION(4) :: gca2d, gca1d

REAL, DIMENSION(      FNZ, 2) :: powax ! (iz, iobj)
REAL, DIMENSION(     FNXY, 2) :: powxy ! (ixy, iobj)
REAL, DIMENSION(FNXY, FNZ, 2) :: pow3d ! (ixy, iz, iobj)

REAL, DIMENSION(2, FNXY) :: cntxy ! (x/y, ixy)

REAL, POINTER, DIMENSION(:) :: xyzpf  ! (iz)
REAL, POINTER, DIMENSION(:) :: hgt    ! (iz)

REAL, POINTER, DIMENSION(:,:) :: xyzmax ! (iz, ABS/REL)
REAL, POINTER, DIMENSION(:,:) :: xyzrms ! (iz, ABS/REL)
REAL, POINTER, DIMENSION(:,:) :: axpow  ! (iz, iobj)

REAL, POINTER, DIMENSION(:,:,:) :: powerr ! (ixy, iz, ABS/REL)
! ------------------------------------------------
TYPE type_dfrm

INTEGER :: ixy, iz, irdir ! SW to Clock-wise
REAL :: dx, dy

END TYPE type_dfrm

TYPE(type_dfrm), DIMENSION(FNXY, 2) :: odfrm
! ------------------------------------------------
TYPE type_ptb

INTEGER :: nptb
INTEGER, DIMENSION(6) :: irdir
REAL, DIMENSION(6) :: dx, dy

END TYPE type_ptb

TYPE(type_ptb), DIMENSION(FNXY) :: optb
! ------------------------------------------------

END MODULE mdat