MODULE mdat

USE param, ONLY : FALSE

IMPLICIT NONE

CHARACTER*2   :: objcn(2)
CHARACTER*100 :: fdir, objfn(2)

LOGICAL :: l02, lerr, lrel, l3d
! ------------------------------------------------
INTEGER :: xstr2d, ystr2d, nsize2d, xstr1d, nsize1d
INTEGER :: gcf2d(4), gcf1d(4)
INTEGER :: plotobj, nz, nptb

INTEGER, PARAMETER :: FNXY = 600 ! Max. # of Rad. Asy.
INTEGER, PARAMETER :: FNX  = 100 ! Max. # of Rad. Ring
INTEGER, PARAMETER :: FNZ  = 100 ! Max. # of Pln.
INTEGER, PARAMETER :: FNV  =  10 ! Max. # of Void. Asy. = Zero Pw.
INTEGER, PARAMETER :: FNP  =  10 ! Max. # of Perturbed Pt.

INTEGER, DIMENSION(2) :: nya, nxy, ndat, nsfc ! (iobj)

INTEGER, DIMENSION(FNX, 2)   :: nxa
INTEGER, DIMENSION(FNX, FNX) :: asy2Dto1D ! (ix, iy)

INTEGER, DIMENSION(0:FNV, FNX, 2) :: izp = FALSE ! (ii, iy, iobj), ii = 0 : # of Void Asy., 1 ~ : Location of Void Asy.
! ------------------------------------------------
REAL :: aoF2F, avghgt, xylim, zlim, errtotmax, errtotrms, powtotpf, erraxmax, erraxrms, powaxpf
REAL :: ystr1d, gca2d(4), gca1d(4)

REAL, DIMENSION(      FNZ, 2) :: powax ! (iz, iobj)
REAL, DIMENSION(     FNXY, 2) :: powxy ! (ixy, iobj)
REAL, DIMENSION(FNXY, FNZ, 2) :: pow3d ! (ixy, iz, iobj)

REAL, DIMENSION(2, FNXY) :: cntxy ! (x/y, ixy)
REAL, DIMENSION(4, FNP)  :: ptbpt ! (1:4, iptb), 1:2 = Org. xy vs. 3:4 = Ptb. xy

REAL, POINTER, DIMENSION(:) :: errplnmax ! (iz)
REAL, POINTER, DIMENSION(:) :: errplnrms ! (iz)
REAL, POINTER, DIMENSION(:) :: powplnpf  ! (iz)
REAL, POINTER, DIMENSION(:) :: hgt       ! (iz)

REAL, POINTER, DIMENSION(:,:) :: powerr ! (ixy, iz)
REAL, POINTER, DIMENSION(:,:) :: axpow  ! (iz, iobj)
! ------------------------------------------------

END MODULE mdat