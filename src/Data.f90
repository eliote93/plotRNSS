MODULE mdat

USE param, ONLY : FALSE

IMPLICIT NONE

INTEGER, PARAMETER :: FNXY = 600 ! Max. # of Rad. Asy.
INTEGER, PARAMETER :: FNX  = 100 ! Max. # of Rad. Ring
INTEGER, PARAMETER :: FNZ  = 100 ! Max. # of Pln.
INTEGER, PARAMETER :: FNV  =  10 ! Max. # of Void. Asy. = Zero Pw.
INTEGER, PARAMETER :: FNP  =  10 ! Max. # of Perturbed Pt.
! ------------------------------------------------
CHARACTER*2   :: objcn(2) ! Mode of Inputted Calculation Result
CHARACTER*100 :: inpfn    ! Name of Input
CHARACTER*100 :: fdir     ! Folder Direction of Input
CHARACTER*100 :: cbnch    ! Name of Becnhmark for Visualization
CHARACTER*100 :: objfn(2) ! Name of Inputted Calculation Result

LOGICAL :: l02   ! Existence of 2nd Input
LOGICAL :: lerr  ! Comparison between Two Calculation Results
LOGICAL :: l3d   ! 3-D Calculation
LOGICAL :: ldfrm ! Deformation
LOGICAL :: lbnch ! Benchmark Descriptions in Visualization
LOGICAL, DIMENSION(FNXY) :: lptb ! Perturbation, useless
! ------------------------------------------------
INTEGER :: xstr2d, ystr2d, nsize2d ! Option for Text in 2-D Assembly Power Distribution
INTEGER :: xstr1d, nsize1d         ! Option for Text in 1-D Plane    Power Distribution
INTEGER :: drho    ! Reactivity
INTEGER :: plotobj ! 1/2 : Objective
INTEGER :: nz      ! # of Planes
INTEGER :: iedterr ! 1 : Make .out / 2 : Plot .out
INTEGER :: nerr    ! Used in Editing Error

INTEGER, DIMENSION(2) :: nya   ! (iobj), # of y-rows
INTEGER, DIMENSION(2) :: nxy   ! (iobj), # of Radial Assemblies
INTEGER, DIMENSION(2) :: ndat  ! (iobj), # of 3-D Assemblies
INTEGER, DIMENSION(2) :: nsfc  ! (iobj), (nya+1) / 2
INTEGER, DIMENSION(2) :: ndfrm ! (iobj), # of Vertex Deviations
INTEGER, DIMENSION(2) :: nMC   ! (iobj), # of McCARD Inputs
INTEGER, DIMENSION(4) :: gcf2d, gcf1d

INTEGER, DIMENSION(FNX, 2)   :: nxa ! # of Assemblies at each y-row
INTEGER, DIMENSION(FNX, FNX) :: asy2Dto1D ! (ix, iy)

INTEGER, DIMENSION(0:FNV, FNX, 2) :: izp = FALSE ! (ii, iy, iobj), ii = 0 : # of Void Asy., 1 ~ : Location of Void Asy.
INTEGER, DIMENSION(6, FNXY) :: inghasy
! ------------------------------------------------
REAL :: avghgt   ! Average Plane Height [cm]
REAL :: xyztotpf ! Peaking Factor for 3-D Assembly Power
REAL :: axpf     ! Peaking Factor for 1-D Plane    Power
REAL :: ystr1d
REAL :: xylmin    ! Minimum Legend for 2-D Assembly Power Distribution
REAL :: xylmax    ! Maximum Legend for 2-D Assembly Power Distribution
REAL :: zlmin     ! Minimum Legend for 1-D Plane    Power Distribution
REAL :: zlmax     ! Maximum Legend for 1-D Plane    Power Distribution

REAL, DIMENSION(2) :: aoF2F     ! (iobj), Assembly Flat-to-flat [cm]
REAL, DIMENSION(2) :: xyztotmax ! (iobj), Maximum Error for 3-D Assembly Power
REAL, DIMENSION(2) :: xyztotrms ! (iobj), RMS     Error for 3-D Assembly Power
REAL, DIMENSION(2) :: xymax     ! (iobj), Maximum Error for 2-D Assembly Power
REAL, DIMENSION(2) :: xyrms     ! (iobj), RMS     Error for 2-D Assembly Power
REAL, DIMENSION(2) :: axmax     ! (iobj), Maximum Error for 1-D Plane Power
REAL, DIMENSION(2) :: axrms     ! (iobj), RMS     Error for 1-D Plane Power
REAL, DIMENSION(2) :: keff      ! (iobj), Multiplication Factor
REAL, DIMENSION(4) :: gca2d, gca1d

REAL, DIMENSION(      FNZ, 2) :: powax ! (iz,      iobj), 1-D Plane Power
REAL, DIMENSION(     FNXY, 2) :: powxy ! (ixy,     iobj), 2-D Assembly Power
REAL, DIMENSION(FNXY, FNZ, 2) :: pow3d ! (ixy, iz, iobj), 3-D Assembly Power
REAL, DIMENSION(FNXY, FNZ, 2) :: vol3d ! (ixy, iz, iobj), 3-D Assembly Volume

REAL, DIMENSION(2, FNXY) :: cntxy ! (x/y, ixy), Coordinates of Radial Assembly

REAL, POINTER, DIMENSION(:) :: xyzpf  ! (iz), Peaking Factor at each Plane
REAL, POINTER, DIMENSION(:) :: hgt    ! (iz), Height of each Plane [cm]

REAL, POINTER, DIMENSION(:,:) :: xyzmax ! (iz, ABS/REL), Maximum Error for 2-D Assembly Power at each Plane
REAL, POINTER, DIMENSION(:,:) :: xyzrms ! (iz, ABS/REL), RMS     Error for 2-D Assembly Power at each Plane

REAL, POINTER, DIMENSION(:,:,:) :: powerr ! (ixy, iz, ABS/REL), Power Error for 3-D Assembly Power
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