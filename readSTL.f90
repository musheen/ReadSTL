PROGRAM readSTL


IMPLICIT NONE

!*************************************************************************************!
! Set Data
!*************************************************************************************!

REAL,ALLOCATABLE,DIMENSION(:,:) :: centroid,surfX
CHARACTER(LEN=1) :: lf=char(10)
CHARACTER(LEN=1024) :: extent,origin,spacing,coffset
INTEGER*4,ALLOCATABLE,DIMENSION(:,:) :: surfElem
CHARACTER header*80,filename*80
INTEGER*2 padding
INTEGER*4 ntri,iunit,nSurfNode,k,i,n,p,kk,share,nSurfElem
REAL*4,ALLOCATABLE,DIMENSION(:,:) :: normals,triangles,nodesT
INTEGER iargc


!*************************************************************************************!
! Import STL Data
!*************************************************************************************!

! import .stl data
call getarg(1,filename)

PRINT*,
PRINT*, " Reading in .stl Mesh "
PRINT*,

iunit=13
OPEN(unit=iunit,file=filename,status='old',access='stream',form='unformatted')

! read .stl header info 
READ(iunit) header
READ(iunit) ntri
   
ALLOCATE(normals(3,ntri))
ALLOCATE(triangles(3,ntri*3))
ALLOCATE(surfELem(ntri,3))
 
! read .stl data
k=1
DO i = 1,ntri
   READ(iunit) normals(1,i),normals(2,i),normals(3,i)
   READ(iunit) triangles(1,k),triangles(2,k),triangles(3,k)
   READ(iunit) triangles(1,k+1),triangles(2,k+1),triangles(3,k+1)
   READ(iunit) triangles(1,k+2),triangles(2,k+2),triangles(3,k+2)
   READ(iunit) padding
  k=k+3
END DO
  
CLOSE(iunit)

ALLOCATE(nodesT(3,ntri*5))
nSurfElem = ntri

! search through data and put into surfX and surfElem style arrays
DO k = 1,ntri
  nodesT(1,k) = 1000000. 
  nodesT(2,k) = 1000000. 
  nodesT(3,k) = 1000000. 
END DO

! eliminate repeated nodes and clean up arrays
i = 1
nSurfNode = 3
k = 0;
DO n = 1,ntri
   DO p = 1,3
      share = 0 
      DO kk = 1,nSurfNode
         IF ((abs(nodesT(1,kk) - triangles(1,i)) < 1.e-13) .AND. &
             (abs(nodesT(2,kk) - triangles(2,i)) < 1.e-13) .AND. &
             (abs(nodesT(3,kk) - triangles(3,i)) < 1.e-13)) THEN
            share = kk
            EXIT
         END IF
      END DO
      IF (share > 0) THEN
         surfElem(n,p) = share
      ELSE
         k             = k+1 
         nodesT(:,k)   = triangles(:,i)
         surfElem(n,p) = k !1-based
      END IF
      i = i+1
   END DO
   nSurfNode = k 
END DO

! allocate surfX
ALLOCATE(surfX(nSurfNode,3))

! fill in surface node data 
DO k = 1,nSurfNode
   surfX(k,1) = nodesT(1,k)
   surfX(k,2) = nodesT(2,k)
   surfX(k,3) = nodesT(3,k)
END DO

! deallocate unnecessary data
DEALLOCATE(nodesT)
DEALLOCATE(triangles)
DEALLOCATE(normals)


!*************************************************************************************!
!
! Program End
!
!*************************************************************************************!

END PROGRAM readSTL


