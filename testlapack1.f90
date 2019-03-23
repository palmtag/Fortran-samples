   program test1
   implicit none
!=======================================================================
!
!  Program to demonstrate Lapack solver to solve Ax=b
!
!  More information on the function dgesv can be found at:
!  http://www.netlib.org/lapack/explore-html/d7/d3b/group__double_g_esolve_ga5ee879032a8365897c3ba91e3dc8d512.html
!
!=======================================================================

      integer, parameter :: nsize=4   ! size of matrix

      integer :: i, j
      integer :: info
      integer :: ipiv(nsize)

      real(8) :: a(nsize,nsize)
      real(8) :: xref(nsize)
      real(8) :: b(nsize)

!--- fill sample matrix "A"

      a=0.0d0

      a(1,1)=2.0d0

      a(2,1)=-1.0d0
      a(2,2)=2.0d0
      a(2,3)=-1.0d0

      a(3,1)=-4.8d0
      a(3,2)=-2.5d0
      a(3,3)=2.0d0
      a(3,4)=1.0d0

      a(4,3)=2.0d0
      a(4,4)=5.0d0

!--- set reference "x"

      xref(1)=0.5d0
      xref(2)=1.0d0
      xref(3)=1.2d0
      xref(4)=0.6d0

!--- calculate "b" using "A" and the reference "x"

      do j=1, nsize   ! rows
        b(j)=0
        do i=1, nsize
          b(j)=b(j)+a(j,i)*xref(i)
        enddo
      enddo

!--- print

      write (*,*)
      write (*,*) 'solving system:'

      do j=1, nsize
        write (*,20) j, a(j,:), xref(j), b(j)
      enddo
   20 format (i4, 4f10.6, ' * ', f10.6,' = ', f10.6)

!--- solve for new x

      write (*,*)
      write (*,*) 'calling solver dgesv'

!   new x is returned in "b"

      call dgesv(nsize,1,a,nsize,ipiv,b,nsize,info)
      if (info .ne. 0) stop 'error in solver'

!--- check to see if the results returned agree with our reference values

      write (*,*)
      write (*,*) 'results, reference, difference'
      do i=1, nsize
        write (*,'(3f10.6)') b(i), xref(i), b(i)-xref(i)
      enddo

      write (*,*) 'finished'
      end
