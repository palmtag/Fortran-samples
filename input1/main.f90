   program main
   use mod_input
   implicit none
!=======================================================================
!
!  Sample driver program for 1D Diffusion equation solver
!
!  NE751  Spring 2022
!
!  Usage:  main.exe [input]
!
!=======================================================================

      integer :: ia

      real(8) :: atotal

      character(len=200) :: fname    ! input file name

!--- read input file name from command line

      ia=command_argument_count()
      if (ia.ne.1) stop 'usage: main.exe [input]'
      call get_command_argument(1,fname)

!--- read input from input file

      call read_input(fname)

!--- print selected edits

      write (*,*)
      write (*,*) 'Selected Input edits:'
      write (*,*)

      write (*,40) 'group  ',  ngroup
      write (*,40) 'dim    ',  ndim
      write (*,40) 'bc1    ',  bc1
      write (*,40) 'bc2    ',  bc2

      write (*,50) 'tolk   ',  tolk
      write (*,50) 'tolflux',  tolflux

      atotal=sum(hx)   ! total problem size
      write (*,50) 'atotal ',  atotal,' (total size of problem)'

   40 format (1x,a,i0)
   50 format (1x,a,1p,e14.5,a)

      write (*,*)

!--- insert solver here

      end program main

