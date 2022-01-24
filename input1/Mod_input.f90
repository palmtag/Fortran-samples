   module mod_input
   implicit none

!=======================================================================
!
! Module to hold input values for 1D diffusion problem
! NE751 Spring 2022
!
!=======================================================================

      integer :: ngroup     ! number of energy groups
      integer :: ndim       ! number of mesh

      real(8) :: tolk       ! tolerance on k
      real(8) :: tolflux    ! tolerance on flux
      integer :: bc1, bc2   ! boundary conditions on left and right

      real(8), allocatable :: hx(:)       ! mesh size
      integer, allocatable :: imat(:)     ! materials by mesh number

!  cross sections

      integer, parameter :: maxmat=10     ! maximum number of materials

      real(8), allocatable :: xsdiff(:,:)   ! diffusion coefficient (ngroup, maxmat)
      real(8), allocatable :: xssiga(:,:)   ! absorption cross section (ngroup, maxmat)
      real(8), allocatable :: xsnufis(:,:)  ! nu*fission cross section (ngroup, maxmat)
      real(8), allocatable :: xschi(:,:)    ! fission spectrum (ngroup, maxmat)
      real(8), allocatable :: xsscat(:,:,:) ! fission spectrum (ngroup, ngroup, maxmat)

   contains

!=======================================================================
!  Subroutine to read input from text file
!
!  Comment lines start with "#"
!
!  Input uses standard Fortran list-directed I/O
!  a slash ("/") in the input card signifies an end of record
!  anything after a slash is considered a comment
!
!  See sample input files
!
!=======================================================================
      subroutine read_input(fname)
      implicit none
      character(len=*), intent(in) :: fname

      integer :: i, k
      logical :: ifxst

      character(len=20)  :: card
      character(len=100) :: line

      write (*,'(2a)') 'reading input file: ', trim(fname)

!--- initialize defaults for constants

      ngroup=0              ! number of energy groups
      ndim=0                ! number of mesh

      tolk=1.0d-8           ! tolerance on k
      tolflux=1.0d-8        ! tolerance on flux
      bc1=0                 ! boundary condition on left side
      bc2=0                 ! boundary condition on right side

!--- check if input file exists

      inquire(file=fname, exist=ifxst)
      if (.not.ifxst) stop 'input file does not exist'

!--- open input file for reading

      open (12,file=fname,status='old',action='read')

!--- read input file one line at a time
!    1. Read entire line into "line" string
!    2. Read "card" as first part of the "line" string
!    3. Determine what values are input on "card" line
!    4. Read entire "line" again, both card name and values

      do
        read (12,'(a)',end=100) line

        k=index(line,'#')        ! check for comment card
        if (k.gt.0) line(k:)=' '
        if (line.eq.' ') cycle   ! skip empty line

        read (line,*) card

        if     (card.eq.'group') then
          read (line,*) card, ngroup
          allocate(xsdiff(ngroup, maxmat))
          allocate(xssiga(ngroup, maxmat))
          allocate(xsnufis(ngroup, maxmat))
          allocate(xschi(ngroup, maxmat))
          allocate(xsscat(ngroup, ngroup, maxmat))
          xsdiff=0.0d0   ! assign default values
          xssiga=0.0d0
          xsnufis=0.0d0
          xschi=0.0d0
          xsscat=0.0d0
          if (ngroup.eq.1) then      ! default for one-group
             xschi(1,:)=1.0d0
          elseif (ngroup.eq.2) then  ! default for two-group
             xschi(1,:)=1.0d0
             xschi(2,:)=0.0d0
          endif

        elseif (card.eq.'tol') then
          read (line,*) card, tolk, tolflux

        elseif (card.eq.'bc') then
          read (line,*) card, bc1, bc2

        elseif (card.eq.'dim') then
          read (line,*) card, ndim
          allocate(hx(ndim))
          allocate(imat(ndim))
          hx=0.0d0
          imat=1

        elseif (card.eq.'hx') then
          if (ndim.le.0) stop 'dim card must come before hx card'
          read (line,*) card, hx(:)

        elseif (card.eq.'mat') then
          if (ndim.le.0) stop 'dim card must come before mat card'
          read (line,*) card, imat(:)

        elseif (card.eq.'diff') then
          if (ngroup.le.0) stop 'group card must come before diff card'
          read (line,*) card, i, xsdiff(:,i)

        elseif (card.eq.'siga') then
          if (ngroup.le.0) stop 'group card must come before siga card'
          read (line,*) card, i, xssiga(:,i)

        elseif (card.eq.'nufis') then
          if (ngroup.le.0) stop 'group card must come before nufis card'
          read (line,*) card, i, xsnufis(:,i)

        elseif (card.eq.'chi') then
          if (ngroup.le.0) stop 'group card must come before chi card'
          read (line,*) card, i, xschi(:,i)

        elseif (card.eq.'scat') then
          if (ngroup.le.0) stop 'group card must come before scat card'
          read (line,*) card, i, xsscat(:,:,i)

        else
          write (*,'(2a)') 'error reading card: ', trim(card)
          stop 'error reading input'
        endif

      enddo
  100 continue    ! end of file

      close (12)

!--- simple error checking

      if (ngroup.le.0) stop 'error: no group card input'
      if (ndim  .le.0) stop 'error: no dim   card input'

      do i=1, ndim
        if (imat(i).le.0) stop 'error: invalid entry in mat array'
      enddo
      do i=1, ndim
        if (hx(i).le.0.0d0) stop 'error: invalid entry in hx array'
      enddo

      write (*,*) 'finished reading input file'

      return
      end subroutine read_input
!=======================================================================
   end module mod_input


