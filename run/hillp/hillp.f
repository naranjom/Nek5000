c-----------------------------------------------------------------------
c  nek5000 user-file template
c
c  user specified routines:
c     - uservp  : variable properties
c     - userf   : local acceleration term for fluid
c     - userq   : local source term for scalars
c     - userbc  : boundary conditions
c     - useric  : initial conditions
c     - userchk : general purpose routine for checking errors etc.
c     - userqtl : thermal divergence for lowMach number flows 
c     - usrdat  : modify element vertices 
c     - usrdat2 : modify mesh coordinates
c     - usrdat3 : general purpose routine for initialization
c     
c-----------------------------------------------------------------------
      subroutine uservp(ix,iy,iz,eg) ! set variable properties

c      implicit none

      integer ix,iy,iz,eg
     
      include 'SIZE'
      include 'TOTAL'
      include 'NEKUSE'

      integer e
c     e = gllel(eg)

      udiff  = 0.0
      utrans = 0.0

      return
      end
c-----------------------------------------------------------------------
      subroutine userf(ix,iy,iz,eg) ! set acceleration term
c
c     Note: this is an acceleration term, NOT a force!
c     Thus, ffx will subsequently be multiplied by rho(x,t).
c
c      implicit none

      integer ix,iy,iz,eg

      include 'SIZE'
      include 'TOTAL'
      include 'NEKUSE'

      integer e
c     e = gllel(eg)

      ffx = 0.0
      ffy = 0.0
      ffz = 0.0

      return
      end
c-----------------------------------------------------------------------
      subroutine userq(ix,iy,iz,eg) ! set source term

c      implicit none

      integer ix,iy,iz,eg

      include 'SIZE'
      include 'TOTAL'
      include 'NEKUSE'

      integer e
c     e = gllel(eg)

      qvol   = 0.0

      return
      end
c-----------------------------------------------------------------------
      subroutine userbc(ix,iy,iz,iside,eg) ! set up boundary conditions
c
c     NOTE ::: This subroutine MAY NOT be called by every process
c
c      implicit none

      integer ix,iy,iz,iside,eg

      include 'SIZE'
      include 'TOTAL'
      include 'NEKUSE'

c      if (cbc(iside,gllel(eg),ifield).eq.'v01')

      ux   = 0.0
      uy   = 0.0
      uz   = 0.0
      temp = 0.0

      return
      end
c-----------------------------------------------------------------------
      subroutine useric(ix,iy,iz,eg) ! set up initial conditions

c      implicit none

      integer ix,iy,iz,eg

      include 'SIZE'
      include 'TOTAL'
      include 'NEKUSE'

      iel = gllel(ieg)
      
      ux   = 1.0
      uy   = 0.0
      uz   = 0.0
      temp = 0.0

      return
      end
c-----------------------------------------------------------------------
      subroutine userchk()

c      implicit none

      include 'SIZE'
      include 'TOTAL'

      return
      end
c-----------------------------------------------------------------------
      subroutine userqtl ! Set thermal divergence

      call userqtl_scig 

      return
      end
c-----------------------------------------------------------------------
      subroutine usrdat()   ! This routine to modify element vertices

c      implicit none

      include 'SIZE'
      include 'TOTAL'

      return
      end
c-----------------------------------------------------------------------
      subroutine usrdat2()  ! This routine to modify mesh coordinates

c      implicit none

      include 'SIZE'
      include 'TOTAL'

      ntot  = nx1 * nx2 * nx3 * nelt
      sa    = 4.5
      sb    = 3.5
      sc    = 1./6

      do i = 1, ntot
      	 xx   = xm1(i, 1, 1, 1)
	 argx = sb * (abs(xx - sa) - sb)
	 A1   = sc + sc * tanh(argx)
	 ym1(i, 1, 1, 1) = ym1(i, 1, 1, 1) + (3 - ym1(i, 1, 1, 1)) * A1
      enddo

      !  Apply mass flux to dirive the flow such that Ubar = 1
      param(54) = -1	 ! x-direction
      param(55) = 1	 ! Ubar


      return
      end
c-----------------------------------------------------------------------
      subroutine usrdat3()

c      implicit none

      include 'SIZE'
      include 'TOTAL'

      return
      end

c automatically added by makenek
      subroutine usrsetvert(glo_num,nel,nx,ny,nz) ! to modify glo_num
      integer*8 glo_num(1)

      return
      end
