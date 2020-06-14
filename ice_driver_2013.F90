	program driver 
		use  ice_aer_cloud
		implicit none
		!Example of calling to the parameterization 
 
      	real*8, dimension(2) :: ndust, ddust !bins for dust (5)
      	real*8 :: nhet, nice, smaxice, nlim, &
                  frac, norg, nbc, nhom, dorg, dbc, tparc, & 
		      	  pparc, sigwparc, K, fact, miuv, nsoot, dsoot, &
		      	  qc, sc_ice, dhom, immIN
      integer :: I

    character*256  :: x
!example of aerosol concentrations (all in SI units)
  !dust
  do I=1, 2 !5
  ddust(I) = 0.1*I*1e-6 
  ndust(I) = 0.1000e6 !dust (m-3)
  end do


!x='GOCART::OCphilic'
!x='GOCART::OCphobic'
!x='GOCART::SO4'
!x='du001'
 


  print *, sum(ndust)*1e-6
  nhom  = 50e6  !sulfate (m-3)
  dhom  = 40e-9
  nsoot = 1e6 !!bc (m-3)
  dsoot = 20e-9
  norg  = 1.0e6 !organics       
  dorg  = 1e-6

  !initialize output
  smaxice=0d0
  nhet=0d0
  nice=0d0
  nlim=0d0
         
  sigwparc=0.16 !m/s width of the updraft distribution 
  pparc=2.2d4
  tparc=245d0   
  qc=1e-6 !liquid water mixing ratio
       
!   tparc=260d0  
!    call ice_activate(tparc,pparc,sigwparc, qc, nice, smaxice, nhet, &
!      nlim, sc_ice, immIN, ddust, ndust, norg, dorg, nhom,  &
!      dhom, nsoot, dsoot)
!          		       
!  print *, 'T (K) =', tparc  
!  print *, 'nc (cm-3) =',  nice*1e-6, 'nhet=', nhet*1e-6, &
!   '%smax =', smaxice*100, 'nlim =',  nlim*1e-6, 'nhet/nlim=',  nhet/nlim, &
!  'sc_ice =' , sc_ice*100, 'ImmIN (L-1) = ', immIN*1e-3 
!      
!      
!      tparc=250d0  
!    call ice_activate(tparc,pparc,sigwparc, qc, nice, smaxice, nhet, &
!      nlim, sc_ice, immIN, ddust, ndust, norg, dorg, nhom,  &
!      dhom, nsoot, dsoot)
!          		       
!  print *, 'T (K) =' , tparc  
!  print *, 'nc (cm-3) =',  nice*1e-6, 'nhet=', nhet*1e-6, &
!   '%smax =', smaxice*100, 'nlim =',  nlim*1e-6, 'nhet/nlim=',  nhet/nlim, &
!  'sc_ice =' , sc_ice*100, 'ImmIN (L-1) = ', immIN*1e-3
!      
!      
!      tparc=245d0  
!    call ice_activate(tparc,pparc,sigwparc, qc, nice, smaxice, nhet, &
!      nlim, sc_ice, immIN, ddust, ndust, norg, dorg, nhom,  &
!      dhom, nsoot, dsoot)
!          		       
!  print *, 'T (K) =' , tparc  
!  print *, 'nc (cm-3) =',  nice*1e-6, 'nhet=', nhet*1e-6, &
!   '%smax =', smaxice*100, 'nlim =',  nlim*1e-6, 'nhet/nlim=',  nhet/nlim, &
!  'sc_ice =' , sc_ice*100, 'ImmIN (L-1) = ', immIN*1e-3
!               
!      
!      tparc=240d0  
!    call ice_activate(tparc,pparc,sigwparc, qc, nice, smaxice, nhet, &
!      nlim, sc_ice, immIN, ddust, ndust, norg, dorg, nhom,  &
!      dhom, nsoot, dsoot)
!          		       
!  print *, 'T (K) =' , tparc  
!  print *, 'nc (cm-3) =',  nice*1e-6, 'nhet=', nhet*1e-6, &
!   '%smax =', smaxice*100, 'nlim =',  nlim*1e-6, 'nhet/nlim=',  nhet/nlim, &
!  'sc_ice =' , sc_ice*100, 'ImmIN (L-1) = ', immIN*1e-3
!      
!      
!    tparc=238d0  
!    call ice_activate(tparc,pparc,sigwparc, qc, nice, smaxice, nhet, &
!      nlim, sc_ice, immIN, ddust, ndust, norg, dorg, nhom,  &
!      dhom, nsoot, dsoot)
!          		       
!  print *, 'T (K) =' , tparc  
!  print *, 'nc (cm-3) =',  nice*1e-6, 'nhet=', nhet*1e-6, &
!   '%smax =', smaxice*100, 'nlim =',  nlim*1e-6, 'nhet/nlim=',  nhet/nlim, &
!  'sc_ice =' , sc_ice*100, 'ImmIN (L-1) = ', immIN*1e-3   
!      
!       tparc=230d0  
!    call ice_activate(tparc,pparc,sigwparc, qc, nice, smaxice, nhet, &
!      nlim, sc_ice, immIN, ddust, ndust, norg, dorg, nhom,  &
!      dhom, nsoot, dsoot)
!          		       
!  print *, 'T (K) =' , tparc  
!  print *, 'nc (cm-3) =',  nice*1e-6, 'nhet=', nhet*1e-6, &
!   '%smax =', smaxice*100, 'nlim =',  nlim*1e-6, 'nhet/nlim=',  nhet/nlim, &
!  'sc_ice =' , sc_ice*100, 'ImmIN (L-1) = ', immIN*1e-3 
!      
!       tparc=225d0  
!    call ice_activate(tparc,pparc,sigwparc, qc, nice, smaxice, nhet, &
!      nlim, sc_ice, immIN, ddust, ndust, norg, dorg, nhom,  &
!      dhom, nsoot, dsoot)
!          		       
!  print *, 'T (K) =' , tparc  
!  print *, 'nc (cm-3) =',  nice*1e-6, 'nhet=', nhet*1e-6, &
!   '%smax =', smaxice*100, 'nlim =',  nlim*1e-6, 'nhet/nlim=',  nhet/nlim, &
!  'sc_ice =' , sc_ice*100, 'ImmIN (L-1) = ', immIN*1e-3 
  
  
   tparc=220d0  
    call ice_activate(tparc,pparc,sigwparc, qc, nice, smaxice, nhet, &
      nlim, sc_ice, immIN, ddust, ndust, norg, dorg, nhom,  &
      dhom, nsoot, dsoot)
          		       
  print *, 'T (K) =' , tparc  
  print *, 'nc (cm-3) =',  nice*1e-6, 'nhet=', nhet*1e-6, &
   '%smax =', smaxice*100, 'nlim =',  nlim*1e-6, 'nhet/nlim=',  nhet/nlim, &
  'sc_ice =' , sc_ice*100, 'ImmIN (L-1) = ', immIN*1e-3 
  
!   tparc=210d0  
!    call ice_activate(tparc,pparc,sigwparc, qc, nice, smaxice, nhet, &
!      nlim, sc_ice, immIN, ddust, ndust, norg, dorg, nhom,  &
!      dhom, nsoot, dsoot)
!          		       
!  print *, 'T (K) =' , tparc  
!  print *, 'nc (cm-3) =',  nice*1e-6, 'nhet=', nhet*1e-6, &
!   '%smax =', smaxice*100, 'nlim =',  nlim*1e-6, 'nhet/nlim=',  nhet/nlim, &
!  'sc_ice =' , sc_ice*100, 'ImmIN (L-1) = ', immIN*1e-3 
!    
!    
!    tparc=200d0  
!    call ice_activate(tparc,pparc,sigwparc, qc, nice, smaxice, nhet, &
!      nlim, sc_ice, immIN, ddust, ndust, norg, dorg, nhom,  &
!      dhom, nsoot, dsoot)
!          		       
!  print *, 'T (K) =' ,tparc  
!  print *, 'nc (cm-3) =',  nice*1e-6, 'nhet=', nhet*1e-6, &
!   '%smax =', smaxice*100, 'nlim =',  nlim*1e-6, 'nhet/nlim=',  nhet/nlim, &
!  'sc_ice =' , sc_ice*100, 'ImmIN (L-1) = ', immIN*1e-3 
    
      
  END program driver
!

!
