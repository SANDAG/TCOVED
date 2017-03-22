c---------------------------------------------------------------------------			
c
c     program to add convert transit fares flags to coaster &lr fares
c
c     fare matrix is based on a flat fare system, need to update premium fare matrix
c	    for coaster "crzone" and trolley "lrtzone" matrix
c	    coaster system has 4 fare zones, transit link field crzone has
c	    4 values to stand for 4 zones starting from North to South: 1, 10, 100, 1000, 
c	  
c	    trolley fare system is stop based, link field lrtzone=1
c	    except for center city has a flat fare, link field lrtzone = 1000
c	    
c     the impcr.mtx has all the combinations of the crzone values. All together 10 unique patterns:
c     1, 10, 100, 1000  -> within one zone 
c     11, 110, 1100     -> within two zones
c     111, 1110         -> within three zones
c     1111              -> within four zones
c     the program will convert them to actual coaster fares

c	    input files:
c				implr.mtx, impcr.mtxc			
c				'imptrfare2500.mth'
c	    output files
c				fare.mtx (2 tables)
c					coaster fare, light rail fare, 
c	
c----------------------------------------------------------------------------
c     Jan, 2010 Ziying Ouyang
c     call getCPIFctor() subroutine to get cpi adjustment factor
c
c     read input file coastFare.csv to test out different coast fare zone scenarios
c     fare need to have at least one decimal place
c     for example, 3 should be 3.0		
c------------------------------------------------------------------------------
      include 'sandag.inc'
      include 'tcov.inc'
      logical used(mxtap),calrun,sprter(20)
	    integer mode(20), mtap 
	    integer*2 rowidtr(mxtap),sph(mxtap),natsph1
	    real*4 lrtfare(20),crfare(4),crf(4),crfLast(4)
      real*4, allocatable :: arrayf(:)
      real*4, allocatable :: arrayfo(:,:)
	    real*4  arrayo1(mxtap), arrayo(2,mxtap,mxtap)
c
c     subroutine strToReal() requires real*8
c
	    real*8 cpiFctor
      character*80 sknamfi(1,2),sknamfo,chead
	    character*140 c140
      character*1213 mtxheadi,mtxheado
      character*76 mtxsepto(1),mtxsepi
	    character*40204 mtxtailo  !for mxtap=2500
      data lunin,natlun,luninf,luno,lunerr/11,12,13,14,15/
      fnami='trfare.err'
      facsmode='sequential'
      call openoutput(lunerr,facsmode)
      fnami='head'
      call opendata(lunin)
      if(istat.ne.0) go to 9000
      read(lunin,10001) chead
      i=index(chead,'calib')
      if(i.gt.0) calrun=.true.
      i=index(chead,'CALIB')
      if(i.gt.0) calrun=.true.
      i=index(chead,'Calib')
      if(i.gt.0) calrun=.true.
      close(lunin)

      fnami='year'
      call opendata(lunin)
      if(istat.ne.0) go to 9000
      icol=1
      read(lunin,10000,iostat=istat) idata,adata
      if(istat.ne.0) go to 9000
      call sd_getdat(idata,adata,icol,iyear,i3,i4,lunerr)
      close(lunin)
c     assume fares are for fiscal year
c      iyear=iyear+1
     
c     get CPI adjustment factor
c      call getCPIFctor(iyear,cpiFctor)
	  cpiFctor=1.0
	  
     
c     read commuter rail fares by model year
      crfare=crfare*0
      fnami='coasterFare.csv'
      call opendata(lunin)
      if(istat.ne.0) go to 9000
      read(lunin,10001) chead
   50 read(lunin,19001,iostat=istat) iyr,crf
      if(istat.ne.0) go to 60
      if (iyear.eq.iyr) then
      do i=1,4
	
      crfare(i)=crf(i)*cpiFctor
      end do
	    end if
	    if (iyr.gt.0)then
	     do i=1,4
	      crfLast(i)=crf(i) !save the latest fare
	     end do
	    end if
      go to 50  
   60 close(lunin)
c
c for future year, the crfare will take whichever fare year is the most current
c 
      if (crfare(1).eq.0.and.iyear.gt.iyr) then
      do i=1,4
      crfare(i)=crf(i)*cpiFctor
      end do
      end if

      lrtfare(1)=1.25
      lrtfare(2)=1.5
      lrtfare(3)=1.75
      do i=4,10
        lrtfare(i)=2
      end do	

      do i=11,19
        lrtfare(i)=2.25
      end do
      lrtfare(20)=2.50

      if(iyear.gt.2003) then
        lrtfare(1)=1.50
        lrtfare(2)=1.75
        lrtfare(3)=2.00
        do i=4,10
          lrtfare(i)=2.25
        end do	
  
        do i=11,19
          lrtfare(i)=2.50
        end do
      endif
      lrtfare(20)=3.00
      
c 	sprinter spheres
      sprter(7)=.true. !Escondido
      sprter(12)=.true. !Oceanside
      sprter(15)=.true. !SM
      sprter(18)=.true. !Vista
c
c     matrix file names
c
	    sknamfi(1,1)='impcr.mtx'
      sknamfi(1,2)='implr.mtx'    
      sknamfo='fare.mtx'
      call dattim
c
c	read transit_bin file to get the number of taps
c
	    numtap=0
      fnami='transit_.bin'
      call openbin(lunin)
      if(istat.ne.0) go to 9000
   91 read(lunin,iostat=istat)  itap,c140
      if(istat.ne.0) go to 99
      if(itap.lt.1.or.itap.gt.mxtap) go to 91
	    used(itap)=.true.
      numtap=numtap+1
	    go to 91   
   99 close(lunin)   
      mtap =numtap !number of taps
	    print *,numtap 
	    isep=0
	    do itap=1,mxtap
	    if(used(itap)) then	
	    isep=isep+1
	    rowidtr(isep)=itap
	    end if
	    end do	
	    allocate(arrayf(1:mtap))
	    allocate(arrayfo(1:mtap,1:mtap))
c
c     open trcov nat file
c
  119 call otrcn
      if(natio.ne.0) go to 9000
      print *,fnamo
      natrec=1
c
c     read trcov node, save array sphere by tap
c
  120 call rtrcn
      if(natio.ne.0) go to 204
      if(natlb.gt.mxtrcn) go to 9120
      if(nattap.gt.0.and.natsph.gt.0)sph(nattap)=natsph
      natrec=natrec+1
      goto 120
  204 close(natlun)      
c
c     loop through premium impedance matrix to update coaster fare
c
      itype=1
	    arrayfo=arrayfo*0
	    call dattim
      print *,ctime
	    fnami=sknamfi(1,itype)
      call openbin(luninf)
      if (istat.ne. 0) go to 9000
      read(luninf) mtxheadi
      do iz=1,mtap
		  read(luninf) arrayf !coaster  
		  do jz=1,mtap
  		  xrl=arrayf(jz)
  		  if (xrl.gt.0) then
  		  if (floor(xrl/1000).gt.0.and.mod(xrl,1000.0).eq.0) then
  				xrl = crfare(1) !within zone 4	    
  		  else if (floor(xrl/1000).gt.0.and.mod(xrl,10.0).gt.0) then
  			  xrl=crfare(4) !from zone 1 to 4		
  		  else if (floor(xrl/1000).gt.0.and.mod(xrl,100.0).ge.10) then
  				xrl=crfare(3)!from zone 2 to 4		 
  		  else if (floor(xrl/1000).gt.0 .and. mod(xrl,1000.0).ge.100) then
  			  xrl=crfare(2) !from zone 3	to 4    
  		  else if (floor(xrl/100).gt.0 .and. mod(xrl,100.0) .eq.0) then
  			  xrl = crfare(1)	 !within zone 3		
  		  else if (floor(xrl/100).gt.0 .and. mod(xrl,10.0).gt.0) then
  			  xrl=crfare(3)	!from zone 1 to 3 	
  		  else if (floor(xrl/100).gt.0 .and. floor(xrl/10).gt.0) then
  			  xrl=crfare(2) !from zone 2 to 3 		
  		  else if (floor(xrl/10).gt.0 .and. mod(xrl,10.0).gt.0) then
  			  xrl=crfare(2)	!from zone 1 to 2 	
   		  else if (floor(xrl/10).gt.0) then
  			  xrl=crfare(1) 	!within zone 2
   		  else if (mod(xrl,10.0).gt.0) then
  			  xrl=crfare(1)  !within zone 1	     	 
  		  end if	
  		  arrayfo(iz,jz)=xrl
  		  end if			
		  end do
      end do 
	    do iz=1,mtap  
	    iiz=rowidtr(iz) !real tap 
		  do jz=1,mtap
		  ijz=rowidtr(jz)
		  arrayo(1,iiz,ijz)=arrayfo(iz,jz)
		  end do
	    end do
c
c	light rail fare
c
      itype=2
	    arrayfo=arrayfo*0
 	    call dattim
      print *,ctime
	    fnami=sknamfi(1,itype)
      call openbin(luninf)
      if (istat.ne. 0) go to 9000
      read(luninf) mtxheadi
      do iz=1,mtap	!number of taps
		    read(luninf) arrayf
		    iiz=rowidtr(iz) !real tap	
        natsph1=int(sph(iiz)/100) !sphere
		  do jz=1,mtap		  
	        xtroly=arrayf(jz)       
	        if (xtroly.gt.0) then !trolley
			      remd=mod(xtroly,1000.0)
    			  if (remd.eq.0) then 
    				xtroly=1.25 !downtown fare
    			  else 
    				  if (floor(xtroly/1000).gt.0) then 
    					  ifare=1+mod(xtroly,1000.0) 
    				  else
    					  ifare=mod(xtroly,1000.0)
    				  end if
  				    ifare=min(ifare,20) ! if ifare>=20 then ifare=20
  				    if (ifare .gt.0.and.ifare.le.20) xtroly=lrtfare(ifare)					    
  			    end if
  			  else
	            xtroly=0
          end if 	        
	        if(sprter(natsph1).and.xtroly.gt.0) xtroly=2.0*cpiFctor !sprinter flat fare      
			    arrayfo(iz,jz)=xtroly 
		  end do	
      end do               
	    do iz=1,mtap  
	    iiz=rowidtr(iz)
		  do jz=1,mtap
		  ijz=rowidtr(jz)
		  arrayo(2,iiz,ijz)=arrayfo(iz,jz)
		  end do
	    end do
c
c     close rail matrix
c
      close(luninf)
c	close(luninf, status='delete')
c	
c     read output trip table header and tail for TransCAD matrix files
c
      fnami='imptrfare2500.mth'
      call openbin(lunin)
      if(istat.ne.0) go to 9000
      read(lunin) mtxheado
	    do i=1,1
        read(lunin) mtxsepto(i)
	    end do
	    read(lunin) mtxtailo
      close(lunin)
c
c	open output file and write fare matrix table
c
     
	    fnami='fare.mtx'
      call openbino(luno)
      if (istat.ne. 0) go to 9000 
      write(luno) mtxheado
      do iz=1,mxtap  
		    do jz=1,mxtap
			    arrayo1(jz)=arrayo(1,iz,jz)			
		    end do
		    write(luno) arrayo1
      end do 
	    write(luno) mtxsepto(1)
	    do iz=1,mxtap  
		    do jz=1,mxtap	
			    arrayo1(jz)=arrayo(2,iz,jz)			
		    end do
		    write(luno) arrayo1
      end do 
	    write(luno) mtxtailo
      call dattim
      print *,ctime
      go to 9999
 9000 write(lunerr,19000) fnamo
      errf=.true.
      go to 9999
 9005 write(lunerr,19005) 
	close(luno)
c	close(luno,status='delete')
      errf=.true.
      go to 9999
 9120 write(lunerr,19120) natlb
      errf=.true.
      go to 9999

10000 format(80i1,t1,80a1)
10001 format(a80)
10010 format(3i10)
19000 format(' missing file: ',a80)
19001 format(i5,4f5.2)
19005 format('Dwell time >0 and IVT =0 for BRT,EXP,LB',a80)
19120 format(' data problem with trcov.nat ', i10)
39999 format('[',a7,'] [',a8,'] [iztoll] [',a<ic1>,
     *'] [average added minutes=',f<ic2>.1,']')
 9999 if(errf)  then
      close(lunerr,status='keep')
      else
      close(lunerr,status='delete')
      end if
      stop
      end
