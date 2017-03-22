      include 'sandag.inc'
      include 'tcov.inc'
c	read trstop.bin from tctr4.f
c	4/21/05	zou	add tap number for zonal fare stops
c				add stop names 
c	9/7/05	zou 	add reading trrt.bin and index mode by route
c				reset farezone for stops (flat:1, zonal: tap)
c
c	output files:
c		updated trstop.bin
c		busvol
c
c
c     program variables
c
      integer ntap(mxhcn),ntrn(mxhcn),trnlb(mxtrcn),
     *lbid(mxtrca),lbhwy(4,999),lbhrs(999),xpk(4),ovol(6)
	integer*2 rtepass(mxtrcn),ipass
      integer*4 lblat(mxtrcn),lblng(mxtrcn),lbnode(mxtrcn),
     *seqstp(mxtrcn),trmode(999)
      integer*4 lat, lng, nnode,stpid(mxtrcn),rteid(mxtrcn),
     *lkid(mxtrcn),fznode(mxtrcn),fzbyr
      real*4 seqdst(mxtrcn),dst
      real*4 pkmin(4),busvol(3,2,mxid)
      real*8 zamhwy,zpmhwy,zophwy,tdist(3,2),frbyr(999)
      character*32 rtname
	character*40 stpnm(mxtrcn)
      data xpk/2,3,1,1/
      data pkmin/1800.0,1800.0,3600.0,3600.0/
      data lunstp,lunrt,lunerr,natlun,aatlun,seclun,luno,ratlun
     */10,11,12,13,14,15,16,17/
      open (unit=lunerr,file='tctr4a.err')
c	
c	open trrt.bin, store mode by route 
c
      fnami='trrt.bin'
      open (unit=lunrt,file=fnami,
     *form='binary',status='old',iostat=istat)
	if(istat.ne.0) goto 9000
  100 read(lunrt,iostat=istat,err=110)
     *ratlb,rtname,trmode(ratlb),zamhwy,zpmhwy,zophwy,ratid,frbyr(ratlb)  
	if(istat.ne.0) go to 110
      go to 100
  110 close(lunrt)
c
c	read trstop.bin
c	
      fnami='trstop.bin'
      open (unit=lunstp,file=fnami,
     *form='binary',status='old',iostat=istat)
	if(istat.ne.0) goto 9000
   60 read(lunstp,iostat=istat,err=80)iout,ratlb,aatid,ipass,
     *dst,lng,lat,nnode
      stpid(iout)=iout
      rteid(iout)=ratlb
      lkid(iout)=aatid
      rtepass(iout)=ipass
      seqdst(iout)=dst
      lblng(iout)=lng
      lblat(iout)=lat
      lbnode(iout)=nnode
	if(istat.ne.0) go to 80
      go to 60
   80 close(lunstp)
c
c     open trcov nat file
c
      call otrcn
      if(natio.ne.0) go to 9000
      natrec=1
    1 call rtrcn
      if(natio.ne.0) go to 9
      ntrn(natlb)=nattrn
      if (nattrn.gt.0) then
	trnlb(nattrn)=natlb
	fznode(nattrn)=nattap  !farezone = tap
      mtrn=max(mtrn,nattrn)
	write(stpnm(nattrn),1000)natxnm(1),natxnm(2)
	endif      
	!stop name of xnm1 + xnm2
 1000	format(2a20)   
      natrec=natrec+1
      go to 1
    9 natrec=1
c
c     open trcov aat file
c
      call otrca
      if(aatio.ne.0) go to 9000
      aatrec=1
   10 call rtrca
      if(aatio.ne.0) go to 19
      lbid(aatlb)=aatid
      if(aatid.lt.mxid) go to 18
      idtap=mxid+mxtap
      idxfer=70000
      if(aatid.gt.idtap) go to 11
      ntap(aatarn(1))=aatid-mxid
      ntap(aatarn(2))=aatid-mxid
      aatmm=3
      aatnm='TAP CONNECTOR'
      aatspd(1)=3
      aatlrz=0
      aatcrz=0
	aattm(1,1)=0.01
      aattm(2,1)=0.01
      aattm(3,1)=0.01
      aattm(1,2)=0.01
      aattm(2,2)=0.01
      aattm(3,2)=0.01
      go to 17
   11 aattm(1,1)=aatlen*.0038
      aattm(2,1)=aatlen*.0038
      aattm(3,1)=aatlen*.0038
      aattm(1,2)=aatlen*.0038
      aattm(2,2)=aatlen*.0038
      aattm(3,2)=aatlen*.0038
      AATSPD(1)=3
      if(aatid.gt.idxfer) then
      aatmm=2
      aatnm='CC WALK LINK'   
      else
      aatmm=1
      aatnm='TRANSFER LINK'
      endif
   17 call wtrca
   18 aatrec=aatrec+1
      go to 10
   19 natrec=1
   20 call rtrcn
      if(natio.ne.0) go to 29
      nattrn=ntrn(natlb)
      if(nattrn.gt.0) go to 23
      nattap=ntap(natlb)
      if(nattap.gt.mxtap) go to 9020
      nattrn=nattap
      if(nattrn.gt.0) go to 23
      mtrn=mtrn+1
      ntrn(natlb)=mtrn
      nattrn=mtrn
   23 natid=nattrn
      call wtrcn
      if(natio.ne.0) go to 9999
   28 natrec=natrec+1
      go to 20 
   29 close(natlun)
c
c     open route data file.
c
      call otrcr
      if(ratio.ne.0) go to 9000
      ratrec=1
   30 call rtrcr
      if(ratio.ne.0) go to 39
      if(ratlb.gt.999) go to 9000
      lbhwy(1,ratlb)=rathwy(1)
      lbhwy(2,ratlb)=rathwy(2)
      lbhwy(3,ratlb)=rathwy(3)
      lbhwy(4,ratlb)=rathwy(4)
      lbhrs(ratlb)=ratnhr
      ratrec=ratrec+1
      go to 30
   39 close(ratlun)
      call otrcs
      if(secio.ne.0) go to 9000
      secrec=1
   41 call rtrcs 
      if(secio.ne.0) go to 49
      inode=sectpn(1)
      if(inode.gt.0.and.inode.lt.mxtrcn) secarn(1)=trnlb(inode)
      inode=sectpn(2)
      if(inode.gt.0.and.inode.lt.mxtrcn) secarn(2)=trnlb(inode)
      if(alink.lt.1.or.alink.gt.mxtrca) go to 9000 
      if(rlink.lt.1.or.rlink.gt.999) go to 9000
      id=lbid(alink)
      idir=1
      if(secpos(2).gt.0) idir=2
      if(id.lt.1.or.id.gt.mxid) go to 9000
      pkmin(4)=float(lbhrs(rlink))*600.0
      do ipk=1,4
      iipk=xpk(ipk)
      xhwy=float(lbhwy(ipk,rlink))
      if(xhwy.gt.0.0) then
      xvol=pkmin(ipk)/xhwy
      busvol(iipk,idir,id)=busvol(iipk,idir,id)+xvol
      endif
      end do
      call wtrcs
      secrec=secrec+1
      go to 41
   49 close(seclun)
      open(unit=luno,file='busvol')
      do 59 id=1,mxid
      ii=0
      isum=0
      do 58 ipk=1,3
      do 57 idir=1,2
      xvol=busvol(ipk,idir,id)+rem
      ivol=ifix(xvol)
      rem=xvol-float(ivol)
      ii=ii+1
      ovol(ii)=ivol
      isum=isum+ivol
   57 continue
   58 continue
      if(isum.lt.1) go to 59
      write(luno,20000) id,ovol
   59 continue
C
C	write out 'rtstop.bin' with fare zone
C   
      open (unit=lunstp,form='binary',file='trstop.bin')
      iout=1
      do while (stpid(iout).gt.0) 
	nattrn=lbnode(iout)
      rtno=rteid(iout)
      if (trmode(rtno).gt.5) then
	 fzbyr=1 !for flat fare route, farezone set to 1
      else	 
	 fzbyr=fznode(nattrn) !coaster and trolley mode=4,5
       if (fzbyr.eq.0) go to 9010
	endif
      write(lunstp)stpid(iout),rteid(iout),lkid(iout),rtepass(iout),
     *seqdst(iout),lblng(iout),lblat(iout),lbnode(iout),
     *fzbyr,stpnm(nattrn)
      iout=iout+1
      end do
      go to 9999
 9000 write(lunerr,19000) fnamo
      errf=.true.
      go to 9999
 9010 write(lunerr,19010) stpid(iout)
      errf=.true.
      go to 9999
 9020 write(lunerr,19020)
      errf=.true.
      go to 9999
19000 format(' problem opening file: ',a80)
19010 format(' stop has 0 farezone value',i4)
19020 format(' invalid tap')
20000 format(7i5)
 9999 if(errf)  then
      close(lunerr,status='keep')
      else
      close(lunerr,status='delete')
      endif
      stop
      end
