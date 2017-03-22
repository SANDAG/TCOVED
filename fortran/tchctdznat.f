      parameter (mxramp=2500,mxrlk=1000,mxrt=500,mxznum=50)
      include 'sandag.inc'
      include 'tcov.inc'
      include 'zone.inc'
c
c     program variables
c
      integer ozone(mxhcn),taztdz(mxzn),znum(mxtdz),
     *znode(mxtdz,mxznum),zrec(mxtdz,mxznum)
      real*8 xmgra(mxmgra),ymgra(mxmgra),xnode(mxhcn),ynode(mxhcn),
     *zdist(mxtdz,mxznum),ztrip(mxtdz,mxznum),xprod(10),xattr(10)
      data lunerr,patlun,aatlun,lunin/11,12,13,14/
      open (unit=lunerr,file='tchcztdznat.err')
      call dattim
c
c     open mgra pat file and save coordinates
c
      call omgrfpt
      if(patio.ne.0) go to 9000
      print *,fnamo
      patrec=1
   10 call rmgrfpt
      if(patio.ne.0) go to 19
      if(patzon.lt.1.or.patzon.gt.mxmgra) go to 9001
      xmgra(patzon)=float(patx)
      ymgra(patzon)=float(paty)
      patrec=patrec+1
      go to 10
   19 close(patlun)

c     taz to tdz conversion
 
      icol=2
      fnami='taz.tdz'
      call opendata(lunin)
      if(istat.ne.0) go to 9000
   20 read(lunin,10000,iostat=istat) idata,adata
      if(istat.ne.0) go to 29
      call sd_getdat(idata,adata,icol,itaz,itdz,i3,lunerr)
      if(itaz.lt.1.or.itaz.gt.mxzn) go to 9001
      if(itdz.lt.1.or.itdz.gt.2000) go to 9001
      taztdz(itaz)=itdz
      go to 20
   29 close(lunin)
      fnami='hwycovtdz/aat.adf'
      call ohcatdz
      if(aatio.ne.0) go to 9000
      aatrec=1
c
c     read hwycov aat and save data.
c
   30 call rhca
      if(aatio.ne.0) go to 39

      iz=aattpn(1)
      if(iz.le.mxagg) then
       ozone(aatarn(1))=iz  !an
       inum=znum(iz)
       if(inum.gt.0) then
        do i=1,inum
        if(aatarn(2).eq.znode(iz,i)) go to 38
        end do
       endif
        inum=inum+1
        if(inum.gt.mxznum) go to 9120
        znum(iz)=inum
        znode(iz,inum)=aatarn(2)
        zrec(iz,inum)=aatrec
      else
       iz=aattpn(2)
       if(iz.le.mxagg) then
       ozone(aatarn(2))=iz  !an
       inum=znum(iz)
       if(inum.gt.0) then
        do i=1,inum
        if(aatarn(1).eq.znode(iz,i)) go to 38
        end do
       endif
        inum=inum+1
        if(inum.gt.mxznum) go to 9120
        znum(iz)=inum
        znode(iz,inum)=aatarn(1)
        zrec(iz,inum)=aatrec
       endif
      endif
   38 aatrec=aatrec+1
      if(aatrec.le.mxhca) go to 30	
   39 fnamo='hwycovtdz/nat.adf'
      open(unit=natlun,convert='big_endian',file=fnamo,access=
     *'direct',form='unformatted',recl=19,status='old',err=9000)
      natio=0
      natrec=1
   40 call rhcn
      if(natio.ne.0) go to 49  
	if (ozone(natlb).gt.0) nattpn=ozone(natlb) !hnode
c
c     insert hnode into nodeid for exporting to transCAD 
      natid=nattpn
      xnode(natlb)=float(natx)
      ynode(natlb)=float(naty)
      call whcn
      natrec=natrec+1
      go to 40
   49 close(natlun)
c
c     read trip generation file and compute weighted distances to each node
c
      fnami='mgrapa'
      call opendata(lunin)		
      if (istat.ne.0) goto 9000
   50 read(lunin,10050,iostat=istat) itaz,imgra,xprod,xattr
      if(istat.ne.0) go to 59
      if(itaz.lt.1.or.itaz.gt.mxzn) go to 9001
      itdz=taztdz(itaz)
      if(itdz.lt.1.or.itdz.gt.2000) go to 9001
      if(imgra.lt.1.or.imgra.gt.mxmgra) go to 9001
      xm=xmgra(imgra)
      ym=ymgra(imgra)
      tot=0.0
      do ipu=1,mxpu
      tot=tot+xprod(ipu)+xattr(ipu)
      end do
      mnum=znum(itdz)
      if(mnum.lt.1.or.mnum.gt.mxznum) go to 9001
      do inum=1,mnum
      in=znode(itdz,inum)
      xn=xnode(in)
      yn=ynode(in)
      xdist=sqrt((xm-xn)**2+(ym-yn)**2)/5280.
      zdist(itdz,inum)=zdist(itdz,inum)+xdist*tot
      ztrip(itdz,inum)=ztrip(itdz,inum)+tot
      end do
      go to 50
   59 close(lunin)
c
c     read hwycov aat and save data.
c
      iz1=mxext+1
      do iz=iz1,mxtdz
      print *,iz
      mnum=znum(iz)
      do inum=1,mnum
      aatrec=zrec(iz,inum)
      call rhca
      if(aatio.ne.0) go to 9000
      xdist=zdist(iz,inum)/ztrip(iz,inum)
      xspd=float(aatspd(1))
      xtime=xdist*60./xspd
      aatcst=xdist*15.
      do idir=1,2
      do ipk=1,3
      aatlt(ipk,idir)=xtime
      end do
      end do
      call whca
      if(aatio.ne.0) go to 9000
      end do
      end do
      go to 9999
 9000 write(lunerr,19000) fnamo
      errf=.true.
      go to 9999
 9001 write(lunerr,19001) fnamo
      errf=.true.
      go to 9999 
 9120 write(lunerr,19120)
      errf=.true.
      go to 9999
10000 format(80i1,t1,80a1)
10050 format(2i7,2(<mxpu>f7.1))
19000 format(a50,' not opened')
19001 format(' data problem with file: ',a80/,80a1)
19070 format(' data problem with file: ',a80/,i10)
19120 format(' increase mxznum')
 9999 if (errf) then
      close(lunerr,status='keep')
      else
      close(lunerr,status='delete')
      end if    
      stop
      end


