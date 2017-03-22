      parameter (mxproj=1000,mxmrg=200,mxpyr=20)
      include 'sandag.inc'
      include 'tcov.inc'
      include 'zone.inc'
      logical ulproj(mxproj),urec(mxtca),szone(mxhcn),szc(mxhcn),sub,
     *zcent(mxhcn),inff,hovacc(mxtca),hovnod(mxtcn)
      integer*2 years(mxpyr),nlink(mxtcn),
     *exit(mxtcn),entry(mxtcn),fc(mxtca),yr(mxtca),way(mxtca)
      integer*4 xid(mxid),fn(mxtca),tn(mxtca),pyr(mxproj),year
      character*43 pjname,prname(mxproj)
      data zcent/mxhcn*.true./
      data aatlun,natlun,lunin,lunerr,luninf,patlun/11,12,13,14,15,16/
c
c     open files
c
      open (unit=lunerr,file='tchc1.err')
      open (unit=luninf,file='tchc1.info')
      ipcol=1
c
c     program will use column 1 of hwyproj.lis unless hwyproj.col exists
c
      fnamo='hwyproj.col'
      open(unit=lunin,file=fnamo,status='old',err=10)
      icol=1
      print *,fnamo
      read(lunin,10000,iostat=istat) idata,adata
      if(istat.ne.0) go to 9001
      call sd_getdat(idata,adata,icol,ipcol,i2,i3,lunerr)
      if(ipcol.lt.1.or.ipcol.gt.mxpyr) go to 9001
      close(lunin)
c
c     read new hwyproj and save project years
c
   10 fnamo='hwyproj.prn'
      open(unit=lunin,file=fnamo,status='old',err=11)
      go to 19
   11 fnamo='../data/hwyproj.prn'
      open(share='denynone',shared,
     *unit=lunin,file=fnamo,status='old',err=9000)
   19 read(lunin,10021,iostat=istat) pjname !Scenario Column Number
      read(lunin,10021,iostat=istat) pjname !Full Scenario Description
      read(lunin,10021,iostat=istat) pjname !Short Scenario Name
      print *,fnamo
   20 read(lunin,10020,iostat=istat) iproj,pjname,years
      if(istat.ne.0) go to 29
      print *,iproj,pjname
      if(iproj.lt.1.or.iproj.gt.mxproj) go to 9020
      iyr=years(ipcol)
      if(iyr.lt.1970.or.iyr.gt.9999) go to 9020
      if(iyr.gt.2099) iyr=2099
      pyr(iproj)=iyr
      prname(iproj)=pjname
      if(iproj.gt.mproj) mproj=iproj
      go to 20
   29 close(lunin)
c
c     read subzone coverage (if any)
c
      fnamo='subzones/pat.adf'
      open(unit=patlun,convert='big_endian',file=fnamo,access=
     *'direct',form='unformatted',recl=10,status='old',err=31)
      go to 32
   31 fnamo='../covs/subzones/pat.adf'
      open(unit=patlun,convert='big_endian',file=fnamo,access=
     *'direct',form='unformatted',recl=10,status='old',err=60)
   32 patrec=2
      sub=.true.
   33 call rzonep
      if(patio.ne.0) go to 39
      if(patzon.lt.1.or.patzon.gt.mxzn) go to 9000
      patrec=patrec+1
      szone(patzon)=.true.
      go to 33
   39 close(patlun)
c
c     read analysis year
c
   60 fnamo='hwyyear'
      open (unit=lunin,file=fnamo,status='old',err=61)
      go to 62
   61 fnamo='year'
      open (unit=lunin,file=fnamo,status='old',err=9000)
   62 icol=1
      read(lunin,10000,iostat=istat) idata,adata
      if(istat.ne.0) go to 9001
      call sd_getdat(idata,adata,icol,year,i2,i3,lunerr)
      close(lunin)
      call ottcn
      fnamo = 'ttcov.nat'
      print *,fnamo
   92 natrec=1
c
c     read node point attribute table saving arrays of node data
c     for node
c
  100 call rtcn
      if(natio.ne.0) go to 159
      nattmp=0
      do 119 iyr=1,2
      iproj=natprj(iyr)
      if(iproj.lt.1) go to 119
      if(iproj.le.mproj) go to 111
      write(luninf,19110) natrec,natxnm(1),natxnm(2),iproj
      inff=.true.
      nattmp=1
      go to 119
  111 if(pyr(iproj).ge.1970.and.pyr(iproj).le.2099) go to 112
      write(luninf,19110) natrec,natxnm(1),natxnm(2),iproj
      inff=.true.
      nattmp=1
  112 natyr(iyr)=pyr(iproj)
      ulproj(iproj)=.true.
  119 continue
      if(natyr(2).ge.1970.and.natyr(2).le.year) go to 131
      go to 150
  131 natcnt(1)=natcnt(2)
      natjur(1)=natjur(2)
  150 call wtcn
      natrec=natrec+1
      go to 100
  159 close(natlun)
      call ottca 
      fnamo = 'ttcov.aat'
      print *,fnamo
  192 aatrec=1
c
c     read hwycov aat and save data.
c
  200 call rtca
      if(aatio.ne.0) go to 299
      aattmp(1)=0
      aattmp(2)=0
      if(aattpn(1).lt.1.or.aattpn(1).gt.mxhcn) go to 9201
      if(aattpn(2).lt.1.or.aattpn(2).gt.mxhcn) go to 9201
      if(aatfc(1).lt.1.or.aatfc(1).gt.14)  go to 9200
      if(aatfc(1).ne.10.or.aatfc(1).ne.14) zcent(aattpn(1))=.false.
      if(szone(aattpn(1)).and.aatfc(1).eq.14) szc(aattpn(1))=.true.
      if(szone(aattpn(2)).and.aatfc(1).eq.14) szc(aattpn(2))=.true.
      if(aatfc(1).eq.9.and.(aathov(1).eq.2.or.aathov(1).eq.3))
     *hovacc(aatrec)=.true.
      fn(aatrec)=aatarn(2)
      tn(aatrec)=aatarn(1)
      way(aatrec)=aatway(1)
c
c     change years if projects are coded on links
c
      do 213 iyr=1,mxtcyr
      iproj=aatprj(iyr)
      if(iproj.lt.1) go to 213
      if(iproj.le.mproj) go to 211
      write(luninf,19210) aatid,aatnm,aatxnm(2),aatxnm(1),iproj
      aattmp(1)=4
      inff=.true.
      if(aatyr(iyr).lt.1970) aatyr(iyr)=1990
      go to 213
  211 if(pyr(iproj).ge.1970.and.pyr(iproj).le.2099) go to 212
      write(luninf,19210) aatrec,aatnm,aatxnm(2),aatxnm(1),iproj
      aattmp(1)=4
      inff=.true.
      if(aatyr(iyr).lt.1970) aatyr(iyr)=1990
c      if(iproj.eq.13) aatlos=2000
      go to 213
  212 aatyr(iyr)=pyr(iproj)
      ulproj(iproj)=.true.
  213 continue

      if(aatdprj.gt.0.and.aatdprj.le.mproj) then 
      if(pyr(aatdprj).ge.1970.and.pyr(aatdprj).le.2099) then      
      aatdyr=pyr(aatdprj)
      ulproj(aatdprj)=.true.
      end if
      end if
c
c     set initial year to lowest of any year
c
      minyr=9999
      do iyr=1,mxtcyr
       if(aatyr(iyr).gt.0) minyr=min(aatyr(iyr),minyr)
      end do
      aatyr(1)=minyr
c
c     copy upgrade attributes to initial attributes where appropriate
c
      do 229 jyr=2,mxtcyr
      if(aatyr(jyr).lt.1970.or.aatyr(jyr).gt.year) go to 229
      aatjur(1)=aatjur(jyr)
      aatfc(1)=aatfc(jyr)
      aathov(1)=aathov(jyr)
      aatspd(1)=aatspd(jyr)
      aatway(1)=aatway(jyr)
      aatmed(1)=aatmed(jyr)
      aatyr(1)=aatyr(jyr)  !JOR 7/24/09
      aatprj(1)=aatprj(jyr)  !RCU 9/3/08
      aattrk(1)=aattrk(jyr) !RCU 10/3/11
      aattoll(1,1)=aattoll(1,jyr) !RCU 8/29/08
      aattoll(2,1)=aattoll(2,jyr) !RCU 8/29/08
      aattoll(3,1)=aattoll(3,jyr) !RCU 8/29/08
      do 228 idir=1,2
      aatln(1,idir,1)=aatln(1,idir,jyr)
      aatln(2,idir,1)=aatln(2,idir,jyr)
      aatln(3,idir,1)=aatln(3,idir,jyr)
      aataux(idir,1)=aataux(idir,jyr)
      aatcnt(idir,1)=aatcnt(idir,jyr)
      aattl(idir,1)=aattl(idir,jyr)
      aatrl(idir,1)=aatrl(idir,jyr)
      aatll(idir,1)=aatll(idir,jyr)
  228 continue
  229 continue
      if(aatdyr.lt.1970.or.aatdyr.gt.year) go to 279
      aatyr(1)=9999
c
c     save data needed for deleting dangles
c
  279 if(aatfc(1).gt.10.and.aatfc(1).lt.14) go to 290
      if(aatarn(2).lt.1.or.aatarn(2).gt.mxtcn) go to 9280
      if(aatarn(1).lt.1.or.aatarn(1).gt.mxtcn) go to 9280
      if(aatid.lt.1.or.aatid.gt.mxid) go to 9280
      if(aatyr(1).gt.year) go to 280

      if(aathov(1).eq.2.or.aathov(1).eq.3) then
       if(aatfc(1).eq.1.or.aatfc(1).eq.8) then
         hovnod(aatarn(2))=.true.
         hovnod(aatarn(1))=.true.
       endif
      endif
 
      nlink(aatarn(1))=nlink(aatarn(1))+1
      nlink(aatarn(2))=nlink(aatarn(2))+1
      exit(aatarn(2))=exit(aatarn(2))+1
      entry(aatarn(1))=entry(aatarn(1))+1
      if(aatway(1).eq.1) go to 280
      exit(aatarn(1))=exit(aatarn(1))+1
      entry(aatarn(2))=entry(aatarn(2))+1
  280 if(xid(aatid).lt.1) go to 281
      write(lunerr,19281) aatid
      errf=.true.
  281 xid(aatid)=aatid
      yr(aatrec)=aatyr(1)
      fc(aatrec)=aatfc(1)
  290 call wtca
      aatrec=aatrec+1
      go to 200
  299 maxrec=aatrec-1
      aatrec=1
      fnamo = 'ttcov.aat'
      print *,fnamo
c
c     reread hwycov aat and check for reg zcs in subarea zones with szc
c
  300 call rtca
      if(aatio.ne.0) go to 400
      if(sub) go to 309
      if(aatfc(1).eq.10) go to 390
      if(aatfc(1).eq.14) go to 311
      go to 390
  309 if(aatfc(1).eq.10) go to 310
      if(aatfc(1).eq.14) go to 320
      go to 390
  310 if(zcent(aattpn(1)).and.szc(aattpn(1))) go to 311
      if(zcent(aattpn(2)).and.szc(aattpn(2))) go to 311
      go to 390
  311 aatyr(1)=9999
      go to 380
  320 if(zcent(aattpn(1)).and.szc(aattpn(1))) go to 321
      if(zcent(aattpn(2)).and.szc(aattpn(2))) go to 321
      aatyr(1)=9999
      go to 380
  321 aatfc(1)=10
      go to 380
  380 call wtca
  390 aatrec=aatrec+1
      go to 300
c
c     identify dangling arcs with "9999" in iyr
c
c 
c     first check for hov access links that are coded as deletes, but have
c     connecting arcs
c
  400 do 406 aatrec=1,maxrec
      if(.not.hovacc(aatrec)) go to 406
      ifn=fn(aatrec)
      itn=tn(aatrec)
      if(yr(aatrec).le.year) go to 402
      if(hovnod(ifn)) go to 401
      if(hovnod(itn)) go to 401
      go to 406
  401 yr(aatrec)=year
      nlink(ifn)=nlink(ifn)+1
      nlink(itn)=nlink(itn)+1
      exit(ifn)=exit(ifn)+1
      entry(itn)=entry(itn)+1
      if(aatway(1).eq.1) go to 4011
      exit(itn)=exit(itn)+1
      entry(ifn)=entry(ifn)+1
 4011 urec(aatrec)=.true.
      call rtca
      if(aatio.ne.0) go to 9430
c     aatyr(1)=year
      call wtca
      go to 406
  402 if(hovnod(ifn)) go to 406
      if(hovnod(itn)) go to 406
      yr(aatrec)=9999
      nlink(ifn)=nlink(ifn)-1
      nlink(itn)=nlink(itn)-1
      exit(ifn)=exit(ifn)-1
      entry(itn)=entry(itn)-1
      if(aatway(1).eq.1) go to 4021
      exit(itn)=exit(itn)-1
      entry(ifn)=entry(ifn)-1
 4021 urec(aatrec)=.true.
      call rtca
      if(aatio.ne.0) go to 9430
      aatyr(1)=9999
      call wtca
  406 continue
c 
c     find unsued link with only 1 link at from node
c
  407 do 408 aatrec=1,maxrec
      if(yr(aatrec).gt.year) go to 408
      if(hovacc(aatrec)) go to 408
      if(fc(aatrec).lt.2) go to 408
      if(fc(aatrec).gt.9) go to 408
c     if(fc(aatrec).gt.9.and.fc(aatrec).lt.14) go to 408
      if(urec(aatrec)) go to 408
      ifn=fn(aatrec)
      if(nlink(ifn).eq.1) go to 410
  408 continue
      do 409 aatrec=1,maxrec
      if(yr(aatrec).gt.year) go to 409
      if(fc(aatrec).lt.1) go to 409
      if(fc(aatrec).gt.9) go to 409
      if(urec(aatrec)) go to 409
      itn=tn(aatrec)
      if(nlink(itn).eq.1) go to 420
  409 continue
      go to 499
  410 itn=tn(aatrec)
      go to 430
  420 ifn=fn(aatrec)
  430 exit(ifn)=exit(ifn)-1
      entry(itn)=entry(itn)-1
      if(way(aatrec).eq.1) go to 431
      exit(itn)=exit(itn)-1
      entry(ifn)=entry(ifn)-1
  431 nlink(ifn)=nlink(ifn)-1
      nlink(itn)=nlink(itn)-1
      urec(aatrec)=.true.
      call rtca
      if(aatio.ne.0) go to 9430
      aatyr(1)=9999
      call wtca
      go to 407

c
c     change stub links to 0ne-way
c
  499 aatrec=1
      fnamo = 'ttcov.aat'
      print *,fnamo
  500 call rtca
      if(aatio.ne.0) go to 900
      if(aatway(1).eq.1) go to 590
      if(aatyr(1).gt.year) go to 590
      if(aatfc(1).gt.9) go to 590
      ifn=fn(aatrec)
      itn=tn(aatrec)
c     if(entry(itn).lt.2) go to 502
c     if(entry(ifn).lt.2) go to 502
c     if(exit(itn).lt.2) go to 502
c     if(exit(ifn).lt.2) go to 501
      go to 590
  501 aatway(1)=1    
      go to 580
  502 aatway(1)=1    
      aattmp(1)=9
c
c     need to flip ba data to ab
c
      in=aattpn(1)
      aattpn(1)=aattpn(2)
      aattpn(2)=in
      aatln(1,1,2)=aatln(1,2,1)
      aatln(2,1,2)=aatln(2,2,1)
      aatln(3,1,2)=aatln(3,2,1)
      aataux(1,2)=aataux(2,1)
      aatcnt(1,2)=aatcnt(2,1)
      aattl(1,2)=aattl(2,1)
      aatrl(1,2)=aatrl(2,1)
      aatll(1,2)=aatll(2,1)
      aatln(1,1,1)=aatln(1,1,2)
      aatln(2,1,1)=aatln(2,1,2)
      aatln(3,1,1)=aatln(3,1,2)
      aataux(1,1)=aataux(1,2)
      aatcnt(1,1)=aatcnt(1,2)
      aattl(1,1)=aattl(1,2)
      aatrl(1,1)=aatrl(1,2)
      aatll(1,1)=aatll(1,2)
      aatln(1,1,2)=aatln(1,2,1)
      aatln(2,1,2)=aatln(2,2,1)
      aatln(3,1,2)=aatln(3,2,1)
      aataux(1,2)=aataux(2,1)
      aatcnt(1,2)=aatcnt(2,1)
      aattl(1,2)=aattl(2,1)
      aatrl(1,2)=aatrl(2,1)
      aatll(1,2)=aatll(2,1)
      write(luninf,19502) aatid,aatnm,aatxnm(2),aatxnm(1)
      inff=.true.
  580 call wtca
  590 aatrec=aatrec+1
      go to 500
c
c     write out unused project codes
  900 do 901 i=1,mproj
      if(ulproj(i)) go to 901
      write(luninf,19901) i,prname(i)
      inff=.true.
  901 continue
      go to 9999
 9000 write(lunerr,19000) fnamo
      errf=.true.
      go to 9999
 9001 write(lunerr,19001) fnamo,adata
      errf=.true.
      go to 9999
 9020 write(lunerr,19020) pjname
      errf=.true.
      go to 20
 9200 write(lunerr,19200) aatrec
      errf=.true.
      go to 9999
 9201 write(lunerr,19201) aatrec
      errf=.true.
      go to 9999
 9280 write(lunerr,19280) aatrec
      errf=.true.
      go to 9999
 9430 write(lunerr,19430) aatrec
      errf=.true.
      go to 9999
10000 format(80i1,t1,80a1)
10010 format(6a1,t9,i4)
10030 format(80i1)
10040 format(6a1,t52,5i5)
10020 format(4x,i4,a43,<mxpyr>(4x,i4))
10021 format(a43)
20391 format(2i10)
19000 format(' problem opening file: ',a80)
19001 format(' data problem with file: ',a80/,80a1)
19020 format(' data problem with hwyproj record: ',a43)
19110 format(' naterr#1 - proj on nat not in hwyproj list:',/,
     *i10,1x,a20,1x,a20,1x,i4)
19200 format(' no ifc coded on tcoved/aat record:',i10)
19201 format(' data problem with tcoved/aat record:',i10)
19210 format('aaterr#4 - proj on aat not in hwyproj.lis:',/,
     *'id = ',i5,' name=',a20,' from = ',a20,' to = ',a20,' proj = ',i4)
19280 format(' data problem with hwycov/aat record:',i10)
19281 format(' duplicate id ', i10)
19430 format(' problem linking hwycov/aat record:', i10)
19502 format('changing arc to one-way to eliminate stub link:',/,
     *'id = ',i5,' name=',a20,' from = ',a20,' to = ',a20)
19901 format(' project not on hwycov/aat:',i4,a50)
39999 format('[',a7,'] [',a8,'] [s9.10/tchc1] [',a<ic1>,
     *'] [tcov records=',i<ic2>,']')
 9999 if(errf)  go to 99991
      tpstat='ok'
      ic1=2
      close(lunerr,status='delete')
      go to 99992
99991 close(lunerr,status='keep')
      tpstat='not'
      ic1=3
99992 if(inff)  go to 99993
      close(luninf,status='delete')
      go to 99994
99993 close(luninf,status='keep')
99994 open (unit=lunerr,file='tplog',access='append')
      ic2=1
      if(maxrec.gt.9) ic2=2
      if(maxrec.gt.99) ic2=3
      if(maxrec.gt.999) ic2=4
      if(maxrec.gt.9999) ic2=5
      if(maxrec.gt.99999) ic2=6
      call dattim
      write(lunerr,39999)  cdate,ctime,tpstat,maxrec
      close(lunerr)
      stop
      end
