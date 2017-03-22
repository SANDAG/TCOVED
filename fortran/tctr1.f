c     first in a series of programs/amls to create tranplan
c     transit inputs from tcov, this program:
c     1) identifies arcs/sections/routes that are used by routes 
c        specified in headways directory
c     2) copies upgrade data to "initial" data
c     3 fills in routes on arcs
c
      parameter (mxproj=1000,mxrlb=999,mxpyr=20)
      include 'sandag.inc'
      include 'tcov.inc'
c
c     program variables
c
      logical ulproj(mxproj)
      integer*2 selrt(mxrlb),selcon(mxrlb,2,mxcon),
     *lbtrt(20,mxtca),lbop(7,mxtca)
      integer*4 rlbid(mxrlb),pyr(mxproj),year,years(mxpyr)
      character*1 adir
      character*43 pjname,prname(mxproj)
      character*80 timnam
      data lunin,lunerr,ratlun,seclun,aatlun,natlun
     */11,12,13,14,15,16/
c
c     open files
c
      open (unit=lunerr,file='tctr1.err')
      ipcol=1
c
c     program will use column 1 of hwyproj.lis unless hwyproj.col exists
c
      fnamo='hwyproj.col'
      open(unit=lunin,file=fnamo,status='old',err=7)
      icol=1
      read(lunin,10000,iostat=istat) idata,adata
      if(istat.ne.0) go to 9001
      print *, fnamo
      call sd_getdat(idata,adata,icol,ipcol,i2,i3,lunerr)
      if(ipcol.lt.1.or.ipcol.gt.mxpyr) go to 9001
      close(lunin)
c
c     read new hwyproj and save project years
c
    7 fnamo='hwyproj.prn'
      open(unit=lunin,file=fnamo,status='old',err=8)
      go to 10
    8 fnamo='../data/hwyproj.prn'
      open(unit=lunin,file=fnamo,status='old',err=9000)
      print *, fnamo
   10 read(lunin,10011,iostat=istat) pjname !Scenario Column Number
      read(lunin,10011,iostat=istat) pjname !Full Scenario Description
      read(lunin,10011,iostat=istat) pjname !Short Scenario Name
   11 read(lunin,10010,iostat=istat) iproj,pjname,years
      if(istat.ne.0) go to 12
      if(iproj.lt.1.or.iproj.gt.mxproj) go to 9010
      iyr=years(ipcol)
      if(iyr.lt.1970.or.iyr.gt.9999) go to 9010
      if(iyr.gt.2099) iyr=2099
      pyr(iproj)=iyr
      prname(iproj)=pjname
      if(iproj.gt.mproj) mproj=iproj
      go to 11
   12 close(lunin)
c
c     read analysis year
c
      fnamo='year'
      open (unit=lunin,file=fnamo,status='old',err=9000)
      print *, fnamo
      icol=1
      read(lunin,10000,iostat=istat) idata,adata
      if(istat.ne.0) go to 9001
      call sd_getdat(idata,adata,icol,year,i2,i3,lunerr)
      close(lunin)
      call ottctrr
      if(ratio.ne.0) go to 9000
      fnamo='ttcov.rat'
      print *, fnamo
      ratrec=1
c
c     read route attribute table
c
   28 call rtctrr
      if(ratio.ne.0) go to 29
      if(ratlb.lt.1.or.ratlb.gt.mxrlb) go to 9028
      if(ratid.lt.1.or.ratid.gt.mxrlb) go to 9028
      rlbid(ratlb)=ratid
      ratrec=ratrec+1
      go to 28
   29 fnamo='headways'
      open (unit=lunin,file=fnamo,status='old',err=9000)
      print *, fnamo
   30 read(lunin,10030,end=31) irt,adir,icon
      if(irt.lt.1) go to 30
      if(irt.gt.999) go to 9030
      idir=0
      if(adir.eq.'i') idir=1
      if(adir.eq.'i') idir=1
      if(adir.eq.'o') idir=2
      if(adir.eq.'o') idir=2
      if(idir.lt.1.or.idir.gt.2) go to 9030
      if(icon.lt.1.or.icon.gt.mxcon) go to 9030
      selrt(irt)=irt
      selcon(irt,idir,icon)=irt
      go to 30
   31 close(lunin)
      go to 39
   32 do 38 irt=1,999
      selrt(irt)=irt
      do 37 idir=1,2
      do 36 icon=1,mxcon
      selcon(irt,idir,icon)=irt
   36 continue
   37 continue
   38 continue
c
c     open section data file.
c
   39 call ottctrs
      if(secio.ne.0) go to 9000
      fnamo='ttcov.sec'
      print *, fnamo
      secrec=1
   40 call rtctrs
      if(secio.ne.0) go to 49
      if(rlink.lt.1.or.rlink.gt.mxrlb) go to 9040
      if(alink.lt.1.or.alink.gt.mxtca) go to 9040
      id=rlbid(rlink)
      if(id.lt.1.or.id.gt.mxrlb) go to 9040
      sectm2=0
      do 42 i=1,mxcon
      if(secic(i).lt.1.or.secic(i).eq.9) go to 42
      irt=selcon(id,1,i)
      if(irt.gt.0) go to 41
      secic(i)=9
      secsic(i)=9999
      go to 42
   41 sectm2=irt
   42 continue
      do 44 i=1,mxcon
      if(secoc(i).lt.1.or.secoc(i).eq.9) go to 44
      irt=selcon(id,2,i)
      if(irt.gt.0) go to 43
      secoc(i)=9
      secsoc(i)=9999
      go to 44
   43 sectm2=irt
   44 continue
      if(sectm2.eq.0) go to 47
      do 45 jrt=1,20
      if(lbtrt(jrt,alink).eq.0) go to 46
      if(lbtrt(jrt,alink).eq.sectm2) go to 460
   45 continue
      go to 460
   46 lbtrt(jrt,alink)=sectm2
  460 iop=1
      if(sectm2.ge.300) iop=2
      if(sectm2.ge.500) iop=3
      if(sectm2.ge.600) iop=4
      if(sectm2.ge.700) iop=5
      if(sectm2.ge.800) iop=6
      if(sectm2.ge.900) iop=7
      if(sectm2.eq.8) iop=7
      if(sectm2.eq.21) iop=7
      if(sectm2.eq.22) iop=7
      if(sectm2.eq.23) iop=7
      if(sectm2.eq.29) iop=7
      lbop(iop,alink)=1
   47 call wtctrs
      if(secio.ne.0) go to 49
   48 secrec=secrec+1
      go to 40
   49 close(seclun)
      ratrec=1
      fnamo='ttcov.rat'
      print *, fnamo
c
c     reread route attribute table set id=0 for unused routes
c
   60 call rtctrr
      if(ratio.ne.0) go to 69
      if(ratlb.lt.1.or.ratlb.gt.mxrlb) go to 9028
      if(selrt(ratid).lt.1) ratid=0
      call wtctrr
      if(ratio.ne.0) go to 9000
      ratrec=ratrec+1
      go to 60
   69 close(ratlun)
c
c     open nat file
c
      call ottcn
      if(natio.ne.0) go to 9000
      fnamo='ttcov.nat'
      print *, fnamo
      natrec=1
c
c     read node point attribute table saving arrays of node data
c     for node
c
  100 call rtcn
      if(natio.ne.0) go to 199
      do 119 iyr=1,2
      iproj=natprj(iyr)
      if(iproj.lt.1) go to 119
      if(iproj.le.mproj) go to 111
      write(lunerr,19110) natrec,natxnm(1),natxnm(2),iproj
      nattmp=1
      go to 119
  111 if(pyr(iproj).ge.1970.and.pyr(iproj).le.2099) go to 112
      write(lunerr,19110) natrec,natxnm(1),natxnm(2),iproj
      nattmp=1
  112 natyr(iyr)=pyr(iproj)
      ulproj(iproj)=.true.
  119 continue
      if(natyr(2).ge.1970.and.natyr(2).le.year) go to 131
      go to 150
  131 natcnt(1)=natcnt(2)
      natjur(1)=natjur(2)
  150 if(nattyr(2).lt.1970.or.year.ge.nattyr(2)) go to 151
      natstp(2)=0
      natprk(2)=0
      nattt(2)=0
      natdwl(2)=0
  151 call wtcn
      if(natio.ne.0) go to 9000
      natrec=natrec+1
      go to 100
  199 close(natlun)
c
c     open tcov aat file
c
      call ottca
      if(aatio.ne.0) go to 9000
      fnamo='ttcov.aat'
      print *, fnamo

      aatrec=1
c
c     read tcov aat and save data.
c
  200 call rtca
      if(aatio.ne.0) go to 9999
      aattmp(1)=lbtrt(1,aatlb)
      i1=1
      i2=1
      i3=1
      i4=1
c
c     change years if projects are coded on links
c
      do 213 iyr=1,4
      iproj=aatprj(iyr)
      if(iproj.lt.1) go to 213
      if(iproj.le.mproj) go to 211
      write(lunerr,19210) aatid,aatnm,aatxnm(2),aatxnm(1),iproj
      aattmp(1)=4
      errf=.true.
      if(aatyr(iyr).lt.1970) aatyr(iyr)=1990
      go to 213
  211 if(pyr(iproj).ge.1970.and.pyr(iproj).le.2099) go to 212
      write(lunerr,19210) aatrec,aatnm,aatxnm(2),aatxnm(1),iproj
      aattmp(1)=4
      errf=.true.
      if(aatyr(iyr).lt.1970) aatyr(iyr)=1990
      go to 213
  212 aatyr(iyr)=pyr(iproj)
      ulproj(iproj)=.true.
  213 continue
c
c     copy upgrade attributes to initial attributes where appropriate
c
      do 229 iyr=2,3
      if(aatyr(iyr).lt.1970.or.aatyr(iyr).gt.year) go to 229
      jyr=iyr-1
      aatjur(iyr)=aatjur(jyr)
      aatfc(iyr)=aatfc(jyr)
      aathov(iyr)=aathov(jyr)
      aatspd(iyr)=aatspd(jyr)
      aatway(iyr)=aatway(jyr)
      aatmed(iyr)=aatmed(jyr)
      do 228 idir=1,2
      aatln(1,idir,iyr)=aatln(1,idir,jyr)
      aatln(2,idir,iyr)=aatln(2,idir,jyr)
      aatln(3,idir,iyr)=aatln(3,idir,jyr)
      aataux(idir,iyr)=aataux(idir,jyr)
      aatcnt(idir,iyr)=aatcnt(idir,jyr)
      aattl(idir,iyr)=aattl(idir,jyr)
      aatrl(idir,iyr)=aatrl(idir,jyr)
      aatll(idir,iyr)=aatll(idir,jyr)
  228 continue
  229 continue
      if(aatyr(4).lt.1970.or.aatyr(4).gt.year) go to 270
      aatyr(1)=9999
c
c     reuse tcov arc attributes to hold transit attributes
c
  270 aatyr(2)=lbtrt(1,aatlb)
      aatprj(2)=lbtrt(2,aatlb)
      aatjur(2)=lbtrt(3,aatlb)
      aatfc(2)=lbtrt(4,aatlb)
      aathov(2)=lbtrt(5,aatlb)
      aatspd(2)=lbtrt(6,aatlb)
      aatway(2)=lbtrt(7,aatlb)
      aatmed(2)=lbtrt(8,aatlb)
      aatln(1,1,2)=lbtrt(9,aatlb)
      aatln(2,1,2)=lbtrt(10,aatlb)
      aatln(3,1,2)=lbtrt(11,aatlb)
      aataux(1,2)=lbtrt(12,aatlb)
      aatcnt(1,2)=lbtrt(13,aatlb)
      aattl(1,2)=lbtrt(14,aatlb)
      aatrl(1,2)=lbtrt(15,aatlb)
      aatll(1,2)=lbtrt(16,aatlb)
      aatln(1,2,2)=lbtrt(17,aatlb)
      aatln(2,2,2)=lbtrt(18,aatlb)
      aatln(3,2,2)=lbtrt(19,aatlb)
      aataux(2,2)=lbtrt(20,aatlb)
      aatcnt(2,2)=lbop(1,aatlb)
      aattl(2,2)=lbop(2,aatlb)
      aatrl(2,2)=lbop(3,aatlb)
      aatll(2,2)=lbop(4,aatlb)
      aatyr(3)=lbop(5,aatlb)
      aatprj(3)=lbop(6,aatlb)
      aatjur(3)=lbop(7,aatlb)
      aattmp(2)=i1*1000+i2*100+i3*10+i4
      call wtca
      if(aatio.ne.0) go to 9000
      aatrec=aatrec+1
      go to 200
 9000 write(lunerr,19000) fnamo
      errf=.true.
      go to 9999
 9001 write(lunerr,19001) fnamo
      errf=.true.
      go to 9999
 9010 write(lunerr,19010) prname
      errf=.true.
      go to 10
 9028 write(lunerr,19028) ratlb,ratid
      errf=.true.
      go to 9999
 9030 write(lunerr,19030) irt,adir,icon
      errf=.true.
      go to 30
 9031 write(lunerr,19031) irt,adir,icon
      errf=.true.
      go to 30
 9040 write(lunerr,19040) rlink,alink
      errf=.true.
      go to 48
 9150 write(lunerr,19150) recnum,natlb
      errf=.true.
      go to 9999
10000 format(80i1,t1,80a1)
10010 format(4x,i4,a43,<mxpyr>(4x,i4))
10011 format(a43)
10030 format(1x,i3,1x,a1,i2)
10050 format(a80)
20391 format(2i10)
19000 format(' problem opening file: ',a80)
19001 format(' data problem with file: ',a80/,80a1)
19010 format(' data problem with hwyproj record: ',a45)
19028 format(' data problem with trtcov/rat record:',2i10)
19030 format(' data problem with headways record:',i5,1x,a2,i3)
19040 format(' data problem with trtcov/sec record:',2i10)
19031 format(' missing trtcov route for headways route:',i5,1x,a2,i3)
19110 format(' proj on nat not in hwyproj list:',/,
     *i10,1x,a20,1x,a20,1x,i4)
19150 format(' data problem with trtcov/nat record:',2i10)
19210 format(' proj on aat not in hwyproj list:',/,
     *i10,1x,a20,1x,a20,1x,a20,1x,i4)
19901 format(' project not on trtcov/aat:', a6)
39999 format('2030fin/tctr1]')
 9999 if(errf)  go to 99991
      tpstat='ok'
      ic1=2
      close(lunerr,status='delete')
      go to 99992
99991 close(lunerr,status='keep')
      tpstat='not'
      ic1=3
99992 open (unit=lunerr,file='tplog',access='append')
      recnum=recnum-1
      ic2=index(timnam,' ')
      ic2=ic2-1
      ic3=1
      if(recnum.gt.9) ic3=2
      if(recnum.gt.99) ic3=3
      if(recnum.gt.999) ic3=4
      if(recnum.gt.9999) ic3=5
      if(recnum.gt.99999) ic3=6
c     call dattim
c     write(lunerr,39999)  cdate,ctime,tpstat,year,timnam,recnum
      write(lunerr,39999) 
      close(lunerr)
      stop
      end
      subroutine getpj(proj,aproj)
      character*1 proj(6),ax(6)
      character*6 aproj
      ax(1)=' '
      ax(2)=' '
      ax(3)=' '
      ax(4)=' '
      ax(5)=' '
      ax(6)=' '
      aproj='      '
      do 10 i=1,6
      if(proj(i).ne.' ') go to 11
   10 continue
      go to 99
   11 ax(1)=proj(i)
      mc=1
      ii=i+1
      do 12 j=ii,6
      if(proj(j).eq.' ') go to 19
      mc=mc+1
      ax(mc)=proj(j)
   12 continue
   19 write(aproj,20019) ax
20019 format(6a1)
   99 return
      end
