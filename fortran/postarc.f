      parameter (mxramp=5000,mxrlk=1000,mxadt=9999,
     *mxradt=12999,mxproj=1000)
      include 'sandag.inc'
      include 'tcov.inc'
      include 'zone.inc'
      common /int41/ recarr(mxtca)
      common /int42/ srec(mxtca),rlkarr(mxtca)
      common /c251/ ndarr(mxtca)
c
c     zoneid pat attributes
c
      real*4 zpat
c
c     lname pat attributes
c
      real*4 area,peri,dist
c
c     program variables
c
      logical ewdir(8,mxtcn),exnode(mxtcn),frend,zcent(mxtcn),
     *fwyrmp(mxtcn),lrtn(mxtcn),xramp(mxfc),rampi,rmpacc(mxramp),
     *stop,toend,tpused(mxtpn),usedup,zcnode(mxtcn),mtrnod(mxtcn),
     *endf,first,entry(mxtcn),exit(mxtcn),ulproj(mxproj),inff,
     *xex(8,mxtcn),rurzn(mxzn),usta(mxcsta)
      integer*2 lbrts(mxtca),nz(mxtcn),rtdir(14999),aatmod,i2,
     *nsph(mxtcn),nzone(mxtcn),nfz4(mxtcn),
     *cnnode(2,mxtcn),fcarr(mxtca),upnode(mxtcn),fynod1(mxtcn),
     *fyzon1(mxtcn),fynod2(mxtcn),fyzon2(mxtcn),
     *mjur(mxfc),mlf(mxfc,8),mln(mxfc),mway(mxfc),mtspd(mxfc),
     *mmed(mxfc),mramp,mspd(mxfc),new,nns,pfc(mxfc),pjur(5),psph(20),
     *rmparr(mxtca),rtrec,lbsta(mxid),lbjur(mxid),
     *lkiyr(mxtcn),lkiproj(mxtcn)
      integer*4 xd,yd,pyr(mxproj), xdapp(mxtcn),myear,xsta(9999),
     *xnode(4,mxtpn),xiyr(4,mxtpn),xdyr(4,mxtpn),aatmln(2,mxtcyr),
     *lbtp(mxtcn)
      integer*4 jjxy,tpxy(mxtpn),xy(mxtcn),nx(mxtcn),ny(mxtcn),jx,jy
      integer*4 mnode,n1,n11,n12,nodarr(2,mxtca),n2,n21,
     *n22,nid(mxid),recarr,
     *rlkarr,rmprec(mxramp),srec,xrec(8,mxtcn),
     *xapp(mxtcn),xstop(mxtcn),adthst(mxradt),stahst(mxcsta),stavols(6)
      real xx,yy,adtvols(29)
      character*1 nln(10),nxl(10),c1
      character*3 nmed(3)
      character*4 aramp,aacc
      character*5 nhov(2),njur(5)
      character*6 ncnt(9),nfc(mxfc),nffc(100) 
      character*7 nway(2)
      character*12 nrdir(4)
      character*15 r1namx(mxramp)
      character*20 ewnm(4),kname(mxrlk),nsnm(4),xnm1,xnm2,
     *r1name(mxramp),lname(mxtca),name1,noname,
     *nmarr(mxtca),r2name(mxramp),xnm(4,mxtcn),lrtxnm(mxtcn)
      character*25 ndarr
      character*45 pjname,pnarr(mxproj)
      character*80 nearnm
      data aatlun,natlun,seclun,lunin,lunout,lunerr,lunpr,luninf,lunlane
     */11,12,13,14,15,16,17,18,19/
      data zcent/mxtcn*.true./,noname/'UNKNOWN             '/
      data aramp/'RAMP'/,aacc/'RAMA'/,endf/.false./,first/.true./,
     *mnode/0/,mramp/0/,stop/.false./,lnode/5000/
      data mspd/65,45,40,35,30,40,35,65,30,30,50,25,25,20/
      data mtspd/55,45,40,35,30,40,35,55,30,30,50,25,25,20/
      data mmed/2,2,2,1,1,1,1,1,1,1,1,1,1,1/
      data mway/1,2,2,2,2,2,2,1,1,2,2,2,2,2/
      data mln/4,3,2,2,1,1,1,1,1,1,1,1,1,1/
      data mjur/1,4,4,5,5,5,5,1,1,5,5,5,5,5/
      data rtdir/14999*1/
      data ncnt/'0-none','1-sig ','2-4stp','3-2stp','4-mtr-',
     *'5-mtr+','6-lrt ','7-toll','8-prev'/
      data nfc/'1-frwy','2-prim','3-majr','4-secn','5-lcol',
     *'6-rcol','7-locl','8-fwfw','9-ramp','10-acc','11-lrt',
     *'12-bus','13-adt','14-sacc'/
      data nffc/'0-none','1-uint','2-ufwy','3-upa ','4-uma ','5-umc ',
     *                   '6-rint','7-rfwy','8-rpa ','9-rma ','10-rmc',
     *89*'x-none'/
      data nhov/'1-mix','2-hov'/
      data njur/'1-sta','2-cmp','3-reg','4-maj','5-loc'/
      data nln/'0','1','2','3','4','5','6','7','8','n'/
      data nxl/'0','1','2','3','4','5','6','f','p','n'/
      data nmed/'1-n','2-m','3-c'/
      data nrdir/'1-southbound','2-eastbound ','3-northbound',
     *'4-westbound '/
      data nway/'one-way','two-way'/
      data fynod1/mxtcn*9999/,upnode/mxtcn*9999/
      data fyzon1/mxtcn*9999/,fynod2/mxtcn*9999/,fyzon2/mxtcn*9999/
      data ndarr/mxtca*'zzzzzzzzzzzzzzzzzzzz99999'/
      data rlkarr/mxtca*99999999/
      data nearnm/'stanear/pat.adf'/
      eject=' '
      xramp(8)=.true.
      xramp(9)=.true.
      do 2 i=1,mxfc
      do 1 j=1,8
      mlf(i,j)=9
    1 continue
    2 continue
      mlf(2,1)=2
      mlf(2,2)=1
      mlf(2,3)=1
      mlf(3,1)=1
      mlf(3,2)=1
      mlf(3,3)=1
      mlf(4,1)=1
      mlf(4,2)=1
      mlf(9,1)=1
      mlf(9,2)=1
      mlf(9,3)=1
      do 12 i=1,4
      do 11 j=1,mxtcn
      xnm(i,j)='unknown             '
   11 continue
   12 continue
       do ista=1,9999
        xsta(i)=i
       end do
      call dattim
c
c     open files
c
      open (unit=lunerr,file='postarc.err')
      open (unit=luninf,file='postarc.info')
      open (unit=lunlane,file='postarc_lane.pr')
c
c     read model year
c
      icol=1
      myear=1995
      fnamo='year'
      write(0,29999) fnamo
      open (unit=lunin,file=fnamo,status='old',iostat=istat)
      if(istat.ne.0) go to 7
      read(lunin,10000,iostat=istat) idata,adata
      call sd_getdat(idata,adata,icol,myear,i2,i3,lunerr)
      if(myear.lt.1995.or.myear.gt.2050) go to 9000
      close(lunin)
c
c     open zoneid pat file (post 6.0)
c
    7 fnamo='fz4id\pat.adf'
c     call opencov(lunin)
      open(unit=lunin,convert='big_endian',file=fnamo,access=
     *'direct',form='unformatted',recl=6,status='old',err=9000)
      patrec=1
      write(0,29999) fnamo
   20 read(lunin,rec=patrec,iostat=istat)
     *patlb,patx,paty,zpat,natsph,patzon,natfz4
      if(istat.ne.0) go to 29
      if(patlb.lt.1.or.patlb.gt.mxtcn) go to 21
      nx(patlb)=patx
      ny(patlb)=paty
      nz(patlb)=nint(zpat)
      nsph(patlb)=natsph
      nzone(patlb)=patzon
      nfz4(patlb)=natfz4
   21 patrec=patrec+1
      go to 20
   29 close(lunin)
c
c     read adt history file
c
      icol=1
      iyear=2008
      fnamo='adtyear'
      write(0,29999) fnamo
      open (unit=lunin,file=fnamo,status='old',iostat=istat)
      if(istat.ne.0) go to 30
      read(lunin,10000,iostat=istat) idata,adata
      call sd_getdat(idata,adata,icol,iyear,i2,i3,lunerr)
      if(iyear.lt.2000.or.myear.gt.2010) go to 9000
      close(lunin)
   30 iadtyr=iyear-1979
      fnami='../data/histfill'
      call opendata(lunin)
      if(istat.ne.0) go to 9000
      read(lunin,10009,iostat=istat) c1
      if(istat.ne.0) go to 9000
   33 read(lunin,10033,iostat=istat) aatadt,adtvols
      if(istat.ne.0) go to 39
      if(aatadt.lt.1.or.aatadt.gt.mxadt) go to 9030
      adthst(aatadt)=nint(adtvols(iadtyr)/100.0)
      go to 33
   39 close(lunin)
c      istayr=iyear-1999
c      fnami='counts.jur'
c      call opendata(lunin)
c      if(istat.ne.0) go to 9000
c      read(lunin,10009,iostat=istat) c1
c      if(istat.ne.0) go to 9000
c   43 read(lunin,10040,iostat=istat) icsta,stavols
c      if(istat.ne.0) go to 49
c      if(icsta.lt.1.or.icsta.gt.mxcsta) go to 9040
c      stahst(icsta)=stavols(istayr)
c      go to 43
c   49 close(lunin)
c
c     read rural zones
c
      icol=1
      fnamo='zones.rural'
      open (unit=lunin,file=fnamo,status='old',err=61)
      go to 62
   61 fnamo='../data/zones.rural'
      open (unit=lunin,file=fnamo,status='old',err=9000)
   62 read(lunin,10000,iostat=istat) idata,adata
      if(istat.ne.0) go to 69
      call sd_getdat(idata,adata,icol,izone,i2,i3,lunerr)
      if(izone.lt.1.or.izone.gt.mxzn) go to 9000
      rurzn(izone)=.true.
      go to 62
   69 close(lunin)
c
c     read hwyproj and save project years
c
      fnamo='hwyproj.prn'
      write(0,29999) fnamo
      open (unit=lunin,file=fnamo,status='old',iostat=istat)
      if(istat.eq.0) then 
      continue
      else
      fnamo='../data/hwyproj.prn'
      open (unit=lunin,file=fnamo,status='old',iostat=istat)
      end if
      if(istat.ne.0) go to 9000
      write(0,29999) fnamo
      read(lunin,10071,iostat=istat) pjname !Scenario Column Number
      read(lunin,10071,iostat=istat) pjname !Full Scenario Description
      read(lunin,10071,iostat=istat) pjname !Short Scenario Name
   73 read(lunin,10070,iostat=istat) iproj,pjname,iyr
      if(istat.ne.0) go to 74
      if(iproj.lt.1.or.iproj.gt.mxproj) go to 9070
      if(iyr.lt.1990.or.iyr.gt.9999) go to 9070
      if(iyr.gt.2099) iyr=2099
      if(pyr(iproj).gt.0) go to 9071
      pyr(iproj)=iyr
      pnarr(iproj)=pjname
      if(iproj.gt.mproj) mproj=iproj
      go to 73
   74 close(lunin)
c
c     open count station near pat file
c
      fnamo=nearnm
      write(0,29999) fnamo
      open(unit=lunin,convert='big_endian',file=fnamo,access=
     *'direct',form='unformatted',recl=7,status='old',err=9000)
      patrec=1
   75 read(lunin,rec=patrec,iostat=istat) area,peri,patlb,patid,
     *aatcj,aatsta,aatid,istatus
      if(istat.ne.0) go to 79
      if(istatus.lt.0) go to 77
      if(aatid.gt.mxid) go to 9000
      if(aatid.lt.1) go to 77
      lbsta(aatid)=aatsta
   77 patrec=patrec+1
      go to 75
   79 close(lunin)
c
c     open local name pat file
c
   81 fnamo='lname/pat.adf'
      write(0,29999) fnamo
      open(unit=lunin,convert='big_endian',file=fnamo,access=
     *'direct',form='unformatted',recl=11,status='old',err=89)
   82 write(0,11001)
      patrec=1
   83 read(lunin,rec=patrec,iostat=istat) area,peri,patlb,patid,aatnm,
     *aatlb,dist
      if(istat.ne.0) go to 88
      if(aatlb.lt.1.or.aatlb.gt.mxtca) go to 87
      lname(aatlb)=aatnm
   87 patrec=patrec+1
      go to 83

   88 close(lunin)
   89 write(0,11002)
      call otca
      if(aatio.ne.0) go to 9000
   92 aatrec=1
  100 call rtca
      if(aatio.ne.0) go to 129
      if(aatid.gt.mxid) go to 9102
      if(aatid.gt.0) nid(aatid)=nid(aatid)+1
      if(aatfc(1).lt.1.or.aatfc(1).gt.14)  go to 9101
      if(aatlp.gt.9999) go to 9000
      if(aatlp.gt.0) ista=aatcj*10000+aatlp
      ista=aatcj*10000+aatsta
      if(ista.gt.0.and.ista.le.mxcsta) usta(ista)=.true.
      do 105 idir=1,2
      if(aatarn(idir).gt.0.and.aatarn(idir).le.mxtcn) go to 101
      write(lunerr,19100) aatrec,aatarn(idir),aatnm,aatxnm(idir)
      errf=.true.
      aatarn(idir)=mxtcn
      stop=.true.
  101 if(aatarn(idir).gt.mnode) mnode=aatarn(idir)
      if (lkiyr(aatarn(idir)).eq.0) then
      lkiyr(aatarn(idir))=aatyr(1)
      else if (lkiyr(aatarn(idir)).gt.aatyr(1).and.aatyr(1).ge.1990)then
      lkiyr(aatarn(idir))=aatyr(1) !save the earliest link iyr by fnode#
      endif
      lkiproj(aatarn(idir))=aatprj(1) !save link iproj by fnode#
      aatx(idir)=nx(aatarn(idir))
      aaty(idir)=ny(aatarn(idir))
      nodarr(idir,aatrec)=aatarn(idir)
      if(aatfc(1).gt.9.and.aatfc(1).ne.13) go to 104
      do 102 i=1,8
      if(xrec(i,aatarn(idir)).eq.0) go to 103
  102 continue
      i=4
  103 xrec(i,aatarn(idir))=aatrec
      if(aatyr(1).le.myear.and.aatdyr.eq.0)
     *xex(i,aatarn(idir))=.true.
      if(aatyr(1).le.myear.and.aatdyr.gt.myear)
     *xex(i,aatarn(idir))=.true.
      xapp(aatarn(idir))=xapp(aatarn(idir))+1

      if(aatcnt(idir,1).eq.3)
     *xstop(aatarn(idir))=xstop(aatarn(idir))+1
      if(aatway(1).eq.1.and.idir.eq.2) go to 104
      xdapp(aatarn(idir))=xdapp(aatarn(idir))+1
  104 continue
  105 continue
      if(aatfc(1).lt.1.or.aatfc(1).gt.mxfc) go to 128
      recarr(aatrec)=aatrec
      nmarr(aatrec)=aatnm
      if(aatfc(1).eq.10.or.aatfc(1).eq.14) go to 120
      zcent(aatarn(2))=.false.
      zcent(aatarn(1))=.false.
      entry(aatarn(1))=.true.
      exit(aatarn(2))=.true.
      xx=float(aatx(2)-6140000)/100.0
      yy=float(2130000-aaty(1))/100.0
      dist=sqrt(xx**2+yy**2)/10.0
      idist=nint(dist)
      if(idist.ge.1.and.idist.le.9999) go to 106
      write(lunerr,19106) aatrec,aatnm,idist
      idist=9999
      errf=.true.
  106 xd=iabs(aatx(1)-aatx(2))
      yd=iabs(aaty(1)-aaty(2))
      if(aatway(1).ne.1) aatway(1)=2
      write(ndarr(aatrec),1090) aatnm,aatway(1),idist
      fcarr(aatrec)=aatfc(1)
c      if(aatfc(1).gt.9) go to 107
      do 1069 idir=1,2
      if(aatfc(1).ne.1) go to 1061
      if(aathov(1).eq.1.or.aathov(1).eq.4) fwyrmp(aatarn(idir))=.true.
 1061 if(aatyr(2).gt.0.and.aatyr(2).lt.upnode(aatarn(idir)))
     *upnode(aatarn(idir))=aatyr(2)
      if(aatyr(1).le.1990) go to 1068
      if(aatyr(1).eq.fynod1(aatarn(idir))) go to 1069
      if(aatyr(1).eq.fynod2(aatarn(idir))) go to 1069
      if(aatyr(1).lt.fynod1(aatarn(idir))) go to 1062
      if(aatyr(1).lt.fynod2(aatarn(idir))) go to 1063
      go to 1069
 1062 if(fynod1(aatarn(idir)).lt.fynod2(aatarn(idir)))
     *fynod2(aatarn(idir))=fynod1(aatarn(idir))
      fynod1(aatarn(idir))=aatyr(1)
      go to 1069
 1063 fynod2(aatarn(idir))=aatyr(1)
      go to 1069
 1068 exnode(aatarn(idir))=.true.
 1069 continue
  107 if(aatfc(1).ne.11) go to 108
      lrtn(aatarn(2))=.true.
      lrtn(aatarn(1))=.true.
  108 if(xd.gt.yd) ewdir(i,aatarn(1))=.true.
      if(aatway(1).eq.1) go to 111
      entry(aatarn(2))=.true.
      exit(aatarn(1))=.true.
      if(xd.gt.yd) ewdir(i,aatarn(2))=.true.
  111 if(.not.xramp(aatfc(1))) go to 128
      mramp=mramp+1
      if(mramp.lt.mxramp) go to 112
      write(lunerr,19111) mxramp
      errf=.true.
      go to 9999
  112 rmparr(aatrec)=mramp
      rmprec(mramp)=aatrec
      ix=index(aatnm,'ACCESS')
      if(ix.gt.0) rmpacc(mramp)=.true.
      go to 128
  120 zcnode(aatarn(2))=.true.
      zcnode(aatarn(1))=.true.
      izone=aattpn(2)
      if(aattpn(1).lt.aattpn(2)) go to 123
      izone=aattpn(2)
      go to 124
  123 izone=aattpn(1)
  124 if(izone.gt.9999) izone=9999
      write(ndarr(aatrec),1093) izone
      fcarr(aatrec)=aatfc(1)
      do 127 idir=1,2
      if(aatyr(1).le.1990) go to 127
      if(aatyr(1).lt.fyzon1(aatarn(idir))) go to 1261
      if(aatyr(1).lt.fyzon2(aatarn(idir))) go to 1262
      go to 127
 1261 if(fyzon1(aatarn(idir)).lt.fyzon2(aatarn(idir)))
     *fyzon2(aatarn(idir))=fyzon1(aatarn(idir))
      fyzon1(aatarn(idir))=aatyr(1)
      go to 127
 1262 fyzon2(aatarn(idir))=aatyr(1)
  127 continue
  128 aatrec=aatrec+1
      if(aatrec.le.mxtca) go to 100
      write(lunerr,19128) 
      errf=.true.
      go to 9999
  129 if(stop) go to 9999
      naat=aatrec-1
      write(0,11004) naat
      call otctrs
      if(secio.ne.0) go to 9000
  131 secrec=1
  132 call rtctrs
      if(secio.ne.0) go to 139
      if(alink.lt.1.or.alink.gt.mxtca) go to 9130
      lbrts(alink)=100
      secrec=secrec+1
      go to 132
  139 close(seclun)
      call otctus
      if(secio.ne.0) go to 9000
  141 secrec=1
  142 call rtctus
      if(secio.ne.0) go to 149
      if(alink.lt.1.or.alink.gt.mxtca) go to 9140
      ix=lbrts(alink)/100
      lbrts(alink)=ix*100+10
      secrec=secrec+1
      go to 142
  149 close(seclun)
      write(0,11005)
c
c     find cross-street names of ramps
c
      do 189 iramp=1,mramp
      krec=rmprec(iramp)
      n1=nodarr(2,krec)
      n2=nodarr(1,krec)
      do 151 i=1,8
      irec=xrec(i,n1)
      if(irec.eq.0) go to 152
      ifc=fcarr(irec)
      if(.not.xramp(ifc).and.ifc.ne.1) go to 156
      if(ifc.eq.1) go to 157
  151 continue
  152 do 155 i=1,8
      jrec=xrec(i,n1)
      if(jrec.lt.1) go to 160
      if(krec.eq.jrec) go to 155
      n11=nodarr(2,jrec)
      if(n11.ne.n1) go to 153
      n11=nodarr(1,jrec)
      if(n11.eq.n1) go to 155
  153 do 154 j=1,8
      irec=xrec(j,n11)
      if(irec.eq.0) go to 155
      ifc=fcarr(irec)
      if(.not.xramp(ifc).and.ifc.ne.1) go to 156
      if(ifc.eq.1) go to 156
  154 continue
  155 continue
      go to 160
  156 xnm(1,n1)=nmarr(irec)
      xnm(1,n2)=nmarr(irec)
      r2name(iramp)=nmarr(irec)
      go to 170
  157 xnm(1,n1)=nmarr(irec)
      xnm(1,n2)=nmarr(irec)
      r1name(iramp)=nmarr(irec)
      r1namx(iramp)=nmarr(irec)
      go to 180
  160 do 161 i=1,8
      irec=xrec(i,n2)
      if(irec.eq.0) go to 162
      ifc=fcarr(irec)
      if(.not.xramp(ifc).and.ifc.ne.1) go to 166
      if(ifc.eq.1) go to 167
  161 continue
  162 do 165 i=1,8
      jrec=xrec(i,n2)
      if(jrec.lt.1) go to 189
      if(krec.eq.jrec) go to 165
      n21=nodarr(2,jrec)
      if(n21.ne.n2) go to 163
      n21=nodarr(1,jrec)
      if(n21.eq.n2) go to 165
  163 do 164 j=1,8
      irec=xrec(j,n21)
      if(irec.eq.0) go to 165
      ifc=fcarr(irec)
      if(.not.xramp(ifc).and.ifc.ne.1) go to 166
      if(ifc.eq.1) go to 167
  164 continue
  165 continue
      go to 189
  166 xnm(1,n1)=nmarr(irec)
      xnm(1,n2)=nmarr(irec)
      r2name(iramp)=nmarr(irec)
      go to 189
  167 xnm(1,n1)=nmarr(irec)
      xnm(1,n2)=nmarr(irec)
      r1name(iramp)=nmarr(irec)
      r1namx(iramp)=nmarr(irec)
      go to 189
  170 do 171 i=1,8
      irec=xrec(i,n2)
      if(irec.eq.0) go to 172
      ifc=fcarr(irec)
      if(ifc.eq.1) go to 176
  171 continue
  172 do 175 i=1,8
      jrec=xrec(i,n2)
      if(jrec.lt.1) go to 189
      if(krec.eq.jrec) go to 175
      n21=nodarr(2,jrec)
      if(n21.ne.n2) go to 173
      n21=nodarr(1,jrec)
      if(n21.eq.n2) go to 175
  173 do 174 j=1,8
      irec=xrec(j,n21)
      if(irec.eq.0) go to 175
      ifc=fcarr(irec)
      if(ifc.eq.1) go to 176
  174 continue
  175 continue
      go to 189
  176 xnm(2,n1)=nmarr(irec)
      xnm(2,n2)=nmarr(irec)
      r1name(iramp)=nmarr(irec)
      r1namx(iramp)=nmarr(irec)
      go to 189
  180 do 181 i=1,8
      irec=xrec(i,n2)
      if(irec.eq.0) go to 182
      if(.not.xramp(ifc).and.ifc.ne.1) go to 186
  181 continue
  182 do 185 i=1,8
      jrec=xrec(i,n2)
      if(jrec.lt.1) go to 189
      if(krec.eq.jrec) go to 185
      n21=nodarr(2,jrec)
      if(n21.ne.n2) go to 183
      n21=nodarr(1,jrec)
      if(n21.eq.n2) go to 185
  183 do 184 j=1,8
      irec=xrec(j,n21)
      if(irec.eq.0) go to 189
      ifc=fcarr(irec)
      if(.not.xramp(ifc).and.ifc.ne.1) go to 186
  184 continue
  185 continue
      go to 189
  186 xnm(2,n1)=nmarr(irec)
      xnm(2,n2)=nmarr(irec)
      r2name(iramp)=nmarr(irec)
  189 continue
      do 191 iramp=i,mramp
      irec=rmprec(iramp)
      read(ndarr(irec),1090) aatnm,aatway(1),idist
       if(rmpacc(iramp)) then
        write(ndarr(irec),1092) aacc,r1name(iramp),aatway(1),idist
       else
        write(ndarr(irec),1092) aramp,r1name(iramp),aatway(1),idist
       endif
  191 continue
c
c     find regular cross street names
c
      do 299 inode=1,mnode
      if(fwyrmp(inode)) go to 299
c
c     first see if there are 2x2 or 2x1 names
c
      nxnm1=0
      nxnm2=0
      xnm1=noname 
      xnm2=noname 
      rampi=.false.
      iapp=0
      do 226 i=1,8
      if(.not.xex(i,inode)) go to 226
      irec=xrec(i,inode)
      if(irec.eq.0) go to 227
      iapp=iapp+1
      aatnm=nmarr(irec)
      iramp=rmparr(irec)
      if(iramp.lt.1) go to 2220
      write(aatnm,19222) r1namx(iramp)
      rampi=.true.
 2220 if(aatnm.eq.xnm1) go to 222
      if(aatnm.eq.xnm2) go to 223
      if(nxnm1.eq.0) go to 224
      if(nxnm2.eq.0) go to 225
  222 nxnm1=nxnm1+1
      go to 226
  223 nxnm2=nxnm2+1
      go to 226
  224 xnm1=aatnm
      nxnm1=1
      go to 226
  225 xnm2=aatnm
      nxnm2=1
  226 continue
  227 if(rampi) go to 229
      if(iapp.eq.3) go to 228
      if(nxnm1.ne.2.or.nxnm2.ne.2) go to 230
      xnm(1,inode)=xnm1
      xnm(2,inode)=xnm2
      go to 299
  228 if(nxnm1.eq.1.and.nxnm2.eq.2) go to 229
      if(nxnm2.eq.1.and.nxnm1.eq.2) go to 229
      go to 230
  229 xnm(1,inode)=xnm1
      xnm(2,inode)=xnm2
      go to 299
  230 nxnm1=0
      nxnm2=0
      xnm1=noname 
      xnm2=noname 
      rampi=.false.
      iapp=0
      do 236 i=1,8
      irec=xrec(i,inode)
      if(irec.eq.0) go to 237
      iapp=iapp+1
      aatnm=nmarr(irec)
      iramp=rmparr(irec)
      if(iramp.lt.1) go to 231
      write(aatnm,19222) r1namx(iramp)
      rampi=.true.
  231 if(aatnm.eq.xnm1) go to 232
      if(aatnm.eq.xnm2) go to 233
      if(nxnm1.eq.0) go to 234
      if(nxnm2.eq.0) go to 235
  232 nxnm1=nxnm1+1
      go to 236
  233 nxnm2=nxnm2+1 
      go to 236
  234 xnm1=aatnm
      nxnm1=1
      go to 236
  235 xnm2=aatnm
      nxnm2=1
  236 continue
  237 if(rampi) go to 239
      if(iapp.eq.3) go to 238
      if(nxnm1.ne.2.or.nxnm2.ne.2) go to 240
      xnm(1,inode)=xnm1
      xnm(2,inode)=xnm2
      go to 299
  238 if(nxnm1.eq.1.and.nxnm2.eq.2) go to 239
      if(nxnm2.eq.1.and.nxnm1.eq.2) go to 239
      go to 240
  239 xnm(1,inode)=xnm1
      xnm(2,inode)=xnm2
      go to 299
c
c     use more complicated procedures for odd-balls
c
  240 nns=0
      new=0
      do 241 i=1,4
      nsnm(i)=noname
      ewnm(i)=noname
  241 continue
      do 249 j=1,8
      irec=xrec(j,inode)
      if(irec.eq.0) go to 250
      aatnm=nmarr(irec)
      if(aatnm.eq.'      ') go to 249
      do 242 i=1,4
      if(aatnm.eq.nsnm(i)) go to 249
      if(aatnm.eq.ewnm(i)) go to 249
  242 continue
      if(ewdir(j,inode)) go to 246
      nns=nns+1
      nns=min(nns,4)
      nsnm(nns)=aatnm
      go to 249
  246 new=new+1
      new=min(new,4)
      ewnm(new)=aatnm
  249 continue
  250 if(nns.eq.1) go to 257
      if(nns.eq.2) go to 260
      if(nns.eq.3) go to 270
      if(nns.eq.4) go to 280
      xnm(1,inode)=ewnm(1)
      if(lname(inode).lt.'1') go to 256
      xnm(2,inode)=lname(inode)
      xnm(3,inode)=ewnm(2)

      xnm(4,inode)=ewnm(3)
      go to 299
  256 xnm(2,inode)=ewnm(2)
      xnm(3,inode)=ewnm(3)
      xnm(4,inode)=ewnm(4)
      if(zcnode(inode).and.xnm(2,inode).eq.'      ')
     *xnm(2,inode)='zone connector'
      go to 299
  257 xnm(1,inode)=nsnm(1)
      if(new.gt.0) go to 258
      if(lname(inode).lt.'1') go to 259
      xnm(2,inode)=lname(inode)
      go to 299
  258 xnm(2,inode)=ewnm(1)
      xnm(3,inode)=ewnm(2)
      xnm(4,inode)=ewnm(3)
      go to 299
  259 if(zcnode(inode)) xnm(2,inode)='zone connector'
      go to 299
  260 xnm(1,inode)=nsnm(1)
      if(new.gt.0) go to 261
      if(lname(inode).lt.'1') go to 262
      xnm(2,inode)=lname(inode)
      xnm(3,inode)=nsnm(2)

      go to 299
  261 xnm(2,inode)=ewnm(1)
      xnm(3,inode)=nsnm(2)
      xnm(4,inode)=ewnm(2)
      go to 299
  262 xnm(2,inode)=nsnm(2)
      go to 299
  270 xnm(1,inode)=nsnm(1)
      if(new.gt.0) go to 271
      if(lname(inode).lt.'1') go to 272
      xnm(2,inode)=lname(inode)
      xnm(3,inode)=nsnm(2)
      xnm(4,inode)=nsnm(3)
      go to 299
  271 xnm(2,inode)=ewnm(1)
      xnm(3,inode)=nsnm(2)
      xnm(4,inode)=nsnm(3)
      go to 299
  272 xnm(2,inode)=nsnm(2)
      xnm(3,inode)=nsnm(3)
      go to 299
  280 xnm(1,inode)=nsnm(1)
      if(lname(inode).lt.'1') go to 281
      xnm(2,inode)=lname(inode)
      xnm(3,inode)=nsnm(2)
      xnm(4,inode)=nsnm(3)
      go to 299
  281 xnm(2,inode)=nsnm(2)
      xnm(3,inode)=nsnm(3)
      xnm(4,inode)=nsnm(4)
  299 continue
c
c     sort records end-to-end by name.
c
      write(0,11006)
      call namsrt
      irt=1
      rtrec=1
      irec=0
      i1=1
      read(ndarr(1),1090) aatxnm(2),aatway(2),idist
c
c     find start and end of records with same name
c
  301 do 302 i4=i1,naat
      read(ndarr(i4),1090) aatxnm(1),aatway(1),idist
      if(aatxnm(2).ne.aatxnm(1)) go to 303
      if(aatway(2).ne.aatway(1)) go to 303
  302 continue
      i4=naat+1
  303 i4=i4-1
      idiff=i4-i1
      n11=nodarr(2,i1)
      n22=nodarr(1,i1)
      if(idiff.lt.1) go to 370
      if(aatway(2).eq.1) go to 330
c
c     find beginning of route ( node with no connecting links)
c
  310 do 317 i3=i1,i4
      j3=recarr(i3)
      if(j3.eq.0) go to 317
      ix=i3
      n11=nodarr(2,j3)
      n12=nodarr(1,j3)
      frend=.true.
      toend=.true.
      do 316 i5=i1,i4
      if(i3.eq.i5) go to 316
      j5=recarr(i5)
      if(j5.eq.0) go to 316
      n21=nodarr(2,j5)
      n22=nodarr(1,j5)
      if(.not.frend) go to 314
      if(n11.eq.n21) frend=.false.
      if(n11.eq.n22) frend=.false.
      go to 315
  314 if(.not.toend) go to 317
  315 if(n12.eq.n21) toend=.false.
      if(n12.eq.n22) toend=.false.

  316 continue

      if(frend) go to 318
      if(toend) go to 319
  317 continue

      write(0,11007) aatxnm(2)
      i3=ix
      j3=recarr(i3)
  318 recarr(i3)=0
      irec=irec+1
      rlkarr(irec)=irt*1000+rtrec
      srec(irec)=j3
      go to 320
  319 recarr(i3)=0
      irec=irec+1
      rlkarr(irec)=irt*1000+rtrec
      srec(irec)=j3
      n12=n11
c
c     find connecting links
c
  320 usedup=.true.
      do 321 i3=i1,i4
      j3=recarr(i3)
      if(j3.eq.0) go to 321
      usedup=.false.
      n21=nodarr(2,j3)
      n22=nodarr(1,j3)
      if(n21.eq.n12) go to 322
      if(n22.eq.n12) go to 323
  321 continue
      go to 329
  322 recarr(i3)=0
      irec=irec+1
      rtrec=rtrec+1
      rlkarr(irec)=irt*1000+rtrec
      srec(irec)=j3
      n12=n22
      go to 320
  323 recarr(i3)=0
      irec=irec+1
      rtrec=rtrec+1
      rlkarr(irec)=irt*1000+rtrec
      srec(irec)=j3
      n12=n21
      go to 320
  329 aatx(1)=nx(n11)
      aaty(1)=ny(n11)
      aatx(2)=nx(n22)
      aaty(2)=ny(n22)
      xd=iabs(aatx(1)-aatx(2))
      yd=iabs(aaty(1)-aaty(2))
      if(xd.gt.yd) go to 3292
      rtdir(irt)=1
      if(aaty(2).gt.aaty(1)) rtdir(irt)=3
      go to 3293
 3292 rtdir(irt)=2

      if(aatx(1).gt.aatx(2)) rtdir(irt)=4
 3293 irt=irt+1
      rtrec=1
      if(.not.usedup) go to 310
      i1=i4+1
      if(i1.gt.naat) go to 391
      j1=recarr(i1)
      aatfc(1)=fcarr(j1)
      if(aatfc(1).eq.10.or.aatfc(1).eq.14) go to 380
      read(ndarr(i1),1090) aatxnm(2),aatway(2),idist
      go to 301
c
c     one-way streets
c
  330 do 337 i3=i1,i4
      j3=recarr(i3)
      if(j3.eq.0) go to 337
      ix=i3
      n11=nodarr(2,j3)
      n12=nodarr(1,j3)
      do 336 i5=i1,i4
      if(i3.eq.i5) go to 336
      j5=recarr(i5)
      if(j5.eq.0) go to 336
      n21=nodarr(2,j5)
      n22=nodarr(1,j5)
      if(n11.eq.n22) go to 337
  336 continue
      go to 338
  337 continue
      write(0,11007) aatxnm(2)
      i3=ix
      j3=recarr(i3)
  338 recarr(i3)=0
      irec=irec+1
      rlkarr(irec)=irt*1000+rtrec






      srec(irec)=j3
c
c
c     find connecting links
c
  340 usedup=.true.
      do 341 i3=i1,i4
      j3=recarr(i3)
      if(j3.eq.0) go to 341
      usedup=.false.
      n21=nodarr(2,j3)

      n22=nodarr(1,j3)
      if(n21.eq.n12) go to 342
  341 continue
      go to 349
  342 recarr(i3)=0
      irec=irec+1
      rtrec=rtrec+1
      rlkarr(irec)=irt*1000+rtrec
      srec(irec)=j3
      n12=n22
      go to 340
  349 aatx(1)=nx(n11)
      aaty(1)=ny(n11)
      aatx(2)=nx(n22)
      aaty(2)=ny(n22)
      xd=iabs(aatx(1)-aatx(2))
      yd=iabs(aaty(1)-aaty(2))
      if(xd.gt.yd) go to 3492
      rtdir(irt)=1
      if(aaty(2).gt.aaty(1)) rtdir(irt)=3
      go to 3493
 3492 rtdir(irt)=2
      if(aatx(1).gt.aatx(2)) rtdir(irt)=4
 3493 irt=irt+1
      rtrec=1
      if(.not.usedup) go to 330
      i1=i4+1
      if(i1.gt.naat) go to 391
      j1=recarr(i1)
      aatfc(1)=fcarr(j1)
      if(aatfc(1).eq.10.or.aatfc(1).eq.14) go to 380
      read(ndarr(i1),1090) aatxnm(2),aatway(2),idist
      go to 301
  370 irec=irec+1
      rlkarr(irec)=irt*1000+rtrec
      srec(irec)=recarr(i4)
      recarr(i4)=0
      aatx(1)=nx(n11)
      aaty(1)=ny(n11)
      aatx(2)=nx(n22)
      aaty(2)=ny(n22)
      xd=iabs(aatx(1)-aatx(2))
      yd=iabs(aaty(1)-aaty(2))
      if(xd.gt.yd) go to 3793
 3792 rtdir(irt)=2
      if(aatx(1).gt.aatx(2)) rtdir(irt)=4
 3793 irt=irt+1
      rtrec=1
      i1=i4+1
      if(i1.gt.naat) go to 391
      j1=recarr(i1)
      aatfc(1)=fcarr(j1)
      if(aatfc(1).eq.10.or.aatfc(1).eq.14) go to 380
      read(ndarr(i1),1090) aatxnm(2),aatway(2),idist
      go to 301
  380 do 382 i3=i1,naat
      j3=recarr(i3)
      aatfc(1)=fcarr(j3)
      if(aatfc(1).ne.10.and.aatfc(1).ne.14) go to 383
      rtrec=rtrec+1
      if(rtrec.lt.1000) go to 381
      rtdir(irt)=1
      irt=irt+1
      rtrec=1
  381 irec=irec+1
      rlkarr(irec)=irt*1000+rtrec
      srec(irec)=recarr(i3)
      recarr(i3)=0


  382 continue
      go to 391

  383 rtdir(irt)=1
      irt=irt+1
      rtrec=1
      i1=i3+1
      if(i1.gt.naat) go to 391
      read(ndarr(i1),1090) aatxnm(2),aatway(2),idist
      go to 301
  391 call recsrt
      stop=.false.
      do 392 i=1,naat
      if(recarr(i).eq.0) go to 392
      write(0,19391) i
      stop=.true.
  392 continue
      if(stop) go to 9999
      write(0,11008)
      call otcn
      if(natio.ne.0) go to 9000
      natrec=1
  401 call rtcn
      if(natio.ne.0) go to 408
      if(natlb.lt.1.or.natlb.gt.mnode) go to 402
      if(nattpn.lt.1.or.nattpn.gt.mxtpn) go to 402
      tpused(nattpn)=.true.
      tpxy(nattpn)=nx(natlb)+ny(natlb)
  402 natrec=natrec+1
      go to 401
  408 natrec=1
      inode=5000 
      print *,fnamo
c
c     read node point attribute table saving arrays of node data
c     for node
c
  409 call rtcn
      if(natio.ne.0) go to 499
      if(natlb.gt.0.and.natlb.le.mnode) go to 410
      natid=natlb
      write(lunerr,19409) natrec,natlb,natxnm(1),natxnm(2)
      errf=.true.
      go to 9999
  410 if(natlb.ne.natrec) go to 499     
      if (natprj(1).eq.0.and.lkiyr(natlb).ne.natyr(1)) then
      !if node iyear is 0 or or 2099, 9999, use link iyr 
      if (natyr(1).eq.0.or.natyr(1).gt.2050) natyr(1)=lkiyr(natlb)
      endif
      !if link yr is controled by project, assign link proj to node proj
      if (lkiyr(natlb).eq.natyr(1).and.natyr(1).eq.2099)then
      natprj(1)=lkiproj(natlb)
      endif
      if (lkiproj(natlb).ne.natprj(1)
     *.and.natprj(1).gt.0.and.lkiproj(natlb).gt.0)then
      write(luninf,19421) natlb,natprj(1),lkiproj(natlb)
      endif
      do 419 iyr=1,2
      iproj=natprj(iyr)
      if(iproj.lt.1) go to 419
      if(iproj.le.mproj) go to 411
      write(luninf,19420) natrec,natxnm(1),natxnm(2),iproj
      inff=.true.
      nattmp=1
      go to 419
  411 if(pyr(iproj).ge.1990.and.pyr(iproj).le.2099) go to 412
      write(luninf,19420) natrec,natxnm(1),natxnm(2),iproj
      inff=.true.
      nattmp=1
  412 natyr(iyr)=pyr(iproj)
      ulproj(iproj)=.true.
  419 continue
      if(exnode(natlb)) go to 421
      if(natyr(1).gt.0) natyr(1)=fynod1(natlb)
      go to 422
  421 natyr(1)=1990
  422 if(natsph.ne.8.and.natsph.ne.9)
     *natsph=nsph(natlb)
      natfz4=nfz4(natlb)
      if(zcent(natlb)) nattpn=nzone(natlb)
      if(natcnt(1).ne.8) go to 431
      natyr(2)=0
      natprj(2)=0
      natcnt(2)=0
      natjur(2)=0
  431 if(nattpn.lt.1.or.nattpn.gt.mxtpn) go to 435

      jx=nx(natlb)
      jy=ny(natlb)
      jjxy=jx+jy
      if(zcent(natlb)) go to 432
      if(tpxy(nattpn).gt.0.and.tpxy(nattpn).ne.jjxy) go to 435
  432 tpused(nattpn)=.true.
      tpxy(nattpn)=jjxy
      lbtp(natlb)=nattpn
      go to 439
  435 do 436 inode=lnode,mxtpn
      if(.not.tpused(inode)) go to 437
  436 continue
      go to 9436
  437 nattpn=inode
      lnode=inode+1
      jjxy=xy(natlb)
      tpused(nattpn)=.true.
      tpxy(nattpn)=jjxy
      lbtp(natlb)=nattpn
  439 if(natnty.eq.2) go to 443
      if(.not.lrtn(natlb)) go to 440
      lrtxnm(natlb)=natxnm(2)
      go to 444
  440 if(xnm(2,natlb).ne.noname) go to 442
      if(natxnm(1).eq.xnm(1,natlb)) go to 441
      natxnm(1)=xnm(1,natlb)
      go to 444
  441 natxnm(2)=xnm(1,natlb)
      go to 444
  442 natxnm(2)=xnm(2,natlb)
      natxnm(1)=xnm(1,natlb)
      go to 444
  443 xnm(1,natlb)=natxnm(1)
      xnm(2,natlb)=natxnm(2)
      if(.not.lrtn(natlb)) go to 444
      lrtxnm(natlb)=natxnm(2)
  444 natiuc=(natcnt(1)+1)*10+natcnt(2)+1
      if(natcnt(2).lt.1) go to 447
      if(natyr(2).gt.0.and.natyr(2).lt.2099) go to 447
      if(fynod1(natlb).eq.natyr(1).or.
     *fynod2(natlb).eq.natyr(1)) go to 447		
      if(fynod1(natlb).gt.natyr(1).and.fynod1(natlb).lt.9999) go to 445		
      if(fynod2(natlb).gt.natyr(1).and.fynod2(natlb).lt.9999) go to 4451
      if(upnode(natlb).gt.natyr(1).and.upnode(natlb).lt.9999) go to 446
      natyr(2)=2099
      go to 447
  445 natyr(2)=fynod1(natlb)
      go to 447
 4451 natyr(2)=fynod2(natlb)
      go to 447
  446 natyr(2)=upnode(natlb)
  447 natx=nx(natlb)
      naty=ny(natlb)
      natz=nz(natlb)
      cnnode(1,natlb)=natcnt(1)
      cnnode(2,natlb)=natcnt(2)
      if(natcnt(1).eq.4.or.natcnt(2).eq.4) mtrnod(natlb)=.true.
      if(natcnt(1).eq.5.or.natcnt(2).eq.5) mtrnod(natlb)=.true.
      if(xapp(natlb).lt.5) go to 448
      write(luninf,19447) natrec,natxnm(1),natxnm(2)
      inff=.true.
      nattmp=2
  448 if(natcnt(1).ne.3) go to 449
      idiff=xapp(natlb)-xstop(natlb)
      if(idiff.lt.3) go to 449
      write(luninf,19448) natrec,natxnm(1),natxnm(2)
      inff=.true.
      nattmp=3
  449 natfz6=nzone(natlb)
      bordnd=index(natxnm(2),'CAMIONES')
      if (bordnd.gt.0)nattt(1)=6 !border transit occur
      call wtcn
      natrec=natrec+1
      go to 409
  499 close(natlun)
      write(0,11010)
      iaat=1
      iline=99
      ipage=1
  500 irtno=rlkarr(iaat)/1000
      ilkno=rlkarr(iaat)-irtno*1000
      aatrec=srec(iaat)
      krec=0
c
c     read tcov aat final time to insert/check data
c
  501 call rtca
      if(aatio.ne.0) go to 9000
      if(aatarn(2).lt.1.or.aatarn(2).gt.mxtcn) aatarn(2)=mxtcn
      if(aatarn(1).lt.1.or.aatarn(1).gt.mxtcn) aatarn(1)=mxtcn
      if(aatid.lt.1) go to 502
      if(nid(aatid).lt.2) go to 505
  502 do 503 i=1,mxid
      if(nid(i).eq.0) go to 504
  503 continue
      go to 9502
  504 if(nid(aatid).gt.1) nid(aatid)=nid(aatid)-1
      aatid=i
      nid(aatid)=nid(aatid)+1
  505 if(aatfc(1).ne.1) aatcl=nzone(aatarn(1))
      aattmp(1)=0
      aattmp(2)=0
      aatsph=nsph(aatarn(2))
      if(nsph(aatarn(1)).eq.1404) aatsph=nsph(aatarn(1))
      if(aatfc(1).eq.10.or.aatfc(1).eq.14) go to 509
      if(.not.entry(aatarn(2))) go to 508
      if(.not.exit(aatarn(2))) go to 508
      if(.not.entry(aatarn(1))) go to 508
      if(.not.exit(aatarn(1))) go to 508
      go to 509
  508 write(lunerr,19508)
     *aatid,aatnm,aatxnm(2),aatxnm(1),aatarn(2),aatarn(1)
      aattmp(1)=1
      errf=.true.
  509 aatx(2)=nx(aatarn(2))
      aatx(1)=nx(aatarn(1))
      aaty(2)=ny(aatarn(2))
      aaty(1)=ny(aatarn(1))
      aatrt=irtno
      aatlk=ilkno
      aatrts=lbrts(aatlb)
      if(aatfc(1).ge.1.and.aatfc(1).le.mxfc) go to 511
      aattmp(1)=2
      write(lunerr,19510) aatid,aatnm,aatxnm(2),aatxnm(1),aatfc(1)
      errf=.true.
      stop=.true.
      go to 940
  511 if(aatfc(1).eq.1) go to 517
      if(aatfc(1).ne.11) go to 512
      aatxnm(2)=lrtxnm(aatarn(2))
      aatxnm(1)=lrtxnm(aatarn(1))
      go to 517
  512 do 513 i=1,4
      if(aatnm.ne.xnm(i,aatarn(2))) go to 514
  513 continue
      i=4
  514 aatxnm(2)=xnm(i,aatarn(2))
      do 515 i=1,4
      if(aatnm.ne.xnm(i,aatarn(1))) go to 516
  515 continue
      i=4
  516 aatxnm(1)=xnm(i,aatarn(1))
  517 aattpn(2)=lbtp(aatarn(2))
      aattpn(1)=lbtp(aatarn(1))
      aatvol=0
      if(aatadt.gt.0.and.aatadt.le.mxradt) aatvol=adthst(aatadt)
      if(aatsta.lt.1.or.aatsta.gt.9999) go to 518
      if(aatcj.lt.1.or.aatcj.gt.mxcj) go to 518
      icsta=aatcj*10000+aatsta
      if(aatsta.lt.1.or.aatsta.gt.mxcsta) go to 518
      if(aatfc(1).eq.1) go to 518
c      aatvol=stahst(icsta)
  518 xd=aatx(1)-aatx(2)
      yd=aaty(1)-aaty(2)
      ixd=iabs(xd)
      iyd=iabs(yd)
      if(ixd.gt.iyd) go to 519
      aatdir=1
      if(aaty(1).lt.aaty(2)) aatdir=3
      go to 520
  519 aatdir=2
      if(aatx(1).gt.aatx(2)) aatdir=4
      if(aatyr(1).lt.1950) aatyr(1)=1990
  520 do 523 iyr=1,mxtcyr
      iproj=aatprj(iyr)
      if(iproj.lt.1) go to 523
      if(iproj.le.mproj) go to 521
      write(luninf,19520) aatid,aatnm,aatxnm(2),aatxnm(1),iproj
      aattmp(1)=4
      inff=.true.
      if(aatyr(iyr).lt.1990) aatyr(iyr)=1990
      go to 523
  521 if(pyr(iproj).ge.1990.and.pyr(iproj).le.2099) go to 522
      write(luninf,19520) aatrec,aatnm,aatxnm(2),aatxnm(1),iproj
      aattmp(1)=4
      inff=.true.
      if(aatyr(iyr).lt.1990) aatyr(iyr)=1990
      go to 523

  522 aatyr(iyr)=pyr(iproj)
      ulproj(iproj)=.true.
  523 continue
      if(aatfc(1).eq.10.or.aatfc(1).eq.14) go to 580
c
c     check for duplicate arcs
c
      if(aatfc(1).gt.9) go to 5269
      inode=min(aattpn(1),aattpn(2))
      jnode=max(aattpn(1),aattpn(2))
      do 524 i=1,4
      if(xnode(i,inode).eq.jnode) go to 526
      if(xnode(i,inode).eq.0) go to 525
  524 continue
      i=4
  525 xnode(i,inode)=jnode
      xiyr(i,inode)=aatyr(1)
      xdyr(i,inode)=aatdyr
      go to 5269
c
c     duplicate arc found, is this a repalcement arc
c
  526 if(aatdyr.gt.0) go to 5261
      if(xdyr(i,inode).gt.0) go to 5262
      write(luninf,19526)
     *aatrec,aatnm,aatxnm(2),aatxnm(1),aatarn(1),aatarn(2)
      aattmp(1)=5
      inff=.true.
      go to 5269
 5261 if(aatdyr.le.xiyr(i,inode)) go to 5269
      write(luninf,19526)
     *aatrec,aatnm,aatxnm(2),aatxnm(1),aatarn(1),aatarn(2)
      aattmp(1)=5
      inff=.true.
      go to 5269
 5262 if(xiyr(i,inode).le.aatdyr) go to 5269
      write(luninf,19526)
     *aatrec,aatnm,aatxnm(2),aatxnm(1),aatarn(1),aatarn(2)
      aattmp(1)=5
      inff=.true.
      go to 5269
c
c     fill in missing initial non-directional data with defaults.
c
 5269 if(aatjur(1).lt.1.or.aatjur(1).gt.6) aatjur(1)=mjur(aatfc(1))
      if(aathov(1).lt.1.or.aathov(1).gt.4) aathov(1)=1
      if(aattrk(1).lt.1.or.aattrk(1).gt.7) aattrk(1)=1
      if(aatspd(1).lt.1.or.aatspd(1).gt.75) aatspd(1)=mspd(aatfc(1))
      if(aattspd(1).lt.1.or.aattspd(1).gt.75) aattspd(1)=mtspd(aatfc(1))
      if(aatway(1).lt.1.or.aatway(1).gt.2) aatway(1)=mway(aatfc(1))
      if(aatmed(1).lt.1.or.aatmed(1).gt.3) aatmed(1)=mmed(aatfc(1))
c
c     fill in missing upgrade and final non-directional data with defaults.
c
      do 529 iyr=2,mxtcyr
      if(aatyr(iyr).lt.1950.or.aatyr(iyr).gt.2099) go to 528
      jyr=iyr-1
      if(aatfc(iyr).lt.1.or.aatfc(iyr).gt.mxfc) aatfc(iyr)=aatfc(jyr)
      if(aatfc(iyr).ne.aatfc(jyr)) go to 527
      if(aatjur(iyr).lt.1.or.aatjur(iyr).gt.6) aatjur(iyr)=aatjur(jyr)
      if(aathov(iyr).lt.1.or.aathov(iyr).gt.4) aathov(iyr)=aathov(jyr)
      if(aattrk(iyr).lt.1.or.aattrk(iyr).gt.4) aattrk(iyr)=aattrk(jyr)
      if(aatspd(iyr).lt.1.or.aatspd(iyr).gt.75) aatspd(iyr)=aatspd(jyr)
      if(aattspd(iyr).lt.1.or.aattspd(iyr).gt.75) 
     *  aattspd(iyr)=aattspd(jyr)
      if(aatway(iyr).lt.1.or.aatway(iyr).gt.2) aatway(iyr)=aatway(jyr)
      if(aatmed(iyr).lt.1.or.aatmed(iyr).gt.3) aatmed(iyr)=aatmed(jyr)
      go to 529
  527 if(aatjur(iyr).lt.1.or.aatjur(iyr).gt.6)
     *aatjur(iyr)=mjur(aatfc(iyr))
      if(aathov(iyr).lt.1.or.aathov(iyr).gt.4)
     *aathov(iyr)=aathov(jyr)
      if(aattrk(iyr).lt.1.or.aattrk(iyr).gt.4)
     *aattrk(iyr)=aattrk(jyr)
      if(aatspd(iyr).lt.1.or.aatspd(iyr).gt.75)
     *aatspd(iyr)=mspd(aatfc(iyr))
      if(aattspd(iyr).lt.1.or.aattspd(iyr).gt.75)
     *aattspd(iyr)=mtspd(aatfc(iyr))
      if(aatway(iyr).lt.1.or.aatway(iyr).gt.2)
     *aatway(iyr)=aatway(jyr)
      if(aatmed(iyr).lt.1.or.aatmed(iyr).gt.3)
     *aatmed(iyr)=mmed(aatfc(iyr))
      go to 529
  528 aatyr(iyr)=0
      aatjur(iyr)=0
      aatfc(iyr)=0
      aathov(iyr)=0
      aattrk(iyr)=0
      aatspd(iyr)=0
      aattspd(iyr)=0
      aatway(iyr)=0
      aatmed(iyr)=0
      aattoll(1,iyr)=0
      aattoll(2,iyr)=0
      aattoll(3,iyr)=0
  529 continue
c
c     fill in missing initial directional data with defaults. 
c
      x=aatlen*60.
      jspd=aatspd(1)
      if(aatfc(1).eq.11) jspd=45
      y=float(jspd)*5280.
      xtime=x/y
      imp=9999
      do 539 idir=1,2
      if(idir.eq.2.and.aatway(1).eq.1) go to 537
      aattm(1,idir)=xtime
      aattm(2,idir)=xtime
      aattm(3,idir)=xtime
      do 531 ipk=1,3
      aatmln(idir,1)=aatln(ipk,idir,1)
      if(aatmln(idir,1).gt.0.and.aatmln(idir,1).lt.9) go to 533
  531 continue
      if(idir.eq.2) go to 532
      aatmln(idir,1)=mln(aatfc(1))
      go to 533
  532 aatmln(2,1)=aatmln(1,1)
  533 do 535 ipk=1,3
      if(aatln(ipk,idir,1).ge.1.and.aatln(ipk,idir,1).le.9) go to 535
      aatln(ipk,idir,1)=aatmln(idir,1)
  535 continue
      if(aataux(idir,1).gt.9) aataux(1,1)=0
c
c     determine control at aatarn(1) from node data
c
      if(.not.fwyrmp(aatarn(1))) go to 5350
      if(aatfc(1).ne.9) go to 5350
      if(aathov(1).eq.2.or.aathov(1).eq.3) go to 5350
      if(.not.mtrnod(aatarn(2))) aattmp(1)=9
 5350 icnt=cnnode(1,aatarn(idir))
      if(icnt.eq.4.and.aatfc(1).eq.1) icnt=0
      if(icnt.eq.5.and.aatfc(1).eq.1) icnt=0
      if(icnt.eq.3) go to 5353
      aatcnt(idir,1)=icnt
      if(aatcnt(idir,1).gt.8) aatcnt(idir,1)=0
      go to 536
 5353 if(aatcnt(idir,1).ne.3) aatcnt(idir,1)=0
  536 if(aatcnt(idir,1).lt.1.or.aatcnt(idir,1).gt.7) go to 539
      aattm(1,idir)=aattm(1,idir)+0.17
      aattm(2,idir)=aattm(2,idir)+0.17
      aattm(3,idir)=aattm(3,idir)+0.17
      if(aattl(idir,1).lt.1.or.aattl(idir,1).gt.9)
     *aattl(idir,1)=aatmln(idir,1)
      if(aatrl(idir,1).lt.1.or.aatrl(idir,1).gt.9) aatrl(idir,1)=9
      if(aatll(idir,1).lt.1) go to 5360
      if(aatll(idir,1).gt.9)
     *aatll(idir,1)=mlf(aatfc(1),aatcnt(idir,1))
      go to 539

 5360 if(xdapp(aatarn(idir)).gt.3) go to 5361
      aatll(idir,1)=8
      go to 539
 5361 aatll(idir,1)=mlf(aatfc(1),aatcnt(idir,1))
      go to 539
  537 aattm(1,idir)=999.9
      aattm(2,idir)=999.9
      aattm(3,idir)=999.9
      aatln(1,idir,1)=0
      aatln(2,idir,1)=0
      aatln(3,idir,1)=0
      aataux(idir,1)=0
      aatpct(idir)=0
      aatphf(idir)=0
      aatcnt(idir,1)=0
  539 continue
c
c     fill in missing upgrade/final directional data with defaults.
c
      do 549 iyr=2,mxtcyr
      do 548 idir=1,2
      if(aatyr(iyr).lt.1) go to 545
      if(idir.eq.2.and.aatway(iyr).eq.1) go to 545
      icnt=cnnode(1,aatarn(idir))
      jcnt=cnnode(2,aatarn(idir))
      if(icnt.eq.4.and.aatfc(1).eq.1) icnt=0
      if(icnt.eq.5.and.aatfc(1).eq.1) icnt=0
      if(jcnt.eq.4.and.aatfc(1).eq.1) jcnt=0
      if(jcnt.eq.5.and.aatfc(1).eq.1) jcnt=0
      if(aatcnt(idir,iyr).eq.icnt.or.aatcnt(idir,iyr).eq.jcnt)
     *go to 5409
      if(icnt.eq.3) go to 5403
      if(jcnt.eq.3) go to 5403
      aatcnt(idir,iyr)=jcnt
      go to 5409
 5403 if(aatcnt(idir,iyr).ne.3) aatcnt(idir,iyr)=0
 5409 jyr=iyr-1
      do 541 ipk=1,3
      aatmln(idir,iyr)=aatln(ipk,idir,iyr)
      if(aatmln(idir,iyr).gt.0.and.aatmln(idir,iyr).lt.9) go to 543
  541 continue
      if(aatfc(iyr).eq.aatfc(jyr)) go to 542
      aatmln(idir,iyr)=mln(aatfc(iyr))
      go to 543
  542 aatmln(idir,iyr)=aatmln(idir,jyr)
  543 do 544 ipk=1,3
      if(aatln(ipk,idir,iyr).ge.1.and.
     *aatln(ipk,idir,iyr).le.9) go to 544
      aatln(ipk,idir,iyr)=aatmln(idir,iyr)
  544 continue
      if(aataux(1,iyr).gt.0.and.aataux(1,iyr).lt.10) go to 5441
      if(aataux(1,iyr).ne.aataux(1,jyr)) then
       write(luninf,19521) aatid,aatnm,aatxnm(2),aatxnm(1),aatprj(iyr)
      end if
 5441 if(aatcnt(1,iyr).lt.1.or.aatcnt(1,iyr).gt.8)
     *aatcnt(1,iyr)=aatcnt(1,jyr)
      if(aatcnt(1,iyr).lt.1.or.aatcnt(1,iyr).gt.7) go to 548
      if(aatrl(1,iyr).lt.1.or.aatrl(1,iyr).gt.9)
     *aatrl(1,iyr)=aatrl(1,jyr)
      if(aatll(1,iyr).lt.1.or.aatll(1,iyr).gt.9)
     *aatll(1,iyr)=aatll(1,jyr)
      iln=aatmln(idir,iyr)-aatmln(idir,jyr)
      if(aattl(1,iyr).lt.1.or.aattl(1,iyr).gt.9)
     *aattl(1,iyr)=aattl(1,jyr)+iln
      go to 548
  545 aatln(1,idir,iyr)=0
      aatln(2,idir,iyr)=0
      aatln(3,idir,iyr)=0
      aataux(idir,iyr)=0
      aatcnt(idir,iyr)=0
  548 continue
  549 continue
c     check to see if anything changes between i,u,x,y,z
c     if not, zero out future project
      do 554 iyr=2,mxtcyr
      jyr=iyr-1
      if(aatfc(iyr).ne.aatfc(jyr)) go to 554
      if(aatjur(iyr).ne.aatjur(jyr)) go to 554
      if(aathov(iyr).ne.aathov(jyr)) go to 554
      if(aattrk(iyr).ne.aattrk(jyr)) go to 554
      if(aatspd(iyr).ne.aatspd(jyr)) go to 554
      if(aattspd(iyr).ne.aattspd(jyr)) go to 554
      if(aatway(iyr).ne.aatway(jyr)) go to 554
      if(aatmed(iyr).ne.aatmed(jyr)) go to 554
      if(aattoll(1,iyr).ne.aattoll(1,jyr)) go to 554
      if(aattoll(2,iyr).ne.aattoll(2,jyr)) go to 554
      if(aattoll(3,iyr).ne.aattoll(3,jyr)) go to 554
      do 551 idir=1,2
      if(aatln(1,idir,iyr).ne.aatln(1,idir,jyr)) go to 554
      if(aatln(2,idir,iyr).ne.aatln(2,idir,jyr)) go to 554
      if(aatln(3,idir,iyr).ne.aatln(3,idir,jyr)) go to 554
      if(aataux(idir,iyr).ne.aataux(idir,jyr)) go to 554
      if(aatcnt(idir,iyr).ne.aatcnt(idir,jyr)) go to 554
      if(aattl(idir,iyr).ne.aattl(idir,jyr)) go to 554
      if(aatrl(idir,iyr).ne.aatrl(idir,jyr)) go to 554
      if(aatll(idir,iyr).ne.aatll(idir,jyr)) go to 554
  551 continue
      aatyr(iyr)=0
      aatprj(iyr)=0
      aatjur(iyr)=0
      aatfc(iyr)=0
      aathov(iyr)=0
      aattrk(iyr)=0
      aatspd(iyr)=0
      aattspd(iyr)=0
      aatway(iyr)=0
      aatmed(iyr)=0
      aattoll(1,iyr)=0
      aattoll(2,iyr)=0
      aattoll(3,iyr)=0
      do 553 idir=1,2
      aatln(1,idir,iyr)=0
      aatln(2,idir,iyr)=0
      aatln(3,idir,iyr)=0
      aataux(idir,iyr)=0
      aatcnt(idir,iyr)=0
      aattl(idir,iyr)=0
      aatrl(idir,iyr)=0
      aatll(idir,iyr)=0
  553 continue
  554 continue
      myr=mxtcyr-1
      do iyr=2,myr
       if(aatyr(iyr).lt.1) then !if an intermediate year is zero
        iiyr=iyr+1
        do jyr=iiyr,mxtcyr !check to see if higher year exists
         if(aatyr(jyr).gt.0) then !if so, move higher year to lower year
          aatyr(iyr)=aatyr(jyr)
          aatprj(iyr)=aatprj(jyr)
          aatjur(iyr)=aatjur(jyr)
          aatfc(iyr)=aatfc(jyr)
          aathov(iyr)=aathov(jyr)
          aattrk(iyr)=aattrk(jyr)
          aatspd(iyr)=aatspd(jyr)
          aattspd(iyr)=aattspd(jyr)
          aatway(iyr)=aatway(jyr)
          aatmed(iyr)=aatmed(jyr)
          aattoll(1,iyr)=aattoll(1,jyr)
          aattoll(2,iyr)=aattoll(2,jyr)
          aattoll(3,iyr)=aattoll(3,jyr)
           do idir=1,2
            do iiper=1,3
            aatln(iiper,idir,iyr)=aatln(iiper,idir,jyr)            
            end do  
            aataux(idir,iyr)=aataux(idir,jyr)
            aatcnt(idir,iyr)=aatcnt(idir,jyr)
            aattl(idir,iyr)=aattl(idir,jyr)
            aatrl(idir,iyr)=aatrl(idir,jyr)
            aatll(idir,iyr)=aatll(idir,jyr)
           end do
          aatyr(jyr)=0
          aatprj(jyr)=0
          aatjur(jyr)=0
          aatfc(jyr)=0
          aathov(jyr)=0
          aattrk(jyr)=0
          aatspd(jyr)=0
          aattspd(jyr)=0
          aatway(jyr)=0
          aatmed(jyr)=0
          aattoll(1,jyr)=0
          aattoll(2,jyr)=0
          aattoll(3,jyr)=0
           do idir=1,2
            aatln(1,idir,jyr)=0
            aatln(2,idir,jyr)=0
            aatln(3,idir,jyr)=0
            aataux(idir,jyr)=0
            aatcnt(idir,jyr)=0
            aattl(idir,jyr)=0
            aatrl(idir,jyr)=0
            aatll(idir,jyr)=0
           end do
         endif
        end do
       endif
      end do
      go to 590
  580 aatnm='zone connector'
      in=aattpn(2)
      iin=aatarn(2)
      jn=aatarn(1)
      if(aattpn(2).lt.5000) go to 581
      if(aattpn(1).lt.5000) go to 582
      aattmp(1)=6
      write(lunerr,19580) aatid,aatarn(2),aatarn(1),in
      errf=.true.
      go to 583
  581 if(aattpn(1).ge.5000) go to 583
      aattmp(1)=7
      write(lunerr,19581) aatrec,aatarn(2),aatarn(1),in
      errf=.true.
      go to 583
  582 in=aattpn(1)
      iin=aatarn(1)
      jn=aatarn(2)
      aattmp(2)=1
  583 write(aatxnm(2),1091) in
      aatxnm(1)=xnm(1,jn)
      if(aatyr(1).le.1990) go to 584
      if(aatyr(1).ne.fynod1(jn).and.fynod1(jn).ne.9999)
     *aatyr(1)=fynod1(jn)
      go to 585
  584 if(exnode(jn)) go to 585
      aatyr(1)=fynod1(jn)
      if(aatyr(1).eq.9999) aatyr(1)=2099
      go to 586
  585 if(aatyr(1).lt.1985) aatyr(1)=1990
  586 if(aatdyr.lt.aatyr(1)) aatdyr=0
c     if(aatdyr.eq.2050.and.fyzon2(iin).lt.2050)
c    *aatdyr=fyzon2(iin)
      aatjur(1)=5
      aatway(1)=2
      if(aatspd(1).lt.1.or.aatspd(1).gt.99) aatspd(1)=20
      if(aattspd(1).lt.1.or.aattspd(1).gt.99) aattspd(1)=20
      aatmed(1)=1
      do 587 idir=1,2
      aataux(idir,1)=0
      aatln(1,idir,1)=1
      aatln(2,idir,1)=1
      aatln(3,idir,1)=1
      aatcnt(idir,1)=0
      aattl(idir,1)=0
      aatrl(idir,1)=0
      aatll(idir,1)=0
  587 continue
      do 589 iyr=2,mxtcyr
      aatyr(iyr)=0
      aatprj(iyr)=0
      aatjur(iyr)=0
      aatfc(iyr)=0
      aathov(iyr)=0
      aattrk(iyr)=0
      aatspd(iyr)=0
      aattspd(iyr)=0
      aatway(iyr)=0
      aatmed(iyr)=0
      aattoll(1,iyr)=0
      aattoll(2,iyr)=0
      aattoll(3,iyr)=0
      do 588 idir=1,2
      aatln(1,idir,iyr)=0
      aatln(2,idir,iyr)=0
      aatln(3,idir,iyr)=0
      aataux(idir,iyr)=0
      aatcnt(idir,iyr)=0
      aattl(idir,iyr)=0
      aatrl(idir,iyr)=0
      aatll(idir,iyr)=0
  588 continue
  589 continue
      if(xrec(1,jn).gt.0) go to 590
      write(lunerr,19589) aatid,aatarn(2),aatarn(1),in
      errf=.true.
      aattmp(1)=8
c
c     write out record.
c
  590 ista=aatcj*10000+aatsta
      aatloc=0
      if(ista.lt.1.or.ista.gt.mxcsta) go to 599
      if(lbsta(aatid).gt.0) then
       if(aatfc(1).eq.1.and.iyear.ne.2000) then !use rloop field for sr11 (year ne 2000) freeways
        if(lbsta(aatid).eq.aatlp) then
         aatloc=1
        else
         if(lbsta(aatid).ne.aatsta) then
          write(lunerr,19592) aatid,aatcj,aatlp,lbsta(aatid)
          errf=.true.
         endif
        end if
       else
        if(lbsta(aatid).eq.aatsta) then
         aatloc=1
        else
         write(lunerr,19591) aatid,aatcj,aatsta,lbsta(aatid)
         errf=.true.
        end if
       end if
      usta(ista)=.false. !turn off station once match is found
      end if
  599 if(aatfc(1).ne.1) then
       iz1=nzone(aatarn(1))
       iz2=nzone(aatarn(2))
       iz1=max(iz1,1)
       iz2=max(iz2,1)
       if(rurzn(iz1).and.rurzn(iz2)) then
       aatplc(1)=950
       aatplc(2)=950
       else 
       aatplc(1)=0
       aatplc(2)=0
       endif
       endif
c check to see if lane discrepancies exists, report aatid, phase
      
      if (aathov(1).eq.1)then !hov=1
      do iyr=1,mxtcyr
      do idir=1,2
      if (aatln(1,idir,iyr).ne.aatln(2,idir,iyr).or.
     *aatln(1,idir,iyr).ne.aatln(3,idir,iyr).or.
     *aatln(2,idir,iyr).ne.aatln(3,idir,iyr)) then
      ix=index(aatnm,'75')!coronado bridge
      ix1=index(aatnm,'TUNNEL')!coronado tunnel
      ix2=index(aatnm,'-54')!SR-54
      ix3=index(aatnm,'-15 TOLL')!I-15 Toll Lanes
      ix4=index(aatnm,'01ST AVE')!1st Downtown
      ix5=index(aatnm,'F ST')!F Downtown
      ix6=index(aatnm,'A ST')!A Downtown
      write(lunlane,19594) aatnm,aatid,iyr,idir,
     * (aatln(iiper,idir,iyr),iiper=1,3)
      if(.not.((ix4.gt.0.and.aatsph.eq.1404).or.
     * (ix5.gt.0.and.aatsph.eq.1404).or.
     * (ix6.gt.0.and.aatsph.eq.1404))) then
      if(.not.(ix.gt.0.or.ix1.gt.0.or.ix2.gt.0.or.ix3.gt.0))
     *write(lunerr,19593) aatid,iyr,idir,(aatln(iiper,idir,iyr),
     *iiper=1,3)	
      endif
      endif
      end do  
      end do 
      endif  
      call wtca
  940 iaat=iaat+1
      if(iaat.gt.naat) go to 949
      lrtno=aatrt
      irtno=rlkarr(iaat)/1000
      ilkno=rlkarr(iaat)-irtno*1000
      aatrec=srec(iaat)
      if(irtno.ne.lrtno) go to 950
      go to 501
  949 endf=.true.
  950 first=.true.
  959 if(endf) go to 998
      go to 500

c     write out unused project codes
 
  998 do i=1,mproj
       if(.not.ulproj(i)) then
        write(luninf,19991) i,pnarr(i)
        inff=.true.
       endif
      end do

c     write out count stations that are used in the costat coverage, but not used in tcov
 
      do i=1,mxcsta
       if(usta(i)) then
        write(luninf,19992) i
        inff=.true.
       endif
      end do

      write(0,11011) naat
      open (unit=lunout,file='complete')
      go to 9999
 9030 write(lunerr,19030) aatadt
      errf=.true.
      go to 30
c 9040 write(lunerr,19040) aatsta
c      errf=.true.
c      go to 43
 9436 write(lunerr,19436)
      errf=.true.
      go to 9999
 9502 write(lunerr,19502) 
      errf=.true.
      go to 9999
 9000 write(lunerr,19000) fnamo
      errf=.true.
      go to 9999
 9101 write(lunerr,19101) aatrec
      errf=.true.
      go to 9999
 9102 write(lunerr,19102) aatrec,aatid
      errf=.true.
      go to 9999
 9130 write(lunerr,19130) alink
      errf=.true.
      go to 9999
 9140 write(lunerr,19140) alink
      errf=.true.
      go to 9999
 9150 write(lunerr,19150) alink
      errf=.true.
      go to 9999
 9070 write(lunerr,19070) iproj
      errf=.true.
      go to 73
 9071 write(lunerr,19071) iproj
      errf=.true.
      go to 9999
10000 format(80i1,t1,80a1)
10009 format(a1)
10033 format(i7,29(f7.0,2x))
10040 format(76i10)
10050 format(4x,i4,<iskip>x,i4)
10070 format(4x,i4,a43,4x,i4)
10071 format(a43)
10001 format(20i4)
 1090 format(a20,i1,i4)
 1091 format('zone ',i4)

 1092 format(a4,a16,i1,i4)
 1093 format('zzone connector     ',i4)
19222 format('ramp ',a15)
11001 format(' ** reading and storing local names  ** ')

11002 format(' ** reading and storing tcov data ** ')
11004 format(' ** ',i6,' records read - - initial reading of aat',
     *' complete **')
11005 format(' ** finding cross street names **')
11006 format(' ** sorting records end-to-end by name **')
11007 format(' ** ',a20,' is a circular route **')
11008 format(' ** reading and writing node attribute table **')
11010 format(' ** reading and writing arc attribute table **')
11011 format(' ** ',i5,' records written - - successful program',
     *' completion **')
20000 format(a1,1x,a8,41x,
     *'transportation coverage arc data',
     *42x,'page',i4,//,
     *'   rec   arc link                       adt  adt ',
     *'obs adj  del  del',/,
     *'    no    id dist cross street name    link  vol ',
     *'spd spd year proj year proj   jur     fc   hov sp med',
     *' lanes aux control t r l')
20001 format(/,t36,'route number: ',i4,' name: ',a20,1x,a7,1x,a12,/,
     *18x,a20)
20002 format(2i6,i5,1x,a20,2i5,2i4,4i5,1x,a5,
     *1x,a6,1x,a5,i3,1x,a3,1x,2(a1,'/'),a1,i4,2x,a6,3(1x,a1),i3,a1)

20003 format(' ')
20004 format(66x,2i5,1x,a5,

     *1x,a6,1x,a5,i3,1x,a3,1x,2(a1,'/'),a1,i4,2x,a6,3(1x,a1),i3,a1)
20090 format(3i5)
29991 format('Opening tcov aat')
29999 format('Opening ',a80)
19000 format(a50,' not opened')
19076 format('no tcov arc near costat',2i5)
19102 format(' increase size of mxid, aatrec=',i5,' aatid=',i5)
19100 format(' invalid node in arc attribute table: ',
     *' record=',i5,'node = ',i5,' name=',a20,'xname = ',a20)
19106 format(' invalid distance: ',
     *' record=',i5,' name=',a20,'distance = ',i10)
19111 format(' increase size of mxramp')
19128 format(' increase size of mxtca')
19409 format(' invalid node in node attribute table: ',
     *' record=',i5,'node id = ',i5,
     *'1st cross street=',a20,'2nd cross street = ',a20)
19927 format(' more than',i6,' links in route:',a21)
19391 format(' record not sorted',i6)
19591 format('arc/costat mismatch: arcid= ',i5,' arcjur= ',i2,
     *' arcsta= ',i4,' costat= ',i4)
19592 format('arc/costat mismatch: arcid= ',i5,' arcjur= ',i2,
     *' arcloop= ',i4,' costat= ',i4)
19593 format('inconsistent lane by time period: tcov-id= ',i5, 
     *' phase ',i4,' direction ',i4,' lanes ',3i4)
19594 format('variable lane by time period: name= ',a20,
     *'tcov-id= ',i5,' phase ',i4,' direction ',i4,' lanes ',3i4)
19030 format(' data problem with histfill file:',i10)
19040 format(' data problem with counts.jur file:',i10)

19070 format(' data problem with hwyproj record: ',i5)

19071 format(' project used more than once in hwyproj.lis',i5)
19420 format(' naterr#1 - proj on nat not in hwyproj list:',
     *i10,1x,a20,1x,a20,1x,i4)
19421 format(' naterr#4 - node iproj not equal to link iproj: '
     *,3(i5,1x))
19436 format(' increase size of mxtpn')
19447 format(' naterr#2 - more than 4 streets:',i10,1x,a20,1x,a20)
19448 format(' naterr#3 - two-way stop with 3+ unsigned streets:',
     *i10,1x,a20,1x,a20)
19502 format(' no unused id's)
19508 format('aaterr#1 - entry/exit problem: ',
     *'id = ',i5,' name=',a20,' from = ',a20,
     *' to = ',a20,' nodes = ',2i6)
19510 format('aaterr#2 - invalid functional class: ',
     *'id = ',i5,' name=',a20,' from = ',a20,' to = ',a20,' fc = ',i10)
19520 format('aaterr#4 - proj on aat not in hwyproj.lis:',
     *'id = ',i5,' name=',a20,' from = ',a20,' to = ',a20,' proj = ',i4)
19526 format(' aaterr#5 - duplicate arc: ',
     *'id = ',i5,' name=',a20,' from = ',a20,
     *' to = ',a20,' nodes = ',2i6)
19580 format(' aaterr#6 - zone connector with both ends at zone: ',
     *'id = ',i5,' nodes=',2i5,' zone = ',i4)
19581 format(' aaterr#7 - zone connector with neither at zone: ',
     *'id = ',i5,' nodes=',2i5,' zone = ',i4)
19589 format(' aaterr#8 - zone connector with neither end at street: ',
     *'id = ',i5,' nodes=',2i5,' zone = ',i4)
19590 format('
     * aaterr#9 - mismatch between costat attribute and coverage: ',
     *'id = ',i5,' name=',a20,' from = ',a20,' to = ',a20,
     *' costat = ',2i10)
19521 format(' check aat - decrease in aux lanes:',
     *'id = ',i5,' name=',a20,' from = ',a20,' to = ',a20,' proj = ',i4)
19101 format(' no ifc coded on tcoved/aat record:',i10)
19130 format(' data problem with tcov/transitsec file:',i10)
19140 format(' data problem with tcov/turnssec file:',i10)
19150 format(' data problem with tcov/hovsec file:',i10)
19991 format(' project not on tcov/aat:', i4,a50)
19992 format(' count ststion not on tcov/aat:', i0)
39999 format('[',a7,'] [',a8,'] [s9.10/postarc] [',a<ic1>,
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
      if(naat.gt.9) ic2=2
      if(naat.gt.99) ic2=3
      if(naat.gt.999) ic2=4
      if(naat.gt.9999) ic2=5
      if(naat.gt.99999) ic2=6
      call dattim
      write(lunerr,39999)  cdate,ctime,tpstat,naat
      close(lunerr)
      close(lunin)
      close(lunpr)
      close(lunlane)
      stop
      end
      subroutine namsrt
      include 'sandag.inc'
      common /c251/ ndarr(mxtca)
      common /int41/ recarr(mxtca)
      integer*4 recarr
      character*25 ndarr
      character*25       t,tt
      integer*4 nt,ntt
      dimension iu(21),il(21)
      jj=mxtca
      m=1
      ii=1
      i=ii
      j=jj
10    if(i.ge.j)go to 80
20    k=i
      ij=(j+i)/2
      t=ndarr(ij)
      if(ndarr(i).le.t)go to 30
      nt=recarr(ij)
      ndarr(ij)=ndarr(i)
      recarr(ij)=recarr(i)
      ndarr(i)=t
      recarr(i)=nt

      t=ndarr(ij)
30    l=j
      if(ndarr(j).ge.t)go to 50
      nt=recarr(ij)
      ndarr(ij)=ndarr(j)
      recarr(ij)=recarr(j)
      ndarr(j)=t
      recarr(j)=nt
      t=ndarr(ij)
      if(ndarr(i).le.t)go to 50
      nt=recarr(ij)
      ndarr(ij)=ndarr(i)
      recarr(ij)=recarr(i)
      ndarr(i)=t
      recarr(i)=nt
      t=ndarr(ij)
      go to 50
40    ntt=recarr(l)
      ndarr(l)=ndarr(k)
      recarr(l)=recarr(k)
      ndarr(k)=tt
      recarr(k)=ntt
50    l=l-1
      if(ndarr(l).gt.t)go to 50
      tt=ndarr(l)
60    k=k+1
      if(ndarr(k).lt.t)go to 60
      if(k.le.l)go to 40
      if(l-i.le.j-k)go to 70
      il(m)=i

      iu(m)=l
      i=k
      m=m+1

      go to 90

70    il(m)=k
      iu(m)=j
      j=l
      m=m+1
      go to 90
80    m=m-1
      if(m.lt.1) go to 99
      i=il(m)
      j=iu(m)
90    if(j-i.ge.11)go to 20
      if(i.eq.ii)go to 10
      i=i-1
100   i=i+1
      if(i.eq.j)go to 80
      t=ndarr(i+1)
      if(ndarr(i).le.t)go to 100
      nt=recarr(i+1)
      k=i
110   ndarr(k+1)=ndarr(k)
      recarr(k+1)=recarr(k)
      k=k-1
      if(t.lt.ndarr(k))go to 110
      ndarr(k+1)=t
      recarr(k+1)=nt
      go to 100
   99 return
      end
      subroutine recsrt
      include 'sandag.inc'
      common /int42/ srec(mxtca),rlkarr(mxtca)
      integer*4 rlkarr,srec,tt,t
      dimension iu(21),il(21)
      jj=mxtca
      ii=1
      m=1
      i=ii
      j=jj
5     if(i.ge.j) go to 70
10    k=i
      ij=(j+i)/2
      t=rlkarr(ij)
      if(rlkarr(i).le.t) go to 20
      nt=srec(ij)
      rlkarr(ij)=rlkarr(i)
      srec(ij)=srec(i)
      rlkarr(i)=t
      srec(i)=nt
      t=rlkarr(ij)
20    l=j
      if(rlkarr(j).ge.t)go to 40
      nt=srec(ij)
      rlkarr(ij)=rlkarr(j)
      srec(ij)=srec(j)
      rlkarr(j)=t
      srec(j)=nt
      t=rlkarr(ij)
      if(rlkarr(i).le.t) go to 40
      nt=srec(ij)
      rlkarr(ij)=rlkarr(i)
      srec(ij)=srec(i)
      rlkarr(i)=t
      srec(i)=nt
      t=rlkarr(ij)
      go to 40
30    ntt=srec(l)
      rlkarr(l)=rlkarr(k)
      srec(l)=srec(k)
      rlkarr(k)=tt
      srec(k)=ntt
40    l=l-1
      if(rlkarr(l).gt.t) go to 40
      tt=rlkarr(l)
50    k=k+1
      if(rlkarr(k).lt.t) go to 50
      if(k.le.l) go to 30
      if(l-i.le.j-k) go to 60
      il(m)=i
      iu(m)=l
      i=k
      m=m+1
      go to 80
60    il(m)=k
      iu(m)=j
      j=l
      m=m+1
      go to 80
70    m=m-1
      if(m.lt.1) return
      i=il(m)
      j=iu(m)
80    if(j-i.ge.11) go to 10
      if(i.eq.ii) go to 5
      i=i-1
90    i=i+1

      if(i.eq.j) go to 70
      t=rlkarr(i+1)
      if(rlkarr(i).le.t) go to 90
      nt=srec(i+1)
      k=i
100   rlkarr(k+1)=rlkarr(k)
      srec(k+1)=srec(k)
      k=k-1
      if(t.lt.rlkarr(k)) go to 100
      rlkarr(k+1)=t
      srec(k+1)=nt
      go to 90
      end
