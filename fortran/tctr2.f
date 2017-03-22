c
c     second in a series of programs/amls to create tranplan
c     transit inputs from tcov, this program:
c     1) sorts transit sections according to measures
c     2) checks for gaps/loops in routes
c     3) fills in nodes used by sections
c
      parameter (mxrt=999,mxrrec=1000,mxrec=120000)
      include 'sandag.inc'
      include 'tcov.inc'
      common /rsort/ sort1,sort2
c
c     trinfo attributes
c
      character*1 intnam(8)
      integer*2 infrt,infcon,infdir,
     *am1,op1,pm1,nt1,nh1,am2,op2,pm2,nt2,nh2,
     *am3,op3,pm3,nt3,nh3,am4,op4,pm4,nt4,nh4,
     *am5,op5,pm5,nt5,nh5
      character*20 org
      character*40 use
      character*32 extnam
      character*150 idat
c
c
c     program variables
c
      logical endf
      integer*2 ac(2,mxcon,mxrrec),sac(2,mxcon,mxrrec),
     *aatmin(mxtrca),aatmax(mxtrca),a2(mxrec),ln,
     *aco(mxrt,2,mxcon),amode(mxrt,2,mxcon),hrt(mxrt,2,mxcon),
     *am(mxrt,2,mxcon),op(mxrt,2,mxcon),pm(mxrt,2,mxcon),
     *nt(mxrt,2,mxcon),nh(mxrt,2,mxcon)
      integer*4 rtlbid(mxrt),lbfn(mxtrca),lbtn(mxtrca),irec,
     *lrlink,mrec,aalink(mxrrec),sort1(mxrec),
     *sort2(mxrec),a1(mxrec),a3(mxrec),rorder(mxmoda,999,2,mxcon),id
      real*4 lblen(mxtrca)
      character*1 adir
      character*20 lbnm(mxtrca),lbfxnm(mxtrca),lbtxnm(mxtrca),
     *avia(mxrt,2,mxcon)
      character*24 aorg(mxrt,2,mxcon),ades(mxrt,2,mxcon)
      data lunerr,lunin,ratlun,aatlun,seclun
     */11,12,13,14,15/
      data sort1,sort2/mxrec*1999999999,mxrec*0/
      data aatmin/mxtrca*mxmoda/
      open (unit=lunerr,file='tctr2.err')
      fnamo='headways'
      open (unit=lunin,file=fnamo,status='old',err=3)
      print *,fnamo
    1 read(lunin,10001,iostat=istat) irt,adir,icon,ih
      if(istat.ne.0) go to 2
      if(irt.lt.1) go to 1
      if(irt.gt.999) go to 9001
      idir=0
      if(adir.eq.'i') idir=1
      if(adir.eq.'i') idir=1
      if(adir.eq.'o') idir=2
      if(adir.eq.'o') idir=2
      if(idir.lt.1.or.idir.gt.2) go to 9001
      if(icon.lt.1.or.icon.gt.mxcon) go to 9001
      if(ih.ge.1.and.ih.le.5)
     *hrt(irt,idir,icon)=ih
      go to 1
    2 close(lunin)
c
c     open trinfo data file.  first look for project specific
c     file under current workspace.
c
    3 fnamo='info/arc.dir'
      open(unit=lunin,convert='big_endian',file=fnamo,access=
     *'direct',form='unformatted',recl=95,status='old',err=9000)
      print *,fnamo
      infrec=1
    4 read(lunin,rec=infrec,iostat=istat) extnam,intnam,idat
      if(istat.ne.0) go to 6
      i=index(extnam,'TRINFO')
      if(i.gt.0) go to 5
      infrec=infrec+1
      go to 4
    5 write(fnamo,20005) (intnam(i),i=4,7)
      go to 50
    6 close(lunin)
   50 open(unit=lunin,convert='big_endian',file=fnamo,access=
     *'direct',form='unformatted',recl=48,status='old',err=9000)
      print *,fnamo
      infrec=1
   51 read(lunin,rec=infrec,iostat=istat) infrt,infdir,infcon,id,
     *ratco,ratmod,
     *ratorg,ratdes,ratvia,org,use,
     *am1,op1,pm1,nt1,nh1,am2,op2,pm2,nt2,nh2,
     *am3,op3,pm3,nt3,nh3,am4,op4,pm4,nt4,nh4,
     *am5,op5,pm5,nt5,nh5
      if(istat.ne.0) go to 59
      if(infrt.lt.1.or.infrt.gt.mxrt) go to 9051
      if(infdir.lt.1.or.infdir.gt.2) go to 9051
      if(infcon.lt.1.or.infcon.gt.mxcon) go to 9051
      if(ratmod.lt.4.or.ratmod.gt.mxmoda) go to 9051
      if(ratco.lt.1.or.ratco.gt.9) ratco = 1
      aco(infrt,infdir,infcon)=ratco
      amode(infrt,infdir,infcon)=ratmod
      aorg(infrt,infdir,infcon)=ratorg
      ades(infrt,infdir,infcon)=ratdes
      avia(infrt,infdir,infcon)=ratvia
      rorder(ratmod,infrt,infdir,infcon)=1
      if(hrt(infrt,infdir,infcon).eq.2) go to 52
      if(hrt(infrt,infdir,infcon).eq.3) go to 53
      if(hrt(infrt,infdir,infcon).eq.4) go to 54
      if(hrt(infrt,infdir,infcon).eq.5) go to 55
      am(infrt,infdir,infcon)=am1
      op(infrt,infdir,infcon)=op1
      pm(infrt,infdir,infcon)=pm1
      nt(infrt,infdir,infcon)=nt1
      nh(infrt,infdir,infcon)=nh1
      go to 58
   52 am(infrt,infdir,infcon)=am2
      op(infrt,infdir,infcon)=op2
      pm(infrt,infdir,infcon)=pm2
      nt(infrt,infdir,infcon)=nt2
      nh(infrt,infdir,infcon)=nh2
      go to 58
   53 am(infrt,infdir,infcon)=am3
      op(infrt,infdir,infcon)=op3
      pm(infrt,infdir,infcon)=pm3
      nt(infrt,infdir,infcon)=nt3
      nh(infrt,infdir,infcon)=nh3
      go to 58
   54 am(infrt,infdir,infcon)=am4
      op(infrt,infdir,infcon)=op4
      pm(infrt,infdir,infcon)=pm4
      nt(infrt,infdir,infcon)=nt4
      nh(infrt,infdir,infcon)=nh4
      go to 58
   55 am(infrt,infdir,infcon)=am5
      op(infrt,infdir,infcon)=op5
      pm(infrt,infdir,infcon)=pm5
      nt(infrt,infdir,infcon)=nt5
      nh(infrt,infdir,infcon)=nh5
   58 infrec=infrec+1
      go to 51
   59 close(lunin)
      inum=0
      do 69 ratmod=4,mxmoda
      do 68 infrt=1,999
      do 67 infdir=1,2
      do 66 infcon=1,mxcon
      if(rorder(ratmod,infrt,infdir,infcon).lt.1) go to 66
      inum=inum+1
      rorder(ratmod,infrt,infdir,infcon)=inum
   66 continue
   67 continue
   68 continue
   69 continue
c
c     open route data file.
c
      call ottrcr
      if(ratio.ne.0) go to 9000
      print *,fnamo
      ratrec=1
   70 call rtctrr
      if(ratio.ne.0) go to 79
      if(ratlb.lt.1.or.ratlb.gt.mxrt) go to 9070
      if(ratid.lt.1.or.ratid.gt.mxrt) go to 9070
      rtlbid(ratlb)=ratid
      ratrec=ratrec+1
      go to 70
   79 close(ratlun)
c
c     open trcov aat file
c
      call otrca
      if(aatio.ne.0) go to 89 
      print *,fnamo
      aatrec=1
c
c     read trcov arc attribute table saving arrays of arc data
c
   80 call rtrca
      if(aatio.ne.0) go to 89
      if(aatarn(2).gt.mxtrcn) go to 9080
      if(aatarn(1).gt.mxtrcn) go to 9080
      if(aatlb.gt.mxtrca) go to 9080
      lbfn(aatlb)=aatarn(2)
      lbtn(aatlb)=aatarn(1)
      lblen(aatlb)=aatlen
      lbnm(aatlb)=aatnm
      lbfxnm(aatlb)=aatxnm(2)
      lbtxnm(aatlb)=aatxnm(1)
      aatrec=aatrec+1
      go to 80
c
c     open old section data file and sort sections
c
   89 call ottrcs
      if(secio.ne.0) go to 9000
      print *,fnamo
      secrec=1
      irec=0
      jrec=0
      endf=.false.
      call rtctrs
      if(secio.ne.0) go to 9000
      lrlink=rlink
      if(lrlink.lt.1.or.lrlink.gt.mxrt) go to 9090
      irt=rtlbid(lrlink)
      if(irt.lt.1.or.irt.gt.mxrt) go to 9090
   90 call rtctrs
      if(secio.ne.0) go to 98
      if(alink.lt.1.or.alink.gt.mxtrca) go to 9090
      if(rlink.ne.lrlink) go to 92
      irec=irec+1
      if(irec.gt.mxrrec) go to 9091
      aalink(irec)=alink
      do 91 i=1,mxcon
      ac(1,i,irec)=secic(i)
      ac(2,i,irec)=secoc(i)
      sac(1,i,irec)=secsic(i)
      sac(2,i,irec)=secsoc(i)
   91 continue
      mrec=irec
      secrec=secrec+1
      go to 90
   92 do 95 idir=1,2
      do 94 icon=1,mxcon
      ratmod=amode(irt,idir,icon)
      if(ratmod.lt.1.or.ratmod.gt.mxmoda) go to 94
      ii=rorder(ratmod,irt,idir,icon)
      if(ii.lt.1) go to 94
      id=irt*1000+idir*100+icon
      do 93 iirec=1,mrec
      i=sac(idir,icon,iirec)
      if(i.lt.1.or.i.eq.9999) go to 93
      iorder=ii*10000+i
      jrec=jrec+1
      if(jrec.gt.mxrec) go to 9092
      nrec=jrec
      sort1(jrec)=iorder
      sort2(jrec)=jrec
      a1(jrec)=aalink(iirec)
      a2(jrec)=ac(idir,icon,iirec)
      a3(jrec)=id
      if(irt.ne.2) go to 93
      if(idir.ne.1) go to 93
      if(icon.ne.2) go to 93
   93 continue
   94 continue
   95 continue
      if(endf) go to 99
      lrlink=rlink
      if(lrlink.lt.1.or.lrlink.gt.mxrt) go to 9090
      irt=rtlbid(lrlink)
      if(irt.lt.1.or.irt.gt.mxrt) go to 9090
      irec=0
      go to 90
   98 endf=.true.
      go to 92
   99 close(seclun)
      call rtsort
      secmes(2)=0.0
      secmes(1)=0.0
      call otrcs
      if(secio.ne.0) go to 9000
      print *,fnamo
      secrec=1
      call otrcr
      if(ratio.ne.0) go to 9000
      print *,fnamo
      irec=1
      ratrec=1
      ratlb=1
      mrec=0
      iorder=sort1(irec)
      jrec=sort2(irec)
      id=a3(jrec)
      irt=id/1000
      i=id-irt*1000
      idir=i/100
      icon=i-idir*100
      if(irt.lt.1.or.irt.gt.mxrt) go to 9101
      if(idir.lt.1.or.idir.gt.2) go to 9101
      if(icon.lt.1.or.icon.gt.mxcon) go to 9101
      ratmod=amode(irt,idir,icon)
      if(ratmod.lt.4.or.ratmod.gt.mxmoda) go to 9101
      ratco=aco(irt,idir,icon)
      if(ratco.lt.1.or.ratco.gt.9) ratco=1
      ratid=id
  101 iorder=sort1(irec)
      jrec=sort2(irec)
      id=a3(jrec)
      if(id.ne.ratid) go to 119
      ii=iorder/10000
      i=iorder-ii*10000
      ix=i/10
      irev=i-ix*10
      alink=a1(jrec)
      if(alink.lt.1.or.alink.gt.mxtrca) go to 9113
      inum=i/10
      if(irev.eq.1) go to 114
      if(irev.eq.2) go to 115
      go to 9115
  114 secarn(2)=lbfn(alink)
      secarn(1)=lbtn(alink)
      if(mrec.gt.1.and.secarn(2).ne.ln) go to 9114
      secmes(2)=secmes(1)
      secmes(1)=secmes(2)+lblen(alink)
      secpos(2)=0.0
      secpos(1)=100.0
      secxnm(2)=lbfxnm(alink)
      secxnm(1)=lbtxnm(alink)
      go to 116
  115 secarn(1)=lbfn(alink)
      secarn(2)=lbtn(alink)
      if(mrec.gt.1.and.secarn(2).ne.ln) go to 9115
      secmes(2)=secmes(1)
      secmes(1)=secmes(2)+lblen(alink)
      secpos(2)=100.0
      secpos(1)=0.0
      secxnm(2)=lbtxnm(alink)
      secxnm(1)=lbfxnm(alink)
  116 seclb=a2(jrec)
      secid=ii
      secnm=lbnm(alink)
      sectpn(2)=0
      sectpn(1)=0
      secstp(2)=0
      secstp(1)=0
      ln=secarn(1)
      mrec=mrec+1
      rlink=ratlb
      call wtrcs
      secrec=secrec+1
      if(secio.ne.0) go to 9999
      if(ratmod.lt.aatmin(alink)) aatmin(alink)=ratmod
      if(ratmod.gt.aatmax(alink)) aatmax(alink)=ratmod
      irec=irec+1
      if(irec.gt.nrec) go to 119
      go to 101
  119 if(mrec.lt.1) go to 120
      ratorg=aorg(irt,idir,icon)
      ratdes=ades(irt,idir,icon)
      ratvia=avia(irt,idir,icon)
      rathwy(1)=am(irt,idir,icon)
      rathwy(2)=pm(irt,idir,icon)
      rathwy(3)=op(irt,idir,icon)
      rathwy(4)=nt(irt,idir,icon)
      ratnhr=nh(irt,idir,icon)
      call wtrcr
      if(ratio.ne.0) go to 9999
      ratrec=ratrec+1
      ratlb=ratrec
  120 mrec=0
      if(irec.gt.nrec) go to 199
      iorder=sort1(irec)
      jrec=sort2(irec)
      id=a3(jrec)
      ratid=id
      irt=id/1000
      i=id-irt*1000
      idir=i/100
      icon=i-idir*100
      if(irt.lt.1.or.irt.gt.mxrt) go to 9101
      if(idir.lt.1.or.idir.gt.2) go to 9101
      if(icon.lt.1.or.icon.gt.mxcon) go to 9101
      ratmod=amode(irt,idir,icon)
      if(ratmod.lt.4.or.ratmod.gt.mxmoda) go to 9101
      ratco=aco(irt,idir,icon)
      if(ratco.lt.1.or.ratco.gt.9) ratco=1
      secmes(2)=0.0
      secmes(1)=0.0
      go to 101
  199 close(seclun)
      close(ratlun)
      aatrec=1
c
c     read trcov arc attribute table write in miminum mode
c
  200 call rtrca
      if(aatio.ne.0) go to 209
      if(aatlb.gt.mxtrca) go to 9200
      aatmm=aatmin(aatlb)
      aattmp(1)=aatmax(aatlb)
      call wtrca
      if(aatio.ne.0) go to 9000
      aatrec=aatrec+1
      go to 200
  209 close(aatlun)
      go to 9999
 9000 write(lunerr,19000) fnamo
      errf=.true.
      go to 9999
 9001 write(lunerr,19001) irt,adir,icon
      errf=.true.
      go to 1
 9010 write(lunerr,19010) 
      errf=.true.
      go to 9999
 9051 write(lunerr,19051) infrt,infdir,infcon,id,ratco,ratmod
      errf=.true.
      go to 9999
 9070 write(lunerr,19070) ratlb,ratid
      errf=.true.
      go to 9999
 9080 write(lunerr,19080) aatlb,aatarn(2),aatarn(1)
      errf=.true.
      go to 9999
 9090 write(lunerr,19090) rlink,irt,alink
      errf=.true.
      go to 9999
 9091 write(lunerr,19091)
      errf=.true.
      go to 9999
 9092 write(lunerr,19092)
      errf=.true.
      go to 9999
 9101 write(lunerr,19101) irt,idir,icon,id,ratmod
      errf=.true.
      go to 9999
 9111 write(lunerr,19111) id,idir,icon
      errf=.true.
      go to 119
 9112 write(lunerr,19112) id,idir,icon
      errf=.true.
      go to 119
 9113 write(lunerr,19113) id,idir,icon
      errf=.true.
      go to 119
 9114 write(lunerr,19114) irt,idir,icon
      errf=.true.
      go to 119
 9115 write(lunerr,19115) irt,idir,icon
      errf=.true.
      go to 119
 9200 write(lunerr,19200) aatlb,aatarn(2),aatarn(1)
      errf=.true.
      go to 9999
10001 format(1x,i3,1x,a1,i2,2x,i1)
19000 format(' problem opening file: ',a80)
19001 format(' data problem with headways record:',i5,1x,a2,i3)
19010 format(' arcdr9 file not found')
19051 format(' data problem with trinfo',6i7)
19070 format(' data problem with ratdata',2i10)
19080 format(' data problem with trcov.aat ', 5i10)
19090 format(' data problem with secdata',3i10)
19091 format(' increase size of mxrrec')
19092 format(' increase size of mxrec')
19101 format(' problem decoding id',6i5)
19111 format(' problem #1 with  route ',i5,i2,i2)
19112 format(' problem #2 with  route ',i5,i2,i2)
19113 format(' problem #3 with  route ',i5,i2,i2)
19114 format(' problem #4 with  route ',i5,i2,i2)
19115 format(' problem #5 with  route ',i5,i2,i2)
19200 format(' data problem with rereding trcov.aat ', 5i10)
20005 format('info/arc',4a1,'.dat')
20025 format('/box2/tp/covs/s9.10/info/arc',4a1,'.dat')
39999 format('[',a7,'] [',a8,'] [2030fin/tctr2] [',a<ic1>,
     *'] [trcov records=',i<ic2>,']')
 9999 if(errf)  go to 99991
      tpstat='ok'
      ic1=2
      close(lunerr,status='delete')
      go to 99992
99991 close(lunerr,status='keep')
      tpstat='not'
      ic1=3
99992 open (unit=lunerr,file='tplog',access='append')
      infrec=infrec-1
      ic2=1
      if(infrec.gt.9) ic2=2
      if(infrec.gt.99) ic2=3
      if(infrec.gt.999) ic2=4
      if(infrec.gt.9999) ic2=5
      if(infrec.gt.99999) ic2=6
      call dattim
      write(lunerr,39999)  cdate,ctime,tpstat,infrec
      close(lunerr)
      close(lunin)
      stop
      end
      subroutine rtsort
      parameter (mxrec=120000)
      common /rsort/ sort1,sort2
      integer*4 sort1(mxrec),sort2(mxrec),t,nt,tt,ntt
      dimension iu(21),il(21)
      ii=1
      m=1
      i=ii
      j=mxrec
5     if(i.ge.j) go to 70
10    k=i
      ij=(j+i)/2
      t=sort1(ij)
      if(sort1(i).le.t) go to 20
      nt=sort2(ij)
      sort1(ij)=sort1(i)
      sort2(ij)=sort2(i)
      sort1(i)=t
      sort2(i)=nt
      t=sort1(ij)
20    l=j
      if(sort1(j).ge.t)go to 40
      nt=sort2(ij)
      sort1(ij)=sort1(j)
      sort2(ij)=sort2(j)
      sort1(j)=t
      sort2(j)=nt
      t=sort1(ij)
      if(sort1(i).le.t) go to 40
      nt=sort2(ij)
      sort1(ij)=sort1(i)
      sort2(ij)=sort2(i)
      sort1(i)=t
      sort2(i)=nt
      t=sort1(ij)
      go to 40
30    ntt=sort2(l)
      sort1(l)=sort1(k)
      sort2(l)=sort2(k)
      sort1(k)=tt
      sort2(k)=ntt
40    l=l-1
      if(sort1(l).gt.t) go to 40
      tt=sort1(l)
50    k=k+1
      if(sort1(k).lt.t) go to 50
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
      t=sort1(i+1)
      if(sort1(i).le.t) go to 90
      nt=sort2(i+1)
      k=i
100   sort1(k+1)=sort1(k)
      sort2(k+1)=sort2(k)
      k=k-1
      if(t.lt.sort1(k)) go to 100
      sort1(k+1)=t
      sort2(k+1)=nt
      go to 90
      end
