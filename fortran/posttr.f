      parameter (mxrec=1000)
      include 'sandag.inc'
      include 'tcov.inc'
c
c     trinfo attributes
c
      character*1 intnam(8)
      integer*2 route,dir,con,rco,rmode,
     *am1,op1,pm1,nt1,nh1,am2,op2,pm2,nt2,nh2,
     *am3,op3,pm3,nt3,nh3,am4,op4,pm4,nt4,nh4,
     *am5,op5,pm5,nt5,nh5
      character*20 via,org
      character*24 start,stop
      character*40 use
      character*32 extnam
      character*150 idat
c
c     program variables
c
      logical endf,rev(mxrec),nstop(mxtcn),ntrn(mxtcn),fcwalk(14),
     *xfnode(mxtcn),delsec(999999),utap
      integer*4 snode(mxtcn),nalink(mxtca),arlink(mxrec),arec(mxrec),
     *ac(2,mxcon,mxrec),order(2,mxcon,mxrec),ntl(mxrec),nfl(mxrec),
     *flinks(6,mxrec),tlinks(6,mxrec),
     *nnode1(mxtcn),nnode2(mxtcn),rtfrno(999,2,mxcon),
     *rttono(999,2,mxcon),lbfc(mxtca)
      integer*4 lbfn(mxtca),lbtn(mxtca),lbid(999),aalink(mxrec),
     *lbtrn(mxtcn),trnlb(mxtrn),lbtap(mxtcn),taplb(mxtap),l1(mxcon)
      character*8 dirnam(2)
      character*24 snm,sphnam(1999)
      data aatlun,natlun,ratlun,seclun,lunin,lunpr,lunerr,luno
     */11,12,13,14,15,16,17,18/
      data fcwalk/.false.,6*.true.,3*.false.,3*.true.,.false./
      data dirnam/'inbound ','outbound'/
c
c     open files
c
      open (unit=lunerr,file='posttr.err')
      open (unit=lunpr,file='posttr.pr')
c     open (unit=luno,file='trsphere')
c
c     read sphere names
c
      fnamo='../data/sphere.name'
      open (unit=lunin,file=fnamo,status='old',err=9000)
      print *, fnamo
   11 read(lunin,10010,iostat=istat) isph,snm
      if(istat.ne.0) go to 19
      if(isph.lt.1.or.isph.gt.1999) go to 9010
      sphnam(isph)=snm
      go to 11
   19 close(lunin)
c
c     read transfer link nodes for consisent tap numbering
c
      fnamo='posttr.in'
      open (unit=lunin,file=fnamo,status='old',err=90)
   21 read(lunin,10020,iostat=istat) inode,jnode
      if(istat.ne.0) go to 29
      if(inode.lt.1.or.inode.gt.mxtcn) go to 9020
      if(jnode.lt.1.or.jnode.gt.mxtcn) go to 9020
      xfnode(inode)=.true.
      xfnode(jnode)=.true.
      go to 21
   29 close(lunin)
   90 call otcea
      fnamo='tcoved.aat'
      print *, fnamo
   92 aatrec=1
c
c     read tcov aat and save data.
c
  101 call rtca
      if(aatio.ne.0) go to 109
      if(aatarn(2).lt.1.or.aatarn(2).gt.mxtcn) go to 9100
      if(aatarn(1).lt.1.or.aatarn(1).gt.mxtcn) go to 9100
      if(aatlb.lt.1.or.aatlb.gt.mxtca) go to 9100
      lbfn(aatlb)=aatarn(2)
      lbtn(aatlb)=aatarn(1)
      lbfc(aatlb)=aatfc(1)
      if(aatrts.lt.100) go to 102
      ntrn(aatarn(1))=.true.
      ntrn(aatarn(2))=.true.
      go to 108
  102 if(.not.fcwalk(aatfc(1))) go to 108
      if(aatsph.ne.1404) go to 108
      ntrn(aatarn(1))=.true.
      ntrn(aatarn(2))=.true.
  108 aatrec=aatrec+1
      go to 101
  109 close(aatlun)
c
c     open nat file 
c
      call otcen
      fnamo='tcoved.nat'
      print *, fnamo
      natrec=1
  201 call rtcn
      if(natio.ne.0) go to 209
      if(natlb.lt.1.or.natlb.gt.mxtcn) go to 9200
      if(natstp(1).ge.4.and.natstp(1).le.mxmoda) nstop(natlb)=.true.
      if(natstp(2).ge.4.and.natstp(2).le.mxmoda) nstop(natlb)=.true.
      utap=.false.
      if(natstp(1).ge.4.and.natstp(1).le.7) utap=.true.
      if(natstp(2).ge.4.and.natstp(2).le.7) utap=.true.
      if(natprk(1).gt.1) utap=.true.
      if(natprk(2).gt.1) utap=.true.
      if(nattap.gt.0) utap=.true. 
      if(utap) then
        if(nattap.gt.0.and.nattap.lt.mxtap)then
          if(taplb(nattap).gt.0) nattap=mxtap
          taplb(nattap)=natlb
          lbtap(natlb)=nattap
      if(nattap.eq.44) 
     *print *, natlb,nattap,taplb(nattap),lbtap(natlb)
         else
         lbtap(natlb)=mxtap
         endif
       else
       continue
       endif
      if(ntrn(natlb)) then
        if(nattrn.gt.mxtap.and.nattrn.le.mxtrn) then 
          if(trnlb(nattrn).gt.0) nattrn=mxtrn
          trnlb(nattrn)=natlb
          lbtrn(natlb)=nattrn
          else 
        lbtrn(natlb)=mxtrn
        endif
      else
      continue
      endif
      if(natsph.lt.1.or.natsph.gt.1999) natsph=1
      snode(natlb)=natsph
      natrec=natrec+1
      go to 201
c
c     open tcoved rat
c
  209 call otcetrr
      fnamo='tcoved.rat'
      print *, fnamo
      ratrec=1
c
c     read route attribute table
c
  301 call rtctrr
      if(ratio.ne.0) go to 309
      if(ratlb.lt.1.or.ratlb.gt.999) go to 9300
      if(ratid.lt.1.or.ratid.gt.999) go to 9300
      lbid(ratlb)=ratid
      ratrec=ratrec+1
      go to 301
  309 close(ratlun)
c
c     open tcoved section file
c
      call otcetrs
      fnamo='tcoved.sec'
      print *, fnamo
      secrec=1
c
c     read sections for each route
c
  400 irec=1
      call rtctrs
      if(secio.ne.0) go to 499
      if(rlink.lt.1.or.rlink.gt.999) go to 9400
      if(alink.lt.1.or.alink.gt.mxtca) go to 9400
      nalink(alink)=nalink(alink)+1
      arec(irec)=secrec
      arlink(irec)=rlink
      aalink(irec)=alink
      id=lbid(rlink)
      do 4000 i=1,mxcon
      ac(1,i,irec)=secic(i)
      ac(2,i,irec)=secoc(i)
 4000 continue
      secrec=secrec+1
  401 call rtctrs
      if(secio.ne.0) go to 498
      if(rlink.lt.1.or.rlink.gt.999) go to 9400
      if(alink.lt.1.or.alink.gt.mxtca) go to 9400
      if(rlink.ne.arlink(1)) go to 409
      if(nalink(alink).lt.1) go to 4011
      if(secpos(1).gt.0.0.and.secpos(1).lt.100.0) go to 4080
      if(secpos(2).gt.0.0.and.secpos(2).lt.100.0) go to 4080
 4011 nalink(alink)=nalink(alink)+1
      irec=irec+1
      if(irec.gt.mxrec) go to 9401
      arlink(irec)=rlink
      arec(irec)=secrec
      aalink(irec)=alink
      do 408 i=1,mxcon
      ac(1,i,irec)=secic(i)
      ac(2,i,irec)=secoc(i)
  408 continue
      go to 4081
 4080 delsec(secrec)=.true.
      sectm2=9
      call wtctrs
      if(secio.ne.0) go to 9000




 4081 secrec=secrec+1
      go to 401
  409 mrec=irec
      nrec=secrec
c      write(0,20409) id
      if(id.lt.1.or.id.gt.999) go to 9409
c
c     find first and last links on configfuration
c     and save connections
c
      do 479 i=1,2
      do 478 j=1,mxcon
       do ii=1,mxcon
        l1(ii)=0
       end do
      l3=0
      l99=0
      nl=0
      inum=1
      mf=0
      do 419 k=1,mrec
      if(ac(i,j,k).eq.1) go to 410
      if(ac(i,j,k).eq.3) go to 411
      if(ac(i,j,k).eq.99) go to 412
      go to 413
  410 mf=mf+1
      l1(mf)=k
      go to 414
  411 l3=k
      go to 414
  412 l99=k
      go to 414
  413 if(ac(i,j,k).gt.0.and.ac(i,j,k).ne.9) go to 414
      order(i,j,k)=9999
      go to 419
  414 ilink=aalink(k)
      n11=lbfn(ilink)
      n12=lbtn(ilink)
      nl=nl+1
      do 418 kk=1,mrec
      if(k.eq.kk) go to 418
      if(ac(i,j,kk).lt.1.or.ac(i,j,kk).eq.9) go to 418
      jlink=aalink(kk)
      n21=lbfn(jlink)
      n22=lbtn(jlink)
      if(n11.eq.n21) go to 416
      if(n11.eq.n22) go to 416
      if(n12.eq.n22) go to 417
      if(n12.eq.n21) go to 417
      go to 418
  416 il=nfl(k)+1
      if(il.gt.6) go to 9412
      nfl(k)=il
      flinks(il,k)=kk
      if(ilink.eq.jlink) go to 417
      go to 418
  417 il=ntl(k)+1
      if(il.gt.6) go to 9412
      ntl(k)=il
      tlinks(il,k)=kk
  418 continue
c     write(lunerr,20998) id,i,j,k,ilink,n11,n12,
c    *nfl(k),(flinks(ii,k),ii=1,4),ntl(k),(tlinks(ii,k),ii=1,4)
  419 continue
c
c     skip unused configurations
c
      if(nl.lt.1) go to 470
c    
c     find first node
c
      if(l3.eq.0) l3=l99
      if(l1(1).eq.0) go to 9420
      mi=mf
      if(mi.lt.1) mi=1
      do 4201 iil1=1,mi
      ilink=aalink(l1(iil1))
      ik=l1(iil1)
      if(nfl(l1(iil1)).eq.0) go to 421
      if(ntl(l1(iil1)).eq.0) go to 422
 4201 continue
      iil1=1
      n11=lbfn(ilink)
      n12=lbtn(ilink)
      if(lbtrn(n11).gt.0.and.lbtrn(n12).lt.1) go to 421
      if(lbtrn(n12).gt.0.and.lbtrn(n11).lt.1) go to 422
      if(lbtrn(n11).gt.0.and.lbtrn(n12).lt.1) go to 421
      if(lbtrn(n12).gt.0.and.lbtrn(n11).lt.1) go to 422
      if(lbtap(n11).gt.0.and.lbtap(n12).lt.1) go to 421
      if(lbtap(n12).gt.0.and.lbtap(n11).lt.1) go to 422
      if(lbtap(n11).gt.0.and.lbtap(n12).lt.1) go to 421
      if(lbtap(n12).gt.0.and.lbtap(n11).lt.1) go to 422
      if(l3.gt.0) go to 420
      if(ntl(l1(iil1)).lt.nfl(l1(iil1))) go to 421
      go to 422
c 
c     check for circular route
c 
  420 n11=lbfn(ilink)
      n12=lbtn(ilink)
      jlink=aalink(l3)
      n31=lbfn(jlink)
      n32=lbtn(jlink)
      if(n12.eq.n31) go to 422
      if(n12.eq.n32) go to 422
      if(n11.eq.n31) go to 421
      if(n11.eq.n32) go to 421
      if(ntl(l1(iil1)).lt.nfl(l1(iil1))) go to 421
      go to 422
  421 n1=lbfn(ilink)
      if(.not.lbtap(n1).lt.1.and..not.nstop(n1))
     *write(lunpr,19429) id,i,j
c      if(nstop(n1)) lbtap(n1)=mxtap
      nnode1(n1)=nnode1(n1)+1
      isph=snode(n1)
      if(isph.lt.1.or.isph.gt.1999) isph=20
      rtfrno(id,i,j)=isph
      nf=n1
c     order(i,j,l1(iil1))=11
      go to 430
  422 n1=lbtn(ilink)
      if(.not.lbtap(n1).lt.1.and..not.nstop(n1))
     *write(lunpr,19429) id,i,j
c      if(nstop(n1)) lbtap(n1)=mxtap
      nnode1(n1)=nnode1(n1)+1
      isph=snode(n1)
      if(isph.lt.1.or.isph.gt.1999) isph=1
      rtfrno(id,i,j)=isph
      nf=n1
      rev(l1(iil1))=.true.
c     order(i,j,l1(iil1))=12
      go to 440
  430 order(i,j,ik)=inum*10+1
      n1=lbfn(ilink)
      n2=lbtn(ilink)
      nnode1(n2)=nnode1(n2)+1
      il=ntl(ik)
      if(il.lt.1) go to 450
      if(il.lt.2) go to 432
c     
c     if at branch, find section with lowest number
c
      ix=9999
      iz=9999
      izz=9999
      do 431 ii=1,il
      jk=tlinks(ii,ik)
      if(jk.lt.1) go to 431
      if(order(i,j,jk).gt.0) go to 431
      jlink=aalink(jk)
      if(ilink.eq.jlink) go to 4310
      kk=ac(i,j,jk)
      if(kk.ge.ix) go to 431

      ix=kk
      iz=ii
      go to 431
 4310 izz=ii
  431 continue
      il=iz
      if(il.gt.mrec) il=izz
      if(il.gt.mrec) go to 450
  432 ik=tlinks(il,ik)
      if(ik.lt.1) go to 450
      if(order(i,j,ik).gt.0) go to 450
      inum=inum+1
      ilink=aalink(ik)
      n21=lbfn(ilink)
      n22=lbtn(ilink)
      if(n2.eq.n21) go to 430
      if(n2.eq.n22) go to 440
      go to 450
  440 order(i,j,ik)=inum*10+2
      n1=lbtn(ilink)
      n2=lbfn(ilink)
      nnode1(n2)=nnode1(n2)+1
      rev(ik)=.true.
      il=nfl(ik)
      if(il.lt.1) go to 450
      if(il.lt.2) go to 442
c     
c     if at branch, find section with lowest number
c
      ix=9999
      iz=9999
      izz=9999
      do 441 ii=1,il
      jk=flinks(ii,ik)
      if(jk.lt.1) go to 441
      if(order(i,j,jk).gt.0) go to 441
      jlink=aalink(jk)
      if(ilink.eq.jlink) go to 4410
      kk=ac(i,j,jk)
      if(kk.ge.ix) go to 441
      ix=kk
      iz=ii
      go to 441
 4410 izz=ii
  441 continue
      il=iz
      if(il.gt.mrec) il=izz
      if(il.gt.mrec) go to 450
  442 ik=flinks(il,ik)
      if(ik.lt.1) go to 450
      if(order(i,j,ik).gt.0) go to 450
      inum=inum+1
      ilink=aalink(ik)
      n21=lbfn(ilink)
      n22=lbtn(ilink)
      if(n2.eq.n21) go to 430
      if(n2.eq.n22) go to 440
  450 do 451 k=1,mrec
      if(order(i,j,k).lt.1) go to 452
  451 continue
      go to 460
  452 write(lunerr,19451) id,dirnam(i),j
      errf=.true.
      go to 470
  460 isph=snode(n2)
      if(lbtap(n2).lt.1.and..not.nstop(n2))
     *write(lunpr,19429) id,i,j
c      if(nstop(n2)) lbtap(n2)=mxtap
      if(isph.lt.1.or.isph.gt.1999) isph=20
      rttono(id,i,j)=isph
      if(nf.eq.n2) rttono(id,i,j)=21
  470 do 472 k=1,mrec
      ntl(k)=0
      nfl(k)=0
      do 471 ii=1,6
      tlinks(ii,k)=0
      flinks(ii,k)=0
  471 continue
  472 continue
      do 473 k=1,mxtcn
      if(nnode1(k).gt.nnode2(k)) nnode2(k)=nnode1(k)
      if(nnode1(k).le.3) go to 4720
      write(lunerr,19472) nnode1(k),k,id,dirnam(i),j
      errf=.true.
 4720 nnode1(k)=0



  473 continue
  478 continue
  479 continue
      do 489 k=1,mrec
      secrec=arec(k)
      if(delsec(secrec)) go to 489



      call rtctrs
      if(secio.ne.0) go to 9490
      if(alink.lt.1.or.alink.gt.mxtca) go to 9400
      sectm2=nalink(alink)
      id=lbid(rlink)
      imode=1
      if(id.ge.500.and.id.le.599) imode=2
      if(id.ge.398.and.id.le.399) imode=2
      if(lbfc(alink).eq.11.and.imode.eq.1) go to 481
      if(lbfc(alink).ne.11.and.imode.eq.2) go to 482
      sectm4=0
      go to 484
  481 write(lunerr,19481) id
      errf=.true.
      sectm4=1
      go to 484
  482 write(lunerr,19482) id
      errf=.true.
      sectm4=1
  484 do 485 i=1,mxcon
      if(secic(i).lt.1) secic(i)=9
      if(secoc(i).lt.1) secoc(i)=9
      secsic(i)=order(1,i,k)
      secsoc(i)=order(2,i,k)
  485 continue
      secpos(2)=100.0
      secpos(1)=0.0
      if(rev(k)) go to 486
      secpos(2)=0.0
      secpos(1)=100.0
  486 call wtctrs
      if(secio.ne.0) go to 9490
      arlink(k)=0
      arec(k)=0
      aalink(k)=0
      rev(k)=.false.
      do 488 i=1,2
      do 487 j=1,mxcon
      order(i,j,k)=0
      ac(i,j,k)=0
  487 continue
  488 continue
  489 continue
      if(endf) go to 499
      do 497 i=1,mxtca
      nalink(i)=0
  497 continue
      secrec=nrec
      go to 400
  498 endf=.true.
      go to 409
  499 close(seclun)
      fnamo='trinfo'
      print *, fnamo
c
c     open trinfo data file
c
      fnamo='info/arc.dir'
      open(unit=lunin,convert='big_endian',file=fnamo,access=
     *'direct',form='unformatted',recl=95,status='old',err=503)
      recnum=1
      print *, fnamo
      go to 504
  503 fnamo='..\covs\info/arc.dir'
      open(unit=lunin,convert='big_endian',file=fnamo,access=
     *'direct',form='unformatted',recl=95,status='old',err=9000)
      recnum=1
      print *, fnamo
  504 read(lunin,rec=recnum,err=9500) extnam,intnam,idat
      i=index(extnam,'TRINFO')
      if (i.gt.0) go to 505
      recnum=recnum+1
      go to 504
  505 write(fnamo,20505) (intnam(i),i=4,7)
      close(lunin)
c
c     read trinfo and insert start and stop sphere names
c
  509 open(unit=lunin,convert='big_endian',file=fnamo,access=
     *'direct',form='unformatted',recl=48,status='old',err=9000)
      recnum=1
      print *, fnamo
  510 read(lunin,rec=recnum,iostat=istat)
     *route,dir,con,id,rco,rmode,start,stop,via,org,use,
     *am1,op1,pm1,nt1,nh1,am2,op2,pm2,nt2,nh2,
     *am3,op3,pm3,nt3,nh3,am4,op4,pm4,nt4,nh4,
     *am5,op5,pm5,nt5,nh5
      if(istat.ne.0) go to 599
      if(route.lt.1.or.route.gt.999) go to 9501
      if(dir.lt.1.or.dir.gt.2) go to 9501
      if(con.lt.1.or.con.gt.mxcon) go to 9501
      isph=rtfrno(route,dir,con)
      iisph=isph/100
      jsph=rttono(route,dir,con)
      jjsph=jsph/100
      if(iisph.eq.jjsph) go to 511
      if(iisph.eq.14) go to 512
      if(iisph.eq.19) go to 512
      if(jjsph.eq.14) go to 513
      if(jjsph.eq.19) go to 513
  511 iisph=isph
      jjsph=jsph
      go to 514
  512 iisph=isph
      if(jjsph.eq.14) go to 513
      if(jjsph.eq.19) go to 513
      go to 514
  513 jjsph=jsph
  514 if(iisph.lt.1.or.iisph.gt.1999) iisph=20
      start=sphnam(iisph)
      if(jjsph.lt.1.or.jjsph.gt.1999) jjsph=20
      stop=sphnam(jjsph)
      write(lunin,rec=recnum,iostat=istat)
     *route,dir,con,id,rco,rmode,start,stop,via,org,use,
     *am1,op1,pm1,nt1,nh1,am2,op2,pm2,nt2,nh2,
     *am3,op3,pm3,nt3,nh3,am4,op4,pm4,nt4,nh4,
     *am5,op5,pm5,nt5,nh5
      if(istat.ne.0) go to 599
      recnum=recnum+1
      go to 510
  599 close(lunin)
c
c     re-open nat file 
c 
      lnode=mxtap+1
      ltap=1
      natrec=1
      fnamo='tcoved.nat'
      print *, fnamo
  601 call rtcn
      if(natio.ne.0) go to 609
c     fill in missing node numbers
c
      nattrn=lbtrn(natlb)
      if(nattrn.lt.mxtrn) go to 603
      do 602 inode=lnode,mxtrn
      if(trnlb(inode).gt.0) go to 602
      lbtrn(natlb)=inode
      trnlb(inode)=natlb
      lnode=inode
      nattrn=inode
      go to 603
  602 continue 
      lbtrn(natlb)=inode
      trnlb(inode)=natlb
      lnode=inode
      nattrn=inode
c     go to 9602
c
c     fill in missing tap numbers

c
  603 if(natlb.eq.470) 
     *print *, natlb,nattap,lbtap(natlb)
      nattap=lbtap(natlb)
      if(nattap.lt.mxtap) go to 608
      do 604 itap=ltap,mxtap
      if(taplb(itap).gt.0) go to 604
      lbtap(natlb)=itap
      taplb(itap)=natlb
      ltap=itap
      nattap=itap
      go to 608
  604 continue
      go to 9604
  608 call wtcn
c      print *,natrec,lbtap(natlb),nattap,lbtrn(natlb),nattrn
      if(natio.ne.0) go to 609
      natrec=natrec+1
      go to 601
  609 close(lunin)
      go to 9999
 9000 write(lunerr,19000) fnamo
      errf=.true.
      go to 9999
 9010 write(lunerr,19010) isph
      errf=.true.
      go to 9999
 9020 write(lunerr,19020) inode,jnode
      errf=.true.
      go to 9999
 9100 write(lunerr,19100) aatlb,aatarn(2),aatarn(1)
      errf=.true.
      go to 9999
 9200 write(lunerr,19200) natlb,natsph
      errf=.true.
      go to 9999
 9300 write(lunerr,19300) ratlb,ratid
      errf=.true.
      go to 9999
 9400 write(lunerr,19400) ratlb,ratid
      errf=.true.
      go to 9999
 9401 write(lunerr,19401) 
      errf=.true.
      go to 9999
 9409 write(lunerr,19409) arlink(1),id
      errf=.true.
      go to 470
 9412 write(lunerr,19412) id,dirnam(i),j
      errf=.true.
      go to 470
 9420 write(lunerr,19420) id,dirnam(i),j
      errf=.true.
      go to 470
 9421 write(lunerr,19421) id,dirnam(i),j
      errf=.true.
      go to 470
 9422 write(lunerr,19422) id,dirnam(i),j
      errf=.true.
      go to 470
 9490 write(lunerr,19490) ratlb,ratid
      errf=.true.
      go to 9999
 9500 write(lunerr,19500)
      errf=.true.
      go to 9999
 9501 write(lunerr,19501) recnum,route,dir,con
      errf=.true.
      go to 9999
 9602 write(lunerr,19602) 
      errf=.true.
      go to 9999
 9604 write(lunerr,19604) 
      errf=.true.
      go to 9999
10010 format(i6,1x,a24)
10020 format(2i10)
19000 format('missing file ...',a128)
19010 format('data problem with sphere.name ...',i10)
19020 format('data problem with posttr.in ...',2i10)
19100 format('data problem with tcoved.aat ...',3i10)

19200 format('data problem with tcoved.nat ...',2i10)
19300 format('data problem with tcoved.rat ...',2i10)
19400 format('data problem with tcoved.sec ...',2i10)
19401 format('increase size of mxrec')
19409 format('id problem with route ...',2i10)
19412 format('more than six connecting links on route  ...',
     *i3,1x,a8,i2)
19420 format('first link not indicated for  ...',i3,1x,a8,i2)
19421 format('last link not indicated for  ...',i3,1x,a8,i2)
19422 format('unable to find first node for  ...',i3,1x,a8,i2)
19429 format('start/end node not tap for ',
     *'route ... ',i3,' dir ... ',i1,' config ... ',i1)
19451 format('break in route for  ...',i3,1x,a8,i2)
19472 format('node used more than 3 times on route  ... ',
     *i3,i6,i4,1x,a8,i2)


19481 format('bus route on rail line ...',i5,
     *'(select sections with tmp4 = 1)')
19482 format('rail route on street ...',i5,
     *'(select sections with tmp4 = 1)')
19490 format('data problem when rereading tcoved.sec ...',2i10)
19500 format('arcdr9 file not found')
19501 format('data problem with trinfo ...',4i10)
19602 format('increase size of mxtrn')
19604 format('increase size of mxtap')
20409 format('processing route  ...',i10)
20505 format('info\arc',4a1,'.dat')
20998 format(17i5)
 9999 if(errf) close(lunerr,status='keep')
      if(.not.errf) close(lunerr,status='delete')
      end
