c adding external zone delay cost (extadd(mxext),extcst) to generalized cost field
c station 1, adding 4500 cents to abcst and bacst
c station 10 adding 1500 cents to abcst and bacst
c station 10 adding 3000 cents to abcst and bacst - try upping to 3000 to increase trip lengths
c station 11,12 adding 3000 cents to abcst and bacst
c spd of hov is 5mph > sov spd  
c 1/25/05 	add code to automaticly generate turn prohibitors between two ramps if
c 		intersect with none ramps
c 2/2/05    cap fwy capacity to certain threashhold
c     hwycov to transcad conversion program
c
      parameter (mxramp=2500,mxrlk=1000,mxrt=500,mxnrt=10,mxzrt=50,
     *mxradt=12999,mxadt=9999)
      common /reccom/ srec,rlkarr
      include 'sandag.inc'
      include 'tcov.inc'
      include 'zone.inc'
c
c     program variables
c
      logical uniq(mxhca),stop,inff,used,zoneo(mxzn),hov3,toll,hot3,
     *first,endf,entry(mxhcn),exit(mxhcn),tsm,new,nused(mxhcn)
      integer*2 pfc(15),pjur(6),psph(20),fcarr(mxhca),fwysta(mxid),
     *cnnode(mxhcn),snode(mxhcn),adtmtr(9999),xang(2,mxhca,3),angle(3),
     *mjur(mxfc),mway(mxfc),mspd(mxfc),mmed(mxfc),mln(mxfc),mlf(mxfc,8),
     *gcsig(4,9,9),gcstp4(9,9),gcstp2(9),mtspd(mxfc),
     *kdist(mxrlk),jur(mxrlk),med(mxrlk),tfln(3,mxrlk),lfcap(10),
     *tfaux(mxrlk),hov(mxrlk),tfcnt(mxrlk),
     *tftl(mxrlk),tfrl(mxrlk),tfll(mxrlk),tfgc(mxrlk),
     *tfotm(mxrlk),tfptm(mxrlk),xan(2),xbn(2)
      integer*4 n1arr(mxhca),n2arr(mxhca),tpxy(mxtpn),
     *xapp(mxhcn),xstop(mxhcn),xdapp(mxhcn),
     *rlkarr(mxhca),srec(mxhca),luno,hovfwy(mxid),
     *nid(mxid),krno(mxrlk),kid(mxrlk),fc(mxrlk),tplb(3,mxrt),
     *spd(mxrlk),tfcap(3,mxrlk),znspd(3),
     *aatxfc(2),zonety(mxzn),aatmln(2),
     *lbid(mxhca),lbfc(mxhca),lbrt(mxhca),lid,xlb(2,mxhca,3),hrpk(24),
     *nodemrt(mxhcn),nodert(mxtpn,mxnrt),
     *tdzmrt(mxtdz),tdzrt(mxtdz,mxzrt),
     *tdzrtnode(mxtdz,mxzrt),tdzrtid(mxtdz,mxzrt),taztdz(mxzn)
      real xtdz(mxtdz),ytdz(mxtdz),xnode(mxtpn),ynode(mxtpn),
     *tdzrtdist(mxtdz,mxzrt),adtvols(29),adthst(mxradt)
      real rsafac(6),bordel(5,3,2),fwycap,auxcap,extadd(mxext),
     *dsum(mxid),tisum(mxid),tasum(mxid),remtoll(3),
     *stappf(3,2,mxbsta),hrpct(24),pkmax(3),
     *frmile(17),flmile(3,17),fcmile(17),frhour(17),
     *jrmile(3,10),jlmile(3,10),jcmile(3,10),jrhour(3,10)
      character*1 intnam(8),c1
      character*32 extnam
      character*150 idat
      character*1 nln(10),nxl(10)
      character*2 cdir
      character*3 njur(6),nmed(3)
      character*4 nhov(4)
      character*6 ncnt(9)
      character*7 nway(2)
      character*11 nfc(12)
      character*12 nrdir(4)
      character*17 nmjur(10)
      character*19 nminc(3)
      character*20 nmarr(mxhca),kname(mxrlk),name1,nmhov(4)
      data angle/0,-90,90/
      data first/.true./,stop/.false./,lnode/5000/
      data znspd/20,30,45/,xan/2,1/,xbn/1,2/
      data hrpk/1,1,1,1,1,1,2,2,2,1,1,1,1,1,1,3,3,3,1,1,1,1,1,1/
      data mspd/65,45,40,35,30,40,35,65,30,30,50,25,25,30/
      data mtspd/55,45,40,35,30,40,35,55,30,30,50,25,25,30/
      data mmed/2,2,2,1,1,1,1,1,1,1,1,1,1,1/
      data mway/1,2,2,2,2,2,2,1,1,2,2,2,2,2/
      data mln/4,3,2,2,1,1,1,1,1,1,1,1,1,1/
      data mjur/1,5,5,6,6,6,6,1,1,6,6,6,6,1/
      data extadd/4500,0,0,0,0,0,0,0,0,3000,3000,3000/
      data ncnt/'0-none','1-sig ','2-4stp','3-2stp','4-mtr-',
     *'5-mtr+','6-lrt ','7-toll','8-prev'/
      data nfc/'1:freeway  ','2:prime art',
     *         '3:major art','4:collector', 
     *         '5:local col','6:rural col',
     *         '7:local    ','8:fwy-fwy  ',
     *         '9:ramp     ','10:access  ',
     *         '  sub-total','   total   '/
      data nminc/
     *'Incorporated Cities',
     *'Unincorporated Area',
     *'Regional Total      '/
      data nmjur/
     *'Caltrans Freeways',
     *'Caltrans HOVs    ',
     *'Caltrans Highways',
     *'Caltrans Ramps   ',
     *'CMP Routes       ',
     *'Non-CMP RSA Roads',
     *'Other RA Roads   ',
     *'Other Major Roads',
     *'Other Local Roads',
     *'Total            '/	
      data njur/'1-s','2-c','3-r','4-r','5-m','6-l'/,rsafac/6*1.0/
      data nhov/'mix','hov2','hov3','toll'/
      data nln/'0','1','2','3','4','5','6','7','8','n'/
      data nxl/'0','1','2','3','4','5','6','f','x','n'/
      data nmed/'1:n','2:m','3:c'/
      data nrdir/'1-southbound','2-eastbound ','3-northbound',
     *'4-westbound '/
      data nway/'one-way','two-way'/
      data nmhov/'sbhov.sbfwy','nbhov.nbfwy',
     *'ebhov.ebfwy','wbhov.wbfwy'/
      data lfcap/250,250,150,100,100,100,100,100,100,0/
      data gcstp2/50,50,75,100,125,125,150,150,100/
      data lunin,lunerr,lunpr,luno,aatlun,natlun,lunsum,patlun,
     *seclun,luninf,lunsig,tatlun,lunxy,lunsta
     */11,12,13,14,15,16,17,18,19,20,21,22,23,24/
      data rlkarr/mxhca*99999999/
      data uniq/mxhca*.true./
      eject = ' '
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
      call dattim
c
c     create output files
c
      open (unit=lunerr,file='tchc.err')
      open (unit=lunsta,file='tchc-sta.prn')
      open (unit=luninf,file='tchc.info')
      open (unit=lunpr,file='tchc.pr')
      open (unit=luno,file='turns.csv')
      write(luno,20920)
      open (unit=lunsum,file='tchc.sum')
      open (unit=lunxy,file='zc.gen')
c
c     read program control
c
      psph(1)=9999
      pjur(1)=9999
      pfc(1)=1
      pfc(2)=2
      pfc(3)=3
      fnamo='tchc.in'
      open (unit=lunin,file=fnamo,status='old',err=9)
      read(lunin,10001) (psph(i),i=1,20)
      read(lunin,10001) (pjur(i),i=1,6)
      read(lunin,10001) (pfc(i),i=1,15)
      close(lunin)
c
c     read printout heading
c
    9 fnami='head'
      call opendata(lunin)
      if(istat.ne.0) go to 9000
      read(lunin,10003) cheadi
      call cheadr
      close(lunin)
c
c     read analysis year
c
      fnamo='hwyyear'
      open (unit=lunin,file=fnamo,status='old',err=11)
      go to 12
   11 fnamo='year'
      open (unit=lunin,file=fnamo,status='old',err=9000)
   12 icol=1
      read(lunin,10000,iostat=istat) idata,adata
      if(istat.ne.0) go to 9001
      call sd_getdat(idata,adata,icol,iyear,i2,i3,lunerr)
      close(lunin)
      tsm=.false.
      if(iyear.gt.2015) tsm=.true.
      open (unit=lunin,file=fnamo,status='old',err=13)
      tsm=.false.
   13 close(lunin)
      fnamo='notsm'
      if(iyear.lt.2010) go to 20
      if(.not.tsm) go to 20
      jyear=min(iyear,2020)
      xfac=1.0+(jyear-2010)*0.01
      do 14 i=1,4
      rsafac(i)=xfac
   14 continue
c    read arterial traffic count
c
   20 if(iyear.eq.2010) iadtyr = iyear-1982+1
      
	fnami='../data/histfill1982_2010'
      call opendata(lunin)
      if(istat.ne.0) go to 9000
      read(lunin,10009,iostat=istat) c1
      if(istat.ne.0) go to 9000
   16 read(lunin,10033,iostat=istat) aatadt,adtvols
      if(istat.ne.0) go to 18
      if(aatadt.lt.1.or.aatadt.gt.mxadt) go to 9030
      adthst(aatadt)=nint(adtvols(iadtyr)/100.0)
      go to 16
   18 close(lunin)
      
c
c     flag cbd zones (zonety=1)
c
      icol=2
      fnamo='zone.avrzone'
      open (unit=lunin,file=fnamo,status='old',err=21)
      go to 22
   21 fnamo='../data/zone.avrzone'
      open (unit=lunin,file=fnamo,status='old',err=9000)
   22 read(lunin,10000,iostat=istat) idata,adata
      if(istat.ne.0) go to 23
      call sd_getdat(idata,adata,icol,izone,iavr,i3,lunerr)
      if(izone.lt.1.or.izone.gt.mxzn) go to 9001
      if(iavr.eq.1) zonety(izone)=1
      go to 22
   23 close(lunin)
      call ozonep
      if(patio.ne.0) go to 9000
      patrec=2 
   24 call rzonep
      if(patio.ne.0) go to 29
      if(patzon.lt.1.or.patzon.gt.mxzn) go to 9001
      mzone=max(mzone,patzon)
      xacres=patare/43560.
      if(zonety(patzon).lt.1) then
      zonety(patzon)=1
      if(xacres.gt.200) zonety(patzon)=2
      if(xacres.gt.5000) zonety(patzon)=3
      endif
      patrec=patrec+1
      go to 24
   29 close(patlun)
c
c     gcdata
c
   30 fnamo='gc'
      open (unit=lunin,file=fnamo,status='old',err=31)
      go to 33
   31 fnamo='../data/gc'
      open (unit=lunin,file=fnamo,status='old',err=9000)
   33 do 36 ifc=1,9
      read(lunin,10030,iostat=istat) (gcsig(4,ifc,itofc),itofc=1,9)
      if(istat.ne.0) go to 9000
   36 continue
      do 37 ifc=1,9
      read(lunin,10030,iostat=istat) (gcsig(3,ifc,itofc),itofc=1,9)
      if(istat.ne.0) go to 9000
   37 continue
      do 38 ifc=1,9
      read(lunin,10030,iostat=istat) (gcsig(2,ifc,itofc),itofc=1,9)
      if(istat.ne.0) go to 9000
   38 continue
      do 39 ifc=1,9
      read(lunin,10030,iostat=istat) (gcstp4(ifc,itofc),itofc=1,9)
      if(istat.ne.0) go to 9000
   39 continue
      close(lunin)
c
c     border delay times
c
      fnamo='border.del'
      open (unit=lunin,file=fnamo,status='old',err=41)
      go to 43
   41 fnamo='../data/border.del'
      open (unit=lunin,file=fnamo,status='old',err=9000)
   43 do 44 ibord=1,5
      read(lunin,10040,iostat=istat)
     *(bordel(ibord,ipk,1),bordel(ibord,ipk,2),ipk=1,3)
      if(istat.ne.0) go to 9000
   44 continue
      close(lunin)
c
c     read freeway-hov lane connections
c
      icol=2
      do 59 ifile=1,4
      fnami=nmhov(ifile)
      call opendata(lunin)
      if(istat.ne.0) go to 9000
   51 read(lunin,10000,iostat=istat) idata,adata
      if(istat.ne.0) go to 58
      call sd_getdat(idata,adata,icol,ihov,ifwy,i3,lunerr)
      if(ifwy.lt.1.or.ifwy.gt.mxid) go to 9001
      if(ihov.lt.1.or.ihov.gt.mxid) go to 51
      hovfwy(ihov)=ifwy
      go to 51
   58 close(lunin)
   59 continue
c
c     adt links with new meters
c
      fnamo='adtlk.mtr'
      open (unit=lunin,file=fnamo,status='old',err=61)
      go to 63
   61 fnamo='../data/adtlk.mtr'
      open (unit=lunin,file=fnamo,status='old',err=9000)
   63 read(lunin,10060,iostat=istat) aatadt,cdir
      if(istat.ne.0) go to 69
      idir=9
      i=index(cdir,'SB')
      if(i.gt.0) idir=1
      i=index(cdir,'EB')
      if(i.gt.0) idir=2
      i=index(cdir,'NB')
      if(i.gt.0) idir=3
      i=index(cdir,'WB')
      if(i.gt.0) idir=4
      adtmtr(aatadt)=idir
      go to 63
   69 close(lunin)
      fnamo='sta.hrpct'
      open (unit=lunin,file=fnamo,status='old',err=71)
      go to 73
   71 fnamo='../data/sta.hrpct'
      open (unit=lunin,file=fnamo,status='old',err=9000)
      read(lunin,10009,iostat=istat) c1
      if(istat.ne.0) go to 9000
   73 read(lunin,10070,iostat=istat)
     *ista,idir,hrpct
      if(istat.ne.0) go to 79
      if(ista.lt.1.or.ista.gt.mxbsta) go to 9070
      do ipk=1,3
      pkmax(ipk)=0.0
      end do
c     
c     for peak periods use max hour
c
      do ihr=1,24
      ipk=hrpk(ihr)
      if(ipk.ne.1) 
     *pkmax(ipk)=max(pkmax(ipk),hrpct(ihr))
      end do
c
c     for off-peak period use mid-day hour
c
      do ihr=11,13
      ipk=hrpk(ihr)
      pkmax(ipk)=max(pkmax(ipk),hrpct(ihr))
      end do
      do ipk=1,3
      stappf(ipk,idir,ista)=1.0/pkmax(ipk)
      end do
      go to 73
   79 close(lunin)
c     commented out until we decide on better way of handling peak spreading
c      if(iyear.lt.2020) go to 89
c      fnami='notdm'
c      call opendata(lunin)
c      if(istat.ne.0) go to 80
c      close(lunin)
c      go to 89
c   80 do idir=1,2
c      do ipk=2,3
c      do ista=1,999
c      stappf(ipk,idir,ista)=3.0
c      end do
c      end do
c      end do
   89 write(0,11002)
      fnami='../data/taz.tdz'
      call opendata(lunin)
	if(istat.ne.0) go to 9000
   95 read(lunin,10000,iostat=istat) idata,adata
      if(istat.ne.0) go to 96
      call sd_getdat(idata,adata,icol,izone,itdz,i3,lunerr)
      if(izone.lt.1.or.izone.gt.mxzn) go to 9001
      taztdz(izone)=itdz
      go to 95
   96 close(lunin)
      fnamo='../covs/tdzs/pat.adf'
      open(unit=patlun,convert='big_endian',file=fnamo,access=
     *'direct',form='unformatted',recl=12,status='old',err=9000)
       patrec=2 
   98 call rzonep	
      if(patio.ne.0) go to 99
      xtdz(patzon)=float(patx)
      ytdz(patzon)=float(paty)	
      patrec=patrec+1
      go to 98
   99 close(patlun)
      call ohca
      if(aatio.ne.0) go to 9000
      aatrec=1
      mid=0
c
c     read hwycov aat and save data.
c
  100 call rhca
      if(aatio.ne.0) go to 129
      if(aathov(1).eq.3.and.(hov3.eq..false..or.hot3.eq..false.)) then
       hov3=.true. !flag whether any hov3+ facilities exist
       if(aattoll(1,1).gt.0.and.aatln(1,1,1).lt.9) hot3=.true. !flag whether any hot3+ facilities exist
       if(aattoll(2,1).gt.0.and.aatln(2,1,1).lt.9) hot3=.true. !flag whether any hot3+ facilities exist
       if(aattoll(3,1).gt.0.and.aatln(3,1,1).lt.9) hot3=.true. !flag whether any hot3+ facilities exist
      endif
      if(aathov(1).eq.4) toll=.true. !flag whether any toll facilities exist	 
      rlkarr(aatrec)=aatrt*1000+aatlk
      lbid(aatlb)=aatid
      lbfc(aatlb)=aatfc(1)
      lbrt(aatlb)=aatrt 
      if(aatfc(1).eq.1) fwysta(aatid)=aatsta 
      do idir=1,2 !loop through each direction for link
       in=aatarn(idir)
       itpn=aattpn(idir)

        if(in.lt.1.or.in.gt.mxhcn) then !check for valid node number
         write(lunerr,19100) aatrec,in,aatnm,aatxnm(idir)
         errf=.true.
         aatarn(idir)=mxhcn
         stop=.true.
        endif

       if(in.gt.mnode) mnode=in
       if(aattpn(idir).gt.mtpn) mtpn=aattpn(idir)
       if(aatfc(1).le.9) then  !check for non-zone connector links
        xapp(in)=xapp(in)+1
        if(aatcnt(idir,1).eq.3) xstop(in)=xstop(in)+1
        if(aatway(1).eq.2.or.idir.eq.1) xdapp(in)=xdapp(in)+1

c       save route numbers at node

        mnrt=nodemrt(itpn)
        new=.true.
         if(mnrt.gt.0) then !if node-route connenctions already exist then find out if this is a new one
          do irt=1,mnrt !loop through routes at this node
           if(aatrt.eq.nodert(itpn,irt)) new=.false.
          end do !end of old route loop
         end if !end of old connection check

         if(new) then !if this is a new route connection then add to matrix 
          mnrt=mnrt+1
           if(mnrt.gt.mxnrt) then
            write(lunerr,19104)
            errf=.true.
            go to 9999
           endif

          nodemrt(itpn)=mnrt
          nodert(itpn,mnrt)=aatrt
        end if !end of new route check
       end if !end of check for non-zone connector links
      end do ! end of directional loop

      if(aatfc(1).lt.1.or.aatfc(1).gt.mxfc) go to 128
      srec(aatrec)=aatrec
      nmarr(aatrec)=aatnm
      n1arr(aatrec)=aatarn(1)
      n2arr(aatrec)=aatarn(2)
      if(aatfc(1).eq.10) go to 120
      entry(aatarn(1))=.true.
      exit(aatarn(2))=.true.
      fcarr(aatrec)=aatfc(1)

c     for HOV lanes sum up unadjusted and adjusted times on adjacent main lanes

      if(aatfc(1).eq.1) then
       do id=1,mxid
        if(hovfwy(id).eq.aatid) then
        xdist=aatlen/5280.0
        dsum(id)=dsum(id)+xdist
        if(aatspd(1).lt.1.or.aatspd(1).gt.75) aatspd(1)=mspd(aatfc(1))
        if(aattspd(1).lt.1.or.aattspd(1).gt.75) 
     *    aattspd(1)=mtspd(aatfc(1))
        ispd=aatspd(1)
        tisum(id)=tisum(id)+xdist/float(ispd)
        if(aatasp.gt.0.and.aatasp.le.75) ispd=aatasp
        tasum(id)=tasum(id)+xdist/float(ispd)
        end if
       end do
      end if
  107 if(aatway(1).eq.1) go to 128
      entry(aatarn(2))=.true.
      exit(aatarn(1))=.true.
      go to 128
  120 fcarr(aatrec)=aatfc(1)
      go to 128
  128 aatrec=aatrec+1
      if(aatrec.le.mxhca) go to 100
      write(lunerr,19128) 
      errf=.true.
      go to 9999
  129 if(stop) go to 9999
      naat=aatrec-1
      write(0,11004) naat
      if(stop) go to 9999
      do id=1,mxid
      denom=tisum(id)
      if(denom.gt.0.0)
     *tisum(id)=dsum(id)/denom
      denom=tasum(id)
      if(denom.gt.0.0)
     *tasum(id)=5.0+dsum(id)/denom
      end do
      fnamo='info/arc.dir'
      open(unit=lunin,convert='big_endian',file=fnamo,access=
     *'direct',form='unformatted',recl=95,status='old',err=9000)
      infrec=1
  132 read(lunin,rec=infrec,iostat=istat) extnam,intnam,idat
      if(istat.ne.0) go to 9130
      i=index(extnam,'HWYCOV.TRN')
      if(i.gt.0) go to 133
      infrec=infrec+1
      go to 132
  133 write(fnamo,20130) (intnam(i),i=4,7)
      close(lunin)
      open(unit=tatlun,convert='big_endian',file=fnamo,access=
     *'direct',form='unformatted',recl=7,status='old',err=9000) 
c
c     preset arcs on same route as through move
c
      tatrec=1
  134 call rhct
      if(tatio.ne.0) go to 139
      if(fcarr(tatlb2).eq.10) go to 138
      if(lbrt(tatlb1).ne.lbrt(tatlb2)) go to 138
      if(tatlb.eq.n1arr(tatlb1)) then
      idir=1
      else
       if(tatlb.eq.n2arr(tatlb1)) then
       idir=2
       else
       go to 9140
       endif
      endif
        ipos=1
      if(xlb(idir,tatlb1,ipos).gt.0) go to 138
      xlb(idir,tatlb1,ipos)=tatlb2
      xang(idir,tatlb1,ipos)=iang
  138 tatrec=tatrec+1
      go to 134
  139 tatrec=1
  140 call rhct
      if(tatio.ne.0) go to 149
      if(fcarr(tatlb2).eq.10) go to 148
      if(lbrt(tatlb1).eq.lbrt(tatlb2)) go to 148
      iang=nint(tatang)
      if(tatlb.eq.n1arr(tatlb1)) then
      idir=1
      else
       if(tatlb.eq.n2arr(tatlb1)) then
       idir=2
       else
       go to 9140
       endif
      endif
      ipos=2
      if(iang.gt.0) ipos=3
      if(iang.gt.-45.and.iang.le.45) ipos=1
      if(tatlb1.eq.22011) then
      print *,tatlb1
      endif
      if(xlb(idir,tatlb1,ipos).gt.0) go to 141
      xlb(idir,tatlb1,ipos)=tatlb2
      xang(idir,tatlb1,ipos)=iang
      go to 148
  141 idiff1=abs(angle(ipos)-xang(idir,tatlb1,ipos))
      idiff2=abs(angle(ipos)-iang)
      if(idiff1.gt.idiff2) go to 144
      mdiff=999
      mpos=0
      do 142 jpos=1,3
      if(jpos.eq.ipos) go to 142
      idiff=abs(angle(jpos)-iang)
      if(idiff.lt.mdiff) then
      mdiff=idiff
      mpos=jpos
      endif
  142 continue
      if(xlb(idir,tatlb1,mpos).gt.0) go to 143
      xlb(idir,tatlb1,mpos)=tatlb2
      xang(idir,tatlb1,mpos)=iang
      go to 148
  143 idiff1=abs(angle(mpos)-xang(idir,tatlb1,mpos))
      idiff2=abs(angle(mpos)-iang)
      if(idiff2.gt.idiff1) go to 148
      xlb(idir,tatlb1,mpos)=tatlb2
      xang(idir,tatlb1,mpos)=iang
      go to 148
  144 kang=xang(idir,tatlb1,ipos)
      klb=xlb(idir,tatlb1,ipos)
      xlb(idir,tatlb1,ipos)=tatlb2
      xang(idir,tatlb1,ipos)=iang
      mdiff=999
      mpos=0
      do 145 jpos=1,3
      if(jpos.eq.ipos) go to 145
      idiff=abs(angle(jpos)-kang)
      if(idiff.lt.mdiff) then
      mdiff=idiff
      mpos=jpos
      endif
  145 continue
      if(xlb(idir,tatlb1,mpos).gt.0) go to 146
      xlb(idir,tatlb1,mpos)=klb
      xang(idir,tatlb1,mpos)=kang
      go to 148
  146 idiff1=abs(angle(mpos)-xang(idir,tatlb1,mpos))
      idiff2=abs(angle(mpos)-kang)
      if(idiff2.gt.idiff1) go to 148
      xlb(idir,tatlb1,mpos)=klb
      xang(idir,tatlb1,mpos)=kang
  148 tatrec=tatrec+1
      go to 140
  149 tatrec=1
  150 call rhct
      if(tatio.ne.0) go to 159
      if(fcarr(tatlb2).ne.10) go to 158
      iang=nint(tatang)
      if(tatlb.eq.n1arr(tatlb1)) then
      idir=1
      else
       if(tatlb.eq.n2arr(tatlb1)) then
       idir=2
       else
       go to 9140
       endif
      endif
      ipos=2
      if(iang.gt.0) ipos=3
      if(iang.gt.-45.and.iang.le.45) ipos=1
      if(xlb(idir,tatlb1,ipos).gt.0) go to 158
      xlb(idir,tatlb1,ipos)=tatlb2
      xang(idir,tatlb1,ipos)=iang
  158 tatrec=tatrec+1
      go to 150
c
c     sort records end-to-end by name.
c
  159 write(0,11006)
      call recsrt
      do 398 i=1,naat
      if(srec(i).ne.0) go to 398
      write(0,19399) i
      stop=.true.
  398 continue
      if(stop) go to 9999
      write(0,11008)
c
c     rhca nat file
c
      write(0,11010)
      call ohcn
      natrec=1


c
c     read node point attribute table saving arrays of node data
c     for node
c
  410 call rhcn
      if(natio.ne.0) go to 419
      if(nattpn.lt.1.or.nattpn.gt.mxtpn) go to 9000
      xnode(nattpn)=float(natx)
      ynode(nattpn)=float(naty)
      nattmp=0
      if(natlb.gt.0.and.natlb.le.mnode) go to 412
      nattmp=1
      write(lunerr,19411) natlb,natxnm(1),natxnm(2)
      errf=.true.
      go to 418
  412 snode(natlb)=natsph
      cnnode(natlb)=natcnt(1)
      if(xdapp(natlb).gt.4) xdapp(natlb)=4
      if(xdapp(natlb).lt.2) xdapp(natlb)=2
      if(nattpn.gt.0.and.nattpn.le.mxtpn) go to 413
      nattmp=2
      write(lunerr,19412) natlb,nattpn,natxnm(1),natxnm(2)
      errf=.true.
      go to 418
  413 jjxy=natx+naty
      if(tpxy(nattpn).gt.0) go to 414
      tpxy(nattpn)=jjxy
      go to 418
  414 if(tpxy(nattpn).eq.jjxy) go to 415
      nattmp=3
      write(lunerr,19413) natlb,nattpn,natxnm(1),natxnm(2)
      errf=.true.
      go to 418
  415 if(xapp(natlb).lt.5) go to 416
      nattmp=4
      write(luninf,19414) natlb,natxnm(1),natxnm(2)
      inff=.true.
      go to 418
  416 if(natcnt(1).ne.3) go to 418
      idiff=xapp(natlb)-xstop(natlb)
      if(idiff.lt.3) go to 418
      nattmp=5
      write(luninf,19415) natrec,natxnm(1),natxnm(2)
      inff=.true.
  418 natiuc=(natcnt(1)+1)*10+1
c
c     insert hnode into nodeid for exporting to transCAD 
      natid=nattpn
      call whcn
      natrec=natrec+1
      go to 410
  419 close(natlun)
c
c     reread aat for final time in route-link order.
c
      iaat=1
      iline=99
      ipage=1
      isec=20000
      secdst=0.0
  500 irtno=rlkarr(iaat)/1000
      ilkno=rlkarr(iaat)-irtno*1000
      aatrec=srec(iaat)
      krec=0
      isec=isec+1
      secdst=0.0
  501 call rhca
      if(aatio.ne.0) go to 9000
      if(aatarn(2).lt.1.or.aatarn(2).gt.mxhcn) aatarn(2)=mxhcn
      if(aatarn(1).lt.1.or.aatarn(1).gt.mxhcn) aatarn(1)=mxhcn
	if (aatfc(1).gt.1.and.aatfc(1).lt.8)then
	   aatvol=0
	   if (aatadt.gt.0.and.adthst(aatadt).gt.0)aatvol=adthst(aatadt)
	endif
      extcst=0
      isph=snode(aatarn(2))/100
      jsph=snode(aatarn(1))/100
      inc=1 
      if(isph.eq.19.or.jsph.eq.19) inc=2 
      ijur=aatjur(1)+3
      if(ijur.eq.4) ijur=3
      if(ijur.eq.3.and.aatfc(1).eq.1) ijur=1
      if(aatfc(1).eq.1.and.(aathov(1).eq.2.or.aathov(1).eq.3)) ijur=2
      if(ijur.eq.3.and.aatfc(1).eq.8) ijur=4
      if(ijur.eq.3.and.aatfc(1).eq.9) ijur=4
      if(nid(aatid).eq.0) go to 504
      do 502 i=1,mxid
      if(nid(i).eq.0) go to 504
  502 continue
      go to 9502
  503 aatid=i
  504 nid(aatid)=nid(aatid)+1
      aattmp(1)=0
      aattmp(2)=0
      if(aatfc(1).eq.10) go to 511
      if(.not.entry(aatarn(2))) go to 505
      if(.not.exit(aatarn(2))) go to 505
      if(.not.entry(aatarn(1))) go to 505
      if(.not.exit(aatarn(1))) go to 505
      go to 510
  505 write(luninf,19505)
     *aatid,aatnm,aatxnm(2),aatxnm(1),aatarn(2),aatarn(1)
      aattmp(1)=1
      inff=.true.
  510 if(aatfc(1).ge.1.and.aatfc(1).le.mxfc) go to 511
      aattmp(1)=2
      write(lunerr,19510) aatid,aatnm,aatxnm(2),aatxnm(1),aatfc(1)
      errf=.true.
      stop=.true.
      go to 878
  511 if(aatfc(1).eq.1) go to 516
      if(aatid.eq.14836) then
      print *, aatid
      endif
      do 513 idir=1,2
      if(aatway(1).eq.1.and.idir.gt.1) go to 513
      aattlb(idir)=xlb(idir,aatlb,1)
      aatrlb(idir)=xlb(idir,aatlb,2)
      aatllb(idir)=xlb(idir,aatlb,3)
c
c     for ramps add turn probibitors for when through link is also a ramp and link
c     itersects with arterial
c
      if(aatfc(1).eq.9) then
       ilb=aattlb(idir)
       if(ilb.gt.0.and.lbfc(ilb).eq.9) then
        id=lbid(ilb)
        ilb=(aatrlb(idir))
        irfc=0
        if(ilb.gt.0) irfc=lbfc(ilb)
        ilb=(aatllb(idir))
        ilfc=0
        if(ilb.gt.0) ilfc=lbfc(ilb)
         if(irfc.ge.2.and.irfc.le.7) then
         write(luno,20925) aatid,id
         else
         if(ilfc.ge.2.and.ilfc.le.7) write(luno,20925) aatid,id
         endif 
       endif 
      endif 
      aatxfc(idir)=7
      tatlb1=xlb(idir,aatlb,2)
      tatlb2=xlb(idir,aatlb,3)
      ifc1=10
      ifc2=10
      if(tatlb1.gt.0) ifc1=fcarr(tatlb1)
      if(tatlb2.gt.0) ifc2=fcarr(tatlb2)
      if(ifc2.lt.ifc1) go to 512
      ifc=ifc1
      if(ifc.lt.10) aatxnm(idir)=nmarr(tatlb1)
      if(ifc.lt.2.or.ifc.gt.7) go to 513
      aatxfc(idir)=ifc 
      go to 513
  512 ifc=ifc2
      if(ifc.lt.10) aatxnm(idir)=nmarr(tatlb2)
      if(ifc.lt.2.or.ifc.gt.7) go to 513
      aatxfc(idir)=ifc
  513 continue
      go to 520
  516 aatxfc(1)=7
      aatxfc(2)=7
  520 iln=aatln(1,1,1)+aatln(1,2,1)
      if(aatfc(1).eq.10) go to 580
      aatsec=aatadt
      if(aatsec.gt.0) go to 530
      secdst=secdst+aatlen
      if(aatxfc(1).le.3.and.secdst.gt.5280.0) go to 525
      aatsec=isec
      secdst=secdst+aatlen
      go to 530
  525 isec=isec+1
      aatsec=isec
      secdst=aatlen
c
c     fill in missing initial non-directional data with defaults.
c
  530 if(aatjur(1).lt.1.or.aatjur(1).gt.6) aatjur(1)=mjur(aatfc(1))
      if(aathov(1).lt.1.or.aathov(1).gt.4) aathov(1)=1
      if(aattrk(1).lt.1.or.aattrk(1).gt.7) aattrk(1)=1
      if(tasum(aatid).gt.50.0.and.tasum(aatid).lt.90.0) go to 531
      if(aatspd(1).lt.1.or.aatspd(1).gt.75) aatspd(1)=mspd(aatfc(1))
      if(aattspd(1).lt.1.or.aattspd(1).gt.75)
     *  aattspd(1)=mtspd(aatfc(1))
      ispd=aatspd(1)
      if(aatasp.gt.0.and.aatasp.le.75) ispd=aatasp
      go to 532
  531 ispd=nint(tisum(aatid))
      aatspd(1)=ispd
      ispd=nint(tasum(aatid))
      aatasp=ispd
  532 if(aatway(1).lt.1.or.aatway(1).gt.2) aatway(1)=mway(aatfc(1))
      if(aatmed(1).lt.1.or.aatmed(1).gt.3) aatmed(1)=mmed(aatfc(1))
      do 539 idir=1,2
      if(idir.eq.2.and.aatway(1).eq.1) go to 537
      aatmln(idir)=0
      do 533 ipk=1,3
      if(aatln(ipk,idir,1).lt.1.or.aatln(ipk,idir,1).gt.8) go to 533
      aatmln(idir)=max(aatmln(idir),aatln(ipk,idir,1))
  533 continue
      if(aatmln(idir).lt.1) aatmln(idir)=mln(aatfc(1))
      do 535 ipk=1,3
      if(aatln(ipk,idir,1).ge.1.and.aatln(ipk,idir,1).le.9) go to 535
      if(idir.eq.2) go to 534
      aatln(ipk,idir,1)=aatmln(idir)
      go to 535
  534 aatln(ipk,idir,1)=aatln(ipk,1,1)
  535 continue
      if(aataux(idir,1).lt.1.or.aataux(idir,1).gt.9) aataux(idir,1)=0
c
c     determine control at aatarn(1) from node data
c
      icnt=cnnode(aatarn(idir))
      if(icnt.eq.4.and.aatfc(1).eq.1) icnt=0
      if(icnt.eq.5.and.aatfc(1).eq.1) icnt=0
      if(icnt.eq.3) go to 536
      aatcnt(idir,1)=icnt
      if(aatcnt(idir,1).gt.8) aatcnt(idir,1)=0
  536 if(aatcnt(idir,1).lt.1.or.aatcnt(idir,1).gt.7) go to 538
      if(aattl(idir,1).lt.1.or.aattl(idir,1).gt.9)
     *aattl(idir,1)=aatln(1,idir,1)
      if(aatrl(idir,1).lt.1.or.aatrl(idir,1).gt.9) aatrl(idir,1)=0
      if(aatll(idir,1).lt.1.or.aatll(idir,1).gt.9)
     *aatll(idir,1)=mlf(aatfc(1),aatcnt(idir,1))
      go to 539
  537 aatln(1,idir,1)=0
      aatln(2,idir,1)=0
      aatln(3,idir,1)=0
      aataux(idir,1)=0
      aatcnt(idir,1)=0
  538 aattl(idir,1)=0
      aatrl(idir,1)=0
      aatll(idir,1)=0
  539 continue
      isph=snode(aatarn(2))/100
      jsph=snode(aatarn(1))/100
      go to 590
  580 aatnm='zone connector'
      ian=aattpn(2)
      ibn=aattpn(1)
      in=ian
      jn=ibn
      if(ian.le.mxzn) go to 581
      if(ibn.le.mxzn) go to 582
      aattmp(1)=6
      write(lunerr,19580) aatid,ian,ibn,in
      errf=.true.
      go to 590
  581 if(ibn.ge.mxzn) go to 583
      aattmp(1)=7
      write(lunerr,19581) aatrec,aatarn(2),aatarn(1),in
      errf=.true.
      go to 583
  582 in=ibn
      jn=aatarn(2)
      aattmp(2)=1
  583 zoneo(in)=.true.
      aatjur(1)=6
      aatway(1)=2
      aatspd(1)=znspd(zonety(in))
      aattspd(1)=znspd(zonety(in))
      ispd=aatspd(1)
      aatmed(1)=1
      aathov(1)=1
      aattrk(1)=1
      if (in.le.mxext) extcst=extadd(in)
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

c     save tdz nodes for shortest tdz connection to each route

      itdz=taztdz(in)
      aattmp(1)=itdz
      xz=xtdz(itdz)
      yz=ytdz(itdz)
      xn=xnode(jn)
      yn=ynode(jn)
      xdist=sqrt((xz-xn)**2+(yz-yn)**2) !distance from tdz centroid to node
      mzrt=tdzmrt(itdz)
      mnrt=nodemrt(jn)
       if(mzrt.gt.0) then !if tdz-route connenctions already exist then find out if this is a new one
        do irt=1,mnrt !loop through routes at this node
          irtno=nodert(jn,irt)
          new=.true.

           do jrt=1,mzrt !loop through existing connections
            jrtno=tdzrt(itdz,jrt)
            zdist=tdzrtdist(itdz,jrt)
             if(irtno.eq.jrtno) then !if new route matches old route then this isn't a new connection
              new=.false. 
               if(xdist.lt.zdist) then !if it is an old connection this may be a shorter distance
                tdzrtdist(itdz,jrt)=xdist
                tdzrtnode(itdz,jrt)=jn
               end if !end of distance check
             end if !end of new connection check
            end do !end of extisting tdz-route connections loop

          if(new) then !if this is a new route connection then add to matrix 
           mzrt=mzrt+1
            if(mzrt.gt.mxzrt) then
             write(lunerr,19582)
             errf=.true.
             go to 9999
            endif
           mmzrt=max(mmzrt,mzrt)
           tdzmrt(itdz)=mzrt
           tdzrt(itdz,mzrt)=irtno
           tdzrtdist(itdz,mzrt)=xdist
           tdzrtnode(itdz,mzrt)=jn
           tdzrtid(itdz,mzrt)=aatid !re-use id numbers for tdz connections
          endif !end of new check
        end do !end of loop through node routes
       else !this is the first connection for tdz

        do irt=1,mnrt !loop through routes at this node
          irtno=nodert(jn,irt)
          mzrt=mzrt+1
          tdzmrt(itdz)=mzrt
          tdzrt(itdz,mzrt)=irtno
          tdzrtdist(itdz,mzrt)=xdist
          tdzrtnode(itdz,mzrt)=jn
          tdzrtid(itdz,mzrt)=aatid !re-use id numbers for tdz connections
        end do !end of loop through node routes

       endif !end of number of connections check
c
c     fill in fields needed for transcad
c
  590 xdist=aatlen/5280.0
      xspd=float(ispd)

c     convert per mile toll cost to actual toll

       do ipk=1,3
        xtoll=float(aattoll(ipk,1))*xdist+remtoll(ipk)
        itoll=nint(xtoll)
        remtoll(ipk)=xtoll-float(itoll)
c     added by jor and zou for ML tolling on very short links, July, 2010
        if(aattoll(ipk,1).gt.0.and.itoll.eq.0)itoll=1 
        aattoll(ipk,1)=itoll
       enddo
      xtime=xdist*60./xspd
      aatcst=xdist*15.
      ista=aatsta
      if(aathov(1).eq.2.or.aathov(1).eq.3) then
       ista=0
       ifwy=hovfwy(aatid)
       if(ifwy.gt.0) ista=fwysta(ifwy)
      endif
      if(ista.lt.1.or.ista.gt.mxbsta.or.aatfc(1).ne.1) ista=1
      do 699 idir=1,2
      aattc(idir)=999.999
      do 698 ipk=1,3
      aatlt(ipk,idir)=999.0
      aatxt(ipk,idir)=0.0
      aatlc(ipk,idir)=999999.0
      aatxc(ipk,idir)=999999.0 
      aathc(ipk,idir)=0.0
      if(aatway(1).eq.1.and.idir.eq.2) go to 698
      if(aatln(ipk,idir,1).eq.9) go to 698
      aatlt(ipk,idir)=xtime
      if(aatfc(1).eq.10) go to 698
       if(aatfc(1).eq.1) then
        iidir=1
        i=index(aatnm,'NB')
        if(i.gt.0) iidir=2
        i=index(aatnm,'WB')
        if(i.gt.0) iidir=2
        xppf=stappf(ipk,iidir,ista)
         if(xppf.lt.1.0.or.xppf.gt.15.0) then
          write(lunsta,19698) ista,xppf
c          errf=.true.
          xppf=stappf(ipk,iidir,1)
         endif
       else
        xppf=stappf(ipk,idir,ista)
       endif
  591 fwycap=2000.0
      auxcap=1200.0
      iapp=xdapp(aatarn(idir))
      xadd=0.0
      if(aatfc(1).eq.1) go to 601
      if(aatfc(1).eq.8) go to 605
      if(aatfc(1).eq.9) go to 606
      if(aatplc(idir).eq.950.and.aatln(ipk,idir,1).lt.2) then
      xcap=950.0
      else
      xcap=float(aatln(ipk,idir,1))*1800.-300.
       if(aatmed(1).lt.2) xcap=xcap-200.
      endif
      go to 608
  601 if(aathov(1).eq.2.or.aathov(1).eq.3) then
       !changed from 1600 to 2000. Consistent with GP Lanes
       xcap=float(aatln(ipk,idir,1))*2000.0 
       go to 608
      endif
      if(iyear.ge.2015) then
       if(aatadt.eq.552.or.aatadt.eq.553) aatplc(1)=2000
       if(aatsta.eq.935.or.aatsta.eq.980.or.aatsta.eq.999)
     * aatplc=2200
      endif
      if(aatplc(1).ge.1600.and.aatplc(1).le.2400)
     *fwycap=float(aatplc(1))
      fwycap=min(fwycap,2100.0)
      if(aatsta.ne.936) fwycap=max(fwycap,1900.0) ! don't let capacity go lower than 1900 except for SR-94 coming into CBD
      xcap=float(aatln(ipk,idir,1))*fwycap+float(aataux(idir,1))*auxcap
      if(.not.tsm) go to 608
      iadt=aatadt
      if(iadt.lt.1.or.iadt.gt.9999) iadt=9999
      imtr=adtmtr(iadt)
      if(imtr.lt.1) go to 608
      if(imtr.eq.9) go to 603
      iidir=0
      i=index(aatnm,'SB')
      if(i.gt.0) iidir=1
      i=index(aatnm,'EB')
      if(i.gt.0) iidir=2
      i=index(aatnm,'NB')
      if(i.gt.0) iidir=3
      i=index(aatnm,'WB')
      if(i.gt.0) iidir=4
      if(iidir.ne.imtr) go to 608
  603 xcap=xcap*1.10
      fwycap=fwycap*1.10
      go to 608
  605 i=index(aatnm,'ACCESS')
       if(i.gt.0) then
        aathc(ipk,idir)=9999
        aatlc(ipk,idir)=999999
        go to 609
       else
        xcap=float(aatln(ipk,idir,1))*1800.0
        go to 608
       endif
  606 xcap=float(aatln(ipk,idir,1))*1200.0
  608 aathc(ipk,idir)=xcap
      aatlc(ipk,idir)=xcap*xppf
  609 ith=aattl(idir,1)
      irt=aatrl(idir,1)
      ilf=aatll(idir,1)
      if(ith.gt.7) ith=0
      if(irt.gt.7) irt=0
      if(ilf.gt.7) ilf=0
      if(ith.eq.7) ith=1
      if(irt.eq.7) irt=1
      if(ilf.eq.7) ilf=1
      if(ith.lt.1) then
      iapp=3
      if(irt.gt.ilf) then
      ith=irt
      irt=0
      else
      ith=ilf
      ilf=0
      endif
      endif
      if(aatcnt(idir,1).eq.1) go to 610
      if(aatcnt(idir,1).eq.2) go to 620
      if(aatcnt(idir,1).eq.3) go to 630
      if(aatcnt(idir,1).eq.4.and.ipk.gt.1) go to 640
      if(aatcnt(idir,1).eq.5.and.ipk.gt.1) go to 650
      if(aatcnt(idir,1).eq.6) go to 660
      if(aatcnt(idir,1).eq.7) go to 670 !toll booths
      go to 698
  610 aatxt(ipk,idir)=0.17
      if(aatgc(idir).lt.10)
     *aatgc(idir)=gcsig(iapp,aatfc(1),aatxfc(idir))
      gcth=float(aatgc(idir))/100.0
      gcrt=float(aatgc(idir))/100.0
      xcap=float(ith)*1800.*gcth
      xlfcap=float((ilf+irt)*lfcap(aatfc(1)))
      xcap=xcap+xlfcap
      if(xcap.lt.1000.) xcap=1000.0
      aatxc(ipk,idir)=xcap*rsafac(aatjur(1))*xppf
      aathc(ipk,idir)=xcap*rsafac(aatjur(1))
      go to 698
  620 aatxt(ipk,idir)=0.20
      if(aatgc(idir).lt.1)
     *aatgc(idir)=gcstp4(aatfc(1),aatxfc(idir))
      gcth=float(aatgc(idir))/100.0
      gcrt=float(aatgc(idir))/100.0
      xcap=float(ith)*1800.*gcth
      xlfcap=float((ilf+irt)*lfcap(aatfc(1)))
      xcap=xcap+xlfcap
      if(xcap.lt.500.) xcap=500.0
      aatxc(ipk,idir)=xcap*xppf
      aathc(ipk,idir)=xcap
      go to 698
  630 aatxt(ipk,idir)=0.20
      aatgc(idir)=gcstp2(aatxfc(idir))
      gcth=float(aatgc(idir))/100.0
      gcrt=float(aatgc(idir))/100.0
      gclf=float(aatgc(idir))/100.0
      if(irt.ne.7) go to 632
      gcrt=1.0
      irt=1
  632 xcap=float(ith)*500.*gcth+float(irt)*500.*gcrt+
     *float(ilf)*500.*gclf
      if(xcap.lt.500.) xcap=500.0
      aatxc(ipk,idir)=xcap*xppf
      aathc(ipk,idir)=xcap
      go to 698
  640 aatxt(ipk,idir)=0.50
      xcap=1000.
      if(aatgc(idir).lt.1) go to 641
      gcth=float(aatgc(idir))/100.0
      xcap=xcap*gcth
  641 aatxc(ipk,idir)=xcap*xppf
      aathc(ipk,idir)=xcap
      go to 698
  650 aatxt(ipk,idir)=0.50
      xcap=1000.
      if(aatgc(idir).lt.1) go to 651
      gcth=float(aatgc(idir))/100.0
      xcap=xcap*gcth
  651 aatxc(ipk,idir)=xcap*xppf
      aathc(ipk,idir)=xcap
      go to 698
c  660 aatxc(ipk,idir)=float(ith+irt+ilf)*1300.*xppf !source of this equation is unknown
c      aathc(ipk,idir)=float(ith+irt+ilf)*1300.
  660 aatxt(ipk,idir)=0.02 !RCU - changed to ~1 second delay/veh (T:\data\RailCrossingDelay.xls)
      go to 698
  670 ith=aattl(idir,1)
      if(ith.lt.aatmln(idir)) ith=aatmln(idir)
      aatxc(ipk,idir)=float(ith)*500.*xppf !Period Intersection Capacity = 500 veh/lane/hr * # of lanes * peak period factor
      aathc(ipk,idir)=float(ith)*500. !Hourly Capacity = 500 veh/lane/hr * # of lanes
      i=index(aatnm,'BORDER')
      if(i.gt.0) go to 672
c  Sphere 3 = Coronado; Sphere 14 = City of SD; jsph = Destination Sphere
      if(jsph.ne.3.and.jsph.ne.14) go to 671
c    Coronado and City of SD section 
      aatxt(ipk,idir)=1.0 !Intersection time = 1 minute
      if(ipk.eq.1.and.idir.eq.1) aatcst=aatcst+50.0 !Extra cost (toll booth) = 50 cents
      go to 698
c    Other Cities section
  671 aatxt(ipk,idir)=1.0 !Intersection time = 1 minute
      if(ipk.eq.1.and.idir.eq.1) aatcst=aatcst+25.0 !Extra cost (toll booth) = 25 cents
      go to 698
  672 i=index(aatnm,'YSIDRO')
      if(i.gt.0) ibord=1
      i=index(aatnm,'OTAY')
      if(i.gt.0) ibord=2
      i=index(aatnm,'EAST')
      if(i.gt.0) ibord=3
      i=index(aatnm,'TECATE')
      if(i.gt.0) ibord=4
      i=index(aatnm,'JACUMBA')
      if(i.gt.0) ibord=5
      iidir=1
      i=index(aatnm,'NB')
      if(i.gt.0) iidir=2
      aatxt(ipk,idir)=bordel(ibord,ipk,iidir)
  698 continue
      if(aatfc(1).eq.1) aatplc(idir)=nint(fwycap)
      if(idir.ne.1) go to 6981
      if(aathov(1).eq.2.or.aathov(1).eq.3) go to 6981
      i=index(aatnm,'AUX')
      if(i.gt.0) go to 6981
      frmile(aatfc(1))=frmile(aatfc(1))+xdist
      if(aatfc(1).ne.10)
     *jrmile(inc,ijur)=jrmile(inc,ijur)+xdist
 6981 if(aatway(1).eq.1.and.idir.eq.2) go to 699
      ihov=1
      if(aathov(1).eq.2.or.aathov(1).eq.3) ihov=2
      xlanes=float(aatln(2,idir,1))
      xauxlanes=float(aataux(idir,1))
      if(xlanes.gt.8) then
        xlanes=0.0
      else
        xlanes=xlanes+xauxlanes
      endif
      xmile=xdist*xlanes
      flmile(ihov,aatfc(1))=flmile(ihov,aatfc(1))+xmile
      fcmile(aatfc(1))=fcmile(aatfc(1))+xdist*aathc(2,idir)
c	    August 4, 2009, modified by ZOU,
c     use AM period lane and AM period time,to avoid reversible lane with
c     abtmo = 999
      frhour(aatfc(1))=frhour(aatfc(1))+
     *aatlt(2,idir)*xlanes/60.0
      aattc(idir)=extcst+aatcst+(aatlt(1,idir)+aatxt(1,idir))*35.0+
     *float(aattoll(1,1)+aattoll(2,1))/2.0
      if(ibord.gt.0) then
        do 6982 i=1,3
          aatlt(i,idir)=aatlt(i,idir)+aatxt(1,idir)
          ibord=0
 6982   continue
      end if
      if(aattc(idir).gt.999999.) aattc(idir)=999999.
      if(aatfc(1).eq.10) go to 699
      jlmile(inc,ijur)=jlmile(inc,ijur)+xmile
      jcmile(inc,ijur)=jcmile(inc,ijur)+xdist*aathc(2,idir)
c	    August 4, 2009, modified by ZOU,
c     use AM period lane and AM period time,to avoid reversible lane with
c     abtmo = 999
      jrhour(inc,ijur)=jrhour(inc,ijur)+aatlt(2,idir)*xlanes/60.0
  699 continue
      aatcc=aatfc(1)
      call whca
      idir=1
c      frmile(aatfc(1))=frmile(aatfc(1))+xdist
c      if(aatfc(1).ne.10)
c     *jrmile(inc,ijur)=jrmile(inc,ijur)+xdist
c      flmile(1,aatfc(1))=flmile(1,aatfc(1))+xdist
c      frhour(aatfc(1))=frhour(aatfc(1))+
c     *aatlt(1,idir)/60.0
c
c     check to see whether link aatfc(1),jur,and gpa has been selected
c     for printing.
c
      if(pfc(1).eq.9999) go to 864
      do 863 i=1,15
      if(pfc(i).eq.0) go to 878
      if(pfc(i).eq.aatfc(1)) go to 864
  863 continue
      go to 878
  864 if(pjur(1).eq.9999) go to 866
      do 865 i=1,4
      if(pjur(i).eq.0) go to 878
      if(pjur(i).eq.aatjur(1)) go to 866
  865 continue
      go to 878
  866 if(psph(1).eq.9999) go to 870
      do 867 i=1,20
      if(psph(i).eq.0) go to 878
      if(psph(i).eq.snode(aatarn(2))) go to 870
      if(psph(i).eq.snode(aatarn(1))) go to 870
      if(psph(i).eq.1400.and.isph.eq.14) go to 870
      if(psph(i).eq.1400.and.jsph.eq.14) go to 870
      if(psph(i).eq.1900.and.isph.eq.19) go to 870
      if(psph(i).eq.1900.and.jsph.eq.19) go to 870
  867 continue
      go to 878
c
c     print out from-to data
c
  870 ift=1
      if(first) go to 872

      if(iline.lt.60) go to 875
      iline=6
      write(lunpr,20000) eject,cdate,ipage,cheado
      eject = char(12)
      write(lunpr,20003)
      ipage=ipage+1
      go to 875
  872 if(iline.lt.52) go to 873
      write(lunpr,20000) eject,cdate,ipage,cheado
      eject = char(12)
      ipage=ipage+1
      iline=5
  873 write(lunpr,20001)
     *aatrt,aatnm,nway(aatway(1)),nrdir(aatdir),aatxnm(2)
      iline=iline+3
      first=.false.
  875 xop=aatlt(3,1)
      xpk=aatlt(1,1)
      write(lunpr,20002) aatrec,aatid,aatxnm(1),
     *nfc(aatfc(1)),nhov(aathov(1)),njur(aatjur(1)),ispd,
     *nmed(aatmed(1)),(nln(aatln(idir,1,1)+1),idir=1,3),
     *aataux(1,1),ncnt(aatcnt(1,1)+1),nxl(aattl(1,1)+1),
     *nxl(aatrl(1,1)+1),nxl(aatll(1,1)+1),
     *aatgc(1),xdist,xop,xpk,
     *(aathc(idir,1),idir=1,3)
      iline=iline+1
c
c     store tf data for printing at end of route
c
      krec=krec+1
      if(krec.lt.mxrlk) go to 877
      write(lunerr,19875) mxrlk,aatnm
      errf=.true.
      krec=mxrlk
  877 krno(krec)=aatrec
      kid(krec)=aatid
      kdist(krec)=idist
      kname(krec)=aatxnm(2)

      jur(krec)=aatjur(1)
      fc(krec)=aatfc(1)
      hov(krec)=aathov(1)
      spd(krec)=ispd
      med(krec)=aatmed(1)
      tfln(1,krec)=aatln(1,2,1)
      tfln(2,krec)=aatln(2,2,1)
      tfln(3,krec)=aatln(3,2,1)
      tfaux(krec)=aataux(2,1)
      tfcnt(krec)=aatcnt(2,1)
      tftl(krec)=aattl(2,1)
      tfrl(krec)=aatrl(2,1)
      tfll(krec)=aatll(2,1)
      tfgc(krec)=aatgc(2)
      tfotm(krec)=aatlt(3,2)
      tfptm(krec)=aatlt(1,2)
      tfcap(1,krec)=aatlc(1,2)
      tfcap(2,krec)=aatlc(2,2)
      tfcap(3,krec)=aatlc(3,2)
      name1=aatxnm(1)
  878 iaat=iaat+1
      if(iaat.gt.naat) go to 879
      lrtno=aatrt
      irtno=rlkarr(iaat)/1000
      ilkno=rlkarr(iaat)-irtno*1000
      aatrec=srec(iaat)
      if(irtno.ne.lrtno) go to 880
      go to 501

  879 endf=.true.
  880 first=.true.
      if(aatway(1).eq.1) go to 889
      if(krec.lt.1) go to 889
      if(iline.lt.52) go to 882
      write(lunpr,20000) eject,cdate,ipage,cheado
      ipage=ipage+1
      iline=6
  882 if(aatdir.eq.1) jdir=3
      if(aatdir.eq.2) jdir=4
      if(aatdir.eq.3) jdir=1
      if(aatdir.eq.4) jdir=2
      ift=2
      write(lunpr,20001) aatrt,aatnm,nway(aatway(1)),nrdir(jdir),name1
      iline=iline+3
      do 888 i=1,krec
      ii=krec-i+1
      if(iline.lt.60) go to 883
      iline=6
      write(lunpr,20000) eject,cdate,ipage
      write(lunpr,20003)
      ipage=ipage+1
  883 aatrec=krno(ii)
      aatid=kid(ii)
      xdist=float(kdist(ii))/100.0
      aatxnm(1)=kname(ii)
      aatjur(1)=jur(ii)
      aatfc(1)=fc(ii)
      aathov(1)=hov(ii)
      ispd=spd(ii)
      aatmed(1)=med(ii)
      aatln(1,1,1)=tfln(1,ii)
      aatln(2,1,1)=tfln(2,ii)
      aatln(3,1,1)=tfln(3,ii)
      aataux(1,1)=tfaux(ii)
      aatcnt(1,1)=tfcnt(ii)
      aattl(1,1)=tftl(ii)
      aatrl(1,1)=tfrl(ii)
      aatll(1,1)=tfll(ii)
      aatgc(1)=tfgc(ii)
      xop=float(tfotm(ii))
      xpk=float(tfptm(ii))
      aatlc(1,1)=tfcap(1,ii)
      aatlc(2,1)=tfcap(2,ii)
      aatlc(3,1)=tfcap(3,ii)
      write(lunpr,20002) aatrec,aatid,aatxnm(1),
     *nfc(aatfc(1)),nhov(aathov(1)),njur(aatjur(1)),ispd,
     *nmed(aatmed(1)),(nln(aatln(idir,1,1)+1),idir=1,3),
     *aataux(1,1),ncnt(aatcnt(1,1)+1),nxl(aattl(1,1)+1),
     *nxl(aatrl(1,1)+1),nxl(aatll(1,1)+1),
     *aatgc(1),xdist,xop,xpk,
     *(aathc(idir,1),idir=1,3)
      iline=iline+1
  888 continue
  889 if(endf) go to 890
      go to 500
  890 write(0,11011) naat
      close(lunin)

c     write out tdz zone connector generate records
            
      print *, mmzrt
      do itdz=1,mxtdz !loop through tdzs
       mzrt=tdzmrt(itdz)
        if(mzrt.lt.1) then !make sure at lest one connection was found for tdz
         write(lunerr,19891) itdz
         errf=.true.
        else
  
         do irt=1,mzrt !loop through route connections for this tdz
          in=tdzrtnode(itdz,irt)
          id=tdzrtid(itdz,irt)
           if(.not.nused(in)) then !check to see if tdz-node connection was already made for previous route
C	Added by Ziying Ouyang, Oct 29, 2009 for error checking	
	      if (xtdz(itdz).gt.0.and.ytdz(itdz).gt.0) then	        
               write(lunxy,20401) 
     *         id,xtdz(itdz),ytdz(itdz),xnode(in),ynode(in)
               nused(in)=.true.
	      else
               write(lunerr,19915) id,itdz !invalid x y coordinates
	     endif
	   endif
         end do !end of route connection loop

	 do in=1,mxhcn !reset node used to zero
          nused(in)=.false.
         end do
       end if !end of check fo tdz connections
      end do ! end of tdz loop
      write(lunxy,20409) 
c
c     write out turn prohibitor records
c
c
c     open turnssec file
c 
  900 endf=.false.
      call ohctus
      if(secio.ne.0) go to 9000
      secrec=1  
      call rhctus
      if(secio.ne.0) go to 9000
      if(alink.lt.1.or.alink.gt.mxhca) go to 9910
      aatid=lbid(alink)
      ifc=lbfc(alink)
      go to 917 
c
c     read section attribute table saving aatlbs used as prohibitors
c
  911 call rhctus
      if(secio.ne.0) go to 919
      if(alink.lt.1.or.alink.gt.mxhca) go to 9910
      aatid=lbid(alink)
      ifc=lbfc(alink)
      if(rlink.ne.lrlink) go to 915
      id2=aatid
      if(ifc.ne.lfc) go to 912
      go to 918
  912 write(luno,20925) lid,aatid
      used=.true.
      lfc=ifc
      lid=aatid
      go to 918
  915 if(used) go to 917
      if(id1.gt.0.and.id2.gt.0) go to 916
c      write(lunerr,19920) id1,id2
      go to 917
  916 write(luno,20925) id1,id2
  917 id1=aatid
      lrlink=rlink
      lfc=ifc
      lid=aatid
      id2=0
      used=.false.
  918 if(endf) go to 980
      secrec=secrec+1
      go to 911
  919 endf=.true.
      go to 915
  980 close(luno)
      write(lunsum,20980) ctime,cdate,cheado
      frmile(1)=frmile(1)/2.0
      jrmile(1,1)=jrmile(1,1)/2.0
      jrmile(2,1)=jrmile(2,1)/2.0
      do 981 ifc=1,9
      frmile(11)=frmile(11)+frmile(ifc)
      flmile(1,11)=flmile(1,11)+flmile(1,ifc)
      flmile(2,11)=flmile(2,11)+flmile(2,ifc)
      fcmile(11)=fcmile(11)+fcmile(ifc)
      frhour(11)=frhour(11)+frhour(ifc)
  981 continue
      ifc=10
      frmile(12)=frmile(11)+frmile(ifc)
      flmile(1,12)=flmile(1,11)+flmile(1,ifc)
      flmile(2,12)=flmile(2,11)+flmile(2,ifc)
      fcmile(12)=fcmile(11)+fcmile(ifc)
      frhour(12)=frhour(11)+frhour(ifc)
      do 983 ihov=1,2
      do 982 ifc=1,12
      flmile(3,ifc)=flmile(3,ifc)+flmile(ihov,ifc)
  982 continue
  983 continue
      do 984 ifc=1,9
      if(frhour(ifc).lt.0.01) go to 984
      frhour(ifc)=flmile(3,ifc)/frhour(ifc)
      write(lunsum,20981)
     *nfc(ifc),frmile(ifc),flmile(3,ifc),fcmile(ifc),frhour(ifc)
  984 continue
      ifc=11
      frhour(ifc)=flmile(3,ifc)/frhour(ifc)
      write(lunsum,20981)
     *nfc(ifc),frmile(ifc),flmile(3,ifc),fcmile(ifc),frhour(ifc)
      ifc=10
      frhour(ifc)=flmile(3,ifc)/frhour(ifc)
      write(lunsum,20981)
     *nfc(ifc),frmile(ifc),flmile(3,ifc),fcmile(ifc),frhour(ifc)
      ifc=12
      frhour(ifc)=flmile(3,ifc)/frhour(ifc)
      write(lunsum,20981)
     *nfc(ifc),frmile(ifc),flmile(3,ifc),fcmile(ifc),frhour(ifc)
      write(lunsum,20982) eject,ctime,cdate,cheado
       do inc=1,2
      do ijur=1,9
      jrmile(inc,10)=jrmile(inc,10)+jrmile(inc,ijur)
      jlmile(inc,10)=jlmile(inc,10)+jlmile(inc,ijur)
      jcmile(inc,10)=jcmile(inc,10)+jcmile(inc,ijur)
      jrhour(inc,10)=jrhour(inc,10)+jrhour(inc,ijur)
      end do
      end do
      do inc=1,2
      do ijur=1,10
      jrmile(3,ijur)=jrmile(3,ijur)+jrmile(inc,ijur)
      jlmile(3,ijur)=jlmile(3,ijur)+jlmile(inc,ijur)
      jcmile(3,ijur)=jcmile(3,ijur)+jcmile(inc,ijur)
      jrhour(3,ijur)=jrhour(3,ijur)+jrhour(inc,ijur)
      end do
      end do
      do inc=1,3
      do ijur=1,10
      if(jrhour(inc,ijur).gt.0.0) then
      jrhour(inc,ijur)=jlmile(inc,ijur)/jrhour(inc,ijur)
      write(lunsum,20983) nminc(inc),nmjur(ijur),jrmile(inc,ijur),
     *jlmile(inc,ijur),jcmile(inc,ijur),jrhour(inc,ijur)
      endif
      end do
      write(lunsum,20003)
      end do
      close(lunsum)
      open (unit=lunsum,file='summary.csv',access='append')
      ix=nint(flmile(3,1))
      write(lunsum,20901) ialt,cheadi,cdate,ctime,ix
      ix=nint(flmile(2,1))
      write(lunsum,20902) ialt,cheadi,cdate,ctime,ix
      close(lunsum)
      fnamo='complete'
      lunze=0
      do 992 i=1,mzone
      if(zoneo(i)) go to 992
      if(lunze.ne.0) go to 991
      write(lunerr,19991) i
      errf=.true.
      lunze=15
      open (unit=lunze,file='zone.err')
  991 write(lunze,10001) i
  992 continue
      if(lunze.ne.0) close(lunze)

c     if toll facilities or hov3+ facilities found then write out flag files

      fnami='hov3'                                                       
      call opendata(lunin)
      if(istat.eq.0) close(lunin,status='delete')
       if(hov3) then                                          
        open(lunin,file='hov3')
        write(lunin,20003)
        close(lunin)
       endif

      fnami='hot3'                                                       
      call opendata(lunin)
      if(istat.eq.0) close(lunin,status='delete')
       if(hot3) then                                          
        open(lunin,file='hot3')
        write(lunin,20003)
        close(lunin)
       endif

      fnami='toll'                                                       
      call opendata(lunin)
      if(istat.eq.0) close(lunin,status='delete')
       if(toll) then                                          
        open(lunin,file='toll')
        write(lunin,20003)
        close(lunin)
       endif

      go to 9999

 9000 write(lunerr,19000) fnamo
      errf=.true.
      go to 9999
 9001 write(lunerr,19001) fnamo
      errf=.true.
      go to 9999
 9030 write(lunerr,19030) aatadt
      errf=.true.
      go to 30
 9070 write(lunerr,19070) fnamo,ista
      errf=.true.
      go to 9999
 9130 write(lunerr,19130) 
      errf=.true.
      go to 9999
 9140 write(lunerr,19140) 
      errf=.true.
      go to 9999
 9502 write(lunerr,19502)
      errf=.true.
      go to 9999 
 9910 write(lunerr,19910) alink,rlink
      errf=.true.
      go to 917
10000 format(80i1,t1,80a1)
10001 format(20i4)
10002 format(i2,80a1)
10003 format(80a1)
10009 format(a1)
10030 format(5x,9(3x,i2))
10033 format(i7,29(f7.0,2x))
10040 format(11x,6f6.1)
10060 format(i8,a2)
10070 format(2i5,24f5.2)
11002 format(' ** reading and storing hwycov data ** ')
11004 format(' ** ',i6,' records read - - initial reading of aat',
     *' complete **')
11005 format(' ** finding cross street names **')
11006 format(' ** sorting records end-to-end by name **')
11008 format(' ** reading and writing node attribute table **')
11010 format(' ** reading and writing arc attribute table **')
11011 format(' ** ',i5,' records written - - successful program',
     *' completion **')
19030 format(' data problem with histfill file:',i10)
20000 format(a1,1x,a8,t57,'highway coverage arc data',
     *t130,'page',i4,/,t30,80a1,//,
     *'   rec   arc                      functional                  ',
     *'--lanes--         lanes           ---minutes---',
     *' ------hourly-----'/,
     *'    no    id cross street name    class       hov jur spd med ',
     *' main aux control t r l gc    miles offpeak  peak',
     *'      capacity')
20001 format(/,t36,'route number: ',i4,' name: ',a20,1x,a7,1x,a12,/,
     *13x,a20)
20002 format(2i6,1x,a20,1x,a11,1x,a4,1x,a3,i4,1x,a3,1x,2(a1,'/'),a1,
     *i4,2x,a6,3(1x,a1),i3,1x,f6.2,f8.2,f6.2,1x,2(f6.0,'/'),f6.0)
20003 format(' ')
20401 format(i10,/,f8.0,',',f8.0,/,f8.0,',',f8.0,/,'END')
20409 format('END')
20901 format(i2,','80a1,',47,total freeway lane miles,"',
     *a7,'","',a8,'",',i10,',')
20902 format(i2,','80a1,',48,hov freeway lane miles,"',
     *a7,'","',a8,'",',i10,',')
20920 format('from_id,to_id,penalty')
20925 format(i5,',',i5)
20980 format(1x,a8,t29,'highway network summary',t72,a8,/,80a1,//,
     *' functional',/,
     *' classification    route miles    lane miles',
     *'     capacity miles    average speed',/)
20981 format(1x,a11,t20,f12.0,f14.0,f19.0,f16.1)
20982 format(a1,a8,t37,'highway network summary',t78,a8,/,80a1,//,
     *'location            jurisdiction            route miles',
     *'    lane miles     capacity miles    average speed',/)
20983 format(a19,1x,a17,f19.0,f14.0,f19.0,f16.1)
30001 format(i5,i5,i1,i4,'t',2i4,4i2,i6,i1,i5,'1')
30002 format(i5,i5,i1,i4,'t',i4,4x,4i2,i6,i1,i5,'2')
19000 format(a50,' not opened')
19001 format(' data problem with file: ',a80/,80a1)
19070 format(' data problem with file: ',a80/,i10)
19100 format(' invalid node in arc attribute table: ',/,
     *' record=',i5,'node = ',i5,' name=',a20,'xname = ',a20)
19101 format(' invalid node in arc attribute table: ',/,
     *' record=',i5,'from node = ',i5,'to node =',i5,
     *'name=',a20,'frname = ',a20,'toname = ',a20)
19104 format(' increase size of mxnrt')
19128 format(' increase size of mxhca')
19130 format(' hwycov.trn not found in arc.dir file')
19140 format(' node number mis-match')
19399 format(' record not sorted',i6)
19411 format('naterr#1 - invalid node in node attribute table: ',/,
     *'hwycov# = ',i5,
     *'1st cross street=',a20,'2nd cross street = ',a20)
19412 format('naterr#2 -  invalid tranplan node attribute table: ',/,
     *'hwycov# = ',i5,'hnode = ',i5,
     *'1st cross street=',a20,'2nd cross street = ',a20)
19413 format('naterr#3 -  tranplan node at more than one location: ',/, 
     *'hwycov# = ',i5,'hnode = ',i5,
     *'  1st cross street=',a20,'2nd cross street = ',a20)
19414 format('naterr#4 -  more than four cross streets: ',/, 
     *'hwycov# = ',i5,
     *'1st cross street=',a20,'2nd cross street = ',a20)
19415 format('naterr#5 -  two-way stop with 3+ unsigned streets: ',/, 
     *'hwycov# = ',i5,
     *'1st cross street=',a20,'2nd cross street = ',a20)
19502 format(' no unused id's)
19505 format('aaterr#1 - entry/exit problem: ',/,
     *'id = ',i5,' name=',a20,' from = ',a20,
     *' to = ',a20,' nodes = ',2i6)
19510 format('aaterr#2 - invalid functional class: ',/,
     *'id = ',i5,' name=',a20,' from = ',a20,' to = ',a20,' fc = ',i10)
19526 format('aaterr#5 - duplicate arc: ',/,

     *'id = ',i5,' name=',a20,' from = ',a20,
     *' to = ',a20,' nodes = ',2i6)
19580 format('aaterr#6 - zone connector with both ends at zone: ',/,
     *'id = ',i5,' nodes=',2i5,' zone = ',i4)
19581 format('aaterr#7 - zone connector with neither at zone: ',/,
     *'id = ',i5,' nodes=',2i5,' zone = ',i4)
19582 format(' increase size of mxzrt')
19589 format('aaterr#8 - zone connector with neither end at street: ',/,
     *'id = ',i5,' nodes=',2i5,' zone = ',i4)
19698 format('problem with station hour %s: station=',i4,' ppf=',f5.1)
19875 format(' more than',i6,' links in route:',a21)
19891 format(' missing zone connectors for tdz',i6)
19902 format(' mis-matching nodes on hov ramp links :',3i10)
19907 format(' no tranplan node at hov nodes:',5i10)
19909 format(' hov ramp link not used:',i10)
19910 format(' data problem with hwycov2.turnssec file:',2i10)
19915 format(' hwycovtdz zone connector 
     * invalid x, y coordinate for link id ',i8,'for tdz ',i8)
19920 format(' more than 3 arcs on turn route :',4i10)
19912 format(' more than 3 arcs on turn route :',i10)
19922 format(' no matching nodes on turn route :',5i10)
19991 format(' missing zone connector(s)',i5)
20130 format('info/arc',4a1,'.dat')
39999 format('[',a7,'] [',a8,'] [s9.10/tchc] [',a<ic1>,
     *'] [route miles=',i<ic2>,
     *'] [lane miles=',i<ic3>,
     *'] [averge mph=',f<ic4>.1,']')
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
      i1=nint(frmile(16))
      ic2=1
      if(i1.gt.9) ic2=2
      if(i1.gt.99) ic2=3
      if(i1.gt.999) ic2=4
      if(i1.gt.9999) ic2=5
      if(i1.gt.99999) ic2=6
      i2=nint(flmile(3,16))
      ic3=1
      if(i2.gt.9) ic3=2
      if(i2.gt.99) ic3=3
      if(i2.gt.999) ic3=4
      if(i2.gt.9999) ic3=5
      if(i2.gt.99999) ic3=6
      ic4=3
      if(frhour(16).gt.9.9) ic4=4
      if(frhour(16).gt.99.9) ic4=5
      call dattim
      write(lunerr,39999)  cdate,ctime,tpstat,i1,i2,frhour(16)
      close(lunerr)
      close(lunsta)
      close(lunin)
      close(lunpr)
      stop
      end
      subroutine recsrt
      include 'sandag.inc'
      common /reccom/ srec,rlkarr
      integer*4 srec(mxhca),rlkarr(mxhca),tt,t
      dimension iu(21),il(21)
      jj=mxhca
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
