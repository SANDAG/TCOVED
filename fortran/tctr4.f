c
c     fourth in a series of programs/amls to create transcad
c     transit inputs from tcov, this program:
c     1) creates transcad inputs
c     2) creates summary reports
c-----------------------------------------------------------------------------------------
c     updated 3/11/05 to flag nodes used by time period

c     July 16, 2009, Ziying Ouyang
c     updated section where reading adjust1.csv
c     now reads adjuststr.csv which has two sets of times (LB and BRT time)
c     depending on the mode of the route, use either LB or BRT time 
c     idtm(2,2,3,mxid) indexed by mode (added dimension), by direction, 
c                                by time period, and link id
c
c     August 5, 2009, Ziying Ouyang
c     depending on the presence of the adjuststr.csv file, name of the reports
c     will be tctrfreeflow.pr / tctrfreeflow.sum or tctrcongest.pr / tctrcongest.sum
c
c     Dec 11, 2009, Ziying Ouyang
c     modifed the section where crzone field is set
c     added new variable called crzone() for link connecting two station within the same zone 
c     crzone2() for link connecting two stations in two zones
c     corrected the value of the link b/w two neighboring stations in two different zones
c
c     Jan, 2010 Ziying Ouyang
c     call getCPIFctor() subroutine to get cpi adjustment factor
c
c     May 2010 Ziying Ouyang
c     added one more dimension to fybyr(ir,im) 
c     to accomodate routes with different fares based on faremode
c----------------------------------------------------------------------------------------

      include 'sandag.inc'
      include 'tcov.inc'
      parameter (mxrt=600,mxxfr=24,mxseq=500,mxfnum=25)
      common /psort/ xrt,prt,xline
c
c     program variables
c
      logical selrt(999),selnod(mxtrcn),npark(mxtrcn),ntt(mxtrcn),
     *fwynod(mxtrcn),faterr,selpr,first,endf,used(2,mxtrcn),shtlrt(999),
     *lkused(mxtrca,mxmoda),fareerr
      integer*2 inode,jnode,xfnode(mxtrcn,mxfnum),xfnum(mxtrcn),irt,jrt,
     *xrt(mxxfr,mxtrcn),lbispd(mxtrca),lbospd(mxtrca),lbfc(mxtrca),
     *minmod(mxtrcn),maxmod(mxtrcn),lbrt(mxtrca),
     *lbstp(mxtrca),nstop(mxtrcn),ndwell(mxtrcn),mzn,
     *icon,comp,rtno,ortno,lbtype(mxtrca),nsph(mxtrcn),nfz(mxtrcn),
     *idir,iln,imode,rtdirlb(999,2),
     *npage,xline,jstop,mspd,ipass,npass1(mxid),npass2(mxid),
     *lmode(mxmodt),lco(9),mfare(3,mxmoda),rfare(3,999),cfare(3,9),
     *brtmode(mxmoda),modeFlg,
     *nfz4(mxtrcn),patshp
      integer*4 access,isy,iotay,lbid(mxtrca),lbtap(mxtrcn),
     *lblat(mxtrcn),lblng(mxtrcn),lbnode(mxtrcn),frmode(999),id1,
     *seqstp(mxseq),backid(mxseq),nextid(mxseq),nextp(mxseq),
     *lastn(999),imiss,ixmin,ilb1,ilb2,ratmod4,
     *mamve(mxmoda),mpmve(mxmoda),mopve(mxmoda),mntve(mxmoda),
     *crzone(4),crzone2(4),patlb, zratnhr
      real vmcst(9),vhcst(9),vmncst(9),seqdst(mxseq),
     *lbdist(mxtrca),idtm(2,2,3,mxid),lbff(2,mxtrca),
     *rdist,rpkhtm,rpkttm,rophtm,ropttm,ropstp,rpkstp,
     *pdist,ppkhtm,ppkttm,pophtm,popttm,popstp,ppkstp,
     *ssdist,tldist,tpktim,toptim,pktime,optime,ldist,
     *pkstp,opstp,stpdel(2,mxmoda),actime,detime,dtime,stime,rtime,
     *xspd,acc,xxdist,avspd,acdist,dedist,tspd,ntvm,ntvh,
     *rr,rac,rde,rdw,rs,lndist(1000),lnpktm(1000),lnoptm(1000),
     *burtot(4,19),mrtm(mxmoda),
     *mamvm(mxmoda),mpmvm(mxmoda),mopvm(mxmoda),mntvm(mxmoda),
     *mamvh(mxmoda),mpmvh(mxmoda),mopvh(mxmoda),mntvh(mxmoda),
     *mamcs(mxmoda),mpmcs(mxmoda),mopcs(mxmoda),mntcs(mxmoda)
      real*4 brtlt(3,2)
      real*8 zamhwy,zpmhwy,zophwy,znthwy,tdist(3,2),frbyr(999,10),
     *hovlt(3,2),sovcst(3,2),hovcst(3,2),ffcst(3,2),cpiFctor
      character*1 ast,py,tty,prt(7,12),pkast,opast,astype(2),
     *eject2,via(20),start(24),stop(24),pm
      character*2 adir(2)
      character*4 nmstop(mxstop)
      character*5 ncomp(9),alnode(200)
      character*13 nmode(mxmoda)
      character*20 nxnm1(mxtrcn),nxnm2(mxtrcn),c20
      character*32 rtname
      character*80 chead
      data minmod/mxtrcn*10/,npage/0/,xline/1/,i9/9/,ipass/1/
      data alnode/200*'     '/,astype/'*',' '/
c     ZOU Sept 2, 08
c     zou modified BRT stop dwell time from 0.67 to 0.5 based on midcoast study
c
      data stpdel/.00,.00,
     *            .00,.00,
     *            .00,.00,
     *            .30,.30,
     *            .30,.30,
     *            .50,.50,
     *            .50,.50,
     *            .50,.50,
     *            .50,.50,
     *            .30,.30/
      data nmode/
     *'COMMUTER RAIL',
     *'LIGHT RAIL   ',
     *'REGIONAL BRT ',
     *'CORRIDOR BRT ',
     *'LIMITED BUS  ',
     *'EXPRESS BUS  ',
     *'LOCAL BUS    ',
     *'             ',
     *'             ',
     *'SYSTEM TOTAL '/
      data lmode/13,10,10,7,11,11,9/,adir/'IB','OB'/
      data lco/5*4,3*3,4/
      data ncomp/ 'SDTC ','     ','NCTD ','     ','SDTI ','NCT  ',
     *'CVT  ','CTS  ','MTDB '/
      data nmstop/'    ','    ','    ','COMM','LRT ','YCAR','RCAR',
     *'LIMX','XPRS','LOCL'/
      data vmcst/ 2.21, 1.41, 2.21, 2.10, 1.79, 1.17, 1.23, 1.16, 1.18/
      data vhcst/26.80,25.88,26.88,95.82,20.44,20.28,20.10,18.03,17.14/
      data lunpr,lunerr,luninf,lunin,lunlnk,lunrt,lunstp,lungen,
     *luno,lunsel,aatlun,natlun,seclun,ratlun,lunsum,luntt,luncsv,
     *lunprsum,lunfare,lunrtcsv
     */11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30/
c     indexed by 10 modes (xfer,cc walk, walk access, CR, LRT,BRT, BRT, L EXP,EXP,LB)
      data brtmode/1,1,1,1,1,2,2,2,1,1/ 
c      shtlrt(280)=.true.
      shtlrt(281)=.true.
      shtlrt(282)=.true.
      shtlrt(283)=.true.
c      shtlrt(284)=.true.
c      shtlrt(285)=.true.
c      shtlrt(286)=.true.
c

c     initialize crzone values
      crzone(1)=1
      crzone(2)=10
      crzone(3)=100
      crzone(4)=1000
c     initialize crzone2 values
      crzone2(2)=11
      crzone2(3)=110
      crzone2(4)=1100

      mxstpp=mxstop+1
      do 1 i=1,9
      vmncst(i)=vhcst(i)/60.0
    1 continue
      eject=' '
      eject2=' '
      call dattim
      open (unit=lunerr,file='tctr.err')
      open (unit=luninf,file='tctr.info')
c      open (unit=lunpr,file='tctr.pr')
      open (unit=lungen,file='tap.gen')
      open (unit=luncsv,file='tctrnode.csv')
      open (unit=lunsum,file='summary.csv',access='append')
      open (unit=lunfare,file='tctrfare.err')
      fnami='head'
      call opendata(lunin)
      if(istat.ne.0) go to 9000
      read(lunin,10003) cheadi
      call cheadr
      close(lunin)
c
c     read analysis year
c
      fnamo='year'
      open (unit=lunin,file=fnamo,status='old',err=9000)
      icol=1
      read(lunin,10000,iostat=istat) idata,adata
      if(istat.ne.0) go to 9001
      call sd_getdat(idata,adata,icol,iyear,i2,i3,lunerr)
      close(lunin)
      ifyear=1
      if(iyear.gt.1998) ifyear=2
      if(iyear.gt.2000) ifyear=3
      call getCPIFctor(iyear,cpiFctor)
      fnamo='detrts'
      open (unit=lunin,file=fnamo,status='old',err=2)
      go to 3
    2 fnamo='../detrts'
      open (unit=lunin,file=fnamo,status='old',err=7)
    3 open (unit=lunsel,file='tctr.sel')
      selpr=.true.
      icol=1
    5 read(lunin,10000,iostat=istat) idata,adata
      if(istat.ne.0) go to 6
      call sd_getdat(idata,adata,icol,isel,i2,i3,lunerr)
      if(isel.lt.1.or.isel.gt.999) go to 5
      selrt(isel)=.true.
      go to 5
    6 close(lunin)
    7 fnamo='xferlinks'
      open (unit=lunin,file=fnamo,status='old',err=9000)
      icol=2
    8 read(lunin,10000,iostat=istat) idata,adata
      if(istat.ne.0) go to 9
      call sd_getdat(idata,adata,icol,iinode,ijnode,i3,lunerr)
      if(iinode.lt.1.or.iinode.gt.mxtrcn) go to 9001
       inum=xfnum(iinode)
       inum=inum+1
       xfnode(iinode,inum)=ijnode
       xfnum(iinode)=inum
      if(ijnode.lt.1.or.ijnode.gt.mxtrcn) go to 9001
       inum=xfnum(ijnode)
       inum=inum+1
       xfnode(ijnode,inum)=iinode
       xfnum(ijnode)=inum
      go to 8
    9 close(lunin)
c
c     check for transit errors when building tcov
c
      fnami='tcoved'
      istat=access(fnami,' ')
      if(istat.eq.0) go to 10
      fnami='../covs/tcoved'
      istat=access(fnami,' ')
      if(istat.eq.0) go to 11
      go to 9000
   10 fnamo='posttr.err'
      go to 12
   11 fnamo='../covs/posttr.err'
   12 istat=access(fnamo,' ')
      if(istat.ne.0) go to 19
      faterr=.true.
      errf=.true.
      write(lunerr,19000) fnamo
c   19 fnamo='adjusts1.csv'
c      open (unit=lunin,file=fnamo,status='old',err=29)
c      print *,fnamo
c      read(lunin,10021,iostat=istat) c20
c   20 read(lunin,10020,iostat=istat) aatid,
c     *(ffcst(1,idir),ffcst(2,idir),ffcst(3,idir),idir=1,2),
c     *(aatlt(1,idir),aatlt(2,idir),aatlt(3,idir), 
c     * hovlt(1,idir),hovlt(2,idir),hovlt(3,idir),
c     * sovcst(1,idir),sovcst(2,idir),sovcst(3,idir),
c     * hovcst(1,idir),hovcst(2,idir),hovcst(3,idir),idir=1,2)
c
c      if(istat.ne.0) go to 28
c      if(aatid.lt.1.or.aatid.gt.mxid) go to 9001
c      idtm(1,1,aatid)=hovlt(1,1)
c      idtm(1,2,aatid)=hovlt(2,1)
c      idtm(1,3,aatid)=hovlt(3,1)
c      idtm(2,1,aatid)=hovlt(1,2)
c      idtm(2,2,aatid)=hovlt(2,2)
c      idtm(2,3,aatid)=hovlt(3,2)
c      go to 20
c   28 close(lunin)   
c     July, 2009, Modified by Ziying Ouyang  
   19 fnamo='adjuststr.csv'                                        
      open (unit=lunin,file=fnamo,status='old',err=29)            
      print *,fnamo                                               
      read(lunin,10021,iostat=istat) c20                          
   20 read(lunin,10020,iostat=istat) aatid,                 
     *(hovlt(1,idir),hovlt(2,idir),hovlt(3,idir),idir=1,2),                 
     *(brtlt(1,idir),brtlt(2,idir),brtlt(3,idir),idir=1,2)     
      if(istat.ne.0) go to 28                                     
      if(aatid.lt.1.or.aatid.gt.mxid) go to 9001 
c     localbus time
      do idir=1,2      
      do itm=1,3            
      idtm(1,idir,itm,aatid)=hovlt(itm,idir) 
      end do   
      end do 
c     BRT time                        
      do idir=1,2                                              
      do itm=1,3                                               
      idtm(2,idir,itm,aatid)=brtlt(itm,idir)                                     
      end do                                                                                   
      end do                                                                                
      go to 20                                                    
   28 close(lunin) 
c     Aug 4, 2009, Modified by Ziying Ouyang
c     depending on the presence of adjuststr.csv
c     the name of tctr.pr will be different                                               
      open (unit=lunpr,file='tctrcongest.pr') 
      open (unit=lunprsum,file='tctrcongest.sum')                                                         
      go to 30
   29 print *, 'No adjusts.csv, freeflow times used'
c     Aug 4, 2009, Modified by Ziying Ouyang   
      open (unit=lunpr,file='tctrfreeflow.pr')
      open (unit=lunprsum,file='tctrfreeflow.sum')      

   30 fnamo='faresnu.csv'
      open (unit=lunin,file=fnamo,status='old',err=31)
      go to 32
   31 fnamo='../data/faresnu.csv'
      open (share='denynone',unit=lunin,
     *file=fnamo,status='old',err=9000)
   32 read(lunin,10021,iostat=istat) c20
   33 read (lunin,10031,iostat= istat) ir,im,rf
c   33 read(lunin,10030,iostat=istat) ir,im,rf    
      if(istat.ne.0) go to 35
      if(ir.lt.1.or.ir.gt.999) then
        write(lunfare,10010)'Route out of range: ',ir
        fareerr=.true.
        go to 33
      endif
      if(rf.lt.0.1.and.rf.gt.0) then
        fareerr=.true.
        write(lunfare,10011)
     *   'Potential Fare Value Error, check faresnu.csv. Route:'
     *   ,ir,' Fare: ',rf
      endif
c      if(cpiFctor.gt.0)then
c        if (ir.gt.0.and.im.gt.0)frbyr(ir,im)=rf*cpiFctor
c        write(luninf, 10002)'fares CPI adjusted'
c      else
        if (ir.gt.0.and.im.gt.0)frbyr(ir,im)=rf !fare not adjusted
        write(luninf, 10002)'fares not CPI adjusted'
c      endif
      go to 33
   35 close(lunin)
      if(fareerr) then
        close(lunfare,status='keep')
      else
        close(lunfare,status='delete')
      end if
c
c     open zoneid pat file (post 6.0)
c
   36 fnamo='fz4id\pat.adf'
c     call opencov(lunin)
      open(unit=lunin,convert='big_endian',file=fnamo,access=
     *'direct',form='unformatted',recl=2,status='old',err=9000)
      patrec=1
      print *,fnamo
   37 read(lunin,rec=patrec,iostat=istat)
     *patlb,patshp,natfz4
      if(istat.ne.0) go to 39
      if(patlb.lt.1.or.patlb.gt.mxtcn) go to 38
      nfz4(patlb)=natfz4
   38 patrec=patrec+1
      go to 37
   39 close(lunin)
c
c     open trcov aat file
c
      call otrca
      if(aatio.ne.0) go to 9000
      print *,fnamo
      aatrec=1
c
c     read trcov arc attribute table saving arrays of arc data
c
   40 call rtrca
      if(aatio.ne.0) go to 119
      if(aatarn(2).gt.mxtrcn) go to 9040
      if(aatarn(1).gt.mxtrcn) go to 9040
      if(aatlb.gt.mxtrca) go to 9040
      lbid(aatlb)=aatid
      xft=aatlen*.0114/float(aatspd(1))
      xft=max(0.01,xft)
      xtf=xft
      if(aatcnt(1,1).gt.0) xft=xft+0.17
      if(aatcnt(2,1).gt.0) xtf=xtf+0.17
      lbff(1,aatlb)=xft
      lbff(2,aatlb)=xtf
      do imode=1,2 !from-to direction
      do itm=1,3
         if(idtm(imode,1,itm,aatid).lt.0.01) idtm(imode,1,itm,aatid)=xft
      end do
      end do
      if(aatway(1).eq.2) then !to-from for two way links
       do imode=1,2 
	    do itm=1,3
	      if(idtm(imode,2,itm,aatid).lt.0.01)idtm(imode,2,itm,aatid)=xtf
	    end do
	  end do
      else
        do imode=1,2 
  	    do itm=1,3
  	      idtm(imode,2,itm,aatid)=xtf	      
  	    end do
  	  end do
      endif
      lbdist(aatlb)=aatlen/5280.
      lbfc(aatlb)=aatfc(1)
      lbispd(aatlb)=aatspd(1)
      lbospd(aatlb)=aatosp
      lbtype(aatlb)=aattmp(2)
      lbrt(aatlb)=aatrt
c      lbstp(aatlb)=aatstp(1)*10+aatstp(2)
      if(aatmm.lt.4) go to 49
      if(aatfc(1).ne.1) go to 41
      fwynod(aatarn(2))=.true.
      fwynod(aatarn(1))=.true.
   41 if(aatmm.lt.minmod(aatarn(1)).or.minmod(aatarn(1)).lt.4)
     *minmod(aatarn(1))=aatmm
      if(aatmm.lt.minmod(aatarn(2)).or.minmod(aatarn(2)).lt.4)
     *minmod(aatarn(2))=aatmm
      imax=aattmp(1)
      if(imax.gt.mxstop) imax=mxstop
      if(imax.gt.maxmod(aatarn(1))) maxmod(aatarn(1))=imax
      imax=aattmp(1)
      if(imax.gt.mxstop) imax=mxstop
      if(imax.gt.maxmod(aatarn(2))) maxmod(aatarn(2))=imax
c
c     find intersecting routes at node
c
   49 do 98 irt=1,20
      jrt=aattrt(irt)
      if(jrt.lt.1) go to 99
      do 51 i=1,mxxfr
      if(xrt(i,aatarn(1)).eq.0) go to 52
      if(xrt(i,aatarn(1)).eq.jrt) go to 60
   51 continue
      i=mxxfr
   52 if(i.eq.1) go to 56
      ii=i-1
      do 53 j=1,ii
      if(jrt.lt.xrt(j,aatarn(1))) go to 54
   53 continue
      go to 56
   54 kk=j
      do 55 k=kk,ii
      kkk=ii-k+kk
      jj=kkk+1
      xrt(jj,aatarn(1))=xrt(kkk,aatarn(1))
   55 continue
      xrt(j,aatarn(1))=jrt
      go to 60
   56 xrt(i,aatarn(1))=jrt
   60 do 61 i=1,mxxfr
      if(xrt(i,aatarn(2)).eq.0) go to 62
      if(xrt(i,aatarn(2)).eq.jrt) go to 70
   61 continue
      i=mxxfr

   62 if(i.eq.1) go to 66
      ii=i-1
      do 63 j=1,ii
      if(jrt.lt.xrt(j,aatarn(2))) go to 64
   63 continue
      go to 66
   64 kk=j
      do 65 k=kk,ii
      kkk=ii-k+kk
      jj=kkk+1
      xrt(jj,aatarn(2))=xrt(kkk,aatarn(2))
   65 continue
      xrt(j,aatarn(2))=jrt
      go to 70
   66 xrt(i,aatarn(2))=jrt
   70 inum=xfnum(aatarn(1))
      if(inum.lt.1) go to 80
      do 79 in=1,inum
      inode=xfnode(aatarn(1),in)
      do 71 i=1,mxxfr
      if(xrt(i,inode).eq.0) go to 72
      if(xrt(i,inode).eq.jrt) go to 79
   71 continue
      i=mxxfr
   72 if(i.eq.1) go to 76
      ii=i-1
      do 73 j=1,ii
      if(jrt.lt.xrt(j,inode)) go to 74
   73 continue
      go to 76
   74 kk=j
      do 75 k=kk,ii
      kkk=ii-k+kk
      jj=kkk+1
      xrt(jj,inode)=xrt(kkk,inode)
   75 continue
      xrt(j,inode)=jrt
      go to 79
   76 xrt(i,inode)=jrt
   79 continue
   80 inum=xfnum(aatarn(2))
      if(inum.lt.1) go to 98
      do 89 in=1,inum
      jnode=xfnode(aatarn(2),inum)
      do 81 i=1,mxxfr
      if(xrt(i,jnode).eq.0) go to 82
      if(xrt(i,jnode).eq.jrt) go to 89
   81 continue
      i=mxxfr
   82 if(i.eq.1) go to 86
      ii=i-1
      do 83 j=1,ii
      if(jrt.lt.xrt(j,jnode)) go to 84
   83 continue
      go to 86
   84 kk=j
      do 85 k=kk,ii
      kkk=ii-k+kk
      jj=kkk+1
      xrt(jj,jnode)=xrt(kkk,jnode)
   85 continue
      xrt(j,jnode)=jrt
      go to 89
   86 xrt(i,jnode)=jrt
   89 continue
   98 continue
   99 aatrec=aatrec+1
      go to 40
c
c     open trcov nat file
c
  119 call otrcn
      if(natio.ne.0) go to 9000
      print *,fnamo
      natrec=1
      write(luncsv,20121)
c
c     read stop point attribute table saving arrays of node type for
c     for each node and write out transfer route dataset.
c
  120 call rtrcn
      if(natio.ne.0) go to 159
      if(natlb.gt.mxtrcn) go to 9120
      lbnode(natlb)=nattrn
      nsph(natlb)=natsph
      nfz(natlb)=nfz4(natlb)
      lbtap(natlb)=nattap	
      if(nattt(1).ne.7) go to 123
c	isy=natlb
      isy=nattap
      go to 124
  123 if(nattt(1).ne.8) go to 124
      if(natstp(1).ne.5) go to 124
      iotay=natlb
  124 if(natprk(1).lt.4.or.natprk(1).gt.mxmoda) natprk(1)=1
      if(minmod(natlb).gt.natprk(1)) natprk(1)=1
      if(natprk(2).lt.4.or.natprk(2).gt.mxmoda) natprk(2)=1
      if(minmod(natlb).gt.natprk(2)) natprk(2)=1
      if(natstp(1).ge.4.and.natstp(1).le.mxstop) go to 131
      natstp(1)=mxstpp
      go to 134
  131 if(minmod(natlb).gt.natstp(1)) natstp(1)=minmod(natlb)
      if(maxmod(natlb).lt.natstp(1)) natstp(1)=mxstpp
  134 if(natstp(2).ge.4.and.natstp(2).le.mxstop) go to 135
      natstp(2)=mxstpp
      go to 136
  135 if(minmod(natlb).gt.natstp(2)) natstp(2)=minmod(natlb)
  136 if(natstp(2).ge.natstp(1)) go to 150
      natstp(1)=natstp(2)
      if(maxmod(natlb).lt.natstp(1)) natstp(1)=mxstpp
      if(natprk(2).gt.3) natprk(1)=natprk(2)
  150 if(natstp(1).eq.mxstpp) natstp(1)=1
      if(natstp(1).ge.4.and.natstp(1).le.6) selnod(natlb)=.true.
      if(natstp(1).ne.4.and.natstp(1).ne.5) go to 155
      if(idwell.eq.0) idwell=33
  155 nstop(natlb)=natstp(1)
      ndwell(natlb)=idwell
      if(natprk(1).gt.3) npark(natlb)=.true.
      if(nattt(1).ne.1) go to 158
      ntt(natlb)=.true.
  158 if(natstp(1).lt.4.and.nattap.gt.0) nattap=0
      call wtrcn
      if(natstp(1).ge.4.and.natstp(1).le.8) write(luncsv,20120) 
     *nattap,nmstop(natstp(1)),natxnm(1),natxnm(2),(xrt(i,natlb),i=1,20)
      natrec=natrec+1
      go to 120
c  
c     write out lrt border nodes (used in trext)
c
  159 open (unit=luno,file='bnode')
      if(isy.lt.1.or.isy.gt.mxtrcn) go to 181
      inode=isy
      if(inode.lt.1.or.inode.gt.mxtrn) go to 181
      go to 182
  181 inode=9999
  182 write(luno,20180) inode
      if(iotay.lt.1.or.iotay.gt.mxtrcn) go to 183
      inode=iotay
      if(inode.lt.1.or.inode.gt.mxtrn) go to 183
      go to 184
  183 inode=9999
  184 write(luno,20180) inode
      close(luno)
      fnamo='trcovll'
      natlb=0
      open (unit=lunin,file=fnamo,status='old',err=9000)
  190 read(lunin,10190,iostat=istat) xlng,xlat
      if(istat.ne.0) go to 191
      natlb=natlb+1
      if(natlb.gt.mxtrcn) go to 9000
      lblng(natlb)=nint(xlng*1000000.0)
      lblat(natlb)=nint(xlat*1000000.0)
      go to 190
  191 close(lunin)
      nlink=1
      open (unit=lunlnk,form='binary',file='trlink.bin')
      open (unit=lunstp,form='binary',file='trstop.bin')
      open (unit=lunrt,form='binary',file='trrt.bin')
      open (unit=luntt,form='binary',file='timexfer.bin')
	open (unit=lunrtcsv,file='trrt.csv')
	write(lunrtcsv,20210)
c      open (unit=lunlnk,file='trlink.bin')
c      open (unit=lunstp,file='trstop.bin')
c      open (unit=lunrt,file='trrt.bin')
      secrec=1
      call otrcs
      if(secio.ne.0) go to 9000
c
c     pre-read section attribute table and flag last node of route (needs to be written to trstops.bin)
c
  192 call rtrcs
      if(secio.ne.0) go to 193
      lastn(rlink)=secarn(1)
      secrec=secrec+1
      go to 192
  193 secrec=1
c
c     open route data file.
c
      call otrcr
      if(ratio.ne.0) go to 9000
c      print *,fnamo
      ratrec=1
  200 read(ratlun,rec=ratrec,iostat=ratio) ratlb,ratid,
     *ratco,ratmod,ratlin,rathwy(1),rathwy(2),rathwy(3),
     *rathwy(4),ratnhr,start,stop,via
      if(ratio.ne.0) go to 399
      if(ratlb.lt.1.or.ratlb.gt.mxrt) go to 9200
      if(ratid.lt.1.or.ratid.gt.9999999) go to 9200
      rtno=ratid/1000 
      idir=(ratid-rtno*1000)/100
      icon=ratid-rtno*1000-idir*100
      rtdirlb(rtno,idir)=ratlb
      xamhwy=float(rathwy(1))/10.0
      xpmhwy=float(rathwy(2))/10.0
      xophwy=float(rathwy(3))/10.0
      xnthwy=float(rathwy(4))/10.0
	zratnhr=ratnhr
      if(ratmod.lt.1.or.ratmod.gt.mxmoda) then
      write(lunerr,19201) rtno
      errf=.true. 
      frmode(rtno)=1
      endif
      ir=1
      if(rtno.gt.9) ir=2
      if(rtno.gt.99) ir=3
      zamhwy=xamhwy
      zpmhwy=xpmhwy
      zophwy=xophwy
	znthwy=xnthwy
      write(rtname,20200) ratid
c      write(lunrt,20001)
      ratmod4=ratmod
      write(lunrt)
     *ratlb,rtname,ratmod4,zamhwy,zpmhwy,zophwy,ratid,
     *frbyr(rtno,ratmod4)
	write(lunrtcsv,20220)
     *ratlb,rtname,ratmod4,zamhwy,zpmhwy,zophwy,znthwy,zratnhr,ratid,
     *frbyr(rtno,ratmod4)
	  
      volop=0.0
      if(xophwy.gt.0.0) volop=360.0/xophwy
      volam=0.0
      if(xamhwy.gt.0.0) volam=180.0/xamhwy
      volpm=0.0
      if(xpmhwy.gt.0.0) volpm=180.0/xpmhwy
      volnt=0.0
      if(xnthwy.gt.0.0) volnt=float(ratnhr)*60.0/xnthwy
      iln=iln+1
      imode=ratmod-3
      ratlin=iln
c
c     figure out length of variables for formatting printout
c
      l1=1
      if(rtno.gt.9) l1=2
      if(rtno.gt.99) l1=3
      do 201 i=1,24
      ii=24-i+1
      if(start(ii).ne.' ') go to 202
  201 continue
      start(1)='?'
      ii=1
  202 l2=ii
      do 203 i=1,24
      ii=24-i+1
      if(stop(ii).ne.' ') go to 204
  203 continue
      stop(1)='?'
      ii=1
  204 l3=ii
      do 205 i=1,20
      ii=20-i+1
      if(via(ii).ne.' ') go to 206
  205 continue
      ii=1
  206 l4=ii
      l5=1
      if(iln.gt.9) l5=2
      if(iln.gt.99) l5=3
      l6=lmode(imode)
      l7=lco(ratco)
      l8=3
      if(xamhwy.gt.9.9) l8=4
      if(xamhwy.gt.99.0) l8=5
      l9=3
      if(xophwy.gt.9.9) l9=4
      if(xophwy.gt.99.9) l9=5
      l11=(53-(l5+l6+l7+l8+l9))/2
      write(ratlun,rec=ratrec,iostat=ratio) ratlb,ratid,
     *ratco,ratmod,ratlin,rathwy(1),rathwy(2),rathwy(3),
     *rathwy(4),ratnhr,start,stop,via
      if(ratio.ne.0) go to 9000
      if(ratmod.le.5) go to 230
      iline=8
      npage=npage+1
      l11=(53-(l5+l6+l7+l8+l9))/2
      if(stop(1).eq.'l'.and.stop(2).eq.'o'.and.stop(3).eq.'o')
     *go to 223
      if(l4.lt.2) go to 222
      l10=(77-(l1+l2+l3+l4))/2
      write(lunpr,30221) eject,cheado,npage,
     *rtno,icon,(start(i2),i2=1,l2),(stop(i3),i3=1,l3),
     *(via(i4),i4=1,l4),iln,
     *nmode(imode),ncomp(ratco),xamhwy,xophwy
      eject = char(12)
      go to 240
  222 l10=(81-(l1+l2+l3))/2
      write(lunpr,30222) eject,cheado,npage,
     *rtno,icon,(start(i2),i2=1,l2),(stop(i3),i3=1,l3),
     *iln,nmode(imode),ncomp(ratco),xamhwy,xophwy
      eject = char(12)
      go to 240
  223 if(l4.lt.2) go to 224
      l10=(77-(l1+l2+l4))/2
      write(lunpr,30223) eject,cheado,npage,
     *rtno,icon,(start(i2),i2=1,l2),
     *(via(i4),i4=1,l4),iln,
     *nmode(imode),ncomp(ratco),xamhwy,xophwy
      eject = char(12)
      go to 240
  224 l10=(77-(l1+l2+l4))/2
      write(lunpr,30224) eject,cheado,npage,
     *rtno,icon,(start(i2),i2=1,l2),
     *iln,
     *nmode(imode),ncomp(ratco),xamhwy,xophwy
      eject = char(12)
      go to 240
  230 iline=8
      npage=npage+1
      l11=(53-(l5+l6+l7+l8+l9))/2
      if(stop(1).eq.'l'.and.stop(2).eq.'o'.and.stop(3).eq.'o')
     *go to 233
      if(l4.lt.2) go to 232
      l10=(77-(l1+l2+l3+l4))/2
      write(lunpr,30231) eject,cheado,npage,
     *rtno,icon,(start(i2),i2=1,l2),(stop(i3),i3=1,l3),
     *(via(i4),i4=1,l4),iln,
     *nmode(imode),ncomp(ratco),xamhwy,xophwy
      eject = char(12)
      go to 240
  232 l10=(81-(l1+l2+l3))/2
      write(lunpr,30232) eject,cheado,npage,
     *rtno,icon,(start(i2),i2=1,l2),(stop(i3),i3=1,l3),
     *iln,nmode(imode),ncomp(ratco),xamhwy,xophwy
      eject = char(12)
      go to 240
  233 if(l4.lt.2) go to 234
      l10=(77-(l1+l2+l4))/2
      write(lunpr,30233) eject,cheado,npage,
     *rtno,icon,(start(i2),i2=1,l2),
     *(via(i4),i4=1,l4),iln,
     *nmode(imode),ncomp(ratco),xamhwy,xophwy
      eject = char(12)
      go to 240
  234 l10=(77-(l1+l2+l4))/2
      write(lunpr,30234) eject,cheado,npage,
     *rtno,icon,(start(i2),i2=1,l2),
     *iln,
     *nmode(imode),ncomp(ratco),xamhwy,xophwy
      eject = char(12)
  240 if(.not.selrt(rtno)) go to 280
      if(ratmod.le.5) go to 250
      jline=8
      npage2=npage2+1
      l11=(53-(l5+l6+l7+l8+l9))/2
      if(stop(1).eq.'l'.and.stop(2).eq.'o'.and.stop(3).eq.'o')
     *go to 243
      if(l4.lt.2) go to 242
      l10=(77-(l1+l2+l3+l4))/2
      write(lunsel,30221) eject2,cheado,npage2,
     *rtno,icon,(start(i2),i2=1,l2),(stop(i3),i3=1,l3),
     *(via(i4),i4=1,l4),iln,
     *nmode(imode),ncomp(ratco),xamhwy,xophwy
      eject2 = char(12)
      go to 280
  242 l10=(81-(l1+l2+l3))/2
      write(lunsel,30222) eject2,cheado,npage2,
     *rtno,icon,(start(i2),i2=1,l2),(stop(i3),i3=1,l3),
     *iln,nmode(imode),ncomp(ratco),xamhwy,xophwy
      eject2 = char(12)
      go to 280
  243 if(l4.lt.2) go to 244
      l10=(77-(l1+l2+l4))/2
      write(lunsel,30223) eject2,cheado,npage2,
     *rtno,icon,(start(i2),i2=1,l2),
     *(via(i4),i4=1,l4),iln,
     *nmode(imode),ncomp(ratco),xamhwy,xophwy
      eject2 = char(12)
      go to 280
  244 l10=(77-(l1+l2+l4))/2
      write(lunsel,30224) eject2,cheado,npage2,
     *rtno,icon,(start(i2),i2=1,l2),
     *iln,
     *nmode(imode),ncomp(ratco),xamhwy,xophwy
      eject2 = char(12)
      go to 280
  250 jline=8
      npage2=npage2+1
      l11=(53-(l5+l6+l7+l8+l9))/2
      if(stop(1).eq.'l'.and.stop(2).eq.'o'.and.stop(3).eq.'o')
     *go to 253
      if(l4.lt.2) go to 252
      l10=(77-(l1+l2+l3+l4))/2
      write(lunsel,30231) eject2,cheado,npage2,
     *rtno,icon,(start(i2),i2=1,l2),(stop(i3),i3=1,l3),
     *(via(i4),i4=1,l4),iln,
     *nmode(imode),ncomp(ratco),xamhwy,xophwy
      eject2 = char(12)
      go to 280
  252 l10=(81-(l1+l2+l3))/2
      write(lunsel,30232) eject2,cheado,npage2,
     *rtno,icon,(start(i2),i2=1,l2),(stop(i3),i3=1,l3),
     *iln,nmode(imode),ncomp(ratco),xamhwy,xophwy
      eject2 = char(12)
      go to 280
  253 if(l4.lt.2) go to 254
      l10=(77-(l1+l2+l4))/2
      write(lunsel,30233) eject2,cheado,npage2,
     *rtno,icon,(start(i2),i2=1,l2),
     *(via(i4),i4=1,l4),iln,
     *nmode(imode),ncomp(ratco),xamhwy,xophwy
      eject2 = char(12)
      go to 280
  254 l10=(77-(l1+l2+l4))/2
      write(lunsel,30234) eject2,cheado,npage2,
     *rtno,icon,(start(i2),i2=1,l2),
     *iln,
     *nmode(imode),ncomp(ratco),xamhwy,xophwy
      eject2 = char(12)
  280 rdist=0.0
      rpkhtm=0.0
      rpkttm=0.0
      rophtm=0.0
      ropttm=0.0
      ropstp=0.0
      rpkstp=0.0
      pdist=0.0
      ppkhtm=0.0
      ppkttm=0.0
      pophtm=0.0
      popttm=0.0
      popstp=0.0
      ppkstp=0.0
      first=.true.
      iseq=0
      do i=1,mxid
      npass1(i)=0
      npass2(i)=0
      end do
      do i=1,mxseq
      seqstp(i)=0
      seqdst(i)=0.0
      backid(i)=0
      nextid(i)=0
      nextp(i)=0
      end do
      tmile=0.0
c
c     get ordered list of arcs and nodes for route
c
  300 call rtrcs
      if(secio.ne.0) go to 391
      if(alink.lt.1.or.alink.gt.mxtrca) go to 9286
      if(rlink.ne.ratlb) go to 390
      aatid=lbid(alink)
      if(secpos(1).gt.secpos(2)) then
      pm='+'
      else
      pm='-'
      endif
      write(lunlnk) rlink,aatid,pm 
c     write(lunlnk,20002) rlink,aatid,pm 
      npass1(aatid)=npass1(aatid)+1
      if(iseq.gt.0) then
       if(nextid(iseq).lt.1) then
       nextid(iseq)=aatid
       nextp(iseq)=npass1(aatid)
       endif
      endif
      tmp1=lbtype(alink)
      i1=tmp1/1000
      i=tmp1-i1*1000
      i2=i/100
      i=i-i2*100
      i3=i/10
      i4=i-i3*10
      iana=secarn(2)
      iant=lbnode(iana)
      sectpn(2)=iant
      ibna=secarn(1)
      ibnt=lbnode(ibna)
      sectpn(1)=ibnt
      iastp=nstop(iana)
      if(iastp.gt.ratmod) iastp=1
      ibstp=nstop(ibna)
      if(ibstp.gt.ratmod) ibstp=1
      ipkty=1
      iopty=1
      if(secpos(2).gt.0) go to  301
      if(i1.eq.2) ipkty=2
      if(i2.eq.2) iopty=2
      secstp(2)=iastp
      secstp(1)=ibstp
      ift=1
      itf=2
      go to 302
  301 if(i3.eq.2) ipkty=2
      if(i4.eq.2) iopty=2
      secstp(2)=iastp
      secstp(1)=ibstp
      ift=2
      itf=1
  302 if(.not.first) go to 310
      id1=aatid
      first=.false.
c
c     first record of route
c
      py=' '
      if(npark(iana)) py='Y'
      tty=' '
      if(ntt(iana)) tty='Y'
      call printx(iana,rtno,lbnode(iana),iastp)
	  iseq=iseq+1
      seqstp(iseq)=iana
      backid(iseq)=aatid
      write(lunpr,30017) lbnode(iana),lbtap(iana),secnm,secxnm(2),
     *nmstop(iastp),py,(prt(1,j),j=1,12)
      if(xline.gt.1) write(lunpr,30018) (prt(2,j),j=1,12)
      if(xline.gt.2) write(lunpr,30018) (prt(3,j),j=1,12)
      if(xline.gt.3) write(lunpr,30018) (prt(4,j),j=1,12)
      if(xline.gt.4) write(lunpr,30018) (prt(5,j),j=1,12)
      if(xline.gt.5) write(lunpr,30018) (prt(6,j),j=1,12)
      if(xline.gt.6) write(lunpr,30018) (prt(7,j),j=1,12)
      iline=iline+xline
      if(.not.selrt(rtno)) go to 310
      write(lunsel,30017)lbnode(iana),lbtap(iana),secnm,secxnm(2),
     *nmstop(iastp),py,(prt(1,j),j=1,12)
      if(xline.gt.1) write(lunsel,30018) (prt(2,j),j=1,12)
      if(xline.gt.2) write(lunsel,30018) (prt(3,j),j=1,12)
      if(xline.gt.3) write(lunsel,30018) (prt(4,j),j=1,12)
      if(xline.gt.4) write(lunsel,30018) (prt(5,j),j=1,12)
      if(xline.gt.5) write(lunsel,30018) (prt(6,j),j=1,12)
      if(xline.gt.6) write(lunsel,30018) (prt(7,j),j=1,12)
      jline=jline+xline
  310 xdist=lbdist(alink)
      pdist=pdist+xdist
      rdist=rdist+xdist
      if(.not.lkused(alink,imode).and.idir.eq.1) then
      mrtm(imode)=mrtm(imode)+xdist
      mrtm(mxmoda)=mrtm(mxmoda)+xdist
      lkused(alink,imode)=.true.
      endif
      modeFlg=brtmode(ratmod)
      pophtm=pophtm+idtm(modeFlg,ift,1,aatid)
      ppkhtm=ppkhtm+idtm(modeFlg,ift,2,aatid)
      ifc=lbfc(alink)
      if(ratmod.eq.4) go to 340
      if(ratmod.eq.5) go to 341
      if(ifc.ne.11) go to 311
      write(lunerr,19501) rtno,idir,icon
      errf=.true.
      faterr=.true.
c  311 if(ratmod.eq.6) go to 313
  311 continue
      if(.not.shtlrt(rtno).and.ibstp.gt.3.and..not.fwynod(ibna)) 
     *go to 312
      if(lastn(rlink).eq.ibna) go to 312
c      if(lbtap(ibna).gt.0) go to 312
      go to 380
  312 pkstp=stpdel(1,ratmod)
      opstp=stpdel(2,ratmod)
      go to 320
  313 if(ibstp.gt.3) go to 314
      go to 380
  314 acc=2.48
      if(ispd.ge.35) acc=1.66
      if(ispd.ge.50) acc=0.83
      mspd=ispd
      xspd=float(ispd)*5280./3600.
      xxdist=xdist*5280.
      avspd=xspd/2.0
      actime=float(ispd)/acc
      detime=float(ispd)/3.31
      acdist=(actime)*avspd
      dedist=(detime)*avspd
      detime=dedist/(avspd*60.0)
      actime=acdist/(avspd*60.0)
      pkstp=actime+detime+0.25
      opstp=actime+detime+0.25
  320 py=' '
      if(npark(ibna)) py='Y'
      tty=' '
      if(ntt(ibna)) tty='Y'
      pkhspd=pdist*60.0/ppkhtm
      pkttm=ppkhtm+pkstp
      pktspd=pdist*60.0/pkttm
      ophspd=pdist*60.0/pophtm
      opttm=pophtm+opstp
      optspd=pdist*60.0/opttm
      call printx(ibna,rtno,lbnode(ibna),ibstp)
      iiline=iline+xline
      if(iiline.lt.58) go to 323
      write(lunpr,30901) cdate,ctime
      iline=5
      npage=npage+1
      write(lunpr,30013) eject,cheado,npage
  323 write(lunpr,30017) lbnode(ibna),lbtap(ibna),secnm,secxnm(1),
     *nmstop(ibstp),py,(prt(1,j),j=1,12),pdist,
     *ppkhtm,pkhspd,pkstp,pkttm,pktspd,
     *pophtm,ophspd,opstp,opttm,optspd
      iispd=ifix(optspd/5.0)+1
      iispd=min(19,iispd)
      burtot(1,iispd)=burtot(1,iispd)+volop*pdist
      burtot(4,iispd)=burtot(4,iispd)+volnt*pdist
      iispd=ifix(pktspd/5.0)+1
      iispd=min(19,iispd)
      burtot(2,iispd)=burtot(2,iispd)+volam*pdist
      burtot(3,iispd)=burtot(3,iispd)+volpm*pdist
      iseq=iseq+1
      seqstp(iseq)=ibna
      tmile=tmile+pdist
      seqdst(iseq)=tmile
      backid(iseq)=aatid
      if(xline.gt.1) write(lunpr,30018) (prt(2,j),j=1,12)
      if(xline.gt.2) write(lunpr,30018) (prt(3,j),j=1,12)
      if(xline.gt.3) write(lunpr,30018) (prt(4,j),j=1,12)
      if(xline.gt.4) write(lunpr,30018) (prt(5,j),j=1,12)
      if(xline.gt.5) write(lunpr,30018) (prt(6,j),j=1,12)
      if(xline.gt.6) write(lunpr,30018) (prt(7,j),j=1,12)
      iline=iline+xline
      if(.not.selrt(rtno)) go to 325
      jjline=jline+xline
      if(jjline.lt.58) go to 324
      write(lunsel,30900) cdate,ctime
      jline=5
      npage2=npage2+1
      write(lunsel,30013) eject2,cheado,npage2
  324 write(lunsel,30017) lbnode(ibna),lbtap(ibna),secnm,secxnm(1),
     *nmstop(ibstp),py,(prt(1,j),j=1,12),pdist,
     *ppkhtm,pkhspd,pkstp,pkttm,pktspd,
     *pophtm,ophspd,opstp,opttm,optspd
      if(xline.gt.1) write(lunsel,30018) (prt(2,j),j=1,12)
      if(xline.gt.2) write(lunsel,30018) (prt(3,j),j=1,12)
      if(xline.gt.3) write(lunsel,30018) (prt(4,j),j=1,12)
      if(xline.gt.4) write(lunsel,30018) (prt(5,j),j=1,12)
      if(xline.gt.5) write(lunsel,30018) (prt(6,j),j=1,12)
      if(xline.gt.6) write(lunsel,30018) (prt(7,j),j=1,12)
  325 rpkhtm=rpkhtm+ppkhtm
      rophtm=rophtm+pophtm
      rpkstp=rpkstp+pkstp
      ropstp=ropstp+opstp
      pdist=0.0
      ppkhtm=0.0
      pophtm=0.0
      jline=jline+xline
      go to 380
  340 istpf=1
      dtime=1.0
      ispd=lbispd(alink)
      if(ispd.lt.50) ispd=50
      go to 342
  341 dtime=float(ndwell(ibna))/100.0
      ispd=lbispd(alink)
  342 if(ifc.eq.11) go to 343
      write(lunerr,19502) rtno,idir,icon
      errf=.true.
      faterr=.true.
  343 aatosp=lbospd(alink)
      actime=0.0
      detime=0.0
      stime=0.0
      rtime=0.0
      if(aatosp.lt.1) go to 344
      dtime=0.0
      mspd=ispd
      xspd=float(aatosp)
      rtime=xdist/xspd*60.
      go to 350
  344 acc=2.48
      if(ispd.ge.35) acc=1.66
      if(ispd.ge.50) acc=0.83
      mspd=ispd
      xspd=float(ispd)*5280./3600.
      xxdist=xdist*5280.
      avspd=xspd/2.0
      actime=float(ispd)/acc
      detime=float(ispd)/3.31
      acdist=(actime)*avspd
      dedist=(detime)*avspd
      todist=acdist+dedist
      if(xxdist.lt.todist) go to 345
      rtime=(xxdist-todist)/(xspd*60.)
      detime=dedist/(avspd*60.0)
      actime=acdist/(avspd*60.0)
      go to 350
  345 do 346 i=1,ispd
      mspd=ispd-i
      acc=2.48
      if(mspd.ge.35) acc=1.66
      if(mspd.ge.50) acc=0.83
      xspd=float(mspd)
      avspd=xspd/2.0
      actime=xspd/acc
      detime=xspd/3.31
      acdist=(actime)*avspd
      dedist=(detime)*avspd
      todist=acdist+dedist
      if(xxdist.ge.todist) go to 347
  346 continue
  347 rtime=(xxdist-todist)/(xspd*60.)
      detime=dedist/(avspd*60.0)
      actime=acdist/(avspd*60.0)
c
c     print out data
c
  350 py=' '
      if(npark(ibna)) py='Y'
      tty=' '
      if(ntt(ibna)) tty='Y'
      ttime=rtime+actime+detime+dtime+stime
      idtm(1,ift,1,aatid)=ttime
      idtm(1,ift,2,aatid)=ttime
      idtm(1,ift,3,aatid)=ttime
      tspd=0.0
      if(ttime.gt.0.0)
     *tspd=pdist*60.0/ttime
      call printx(ibna,rtno,lbnode(ibna),ibstp)
      iiline=iline+xline
      if(iiline.lt.58) go to 351
      write(lunpr,30901) cdate,ctime
      iline=5
      npage=npage+1
      write(lunpr,30239) eject,cheado,npage
  351 write(lunpr,30028) lbnode(ibna),lbtap(ibna),secnm,secxnm(1),
     *nmstop(ibstp),py,(prt(1,j),j=1,12),pdist,
     *ispd,mspd,rtime,actime,detime,dtime,stime,ttime,tspd
      iseq=iseq+1
      seqstp(iseq)=ibna
      tmile=tmile+pdist
      seqdst(iseq)=tmile
      backid(iseq)=aatid
      if(xline.gt.1) write(lunpr,30018) (prt(2,j),j=1,12)
      if(xline.gt.2) write(lunpr,30018) (prt(3,j),j=1,12)
      if(xline.gt.3) write(lunpr,30018) (prt(4,j),j=1,12)
      if(xline.gt.4) write(lunpr,30018) (prt(5,j),j=1,12)
      if(xline.gt.5) write(lunpr,30018) (prt(6,j),j=1,12)
      if(xline.gt.6) write(lunpr,30018) (prt(7,j),j=1,12)
      iline=iline+xline
      if(.not.selrt(rtno)) go to 353
      jjline=jline+xline
      if(jjline.lt.58) go to 352
      write(lunsel,30900) cdate,ctime
      jline=5
      npage2=npage2+1
      write(lunsel,30239) eject2,cheado,npage2
  352 write(lunsel,30028) lbnode(ibna),lbtap(ibna),secnm,secxnm(1),
     *nmstop(ibstp),py,(prt(1,j),j=1,12),pdist,
     *ispd,mspd,rtime,actime,detime,dtime,stime,ttime,tspd
      if(xline.gt.1) write(lunsel,30018) (prt(2,j),j=1,12)
      if(xline.gt.2) write(lunsel,30018) (prt(3,j),j=1,12)
      if(xline.gt.3) write(lunsel,30018) (prt(4,j),j=1,12)
      if(xline.gt.4) write(lunsel,30018) (prt(5,j),j=1,12)
      if(xline.gt.5) write(lunsel,30018) (prt(6,j),j=1,12)
      if(xline.gt.6) write(lunsel,30018) (prt(7,j),j=1,12)
      jline=jline+xline
  353 pdist=0.0
      rr=rr+rtime
      rac=rac+actime
      rde=rde+detime
      rdw=rdw+dtime
      rs=rs+stime
  380 call wtrcs
      if(secio.ne.0) go to 9000
      if(xophwy.gt.0.0) then
      if(secarn(1).gt.0) used(1,secarn(1))=.true.
      if(secarn(2).gt.0) used(1,secarn(2))=.true.
      endif
      if(xamhwy.gt.0.0) then
      if(secarn(1).gt.0) used(2,secarn(1))=.true.
      if(secarn(2).gt.0) used(2,secarn(2))=.true.
      endif
      secrec=secrec+1
      go to 300
c
c     write out route totals
c
  390 mseq=iseq
      do iseq=1,mseq
      istop=seqstp(iseq)
      aatid=nextid(iseq)
      if(aatid.lt.1) aatid=backid(iseq)
      if(iseq.eq.1) aatid=id1
      iout=iout+1
      ipass=1
      if(nextp(iseq).gt.0) ipass=nextp(iseq)
      if(iseq.eq.mseq) ipass=npass1(aatid)
c      write(lunstp,20003) iout,ratlb,aatid,ipass,
      write(lunstp) iout,ratlb,aatid,ipass,
     *seqdst(iseq),lblng(istop),lblat(istop),lbnode(istop)
      end do
      if(ratmod.le.5) go to 392
      pkhspd=rdist*60.0/rpkhtm
      pkttm=rpkhtm+rpkstp
      pktspd=rdist*60.0/pkttm
      ophspd=rdist*60.0/rophtm
      opttm=rophtm+ropstp
      optspd=rdist*60.0/opttm
      write(lunpr,30019) rdist,
     *rpkhtm,pkhspd,rpkstp,pkttm,pktspd,
     *rophtm,ophspd,ropstp,opttm,optspd
      lnoptm(iln)=opttm
      lnpktm(iln)=pkttm
      lndist(iln)=rdist
      write(lunpr,30901) cdate,ctime
      if(.not.selrt(rtno)) go to 391
      write(lunsel,30019) rdist,
     *rpkhtm,pkhspd,rpkstp,pkttm,pktspd,
     *rophtm,ophspd,ropstp,opttm,optspd
      write(lunsel,30900) cdate,ctime
  391 rdist=0.0
      rophtm=0.0
      rpkhtm=0.0
      ropstp=0.0
      rpkstp=0.0
      go to 398
  392 ttime=rr+rac+rde+rdw+rs
      tspd=rdist*60.0/ttime
      write(lunpr,30029)
     *rdist,rr,rac,rde,rdw,rs,ttime,tspd
      lnoptm(iln)=ttime
      lnpktm(iln)=ttime
      lndist(iln)=rdist
      write(lunpr,30901) cdate,ctime
      if(.not.selrt(rtno)) go to 393
      write(lunsel,30029)
     *rdist,rr,rac,rde,rdw,rs,ttime,tspd
      write(lunsel,30900) cdate,ctime
  393 rr=0.0
      rac=0.0
      rde=0.0
      rdw=0.0
      rs=0.0
  398 ratrec=ratrec+1
      lon=0
      go to 200
  399 mseq=iseq
      do iseq=1,mseq
      istop=seqstp(iseq)
      aatid=nextid(iseq)
      if(aatid.lt.1) aatid=backid(iseq)
      if(iseq.eq.1) aatid=id1
      iout=iout+1
      ipass=1
      if(nextp(iseq).gt.0) ipass=nextp(iseq)
      if(iseq.eq.mseq) ipass=npass1(aatid)
c      write(lunstp,20003) iout,ratlb,aatid,ipass,
      write(lunstp) iout,ratlb,aatid,ipass,
     *seqdst(iseq),lblng(istop),lblat(istop),lbnode(istop)
      end do
      pkhspd=rdist*60.0/rpkhtm
      pkttm=rpkhtm+rpkstp
      pktspd=rdist*60.0/pkttm
      ophspd=rdist*60.0/rophtm
      opttm=rophtm+ropstp
      optspd=rdist*60.0/opttm
      write(lunpr,30019) rdist,
     *rpkhtm,pkhspd,rpkstp,pkttm,pktspd,
     *rophtm,ophspd,ropstp,opttm,optspd
      write(lunpr,30901) cdate,ctime
      lnoptm(iln)=opttm
      lnpktm(iln)=pkttm
      lndist(iln)=rdist
      close(lunpr)
      if(.not.selrt(rtno)) go to 700
      write(lunsel,30019) rdist,
     *rpkhtm,pkhspd,rpkstp,pkttm,pktspd,
     *rophtm,ophspd,ropstp,opttm,optspd
      write(lunsel,30900) cdate,ctime
c
c     open trcov nat file and write out stop data
c
  700 natrec=1
  701 call rtrcn
      if(natio.ne.0) go to 704
      if(natlb.gt.mxtrcn) go to 9120	 
      ix=natx+100
      iy=naty+100
      if(nattap.gt.0) then
      aatid=mxid+nattap  
      write(lungen,20700) aatid,natx,naty,ix,iy
	endif
      natstp(1)=nstop(natlb)
      natprk(1)=1
      if(npark(natlb)) natprk(1)=2
      natdwl(1)=ndwell(natlb)
      nattmp=0
      if(used(1,natlb)) then
       if(used(2,natlb)) then
       nattmp=2
       else
       nattmp=1
       endif
      else
      if(used(2,natlb)) nattmp=1
      endif
  703 call wtrcn
      if(natio.ne.0) go to 9000
      natrec=natrec+1
      go to 701
  704 close(natlun)
      write(lungen,20709)
c
c     read trcov arc attribute table
c
      aatrec=1
  710 call rtrca 
      if(aatio.ne.0) go to 719 
      do 712 ipk=1,3
      do 711 idir=1,2
      aattm(ipk,idir)=idtm(1,idir,ipk,aatid)
  711 continue
  712 continue 
      aatcrz=0
      aatlrz=0
      if(aatmm.gt.5) go to 713
      if(aatlb.gt.mxtrca) go to 9040 
      aatfz=1
      if(aatmm.eq.4) then !coaster
c      iz=max(nfz(aatarn(1)),nfz(aatarn(2)))
c      aatcrz=1
c      if(iz.eq.2) aatcrz=10
c      if(iz.eq.3) aatcrz=100
c      if(iz.eq.4) aatcrz=1000
c     set up fare zone link flag field aatcrz according to the two node fare zone value
c     two stations within the same zone
      if(nfz(aatarn(1)).eq.nfz(aatarn(2)).and.nfz(aatarn(1)).gt.0)
     *aatcrz=crzone(nfz(aatarn(1))) !within Zone 
c     neighboring two stations in two zones
      if (nfz(aatarn(1)).ne.nfz(aatarn(2)))then
       iz=max(nfz(aatarn(1)),nfz(aatarn(2)))
       if (iz.gt.0)aatcrz=crzone2(iz)
      end if

      else !LRT
c      if LRT links are EXPRESS than don't give them a fare code to avoid skip stops yielding lower fare than local service
       i=index(aatnm,'EXPRESS')  
        if(i.lt.1) then 
         aatlrz=1
         if(aatsph.eq.1404) aatlrz=1000
        else
         aatlrz=5
        endif 
      endif 
  713 call wtrca
      if(aatio.ne.0) go to 9000
      aatrec=aatrec+1
      go to 710
  719 close(aatlun)
c      fnamo='tctr.sum'
c  800 open (unit=lunpr,file=fnamo,err=9000)
c      print *,fnamo
      eject=' '
      npage=0
      iln=0
      iline=65
      ratrec=1
c
c     produce line summary
c
  801 call rtrcr
      if(ratio.ne.0) go to 810
      rtno=ratid/1000
      idir=(ratid-rtno*1000)/100
      icon=ratid-rtno*1000-idir*100
      iln=iln+1
      im=ratmod-3
      xamhwy=float(rathwy(1))/10.0
      xpmhwy=float(rathwy(2))/10.0
      xophwy=float(rathwy(3))/10.0
      xnthwy=float(rathwy(4))/10.0
      ipktm=nint(lnpktm(iln))
      ioptm=nint(lnoptm(iln))
      if(iline.lt.57) go to 802
      npage=npage+1
      if(eject.ne.' ') write(lunprsum,30902) cdate,ctime
      write(lunprsum,30080) eject,cheado,npage
      eject = char(12)
      iline=5
  802 runsop=0.0
      runsam=0.0
      runspm=0.0
      runsnt=0.0
      if(xophwy.gt.0.0) runsop=360.0/xophwy
      if(xamhwy.gt.0.0) runsam=180.0/xamhwy
      if(xpmhwy.gt.0.0) runspm=180.0/xpmhwy
      if(xnthwy.gt.0.0) runsnt=60.0*float(ratnhr)/xnthwy
      iopvm=nint(lndist(iln)*runsop)
      iamvm=nint(lndist(iln)*runsam)
      ipmvm=nint(lndist(iln)*runspm)
      intvm=nint(lndist(iln)*runsnt)
      idavm=iopvm+iamvm+ipmvm+intvm
      iopvh=nint(lnoptm(iln)*runsop)
      iamvh=nint(lnpktm(iln)*runsam)
      ipmvh=nint(lnpktm(iln)*runspm)
      intvh=nint(lnoptm(iln)*runsnt)
      idavh=iopvh+iamvh+ipmvh+intvh
      iopve=0
      iamve=0
      if(xophwy.gt.0.0) iopve=ifix(lnoptm(iln)/(xophwy)+0.9)
      if(xamhwy.gt.0.0) iamve=ifix(lnpktm(iln)/(xamhwy)+0.9)
      iopcs=nint(float(iopvm)*vmcst(ratco)+float(iopvh)*vmncst(ratco))
      iamcs=nint(float(iamvm)*vmcst(ratco)+float(iamvh)*vmncst(ratco))
      ipmcs=nint(float(ipmvm)*vmcst(ratco)+float(ipmvh)*vmncst(ratco))
      intcs=nint(float(intvm)*vmcst(ratco)+float(intvh)*vmncst(ratco))
      idacs=iopcs+iamcs+ipmcs+intcs
      write(lunprsum,30081) iln,rtno,adir(idir),icon,xamhwy,xpmhwy,
     *xophwy,xnthwy,lndist(iln),iamvm,iopvm,idavm,ipktm,ioptm,
     *iamvh,iopvh,idavh,iamve,iopve,iamcs,iopcs,idacs
      iline=iline+1
      ratrec=ratrec+1
      go to 801
c
c     repeat for route summary
c
  810 iline=65
      iln=0
      endf=.false.
      ratrec=1
      amvm=0.0
      pmvm=0.0
      opvm=0.0
      ntvm=0
      amvh=0.0
      pmvh=0.0
      opvh=0.0
      ntvh=0.0
      iamve=0
      ipmve=0
      iopve=0
      intve=0
      call rtrcr
      if(ratio.ne.0) go to 820
      ortno=ratid/1000
      lrco=ratco
      im=ratmod-3
      lim=ratmod
  811 call rtrcr
      if(ratio.ne.0) go to 820
      rtno=ratid/1000

      if(rtno.eq.ortno) go to 813

      if(iline.lt.57) go to 812
      npage=npage+1
      write(lunprsum,30903) cdate,ctime
      write(lunprsum,30082) eject,cheado,npage
      iline=5
  812 iamvm=amvm
      ipmvm=pmvm
      iopvm=opvm
      intvm=ntvm

      iamvh=amvh
      ipmvh=pmvh
      iopvh=opvh
      intvh=ntvh
      idavm=iopvm+iamvm+ipmvm+intvm
      idavh=iopvh+iamvh+ipmvh+intvh
      amcs=amvm*vmcst(lrco)+amvh*vmncst(lrco)
      pmcs=pmvm*vmcst(lrco)+pmvh*vmncst(lrco)
      opcs=opvm*vmcst(lrco)+opvh*vmncst(lrco)
      ntcs=ntvm*vmcst(lrco)+ntvh*vmncst(lrco)
      iamcs=nint(amcs)
      ipmcs=nint(pmcs)
      iopcs=nint(opcs)
      intcs=ntcs
      idacs=iopcs+iamcs+ipmcs+intcs
      write(lunprsum,30083) ortno,ncomp(lrco),nmode(lim),
     *iamvm,ipmvm,iopvm,intvm,idavm,
     *iamvh,ipmvh,iopvh,intvh,idavh,
     *iamve,ipmve,iopve,intve,
     *iamcs,ipmcs,iopcs,intcs,idacs
      mamvm(lim)=mamvm(lim)+amvm
      mpmvm(lim)=mpmvm(lim)+pmvm
      mopvm(lim)=mopvm(lim)+opvm
      mntvm(lim)=mntvm(lim)+ntvm
      mamvh(lim)=mamvh(lim)+amvh
      mpmvh(lim)=mpmvh(lim)+pmvh
      mopvh(lim)=mopvh(lim)+opvh
      mntvh(lim)=mntvh(lim)+ntvh
      mamve(lim)=mamve(lim)+iamve
      mpmve(lim)=mpmve(lim)+ipmve
      mopve(lim)=mopve(lim)+iopve
      mntve(lim)=mntve(lim)+intve
      mamcs(lim)=mamcs(lim)+amcs
      mpmcs(lim)=mpmcs(lim)+pmcs
      mopcs(lim)=mopcs(lim)+opcs
      mntcs(lim)=mntcs(lim)+ntcs
      mamvm(mxmoda)=mamvm(mxmoda)+amvm
      mpmvm(mxmoda)=mpmvm(mxmoda)+pmvm
      mopvm(mxmoda)=mopvm(mxmoda)+opvm
      mntvm(mxmoda)=mntvm(mxmoda)+ntvm
      mamvh(mxmoda)=mamvh(mxmoda)+amvh
      mpmvh(mxmoda)=mpmvh(mxmoda)+pmvh
      mopvh(mxmoda)=mopvh(mxmoda)+opvh
      mntvh(mxmoda)=mntvh(mxmoda)+ntvh
      mamve(mxmoda)=mamve(mxmoda)+iamve
      mpmve(mxmoda)=mpmve(mxmoda)+ipmve
      mopve(mxmoda)=mopve(mxmoda)+iopve
      mntve(mxmoda)=mntve(mxmoda)+intve
      mamcs(mxmoda)=mamcs(mxmoda)+amcs
      mpmcs(mxmoda)=mpmcs(mxmoda)+pmcs
      mopcs(mxmoda)=mopcs(mxmoda)+opcs
      mntcs(mxmoda)=mntcs(mxmoda)+ntcs
      iline=iline+1
      amvm=0.0
      pmvm=0.0
      opvm=0.0
      ntvm=0
      amvh=0.0
      pmvh=0.0
      opvh=0.0
      ntvh=0.0
      iamve=0
      ipmve=0
      iopve=0
      intve=0
      ortno=rtno
      if(endf) go to 820
  813 iln=iln+1
      im=ratmod-3

      lrco=ratco
      lim=im
      xamhwy=float(rathwy(1))/10.0
      xpmhwy=float(rathwy(2))/10.0
      xophwy=float(rathwy(3))/10.0
      xnthwy=float(rathwy(4))/10.0
      runsam=0.0
      runspm=0.0
      runsop=0.0
      runsnt=0.0
      if(xamhwy.gt.0.0) runsam=180.0/xamhwy
      if(xpmhwy.gt.0.0) runspm=180.0/xpmhwy
      if(xophwy.gt.0.0) runsop=360.0/xophwy
      if(xnthwy.gt.0.0) runsnt=float(ratnhr)*60.0/xnthwy

      amvm=amvm+lndist(iln)*runsam
      pmvm=pmvm+lndist(iln)*runspm
      opvm=opvm+lndist(iln)*runsop
      ntvm=ntvm+lndist(iln)*runsnt
      amvh=amvh+lnpktm(iln)*runsam
      pmvh=pmvh+lnpktm(iln)*runspm
      opvh=opvh+lnoptm(iln)*runsop

      ntvh=ntvh+lnoptm(iln)*runsnt
      if(xamhwy.gt.0.0) iamve=iamve+ifix(lnpktm(iln)/xamhwy+0.9)
      if(xpmhwy.gt.0.0) ipmve=ipmve+ifix(lnpktm(iln)/xpmhwy+0.9)
      if(xophwy.gt.0.0) iopve=iopve+ifix(lnoptm(iln)/xophwy+0.9)

      if(xnthwy.gt.0.0) intve=intve+ifix(lnoptm(iln)/xnthwy+0.9)
      ratrec=ratrec+1
      go to 811
  819 endf=.true.
      go to 812
c
c     repeat for mode-commany summary
c
  820 npage=npage+1
      write(lunprsum,30084) eject,cheado,npage
      do im=1,mxmoda
      iamvm=nint(mamvm(im))
      ipmvm=nint(mpmvm(im))
      iopvm=nint(mopvm(im))
      intvm=nint(mntvm(im))
      idavm=iopvm+iamvm+ipmvm+intvm
       if(idavm.gt.0) then
        amvh=mamvh(im)/60.0
        pmvh=mpmvh(im)/60.0
        opvh=mopvh(im)/60.0
        ntvh=mntvh(im)/60.0
        iamvh=nint(amvh)
        ipmvh=nint(pmvh)
        iopvh=nint(opvh)
        intvh=nint(ntvh)
        idavh=iopvh+iamvh+ipmvh+intvh
        iamcs=nint(mamcs(im))
        ipmcs=nint(mpmcs(im))
        iopcs=nint(mopcs(im))
        intcs=nint(mntcs(im))
        irtm=nint(mrtm(im))
        idacs=iopcs+iamcs+ipmcs+intcs
        write(lunprsum,30085) nmode(im),irtm,
     *  iamvm,ipmvm,iopvm,intvm,idavm,
     *  iamvh,ipmvh,iopvh,intvh,idavh,
     *  mamve(im),mpmve(im),mopve(im),mntve(im),
     *  iamcs,ipmcs,iopcs,intcs,idacs
        if(im.eq.mxmoda) xx=idavm
       end if
      end do
      close(lunprsum)
      ialt=1       
      write(lunsum,20901) ialt,cheadi,cdate,ctime,xx
      close(lunsum)

c     write out timed transfer connections

      open (unit=luno,file='timexfer.dcb')
      write(luno,20841)
      write(luno,20842)
      write(luno,20843)
      write(luno,20844)
      write(luno,20845)
      write(luno,20846)
      close(luno)
      imiss=-2147483647
      fnami='timexfer.prn'
      call opendata(lunin)
      if(istat.ne.0) go to 9999
  840 read(lunin,10840,iostat=istat) irt1,idir1,irt2,idir2,ixmin
      if(istat.ne.0) go to 849
      if(irt1.lt.1.or.irt1.gt.999) go to 9001
      if(irt2.lt.1.or.irt2.gt.999) go to 9001
      if(idir1.lt.1.or.idir1.gt.2) go to 9001
      if(idir2.lt.1.or.idir2.gt.2) go to 9001
      ilb1=rtdirlb(irt1,idir1)
      ilb2=rtdirlb(irt2,idir2)
      print *,ixmin
      if(ilb1.gt.0.and.ilb2.gt.0) write(luntt),ilb1,ilb2,imiss,ixmin
      go to 840
  849 close(lunin)

      go to 9999
 9000 write(lunerr,19000) fnamo
      errf=.true.
      go to 9999
 9001 write(lunerr,19001) fnamo,adata
      errf=.true.
      go to 9999
 9040 write(lunerr,19040) aatlb,aatarn
      errf=.true.
      go to 9999
 9120 write(lunerr,19120) natlb
      errf=.true.
      go to 9999
 9200 write(lunerr,19200) ratlb,ratid
      errf=.true.
      go to 9999
 9286 write(lunerr,19286) rlink,alink
      errf=.true.
      go to 9999
 1099 format(i5)

10000 format(80i1,t1,80a1)
10001 format(i2,80a1)
10002 format(a80)
10003 format(80a1)
10004 format(2i10,f10.0)
10010 format(a20,i4)
10011 format(a53,i4,a7,f4.2)
10020 format(i6,12f9.3)
10030 format(2i8,f8.2)
10031 format(2i4,f4.2)
10021 format(a20)
10190 format(2f20.8)
10200 format(6x,i5,1x,a2,1x,a1,i2,2x,4i5,i2,t9,i1)
10840 format(5i8)
19000 format(' problem opening file: ',a80)
19001 format(' data problem with file: ',a80/,80a1)
19002 format(i5,f7.1,4f5.2)
19040 format(' data problem with trcov.aat ', 5i10)
19120 format(' data problem with stopcov.pat ', i10)
19200 format(' data problem with ratdata',2i10)
19201 format(' missing fare mode for route',i10)
19286 format(' data problem with secdata',2i10)
19501 format(' rail route on street: ',3i4)
19502 format(' bus route on rail line: ',3i4)
19608 format(' probably want to insert a tap on route: ',i3,
     *' CONFIGURATION: ',i2,' between tap: ',i5,' and tap: ',i5,
     *' distance= ',f5.1,' stops= ',i2)
20001 format(3i10,3f10.5,i10,f8.2)
20002 format(2i10,1x,a1)
20003 format(4i10,f10.5,3i10)
20013 format(i3,',',i5,',',i1)
20121 format('NODE #,STOP TYPE,FIRST CROSS STREET,SECOND CROSS STREET,',
     *'ROUTE 1,ROUTE 2,ROUTE 3,ROUTE 4,ROUTE 5',
     *'ROUTE 6,ROUTE 7,ROUTE 8,ROUTE 9,ROUTE 10',
     *'ROUTE 11,ROUTE 12,ROUTE 13,ROUTE 14,ROUTE 15',
     *'ROUTE 16,ROUTE 17,ROUTE 18,ROUTE 19,ROUTE 20')
20120 format(i4,',',a4,',',a20,',',a20,20(',',i3))
20180 format(i4)
20200 format(i6)
20210 format('route_id, route_name,Mode,AM_Headway, PM_Headway
     *, OP_Headway, NT_headway, NT_Hour, Config, Fare')
20220 format(i4,',',a32,',' i4,4(',',f8.1),',',i4,',',i6,',',f8.2)
20700 format('      ',i5,/,i7,',',i7,/,i7,',',i7,/,'end')
20709 format('end')
20841 format(' ')
20842 format('16')
20843 format('"FROM_LINE",I,1,4,0,10,0,,,"",,Blank,')
20844 format('"TO_LINE",I,5,4,0,10,0,,,"",,Blank,')
20845 format('"BOARD_STOP",I,9,4,0,10,0,,,"",,Blank,')
20846 format('"WAIT_TIME",I,13,4,0,10,0,,,"",,Blank,')
20901 format(i2,','80a1,',49,daily transit vehicle miles,"',
     *a7,'","',a8,'",',f10.0,',')
29999 format('error:',i3,',',i5,',',i1)
30221 format(a1,t27,80a1,t126,'PAGE',i4,//,
     *<l10>x,'TRANSIT ROUTE: ',i<l1>,'  CONFIGURATION: ',i2,
     *'  FROM: ',<l2>a1,'  TO: ',<l3>a1,'  VIA: ',<l4>a1,/,
     *<l11>x,'TRANSCAD LINE NUMBER: ',i<l5>,'  MODE: ',a<l6>,
     *'  COMPANY: ',a<l7>,'  PEAK HEADWAY: ',f<l8>.1,
     *'  OFFPEAK HEADWAY: ',f<l9>.1,//,
     *t83,'----- PEAK PERIOD -----   --- OFFPEAK PERIOD ----',/,
     *'  TCAD  TCAD STREET',t35,'CROSS STREET',t56,'STOP P TRANSFER',
     *'RING      ',2('   HWY HWY  STOP  TRN TRN '),/,
     *'  NODE   TAP NAME',t35,'NAME',t56,        'TYPE R ROUTES  ',
     *'     MILES ',2('  MIN MPH   MIN  MIN MPH  '))
30222 format(a1,t27,80a1,t126,'PAGE',i4,//,
     *<l10>x,'TRANSIT ROUTE: ',i<l1>,'  CONFIGURATION: ',i2,
     *'  FROM: ',<l2>a1,'  TO: ',<l3>a1,/,
     *<l11>x,'TRANSCAD LINE NUMBER: ',i<l5>,'  MODE: ',a<l6>,
     *'  COMPANY: ',a<l7>,'  PEAK HEADWAY: ',f<l8>.1,
     *'  OFFPEAK HEADWAY: ',f<l9>.1,//,
     *t83,'----- PEAK PERIOD -----   --- OFFPEAK PERIOD ----',/,
     *'  TCAD  TCAD STREET',t35,'CROSS STREET',t56,'STOP P TRANSFER',
     *'RING      ',2('   HWY HWY  STOP  TRN TRN '),/,
     *'  NODE   TAP NAME',t35,'NAME',t56,        'TYPE R ROUTES  ',
     *'     MILES ',2('  MIN MPH   MIN  MIN MPH  '))
30223 format(a1,t27,80a1,t126,'PAGE',i4,//,
     *<l10>x,'TRANSIT ROUTE: ',i<l1>,'  CONFIGURATION: ',i2,
     *' ',<l2>a1,' LOOP VIA: ',<l4>a1,/,
     *<l11>x,'TRANSCAD LINE NUMBER: ',i<l5>,'  MODE: ',a<l6>,
     *'  COMPANY: ',a<l7>,'  PEAK HEADWAY: ',f<l8>.1,
     *'  OFFPEAK HEADWAY: ',f<l9>.1,//,
     *t83,'----- PEAK PERIOD -----   --- OFFPEAK PERIOD ----',/,
     *'  TCAD  TCAD STREET',t35,'CROSS STREET',t56,'STOP P TRANSFER',
     *'RING      ',2('   HWY HWY  STOP  TRN TRN '),/,
     *'  NODE   TAP NAME',t35,'NAME',t56,        'TYPE R ROUTES  ',
     *'     MILES ',2('  MIN MPH   MIN  MIN MPH  '))
30224 format(a1,t27,80a1,t126,'PAGE',i4,//,
     *t83,'----- PEAK PERIOD -----   --- OFFPEAK PERIOD ----',/,
     *'  TCAD  TCAD STREET',t35,'CROSS STREET',t56,'STOP P TRANSFER',
     *'RING      ',2('   HWY HWY  STOP  TRN TRN '),/,
     *'  NODE   TAP NAME',t35,'NAME',t56,        'TYPE R ROUTES  ',
     *'     MILES ',2('  MIN MPH   MIN  MIN MPH  '))
30013 format(a1,t27,80a1,t126,'PAGE',i4,//,
     *t83,'----- PEAK PERIOD -----   --- OFFPEAK PERIOD ----',/,
     *'  TCAD  TCAD STREET',t35,'CROSS STREET',t56,'STOP P TRANSFER',
     *'RING      ',2('   HWY HWY  STOP  TRN TRN '),/,
     *'  NODE   TAP NAME',t35,'NAME',t56,        'TYPE R ROUTES  ',
     *'     MILES ',2('  MIN MPH   MIN  MIN MPH  '))
30014 format(a1,1x,2a8,9x,80a1,t126,'PAGE',i4,//,
     *<l10>x,'TRANSIT ROUTE: ',i<l1>,'  CONFIGURATION: ',i2,
     *<l2>a1,' LOOP','  VIA: ',<l4>a1,/,
     *<l11>x,'TRANSCAD LINE NUMBER: ',i<l5>,'  MODE: ',a<l6>,
     *'  COMPANY: ',a<l7>,'  PEAK HEADWAY: ',f<l8>.1,
     *'  OFFPEAK HEADWAY: ',f<l9>.1,//,
     *t83,'----- PEAK PERIOD -----   --- OFFPEAK PERIOD ----',/,
     *'  TCAD  TCAD STREET',t35,'CROSS STREET',t56,'STOP P TRANSFER',
     *'RING      ',2('   HWY HWY  STOP  TRN TRN '),/,
     *'  NODE   TAP NAME',t35,'NAME',t56,        'TYPE R ROUTES  ',
     *'     MILES ',2('  MIN MPH   MIN  MIN MPH  '))
30015 format(a1,1x,2a8,9x,80a1,t126,'PAGE',i4,//,
     *<l10>x,'TRANSIT ROUTE: ',i<l1>,'  CONFIGURATION: ',i2,
     *<l2>a1,' LOOP',/,
     *<l11>x,'TRANSCAD LINE NUMBER: ',i<l5>,'  MODE: ',a<l6>,
     *'  COMPANY: ',a<l7>,'  PEAK HEADWAY: ',f<l8>.1,
     *'  OFFPEAK HEADWAY: ',f<l9>.1,//,
     *t83,'----- PEAK PERIOD -----   --- OFFPEAK PERIOD ----',/,
     *'  TCAD  TCAD STREET',t35,'CROSS STREET',t56,'STOP P TRANSFER',
     *'RING      ',2('   HWY HWY  STOP  TRN TRN '),/,
     *'  NODE   TAP NAME',t35,'NAME',t56,        'TYPE R ROUTES  ',
     *'     MILES ',2('  MIN MPH   MIN  MIN MPH  '))
30017 format(2i6,1x,a20,1x,a20,1x,a4,1x,a1,1x,12a1,
     *f6.1,2(f6.1,f5.0,f5.1,f6.1,f4.0))
30018 format(t63,12a1)
30019 format(/,t14,'ROUTE TOTAL',t75,
     *f6.1,2(f6.1,f5.0,f5.1,f6.1,f4.0))
30231 format(a1,t27,80a1,t126,'PAGE',i4,//,
     *<l10>x,'TRANSIT ROUTE: ',i<l1>,'  CONFIGURATION:',i2,
     *'  FROM: ',<l2>a1,'  TO: ',<l3>a1,'  VIA: ',<l4>a1,/,
     *<l11>x,'TRANSCAD LINE NUMBER: ',i<l5>,'  MODE: ',a<l6>,
     *'  COMPANY: ',a<l7>,'  PEAK HEADWAY: ',f<l8>.1,
     *'  OFFPEAK HEADWAY: ',f<l9>.1,//,
     *'  TCAD  TCAD RAIL',t35,' STATION',t56,'STOP P TRANSFERRING ',
     *'      PEAK MAX   RUN ACCEL DECEL DWELL SIGNAL SIGNAL TOTAL',/,
     *'  NODE   TAP NAME',t35,'NAME',t56,'TYPE R ROUTES       ',
     *'MILES  MPH MPH   MIN   MIN   MIN   MIN    MIN    MIN   MPH')
30232 format(a1,t27,80a1,t126,'PAGE',i4,//,
     *<l10>x,'TRANSIT ROUTE: ',i<l1>,'  CONFIGURATION:',i2,
     *'  FROM: ',<l2>a1,'  TO: ',<l3>a1,/,
     *<l11>x,'TRANSCAD LINE NUMBER: ',i<l5>,'  MODE: ',a<l6>,
     *'  COMPANY: ',a<l7>,'  PEAK HEADWAY: ',f<l8>.1,
     *'  OFFPEAK HEADWAY: ',f<l9>.1,//,
     *'  TCAD  TCAD RAIL',t35,' STATION',t56,'STOP P TRANSFERRING ',
     *'      PEAK MAX   RUN ACCEL DECEL DWELL SIGNAL SIGNAL TOTAL',/,
     *'  NODE   TAP NAME',t35,'NAME',t56,'TYPE R ROUTES       ',
     *'MILES  MPH MPH   MIN   MIN   MIN   MIN    MIN    MIN   MPH')
30233 format(a1,t27,80a1,t126,'PAGE',i4,//,
     *<l10>x,'TRANSIT ROUTE: ',i<l1>,'  CONFIGURATION:',i2,
     *' ',<l2>a1,' LOOP  VIA: ',<l4>a1,/,
     *<l11>x,'TRANSCAD LINE NUMBER: ',i<l5>,'  MODE: ',a<l6>,
     *'  COMPANY: ',a<l7>,'  PEAK HEADWAY: ',f<l8>.1,
     *'  OFFPEAK HEADWAY: ',f<l9>.1,//,
     *'  TCAD  TCAD RAIL',t35,' STATION',t56,'STOP P TRANSFERRING ',
     *'      PEAK MAX   RUN ACCEL DECEL DWELL SIGNAL SIGNAL TOTAL',/,
     *'  NODE   TAP NAME',t35,'NAME',t56,'TYPE R ROUTES       ',
     *'MILES  MPH MPH   MIN   MIN   MIN   MIN    MIN    MIN   MPH')
30234 format(a1,t27,2a8,9x,80a1,t126,'PAGE',i4,//,
     *<l10>x,'TRANSIT ROUTE: ',i<l1>,'  CONFIGURATION:',i2,
     *' ',<l2>a1,' LOOP',/,
     *<l11>x,'TRANSCAD LINE NUMBER: ',i<l5>,'  MODE: ',a<l6>,
     *'  COMPANY: ',a<l7>,'  PEAK HEADWAY: ',f<l8>.1,
     *'  OFFPEAK HEADWAY: ',f<l9>.1,//,
     *'  TCAD  TCAD RAIL',t35,' STATION',t56,'STOP P TRANSFERRING ',
     *'      PEAK MAX   RUN ACCEL DECEL DWELL SIGNAL SIGNAL TOTAL',/,
     *'  NODE   TAP NAME',t35,'NAME',t56,'TYPE R ROUTES       ',
     *'MILES  MPH MPH   MIN   MIN   MIN   MIN    MIN    MIN   MPH')
30239 format(a1,t27,80a1,t126,'PAGE',i4,//,
     *'  TCAD  TCAD RAIL',t35,' STATION',t56,'STOP P TRANSFERRING ',
     *'      PEAK MAX   RUN ACCEL DECEL DWELL SIGNAL SIGNAL TOTAL',/,
     *'  NODE   TAP NAME',t35,'NAME',t56,'TYPE R ROUTES       ',
     *'MILES  MPH MPH   MIN   MIN   MIN   MIN    MIN    MIN   MPH')
30028 format(2i6,1x,a20,1x,a20,1x,a4,1x,a1,1x,12a1,
     *f6.1,i5,i4,4f6.1,2f7.1,f6.1)
30029 format(/,t14,'ROUTE TOTAL',t75,f6.1,t90,4f6.1,2f7.1,f6.1)
30080 format(a1,t25,80a1,18x,'PAGE',i4,//,34x,
     *'SUMMARY OF TRANSIT SYSTEM STATISTICS BY ROUTE CONFIGURATION',//,
     *'    TRANSCAD                   -----HEADWAYS-----',
     *'           VEHICLE MILES   MINUTES   VEHICLE MINUTES',
     *'   VEHICLES    OPERATING DOLLARS',/,

     *'      LINE   ROUTE DIR CNFG     AM   PM   OP   NT',
     *'   MILES    AM  OP DAILY   PK   OP     AM   OP DAILY',
     *'   PK    OP      AM    OP  DAILY',/)
30081 format(7x,i3,i8,2x,a2,3x,i2,2x,4f5.1,f8.1,2x,2i4,i6,2i5,2x,2i5,i6,
     *i5,i6,2x,2i6,i7)
30082 format(a1,t42,80a1,18x,'PAGE',i4,//,44x,
     *'SUMMARY OF TRANSIT SYSTEM STATISTICS BY ROUTE',//,
     *'                              ----- VEHICLE MILES -----',

     *' ---- VEHICLE MINUTES ---- - VEHICLES --',

     *' -------- OPERATING DOLLARS --------',/,
     *' ROUTE COMPANY MODE             AM   PM   OP NITE DAILY',
     *'   AM   PM   OP NITE DAILY AM PM OP NITE',
     *'     AM     PM     OP   NITE   DAILY',/)
30083 format(i6,3x,a5,1x,a13,1x,4i5,i6,4i5,i6,3i3,i5,4i7,i8)
30084 format(a1,t43,80a1,18x,'PAGE',i4,//,39x,
     *'SUMMARY OF TRANSIT SYSTEM STATISTICS BY MODE',//,
     *'               ROUTE   ------- VEHICLE MILES -------',
     *' ----- VEHICLE HOURS ----- --- VEHICLES ---',
     *' ---------- OPERATING DOLLARS -----------',/,
     *' MODE          MILES    AM    PM    OP  NITE  DAILY',
     *'   AM   PM   OP NITE DAILY  AM  PM  OP NITE',
     *'      AM      PM      OP    NITE    DAILY',/)
30085 format(1x,a13,i6,4i6,i7,4i5,i6,3i4,i5,4i8,i9)
30086 format(/,' SYSTEM TOTAL ',
     *4i6,i7,4i5,i6,i6,3i4,i5,4i8,i9)
30830 format(i2,4f10.0)
30900 format(/,t109,a7,'/',a8,'/tctr.sel')
30901 format(/,t110,a7,'/',a8,'/tctr.pr')
30902 format(/,t104,a7,'/',a8,'/tctr.sum')
30903 format(/,t107,a7,'/',a8,'/tctr.sum')
30904 format(/,t103,a7,'/',a8,'/tctr.sum')
39999 format('[',a7,'] [',a8,'] [2030fin/tctr4] [',a<ic1>,
     *'] [vehicle miles=',i<ic2>,
     *'] [average mph=',f<ic3>.1,']')
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
      ic2=1
      if(idavm.gt.9) ic2=2
      if(idavm.gt.99) ic2=3
      if(idavm.gt.999) ic2=4
      if(idavm.gt.9999) ic2=5
      if(idavm.gt.99999) ic2=6
      x=float(idavm)/float(idavh)
      ic3=3
      if(x.gt.9.9) ic3=4
      if(idavm.gt.99.9) ic3=5
      call dattim
      write(lunerr,39999)  cdate,ctime,tpstat,idavm,x
      close(lunerr)
      close(lunsum)
      stop
      end
      subroutine printx(inode,irtno,jnode,jstop)
      parameter (mxxfr=24)
      include 'sandag.inc'
      common /psort/ xrt,prt,xline
      integer*2 srt(mxxfr),xline,xrt(mxxfr,mxtrcn),irtno
      character*1 char(10),prt(7,12)
      data char/'0','1','2','3','4','5','6','7','8','9'/
      do 104 ichar=1,12
      do 103 xline=1,7
      prt(xline,ichar)=' '
  103 continue
  104 continue
      xline=1
      ichar=1
      if(jnode.lt.1.or.xrt(1,inode).lt.1.or.jstop.lt.4) go to 199
      do 100 irt=1,mxxfr
      srt(irt)=0
  100 continue
      krt=0
      do 101 irt=1,mxxfr
      jrt=xrt(irt,inode)
      if(jrt.eq.irtno.or.jrt.lt.1) go to 101
      krt=krt+1
      srt(krt)=jrt
  101 continue
      do 119 i=1,mxxfr
      if(srt(i).eq.0) go to 119
      if(srt(i).lt.10) go to 113
      if(srt(i).lt.100) go to 111
      i1=srt(i)/100
      i2=(srt(i)-i1*100)/10
      i3=srt(i)-i1*100-i2*10
      i1=i1+1
      i2=i2+1
      i3=i3+1
      if(ichar.le.10) go to 110
      ichar=1
      xline=xline+1
      if(xline.gt.7) go to 199
  110 prt(xline,ichar)=char(i1)
      ichar=ichar+1
      prt(xline,ichar)=char(i2)
      ichar=ichar+1
      prt(xline,ichar)=char(i3)
      ichar=ichar+2
      go to 119
  111 i2=(srt(i))/10
      i3=srt(i)-i2*10
      i2=i2+1
      i3=i3+1
      if(ichar.le.11) go to 112
      ichar=1
      xline=xline+1
      if(xline.gt.7) go to 199
  112 prt(xline,ichar)=char(i2)
      ichar=ichar+1
      prt(xline,ichar)=char(i3)
      ichar=ichar+2
      go to 119
  113 i3=srt(i)
      i3=i3+1
      if(ichar.le.12) go to 114
      ichar=1
      xline=xline+1
      if(xline.gt.7) go to 199
  114 prt(xline,ichar)=char(i3)
      ichar=ichar+2
  119 continue
  199 return
      end