c
c     tcov aat attributes
c
      integer*2 mxtcyr
      parameter (mxtcyr=5)
      integer*2 aatrt,aatlk,aatrts,aatadt,aatvol,
     *aatcj,aatsta,aatloc,iaat2,aatlp,aatsph,aatplt,aattoll(3,mxtcyr),
     *aatppc,aattpc,aatsec,aatdir,aatffc,aatcl,aattrk(mxtcyr),
     *aatosp,aatasp,aatlos,aatls(2),aatyr(mxtcyr),aatprj(mxtcyr),
     *aatdyr,aatdprj,aatjur(mxtcyr),aatfc(mxtcyr),aathov(mxtcyr),
     *aatspd(mxtcyr),aatway(mxtcyr),aatmed(mxtcyr),aatln(3,2,mxtcyr),
     *aataux(2,mxtcyr),aatpct(2),aatphf(2),aatcnt(2,mxtcyr),
     *aattl(2,mxtcyr),aatrl(2,mxtcyr),aatll(2,mxtcyr),
     *aatplc(2),aatat(2),aatgc(2),aattspd(mxtcyr),
     *aattv(2),aatrv(2),aatlv(2),aatstp(2),aatvc(2),aatag(2)
      integer*4 aatarn(2),lpoly,rpoly,aatlb,aatid,aatqid,aatcc,aatx(2),
     *aaty(2),aattmp(2),aatrec,aatlun,aatio,aattpn(2),aatudj,aatadj,
     *aatcp(3,2),aatvla(2),aatvlp(2),
     *aattlb(2),aatrlb(2),aatllb(2)
      real*4 aatlen,aatcst,aatlt(3,2),aatlc(3,2),aatxt(3,2),aatxc(3,2),
     *aatlt6(2,2),aatlt7(2,2),aattc(2),aattm(3,2),aathc(3,2),aatfwylen
      character*9 aatwho
      character*20 aatnm,aatxnm(2)
      common /tcai2/ aatrt,aatlk,aatrts,aatadt,aatvol,
     *aatcj,aatsta,aatloc,iaat2,aatlp,aatsph,aatplt,aattoll,
     *aatppc,aattpc,aatsec,aatdir,aatffc,aatcl,
     *aatosp,aatasp,aatlos,aatls,
     *aatyr,aatprj,aatdyr,aatdprj,aatjur,aatfc,aathov,
     *aatspd,aatway,aatmed,aatln,aatfwylen,aattrk,
     *aataux,aatpct,aatphf,aatcnt,aattspd,
     *aattl,aatrl,aatll,aatplc,aatat,aatgc,
     *aattv,aatrv,aatlv,aatstp,aatvc,aatag
      common /tcai4/ aatarn,lpoly,rpoly,aatlb,aatid,aatqid,
     *aatcc,aatx,aaty,aattmp,aatrec,aatlun,aatio,aattpn,aattlb,
     *aatrlb,aatllb,aatudj,aatadj,aatcp,aatvla,aatvlp
      common /tcar4/ aatlen,aatcst,aatlt,aatlc,aatxt,aatxc,
     *aatlt6,aatlt7,aattc,aattm,aathc
      common /tcaa9/ aatwho
      common /tcaa20/ aatnm,aatxnm
c
c     tcov nat attributes
c
      integer*2 natnty,natz,natiuc,natsph,
     *natcnt(2),natcyc,nattap,nattrn,
     *natfz4,natfz6,natfz7,natyr(2),natprj(2),
     *natjur(2),nattyr(2),natstp(2),natprk(2),nattt(2),natdwl(2)
      integer*4 natlb,natid,natrec,natlun,natio,nattmp,natx,naty,
     *nattpn
      character*20 natxnm(2)
      common /tcni2/ natnty,natz,natiuc,natsph,
     *natcnt,natcyc,nattap,nattrn,natfz4,natfz6,natfz7,
     *natyr,natprj,natjur,nattyr,natstp,natprk,nattt,natdwl
      common /tcni4/ natlb,natid,nattmp,natx,naty,natrec,natlun,
     *natio,nattpn
      common /tcna20/ natxnm
c
c     tcov tat attributes
c
      integer*4 tatlb,tatlb1,tatlb2,tatid1,tatid2,tatrec,tatio,tatlun
      real*4 tatazi,tatang
      common /tctai4/ tatlb,tatlb1,tatlb2,tatid1,tatid2,tatrec,tatio,
     *tatlun 
      common /tctar4/ tatazi,tatang
c
c     tcov transit section attributes
c
      integer*2 secic(20),secoc(20),secsic(20),secsoc(20),sectm2,seci2
      integer*4 rlink,alink,seclb,secid,sectm4,
     *secrec,seclun,secio
      real*4 secmes(2),secpos(2)
      common /tcsi2/ secic,secoc,secsic,secsoc,sectm2,seci2
      common /tcsi4/  rlink,alink,seclb,secid,sectm4,
     *secrec,seclun,secio
      common /tcsr4/ secmes,secpos
c
c     tcov transit route attributes
c
      integer*4 ratlb,ratid,ratrec,ratlun,ratio
      common /tcri4/ ratlb,ratid,ratrec,ratlun,ratio
c
c     trcov arc attributes
c
      integer*2 aattrt(20),aatmm,aatlrz
      integer*4 aattvl(2),aatcrz
      common /trca2/ aattrt,aatmm,aatlrz
      common /trca4/ aattvl,aatcrz
c
c     trcov node attributes
c
      integer*4 natvol(4)
      common /trca4/ natvol
c
c     trcov section attributes
c
      integer*2 secarn(2),sectpn(7),secstp(2)
      character*20 secnm,secxnm(2)
      common /trcs2/ secarn,sectpn,secstp
      common /trcsa/ secnm,secxnm
c
c     trcov route attributes
c
      integer*2 ratco,ratmod,ratlin,rathwy(4),ratnhr
      character*20 ratvia
      character*24 ratorg,ratdes
      common /trcr2/ ratco,ratmod,ratlin,rathwy,ratnhr
      common /trcsa1/ ratvia
      common /trcsa2/ ratorg,ratdes
