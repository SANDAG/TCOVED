c
c     third in a series of programs/amls to create tranplan
c     transit inputs from tcov, this program:
c     1) creates transfer links between nearby routes
c
      parameter(mxdist=1320,mxbar=20000,mxcnum=5000,mxnnum=5000,
     *mxrt=999,mxrec=60000,mxout=10000)
      include 'sandag.inc'
      include 'tcov.inc'
c
c     walkbar arc variables
c
      integer*4 i4,npnts
      real x(500),y(500)
c
c     program variables
c
      logical ccnode(mxtrcn),used(mxtrcn),ok,precon
      integer*2 stype(2,mxtrcn),mbar,cnode(mxcnum),nnode(mxnnum,2),
     *modelb(mxrt),rtnum(mxrt),rtdir(mxrt),lowrt(mxrt),highrt(mxrt)
      integer*4 rtlbid(mxrt),nodarr(mxrec),lrlink,
     *onode(mxout,2),ortdir(mxout,2),odist(mxout)
      real*4 xnode(mxtrcn),ynode(mxtrcn),znode(mxtrcn),ndist(mxnnum),
     *x1bar(mxbar),y1bar(mxbar),x2bar(mxbar),y2bar(mxbar)
      character*80 ratnam,secnam
      data lunerr,luno,lunin,lungen,natlun,ratlun,seclun
     */11,12,13,14,15,16,17/
      mout=0
      wkdist=float(mxdist)
      open (unit=lunerr,file='tctr3.err')
      open (unit=luno,file='xferlinks')
      open (unit=lungen,file='xfer.gen')

c     save node coordinates and stop type

      call otrcn
      if(natio.ne.0) go to 9000
      natrec=1
  110 call rtrcn
      if(natio.ne.0) go to 119
      if(natlb.gt.mxtrcn) go to 9110
      if(natsph.eq.1404) ccnode(natlb)=.true.
      if(natstp(1).eq.mxmoda) natstp(1)=mxstop
      stype(1,natlb)=natstp(1)
      if(natstp(2).eq.mxmoda) natstp(2)=mxstop
      stype(2,natlb)=natstp(2)
      xnode(natlb)=natx
      ynode(natlb)=naty 
      znode(natlb)=natz
  118 natrec=natrec+1
      go to 110
  119 close(natlun)

c
c     open walk barriers file
c
      fnamo='walkbar/arc.adf'
      open(unit=lunin,convert='big_endian',file=fnamo,access=
     *'direct',form='unformatted',recl=1,status='old',err=9000)
c
c     skip over header stuff
c
       do recnum=1,25
        read(lunin,rec=recnum,err=9000) i4
       end do
      recnum=25
c
c     skip over header stuff for each arc
c
  125  do irec=1,7
        recnum=recnum+1
        read(lunin,rec=recnum,err=129) i4
       end do

      recnum=recnum+1
      read(lunin,rec=recnum,err=9000) npnts
c
c     save vertices for each arc
c
       do irec=1,npnts
        recnum=recnum+1
        read(lunin,rec=recnum,err=9000) x(irec)
        recnum=recnum+1
        read(lunin,rec=recnum,err=9000) y(irec)
       end do

       do i=2,npnts
        j=i-1
        mbar=mbar+1
        if(mbar.gt.mxbar) go to 9120
        x1bar(mbar)=x(i)
        y1bar(mbar)=y(i)
        x2bar(mbar)=x(j)
        y2bar(mbar)=y(j)
       end do
      go to 125
  129 close(lunin)
 
c     open route data file.
 
  199 call otrcr
      if(ratio.ne.0) go to 9000
      ratrec=1
  200 call rtrcr
      if(ratio.ne.0) go to 209
      if(ratlb.lt.1.or.ratlb.gt.mxrt) go to 9200
      if(ratid.lt.1.or.ratid.gt.999999) go to 9200
      rtlbid(ratlb)=ratid
      modelb(ratlb)=ratmod
      ratrec=ratrec+1
      go to 200
  209 close(ratlun)
c
c     open section data file.
c
      irec=0
      lrink=0
      call otrcs
      if(secio.ne.0) go to 9000
      secrec=1
  220 call rtrcs
      if(secio.ne.0) go to 229
      if(rlink.lt.1.or.rlink.gt.mxrt) go to 9210
      id=rtlbid(rlink)
      irt=id/1000
      idir=(id-irt*1000)/100
      rtnum(rlink)=irt
      rtdir(rlink)=irt*10+idir
      ratmod=modelb(rlink)
      if(ratmod.gt.mxstop) ratmod=mxstop

       if(rlink.ne.lrlink) then
        do i=1,mxtrcn
         used(i)=.false.
        end do
        lrlink=rlink
       end if
 
       do i=1,2
        inode=secarn(i)
         if(inode.lt.1.or.inode.gt.mxtrcn) go to 9210
         if(.not.ccnode(inode).and..not.used(inode)) then
          used(inode)=.true.
          istop=stype(1,inode)
          ustop=stype(2,inode)
          ok=.false.
          if(istop.gt.0.and.istop.le.mxmoda.and.ratmod.ge.istop) 
     *    ok=.true.
          if(ustop.gt.0.and.ustop.le.mxmoda.and.ratmod.ge.ustop) 
     *    ok=.true.
           if(ok) then
            irec=irec+1 
            if(irec.gt.mxrec) go to 9222
            nodarr(irec)=inode
            if(lowrt(rlink).lt.1) lowrt(rlink)=irec
            highrt(rlink)=irec
           endif
         endif
       end do
      secrec=secrec+1
      go to 220
  229 close(seclun)
c
c     loop through nodes by route looking for transfer links
c
      do irt=1,998
       irtnum=rtnum(irt)
       irtdir=rtdir(irt)
       ilow=lowrt(irt)
       ihigh=highrt(irt)
       if(ilow.gt.0.and.ihigh.gt.ilow) then
        krt=irt+1
         do jrt=krt,999
          jrtnum=rtnum(jrt)
          jrtdir=rtdir(jrt)
           if(irtnum.ne.jrtnum) then
            jlow=lowrt(jrt)
            jhigh=highrt(jrt)
             if(jlow.gt.0.and.jhigh.gt.jlow) then
             mcnum=0
             mnnum=0
  
c             save common nodes and near nodes
  
              do irec=ilow,ihigh 
               inode=nodarr(irec)
               xi=xnode(inode)
               yi=ynode(inode)
               zi=znode(inode)
                do jrec=jlow,jhigh
                 jnode=nodarr(jrec)
                  if(inode.eq.jnode) then
                   mcnum=mcnum+1
                   if(mcnum.gt.mxcnum) go to 9991
                   cnode(mcnum)=inode
                  else
                   xj=xnode(jnode)
                   yj=ynode(jnode)
                   zj=znode(jnode)
                   xd=abs(xi-xj)
                    if(xd.lt.wkdist) then
                     yd=abs(yi-yj)
                      if(yd.lt.wkdist) then
                       dist=sqrt(xd**2+yd**2)
                       zdiff=zi-zj
                       zdiff=abs(zdiff)
                       zdist=zdiff*3.0
                       dist=dist*1.1+zdist
                       if(dist.lt.wkdist) then ! potential transfer connection
                        mnnum=mnnum+1
                        if(mnnum.gt.mxnnum) go to 9991
                         nnode(mnnum,1)=inode
                         nnode(mnnum,2)=jnode
                         ndist(mnnum)=dist
                       endif !end of near check
                      endif !end of y near check
                    endif !end of x near check
                  end if !end of common node check
                end do !end of second route nodes
              end do !end of first node check
              if(mnnum.gt.0) then

c              go through near nodes and delete those near comon nodes
               if(mcnum.gt.0) then ! check if there are nearby nodes in common
              
                do innum=1,mnnum 
                 inode=nnode(innum,1)
                 xi=xnode(inode)
                 yi=ynode(inode)
                  do icnum=1,mcnum
                   knode=cnode(icnum)
                   xk=xnode(knode)
                   yk=ynode(knode)
                   xd=abs(xi-xk)
                   if(xd.lt.wkdist) then
                    ndist(innum)=9999.
                   else
                    yd=abs(yi-yk)
                    if(yd.lt.wkdist) ndist(innum)=9999.
                   endif
                  end do
                end do
               endif !end of common node check
                     
c             go through remaining near nodes and save up to 10 transfer connections
              
              ok=.false.
              sdist=0.0
              do iter=1,10
               if(ok) then !write out connections from previous iteration
                mout=mout+1
                if(mout.gt.mxout) go to 9390
                onode(mout,1)=isnode
                onode(mout,2)=jsnode
                odist(mout)=nint(sdist)
                ortdir(mout,1)=irtdir
                ortdir(mout,2)=jrtdir
                isnode=0
                jsnode=0
               endif
              if(sdist.lt.wkdist) then
               sdist=wkdist
               ok=.false.
                do innum=1,mnnum !find closest unused node
                 if(ndist(innum).lt.sdist) then
                  sdist=ndist(innum)
                  inum=innum
                 endif
                end do
                
                if(sdist.lt.wkdist) then
                 inode=nnode(inum,1)
                 xi=xnode(inode)
                 yi=ynode(inode)
                 jnode=nnode(inum,2)
                 xj=xnode(jnode)
                 yj=ynode(jnode)
                 ok=.true.
                     
c                check if transfer already exists
                  if(mout.gt.0) then

                   do iout=1,mout
                    isnode=onode(iout,1)
                    jsnode=onode(iout,2)
                    if(isnode.eq.inode.and.jsnode.eq.jnode)
     *               ok=.false.
                    if(isnode.eq.jnode.and.jsnode.eq.inode) 
     *               ok=.false.
                   end do

                   if(ok) then !see if nearby tranfer connection exists
                    do iout=1,mout
                     if(ok) then
                     isnode=onode(iout,1)
                     jsnode=onode(iout,2)
                      precon=.false.

                      do irec=ilow,ihigh 
                       if(isnode.eq.nodarr(irec)) then
                        do jrec=jlow,jhigh
                         if(jsnode.eq.nodarr(jrec)) precon=.true.
                        end do
                       end if
                      end do

                      if(precon) then
                       knode=onode(iout,1)
                       xk=xnode(knode)
                       yk=ynode(knode)
                       xd=abs(xi-xk)
                       if(xd.lt.wkdist.and.yd.lt.wkdist) then
                        ok=.false.
                       endif
                      endif
                     endif
                    end do
                   endif

                  endif !end check on presence of transfer links

                 if(ok) then ! check if connection is blocked by barrier
                  do ibar=1,mbar
                    if(ok) then
                     x1b=x1bar(ibar)
                     x2b=x2bar(ibar)
                     y1b=y1bar(ibar)
                     y2b=y2bar(ibar)
                     acx=xi-x1b
                     acy=yi-y1b
                     adx=xi-x2b
                     ady=yi-y2b
                     bcx=xj-x1b
                     bcy=yj-y1b
                     bdx=xj-x2b
                     bdy=yj-y2b
                     a1=(acx*ady)-(acy*adx)
                     a2=(bcx*bdy)-(bcy*bdx)
                     a=a1*a2
                      if(a.gt.0.0) then
                       ok=.true.
                      else
                       a=((acx*bcy)-(acy*bcx))*
     *                   ((adx*bdy)-(ady*bdx))
                        if(a.gt.0.0) then
                         ok=.true.
                        else
                         if(abs(a1).gt.0.0.and.abs(a2).gt.0.0)then
                          ok=.true.
                         else
                          ok=.false.
                         endif
                        endif
                      endif
                    endif
                  end do !end of walk barrier loop
                 endif

                 if(ok) then  
                  isnode=inode
                  jsnode=jnode
                 end if !end of check on whether connection is within walking distance
                 ndist(inum)=9999.
                end if !end of check on whether potential connection was found for this iteration
               end if !end of check on whether connection was found from previous iteration
              end do ! end of iterations do find all eligible newarby nodes
              end if !end of check on whether potential transfer exists
             end if !end of check on whether eligible intersecting route
           end if !end of check on different route numbers
         end do !end of loop on intersecting route
       end if !end of check on whether eligible first route
      end do !end of loop on first route
 
      do i=1,mout
       inode=onode(i,1)
       jnode=onode(i,2)
       write(luno,20410) inode,jnode,odist(i),ortdir(i,1),ortdir(i,2)
       ix=nint(xnode(inode))
       iy=nint(ynode(inode))
       jx=nint(xnode(jnode))
       jy=nint(ynode(jnode))
       id=mxid+mxtap+i
       write(lungen,20411) id,ix,iy,jx,jy
      end do

      write(lungen,20419)
      close(lunin)
      close(luno)
      close(lungen)
      go to 9999
 9000 write(lunerr,19000) fnamo
      errf=.true.
      go to 9999
 9001 write(lunerr,19001) fnamo,adata
      errf=.true.
      go to 9999
 9110 write(lunerr,19110) recnum
      errf=.true.
      go to 9999
 9120 write(lunerr,19120) 
      errf=.true.
      go to 9999
 9191 write(lunerr,19191) 
      errf=.true.
      go to 9999
 9200 write(lunerr,19200) ratlb,ratid
      errf=.true.
      go to 9999
 9210 write(lunerr,19210) rlink,secarn(2),secarn(1)
      errf=.true.
      go to 9999
 9222 write(lunerr,19222) 
      errf=.true.
      go to 9999
 9223 write(lunerr,19223) 
      errf=.true.
      go to 9999
 9390 write(lunerr,19390) 
      errf=.true.
      go to 9999
 9991 write(lunerr,19991) 
      errf=.true.
      go to 9999
10000 format(80i1,t1,80a1)
10200 format(1x,a5,i5,t9,3a1,1x,a2,1x,a1,i2,1x,a1,4i5,i2,t9,i1)
19000 format(' problem opening file: ',a80)
19001 format(' data problem with file: ',a80/,80a1)
19110 format(' data problem with trcov/nat ', i10)
19120 format(' increase size of mxbar')
19191 format(' problem sorting barrier array')
19200 format(' data problem with trcov/rat ',2i10)
19210 format(' data problem with trcov/sec ',3i10)
19222 format(' increase size of mxrec')
19223 format(' increase size of mxrt')
19390 format(' increase size of mxout')
19991 format(' increase size of mxcnum')
20410 format(5i10)
20411 format('      ',i5,/,i7,',',i7,/,i7,',',i7,/,'end')
20419 format('end')
20204 format(' irt=',i4,' rtno=',i4,' dir=',a2,' low=',i6)
20299 format(' irt=',i4,' ilow=',i6,' ihigh=',i6)
20298 format(' jrt=',i4,' jlow=',i6,' jhigh=',i6)
20211 format(' processing irt=',i4,' jrt=',i4)
39999 format('[',a7,'] [',a8,'] [2030fin/tctr3] [',a<ic1>,
     *'] [transfer links=',i<ic2>,']')
 9999 if(errf)  go to 99991
      tpstat='ok'
      ic1=2
      close(lunerr,status='delete')
      go to 99992
99991 close(lunerr,status='keep')
      tpstat='not'
      ic1=3
99992 open (unit=lunerr,file='tplog',access='append')
      ic2=1
      if(mout.gt.9) ic2=2
      if(mout.gt.99) ic2=3
      if(mout.gt.999) ic2=4
      if(mout.gt.9999) ic2=5
      if(mout.gt.99999) ic2=6
      call dattim
      write(lunerr,39999)  cdate,ctime,tpstat,mout
      close(lunerr)
      close(lunin)
      stop
      end
