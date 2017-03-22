      subroutine dattim
      include 'sandag.inc'
      include 'tcov.inc'
c     integer iitime(3)
c     integer mon,day,year
      integer date_time(8)
      character (len = 12) real_clock (3)
      character*1 dnam(10)
      character*2 inam(100)
      character*3 monam(12)
      data monam/
     *'jan',
     *'feb',
     *'mar',
     *'apr',
     *'may',
     *'jun',
     *'jul',
     *'aug',
     *'sep',
     *'oct',
     *'nov',
     *'dec'/
      data dnam/
     *'0','1','2','3','4','5','6','7','8','9'/
      data inam/
     *'00','01','02','03','04','05','06','07','08','09',
     *'10','11','12','13','14','15','16','17','18','19',
     *'20','21','22','23','24','25','26','27','28','29',
     *'30','31','32','33','34','35','36','37','38','39',
     *'40','41','42','43','44','45','46','47','48','49',
     *'50','51','52','53','54','55','56','57','58','59',
     *'60','61','62','63','64','65','66','67','68','69',
     *'70','71','72','73','74','75','76','77','78','79',
     *'80','81','82','83','84','85','86','87','88','89',
     *'90','91','92','93','94','95','96','97','98','99'/
c      call idate(mon,day,year) year in 2 digits
c      call itime(iitime)
       call date_and_time (real_clock (1), real_clock (2), 
     * real_clock (3), date_time)
c      write(ctime,20001) inam(iitime(1)+1),inam(iitime(2)+1),
c    * inam(iitime(3)+1)
       write(ctime,20001) inam(date_time(5)+1),inam(date_time(6)+1),
     *inam(date_time(7)+1)
      iyr=date_time(1)-2000
      iyr1=iyr/10
      iyr2=iyr-iyr1*10+1
      iyr1=iyr1+1
      write(cdate,20002) date_time(3),monam(date_time(2)),
     *dnam(iyr1),dnam(iyr2)
20001 format(a2,':',a2,':',a2)
20002 format(i2,a3,2a1)
      return
      end
c-----------------------------------------------------
c-----------------------------------------------------
c subroutine to pass output file name and concatenate
c alternative path name with the file name 
c-----------------------------------------------------
      subroutine openunfo(lun)
      use dflib
      include 'sandag.inc'
      integer namlen, namfl
      call getarg(1,pname)
c     pname='o:\sr9\2001.1\2020tr'
      namlen=len_trim(pname)
      if(namlen.lt.1) then
      fnamo=fnami
      else
      namfl=len_trim(fnami)
c     print *, namlen,namfl
      write(fnamo,20001) pname,fnami
      endif
      write(0,20009) fnamo
20001 format(a<namlen>,'\',a<namfl>)
20009 format(a80)	
      open(share='denynone',unit=lun,file=fnamo,form='unformatted')
      end
c-----------------------------------------------------
c-----------------------------------------------------
c subroutine to pass output file name and concatenate
c alternative path name with the file name 
c-----------------------------------------------------
      subroutine openoutput(lun,facsmode)
      use dflib
      include 'sandag.inc'
      integer namlen, namfl
      call getarg(1,pname)
c     pname='o:\sr9\2001.1\2020tr'
      namlen=len_trim(pname)
      if(namlen.lt.1) then
      fnamo=fnami
      else
      namfl=len_trim(fnami)
c     print *, namlen,namfl
      write(fnamo,20001) pname,fnami
      endif
      write(0,20009) fnamo
20001 format(a<namlen>,'\',a<namfl>)
20009 format(a80)	
      open(share='denynone',unit=lun,file=fnamo,access=facsmode)
      end
c-----------------------------------------------------
c-----------------------------------------------------
      subroutine outputfnmo()
      use dflib
      include 'sandag.inc'
      integer namlen, namfl
      call getarg(1,pname)
c     pname='o:\sr9\2001.1\2020tr'
      namlen=len_trim(pname)
      if(namlen.lt.1) then
      fnamo=fnami
      else
      namfl=len_trim(fnami)
c     print *, namlen,namfl
      write(fnamo,20001) pname,fnami
      endif
      write(0,20009) fnamo
20001 format(a<namlen>,'\',a<namfl>)
20009 format(a80)	
      end
c-----------------------------------------------------
c subroutine to write file to ../data directory
c-----------------------------------------------------
      subroutine outputfnmodata()
      use dflib
      include 'sandag.inc'
      integer namlen, namfl      
      call getarg(2,sname)
c     pname='o:\sr9\2001.1\2020tr'
      namlen=len_trim(sname)
      if(namlen.lt.1) then
   10 write(fnamo,20000) fnami
20000 format('..\data\',a72)
      else
      namfl=len_trim(fnami)
c     print *, namlen,namfl
      write(fnamo,20001) sname,fnami
      endif
      write(0,20009) fnamo
20001 format(a<namlen>,'\data\',a<namfl>)
20009 format(a80)	
      end
c-----------------------------------------------------
c-----------------------------------------------------
      subroutine fileexist()
      use dflib
      include 'sandag.inc'
      integer namlen, namfl
      logical fexist
      fnamo=fnami
      inquire (file=fnamo,exist=fexist)
      print *,'fexist=',fexist
      if(fexist) goto 6
      call getarg(1,pname)
      namlen=len_trim(pname)
      namfl=len_trim(fnami)
      print *, namlen,namfl
      write(fnamo,20001) pname,fnami
      write(0,20009) fnamo
      inquire (file=fnamo,exist=fexist)
      if(fexist) goto 6
20001 format(a<namlen>,'\',a<namfl>)
20009 format(a80)
        istat=9
      goto 9999
    6 istat=0
 9999 return	
      end
c-----------------------------------------------------
      subroutine readmtxfile()
      use dflib
      include 'sandag.inc'
      integer namlen, namfl
	logical fexist
      fnamo=fnami
	inquire (file=fnamo,exist=fexist)
      print *,'fexist=',fexist
	if(fexist) goto 6
      call getarg(1,pname)
      namlen=len_trim(pname)
	namfl=len_trim(fnami)
	print *, namlen,namfl
      write(fnamo,20001) pname,fnami
      write(0,20009) fnamo
	inquire (file=fnamo,exist=fexist)
	if(fexist) goto 6
    2 call getarg(2,sname)
      namlen=len_trim(sname)
      write(fnamo,20002) sname,fnami
      write(0,20009) fnamo
20002 format(a<namlen>,'\data\',a<namfl>)	
	inquire (file=fnamo,exist=fexist)
	if(fexist) goto 6
    3 call getenv('data',ename)
	ename='t:\data\data'
      namlen=len_trim(ename)
      write(fnamo,20001) ename,fnami
      write(0,20009) fnamo
	inquire (file=fnamo,exist=fexist)
	if (fexist) goto 6
20001 format(a<namlen>,'\',a<namfl>)
20009 format(a80)
      istat=9
	goto 9999
    6 istat=0
 9999 return	
      end
c------------------------------------------------------
c------------------------------------------------------
c-------------------------------------------------------
      subroutine opendata(lun)
      use dflib
      include 'sandag.inc'
      integer namlen, namfl
      open(share='denynone',unit=lun,file=fnami,status='old',
     *iostat=istat)
      if(istat.ne.0) go to 10 
      write(fnamo,20009) fnami
      write(0,20009) fnamo
      return 
   10 write(fnamo,20000) fnami
20000 format('..\data\',a72)
      open(share='denynone',shared,unit=lun,file=fnamo,status='old',
     *iostat=istat)
      if(istat.ne.0) go to 11
      write(0,20009) fnamo
      return 
   11 call getarg(1,pname)
c	pname='o:\sr9\2001.1\testnms'
      namlen=len_trim(pname)
	namfl=len_trim(fnami)
      write(fnamo,20001) pname,fnami
20001 format(a<namlen>,'\',a<namfl>)	
      open(share='denynone',shared,unit=lun,file=fnamo,status='old',
     *iostat=istat)
      if(istat.ne.0) go to 2
      write(0,20009) fnamo
	print *,11
      return 
    2 call getarg(2,sname)
c	sname='o:\2030\ver1\byear'
      namlen=len_trim(sname)
      write(fnamo,20002) sname,fnami
20002 format(a<namlen>,'\data\',a<namfl>)	
      open (share='denynone',shared,unit=lun,file=fnamo,status='old',
     *iostat=istat)
      write(0,20009) fnamo
      return
20009 format(a80)
      return
      end
***********************************************************************
      subroutine ocap(iy,cn,mn)
	use dflib
      include 'sandag.inc'
      logical fexist
	integer cyear(7)
      character*80 cn,mn
	data cyear/2000,2005,2010,2015,2020,2025,2030/
	mdiff=50
	imyear=2000
	do i=1,7
	iyear=cyear(i)
	idiff=abs(iyear-iy)
	if (idiff.lt.mdiff) then
	mdiff=idiff
	imyear=iyear
	endif
	end do
      cn='capacity'
      inquire(file=cn,exist=fexist)
      if (fexist) goto 20	
      call getarg(1,pname)
      namlen=len_trim(pname)
      if(namlen.gt.0) then
	namfl=len_trim(cn)
	print *, namlen,namfl
      write(fnamo,20001) pname,cn
	print *,fnamo
	cn=fnamo
	inquire (file=fnamo,exist=fexist)
      if(fexist) goto 20
      endif
      write(fnamo,20010) imyear
	cn=fnamo
	print *,fnamo
   20 mn='mgrabase'
      inquire(file=mn,exist=fexist)
      if(fexist) goto 9
      call getarg(1,pname)
      namlen=len_trim(pname)
      if(namlen.gt.0) then
	namfl=len_trim(mn)
	print *, namlen,namfl
      write(fnamo,20001) pname,mn
	print *,fnamo
      write(0,20009) fnamo
	mn=fnamo
	inquire (file=fnamo,exist=fexist)
	if (fexist) goto 9
      endif
	write(fnamo,20011) imyear
	mn=fnamo
	print *,fnamo
20001 format(a<namlen>,'\',a<namfl>)
20009 format(a80)
20010 format('t:\box2\tp\data\2030fin\ffep1203\capacity',i4)
20011 format('t:\box2\tp\data\2030fin\ffep1203\mgrabase',i4)
    9 return
      end
c-----------------------------------------------------------
      subroutine opop(iy,gqn,pfn)
	use dflib
      include 'sandag.inc'
      logical fexist
	integer cyear(7)
      character*80 gqn,pfn
	data cyear/2000,2005,2010,2015,2020,2025,2030/
	mdiff=50
	imyear=2000
	do i=1,7
	iyear=cyear(i)
	idiff=abs(iyear-iy)
	if (idiff.lt.mdiff) then
	mdiff=idiff
	imyear=iyear
	endif
	end do
      gqn='mgragq'
      inquire(file=gqn,exist=fexist)
      if (fexist) goto 20	
      call getarg(1,pname)
      namlen=len_trim(pname)
      if(namlen.gt.0) then
	namfl=len_trim(gqn)
	print *, namlen,namfl
      write(fnamo,20001) pname,gqn
	print *,fnamo
	gqn=fnamo
	inquire (file=fnamo,exist=fexist)
      if(fexist) goto 20
      endif
      write(fnamo,20010) imyear
	gqn=fnamo
	print *,fnamo
   20 pfn='pasef'
      inquire(file=pfn,exist=fexist)
      if(fexist) goto 9
      call getarg(1,pname)
      namlen=len_trim(pname)
      if(namlen.gt.0) then
	namfl=len_trim(pfn)
	print *, namlen,namfl
      write(fnamo,20001) pname,pfn
	print *,fnamo
      write(0,20009) fnamo
	pfn=fnamo
	inquire (file=fnamo,exist=fexist)
	if (fexist) goto 9
      endif
	write(fnamo,20011) imyear
	pfn=fnamo
	print *,fnamo
20001 format(a<namlen>,'\',a<namfl>)
20009 format(a80)
20010 format('t:\box2\tp\data\2030fin\ffep1203\mgragq',i4,'.csv')
20011 format('t:\box2\tp\data\2030fin\ffep1203\pasef',i4,'.csv')
    9 return
      end


c-----------------------------------------------------------
      subroutine osr11sgluhh(iy,lunm,hhnm,popnm)
	use dflib
      include 'sandag.inc'
      logical fexist
	integer cyear(6)
      character*80 lunm,hhnm,popnm
      data cyear/2003,2010,2015,2020,2025,2030/
	mdiff=50
	imyear=2000
	do i=1,6
	iyear=cyear(i)
	idiff=abs(iyear-iy)
	if (idiff.lt.mdiff) then
	mdiff=idiff
	imyear=iyear
	endif
	end do
      lunm='ludata'
      inquire(file=lunm,exist=fexist)
      if (fexist) goto 20	
      call getarg(1,pname)
      namlen=len_trim(pname)
      if(namlen.gt.0) then
	namfl=len_trim(lunm)
	print *, namlen,namfl
      write(fnamo,20001) pname,lunm
	print *,fnamo
	lunm=fnamo
	inquire (file=fnamo,exist=fexist)
      if(fexist) goto 20
      endif
      write(fnamo,20010) imyear
	lunm=fnamo
	print *,fnamo
   20 hhnm='hhdata'
      inquire(file=hhnm,exist=fexist)
      if(fexist) goto 21
      call getarg(1,pname)
      namlen=len_trim(pname)
      if(namlen.gt.0) then
	namfl=len_trim(hhnm)
	print *, namlen,namfl
      write(fnamo,20001) pname,hhnm
	print *,fnamo
      write(0,20009) fnamo
	hhnm=fnamo
	inquire (file=fnamo,exist=fexist)
	if (fexist) goto 21
      endif
	write(fnamo,20011) imyear
	hhnm=fnamo
	print *,fnamo
   21 popnm='pasef'
      inquire(file=popnm,exist=fexist)
      if(fexist) goto 9
      call getarg(1,pname)
      namlen=len_trim(pname)
      if(namlen.gt.0) then
	namfl=len_trim(popnm)
	print *, namlen,namfl
      write(fnamo,20001) pname,popnm
	print *,fnamo
      write(0,20009) fnamo
	popnm=fnamo
	inquire (file=fnamo,exist=fexist)
	if (fexist) goto 9
      endif
	write(fnamo,20012) imyear
	popnm=fnamo
	print *,fnamo
20001 format(a<namlen>,'\',a<namfl>)
20009 format(a80)
20010 format('t:\data\data\sr11\sg\ludata',i4,'.csv')
20011 format('t:\data\data\sr11\sg\hhdata',i4,'.csv')
20012 format('t:\data\data\sr11\sg\pasef',i4,'.csv')
    9 return
      end

c--------------------------------------------------------------------------------------------
c		Modified by Wu (March 28, 2008)
c		Purpose of change: to read land use data directory using getProperty('luDir')
c		NOte: A property item 'luDir' must exist in sandag.properties, which is located
c		in alternative folder or data folder
c--------------------------------------------------------------------------------------------
      subroutine osr11luhh(iy,lunm,hhnm,popnm)
      use dflib
      include 'sandag.inc'
      logical fexist
      integer cyear(6)
      character*80 lunm,hhnm,popnm,luDir
      character*4 yearstr
      call getProperty(9999,"luDir",luDir)

      if(index(luDir,'null')==1) then
      luDir='t:\data\data\sr11\ep\'
      endif

      print *,'luDir=',luDir
      data cyear/2003,2010,2015,2020,2025,2030/
      mdiff=50
      imyear=2000
      do i=1,6
      iyear=cyear(i)
      idiff=abs(iyear-iy)
      if (idiff.lt.mdiff) then
      mdiff=idiff
      imyear=iyear
      endif
      end do
      write(yearstr,20010) imyear
      lunm='ludata'
      inquire(file=lunm,exist=fexist)
      if (fexist) goto 20	
      call getarg(1,pname)
      namlen=len_trim(pname)
      if(namlen.gt.0) then
      namfl=len_trim(lunm)
      print *, namlen,namfl
      write(fnamo,20001) pname,lunm
      print *,fnamo
      lunm=fnamo
      inquire (file=fnamo,exist=fexist)
      if(fexist) goto 20
      endif
      fnamo=trim(luDir)//"ludata"//trim(yearstr)//".csv"
      lunm=fnamo
      print *,'ludata=',fnamo
   20 hhnm='hhdata'
      inquire(file=hhnm,exist=fexist)
      if(fexist) goto 21
      call getarg(1,pname)
      namlen=len_trim(pname)
      if(namlen.gt.0) then
      namfl=len_trim(hhnm)
      print *, namlen,namfl
      write(fnamo,20001) pname,hhnm
      print *,fnamo
      write(0,20009) fnamo
      hhnm=fnamo
      inquire (file=fnamo,exist=fexist)
      if (fexist) goto 21
      endif
      fnamo=trim(luDir)//"hhdata"//trim(yearstr)//".csv"
      hhnm=fnamo
      print *,'hhdata=',fnamo
   21 popnm='pasef'
      inquire(file=popnm,exist=fexist)
      if(fexist) goto 9
      call getarg(1,pname)
      namlen=len_trim(pname)
      if(namlen.gt.0) then
      namfl=len_trim(popnm)
      print *, namlen,namfl
      write(fnamo,20001) pname,popnm
      print *,fnamo
      write(0,20009) fnamo
      popnm=fnamo
      inquire (file=fnamo,exist=fexist)
      if (fexist) goto 9
      endif
      fnamo=trim(luDir)//"pasef"//trim(yearstr)//".csv"
      popnm=fnamo
      print *,'pasef=',fnamo
20001 format(a<namlen>,'\',a<namfl>)
20009 format(a80)
20010 format(i4)
    9 return
      end
c--------------------------------------------------------------------------------------------
c		Modification of osr11luhh
c   Changed for SR12 which has different years 
c--------------------------------------------------------------------------------------------
      subroutine osr12luhh(iy,lunm,hhnm,popnm)
      use dflib
      include 'sandag.inc'
      logical fexist
      integer cyear(10)
      character*80 lunm,hhnm,popnm,luDir
      character*4 yearstr
      call getProperty(9999,"luDir",luDir)

      if(index(luDir,'null')==1) then
      luDir='t:\data\data\sr12\ep\'
      endif

      print *,'luDir=',luDir
      data cyear/2008,2010,2015,2020,2025,2030,2035,2040,2045,2050/
      mdiff=50
      imyear=2000
      do i=1,10
      iyear=cyear(i)
      idiff=abs(iyear-iy)
      if (idiff.lt.mdiff) then
      mdiff=idiff
      imyear=iyear
      endif
      end do
      write(yearstr,20010) imyear
      lunm='ludata'
      inquire(file=lunm,exist=fexist)
      if (fexist) goto 20	
      call getarg(1,pname)
      namlen=len_trim(pname)
      if(namlen.gt.0) then
      namfl=len_trim(lunm)
      print *, namlen,namfl
      write(fnamo,20001) pname,lunm
      print *,fnamo
      lunm=fnamo
      inquire (file=fnamo,exist=fexist)
      if(fexist) goto 20
      endif
      fnamo=trim(luDir)//"ludata"//trim(yearstr)//".csv"
      lunm=fnamo
      print *,'ludata=',fnamo
   20 hhnm='hhdata'
      inquire(file=hhnm,exist=fexist)
      if(fexist) goto 21
      call getarg(1,pname)
      namlen=len_trim(pname)
      if(namlen.gt.0) then
      namfl=len_trim(hhnm)
      print *, namlen,namfl
      write(fnamo,20001) pname,hhnm
      print *,fnamo
      write(0,20009) fnamo
      hhnm=fnamo
      inquire (file=fnamo,exist=fexist)
      if (fexist) goto 21
      endif
      fnamo=trim(luDir)//"hhdata"//trim(yearstr)//".csv"
      hhnm=fnamo
      print *,'hhdata=',fnamo
   21 popnm='pasef'
      inquire(file=popnm,exist=fexist)
      if(fexist) goto 9
      call getarg(1,pname)
      namlen=len_trim(pname)
      if(namlen.gt.0) then
      namfl=len_trim(popnm)
      print *, namlen,namfl
      write(fnamo,20001) pname,popnm
      print *,fnamo
      write(0,20009) fnamo
      popnm=fnamo
      inquire (file=fnamo,exist=fexist)
      if (fexist) goto 9
      endif
      fnamo=trim(luDir)//"pasef"//trim(yearstr)//".csv"
      popnm=fnamo
      print *,'pasef=',fnamo
20001 format(a<namlen>,'\',a<namfl>)
20009 format(a80)
20010 format(i4)
    9 return
      end
c----------------------------------------------------------
c open base year files
c
      subroutine openbyrdata(lun)
      use dflib
      include 'sandag.inc'
      integer namlen, namfl
      write(fnamo,20001) fnami
20001 format('../byear/',a71)
      open(share='denynone',unit=lun,file=fnamo,status='old',
     *iostat=istat)
      if(istat.ne.0) go to 2
      return 
    2 call getarg(2,sname)
c	sname='o:\2030'
	namfl=len_trim(fnami)
      namlen=len_trim(sname)
      write(fnamo,20002) sname,fnami
      write(0,20009) fnamo
      print *,fnamo
20002 format(a<namlen>,'\byear\',a<namfl>)	
      open (unit=lun,file=fnamo,status='old',iostat=istat)
c      if(istat.ne.0) go to 3
      return
c    3 call getenv('data',ename)
c      namlen=len_trim(ename)
c      write(fnamo,20003) ename,fnami
c     write(0,20009) fnamo
c20003 format(a<namlen>,'\',a<namfl>)
c      open (unit=lun,file=fnamo,status='old',iostat=istat)
c      if(istat.ne.0) go to 4
c      return
20009 format(a80)
      return
      end
c--------------------------------------------------
c--------------------------------------------------
      subroutine openbin(lun)
      use dflib
      include 'sandag.inc'
      integer namlen, namfl
      open (share='denynone',shared,unit=lun,file=fnami,
     *form='binary',status='old',iostat=istat)
      print *,fnami
      if(istat.ne.0) go to 1
      return
    1	write(fnamo,20000) fnami
20000 format('..\data\',a72)
      open(share='denynone',shared,unit=lun,form='binary',file=fnamo,
     *status='old',iostat=istat)
      if(istat.ne.0) go to 11
      write(0,20009) fnamo      
      return
   11 call getarg(1,pname)
      namlen=len_trim(pname)
      if(namlen.eq.0) goto 2
	namfl=len_trim(fnami)
c	print *, namlen,namfl
      write(fnamo,20001) pname,fnami
20001 format(a<namlen>,'\',a<namfl>)
      open (share='denynone',shared,unit=lun,file=fnamo,
     *form='binary',status='old',iostat=istat)
      if(istat.ne.0) go to 2
      write(0,20009) fnamo
      return 
    2 call getarg(2,sname)
      namlen=len_trim(sname)
      write(fnamo,20002) sname,fnami
      write(0,20009) fnamo
20002 format(a<namlen>,'\data\',a<namfl>)	
      open (share='denynone',shared,unit=lun,file=fnamo,
     *form='binary',status='old',iostat=istat)
c      if(istat.ne.0) go to 3
      return
c    3 call getenv('data',ename)
c      namlen=len_trim(ename)
c      write(fnamo,20003) ename,fnami
c     write(0,20009) fnamo
c20003 format(a<namlen>,'\',a<namfl>)
c      open (share='denynone',unit=lun,file=fnamo,
c     *form='binary',status='old',iostat=istat)
c      return
20009 format(a80)    
      end
c--------------------------------------------------
c--------------------------------------------------
c create binary output file
c--------------------------------------------------
      subroutine openbino(lun)
      use dflib
      include 'sandag.inc'
      integer namlen, namfl
      logical fexist
      call getarg(1,pname)
      namlen=len_trim(pname)
      if(namlen.lt.1) then
        fnamo=fnami
      else
        namfl=len_trim(fnami)
        write(fnamo,20001) pname,fnami
      endif
      write(0,20009) fnamo
      inquire (file=fnamo,exist=fexist)
      if (fexist) then
        open (unit=lun,file=fnamo,
     *  form='binary',iostat=istat)
        close(lun,status='delete')
      endif
      open (unit=lun,file=fnamo,
     *form='binary',status='new',iostat=istat)
20001 format(a<namlen>,'\',a<namfl>)
20009 format(a80)
      return
      end
c--------------------------------------------------
c--------------------------------------------------
      subroutine openunfi(lun)
      use dflib
      include 'sandag.inc'
      integer namlen, namfl
      open (unit=lun,file=fnami,
     *form='unformatted',status='old',iostat=istat)
      if(istat.ne.0) go to 1
      print *,fnami
      return
    1	write(fnamo,20000) fnami
20000 format('..\data\',a72)
      open(share='denynone',unit=lun,file=fnamo,
     *form='unformatted',status='old',iostat=istat)
      if(istat.ne.0) go to 11
      write(0,20009) fnamo
      return 
   11 call getarg(1,pname)
c	pname='o:\2030\ver1\byear'
      namlen=len_trim(pname)
	namfl=len_trim(fnami)
c	print *, namlen,namfl
      write(fnamo,20001) pname,fnami
20001 format(a<namlen>,'\',a<namfl>)
      open (unit=lun,file=fnamo,
     *form='unformatted',status='old',iostat=istat)
      if(istat.ne.0) go to 2
      write(0,20009) fnamo
      return 
    2 call getarg(2,sname)
      namlen=len_trim(sname)
      write(fnamo,20002) sname,fnami
      write(0,20009) fnamo
20002 format(a<namlen>,'\data\',a<namfl>)	
      open (unit=lun,file=fnamo,
     *form='unformatted',status='old',iostat=istat)
      return
20009 format(a80)
      return
      end
c--------------------------------------------------
c----------------------------------------------------
      subroutine cheadr
      include 'sandag.inc'

      do 10 i=1,80
      cheado(i)=' '
   10 continue
      do 11 i=1,80
      if(cheadi(i).ne.' ') go to 12
   11 continue
      i1=1
      go to 20
   12 i1=i
   20 do 21 i=1,80
      j=81-i
      if(cheadi(j).ne.' ') go to 22
   21 continue
      il=80
      go to 30
   22 il=j
   30 ilen=il-i1+1
      ix=(80-ilen)/2
      ii=0
      do 31 i=i1,il
      ii=ii+1
      j=ix+ii
      cheado(j)=cheadi(i)
   31 continue
      return
      end
c---------------------------------------------------------------
c     subroutine to read to columns of data without knowing format
c
      subroutine sd_getdat(ic,ac,icol,i1,i2,i3,lun)
      integer*2  ic(80),int(6)
      character*1 ac(80)

      i1=0
      i2=0

      i3=0
      do 10 i=1,80
      if(ac(i).ne.' ') go to 11
   10 continue
      go to 90
   11 int(1)=ic(i)

      mint=1
      ii=i+1
      do 12 j=ii,80
      if(ac(j).eq.' ') go to 20
      mint=mint+1
      int(mint)=ic(j)
   12 continue
      go to 99
   20 if(mint.eq.1) go to 21
      if(mint.eq.2) go to 22
      if(mint.eq.3) go to 23
      if(mint.eq.4) go to 24
      if(mint.eq.5) go to 25
      if(mint.eq.6) go to 26
      go to 90
   21 i1=int(1)
      go to 29
   22 i1=int(1)*10+int(2)
      go to 29
   23 i1=int(1)*100+int(2)*10+int(3)
      go to 29
   24 i1=int(1)*1000+int(2)*100+int(3)*10+int(4)
      go to 29
   25 i1=int(1)*10000+int(2)*1000+int(3)*100+int(4)*10+int(5)
      go to 29
   26 i1=int(1)*100000+
     *int(2)*10000+int(3)*1000+int(4)*100+int(5)*10+int(6)
   29 jc=j+1
      if(icol.lt.2) go to 99
      do 30 i=jc,80
      if(ac(i).ne.' ') go to 31
   30 continue
      go to 99
   31 int(1)=ic(i)
      mint=1
      ii=i+1
      do 32 j=ii,80
      if(ac(j).eq.' ') go to 40
      mint=mint+1
      int(mint)=ic(j)
   32 continue
      go to 90
   40 if(mint.eq.1) go to 41
      if(mint.eq.2) go to 42
      if(mint.eq.3) go to 43
      if(mint.eq.4) go to 44
      if(mint.eq.5) go to 45
      if(mint.eq.6) go to 46
      go to 90
   41 i2=int(1)
      go to 49
   42 i2=int(1)*10+int(2)
      go to 49
   43 i2=int(1)*100+int(2)*10+int(3)
      go to 49
   44 i2=int(1)*1000+int(2)*100+int(3)*10+int(4)
      go to 49
   45 i2=int(1)*10000+int(2)*1000+int(3)*100+int(4)*10+int(5)
      go to 49
   46 i2=int(1)*100000+
     *int(2)*10000+int(3)*1000+int(4)*100+int(5)*10+int(6)
      go to 49
   49 jc=j+1
      if(icol.lt.3) go to 99
      do 50 i=jc,80
      if(ac(i).ne.' ') go to 51
   50 continue
      go to 99
   51 int(1)=ic(i)
      mint=1
      ii=i+1
      do 52 j=ii,80
      if(ac(j).eq.' ') go to 60
      mint=mint+1
      int(mint)=ic(j)
   52 continue
      go to 90
   60 if(mint.eq.1) go to 61
      if(mint.eq.2) go to 62
      if(mint.eq.3) go to 63
      if(mint.eq.4) go to 64
      if(mint.eq.5) go to 65
      if(mint.eq.6) go to 66
      go to 90
   61 i3=int(1)
      go to 99
   62 i3=int(1)*10+int(2)
      go to 99
   63 i3=int(1)*100+int(2)*10+int(3)
      go to 99
   64 i3=int(1)*1000+int(2)*100+int(3)*10+int(4)
      go to 99
   65 i3=int(1)*10000+int(2)*1000+int(3)*100+int(4)*10+int(5)
      go to 99
   66 i3=int(1)*100000+
     *int(2)*10000+int(3)*1000+int(4)*100+int(5)*10+int(6)
      go to 99
   90 write(lun,1901)
 1901 format(' error finding fields')
   99 return
      end
c--------------------------------------------------------------  
      subroutine ozonep
      use dflib
      include 'sandag.inc'
      include 'zone.inc'
      fnami='zones/pat.adf'
      open(share='denynone',unit=patlun,convert='big_endian',
     *file=fnami,access='direct',form='unformatted',recl=12,
     *status='old',err=2)
      go to 9
    2 fnami='../covs/zones/pat.adf'
      open(share='denynone',unit=patlun,convert='big_endian',
     *file=fnami,access='direct',form='unformatted',recl=12,
     *status='old',err=3)
    9 patio=0
      go to 99
    3 fnami='zones/pat.adf'
      call getarg(1,pname)
      namlen=len_trim(pname)
	namfl=len_trim(fnami)
	print *, namlen,namfl
      write(fnamo,20001) pname,fnami
	print *,fnamo
      write(0,20009) fnamo
20001 format(a<namlen>,'\',a<namfl>)
      open(share='denynone',unit=patlun,convert='big_endian',
     *file=fnamo,access='direct',form='unformatted',recl=12,
     *status='old',err=4)
      patio=0
	go to 99
20009 format(a80)
    4 fnami='zones/pat.adf'
      call getarg(2,sname)
      namlen=len_trim(sname)
      write(fnamo,20002) sname,fnami
20002 format(a<namlen>,'\covs\',a<namfl>)	
      open(share='denynone',unit=patlun,convert='big_endian',
     *file=fnamo,access='direct',form='unformatted',recl=12,
     *status='old',err=98) 
      write(0,20009) fnamo
	patio=0
	goto 99
   98	patio=1
   99 return
      end
c--------------------------------------------------------------
c--------------------------------------------------------------
      subroutine otdzp
      use dflib
      include 'sandag.inc'
      include 'zone.inc'
      fnami='tdzs/pat.adf'
      open(share='denynone',unit=patlun,convert='big_endian',
     *file=fnami,access='direct',form='unformatted',recl=12,
     *status='old',err=2)
      go to 9
    2 fnami='../covs/tdzs/pat.adf'
      open(share='denynone',unit=patlun,convert='big_endian',
     *file=fnami,access='direct',form='unformatted',recl=12,
     *status='old',err=3)
    9 patio=0
      go to 99
    3 fnami='tdzs/pat.adf'
      call getarg(1,pname)
      namlen=len_trim(pname)
	namfl=len_trim(fnami)
	print *, namlen,namfl
      write(fnamo,20001) pname,fnami
	print *,fnamo
      write(0,20009) fnamo
20001 format(a<namlen>,'\',a<namfl>)
      open(share='denynone',unit=patlun,convert='big_endian',
     *file=fnamo,access='direct',form='unformatted',recl=12,
     *status='old',err=4)
      patio=0
	go to 99
20009 format(a80)
    4 fnami='tdzs/pat.adf'
      call getarg(2,sname)
      namlen=len_trim(sname)
      write(fnamo,20002) sname,fnami
20002 format(a<namlen>,'\covs\',a<namfl>)	
      open(share='denynone',unit=patlun,convert='big_endian',
     *file=fnamo,access='direct',form='unformatted',recl=12,
     *status='old',err=98) 
      write(0,20009) fnamo
	patio=0
	goto 99
   98	patio=1
   99 return
      end
c------------------------------------------------------------
      subroutine rzonep
      include 'zone.inc'
      read(patlun,rec=patrec,iostat=patio)
     *patare,patper,patlb,patid,patzon,patint,patcol,pattmp,patx,paty
      return
      end
c------------------------------------------------------------
      subroutine wzonep
      include 'zone.inc'
      write(patlun,rec=patrec,iostat=patio)
     *patare,patper,patlb,patid,patzon,patint,patcol,pattmp,patx,paty
      return
      end
c----------------------------------------------------------
      subroutine omgrf
      use dflib
      include 'sandag.inc'
      include 'zone.inc'
      fnami='mgrf/pat.adf'
      open(share='denynone',unit=patlun,convert='big_endian',
     *file=fnami,access=
     *'direct',form='unformatted',recl=12,status='old',err=2)
      go to 9
    2 fnami='../covs/mgrf/pat.adf'
      open(share='denynone',unit=patlun,convert='big_endian',
     *file=fnami,access=
     *'direct',form='unformatted',recl=12,status='old',err=3)
    9 patio=0 
      go to 99
    3 fnami='mgrf/pat.adf'
      call getarg(1,pname)
      namlen=len_trim(pname)
	namfl=len_trim(fnami)
	print *, namlen,namfl
      write(fnamo,20001) pname,fnami
	print *,fnamo
      write(0,20009) fnamo
20001 format(a<namlen>,'\',a<namfl>)
      open(share='denynone',unit=patlun,convert='big_endian',
     *file=fnamo,access=
     *'direct',form='unformatted',recl=12,status='old',err=4)
      patio=0
	go to 99
20009 format(a80)
    4 fnami='mgrf/pat.adf'
      call getarg(2,sname)
      namlen=len_trim(sname)
      write(fnamo,20002) sname,fnami
20002 format(a<namlen>,'\covs\',a<namfl>)	
      open(share='denynone',unit=patlun,convert='big_endian',
     *file=fnamo,access=
     *'direct',form='unformatted',recl=12,status='old',err=98) 
      write(0,20009) fnamo
	patio=0
	goto 99
   98 patio=1
   99 return
      end
c------------------------------------------------------------
      subroutine omgrfpt
      use dflib
      include 'sandag.inc'
      include 'zone.inc'
      fnami='mgrfpt/pat.adf'
      open(share='denynone',unit=patlun,convert='big_endian',
     *file=fnami,access=
     *'direct',form='unformatted',recl=10,status='old',err=2)
      go to 9
    2 fnami='../covs/mgrfpt/pat.adf'
      open(share='denynone',unit=patlun,convert='big_endian',
     *file=fnami,access=
     *'direct',form='unformatted',recl=10,status='old',err=3)
    9 patio=0
      go to 99
    3 fnami='mgrfpt/pat.adf'
      call getarg(1,pname)
      namlen=len_trim(pname)
	namfl=len_trim(fnami)
	print *, namlen,namfl
      write(fnamo,20001) pname,fnami
	print *,fnamo
      write(0,20009) fnamo
20001 format(a<namlen>,'\',a<namfl>)
      open(share='denynone',unit=patlun,convert='big_endian',
     *file=fnamo,access=
     *'direct',form='unformatted',recl=10,status='old',err=4)
      patio=0
	go to 99
20009 format(a80)
    4 fnami='mgrfpt/pat.adf'
      call getarg(2,sname)
      namlen=len_trim(sname)
      write(fnamo,20002) sname,fnami
20002 format(a<namlen>,'\covs\',a<namfl>)	
      open(share='denynone',unit=patlun,convert='big_endian',
     *file=fnamo,access=
     *'direct',form='unformatted',recl=10,status='old',err=98) 
      write(0,20009) fnamo
	patio=0
	goto 99
   98 patio=1
   99 return
      end
c------------------------------------------------------------
      subroutine rmgrfpt
      include 'zone.inc'
      read(patlun,rec=patrec,iostat=patio) patare,patper,
     *patlb,patid,patzon,patx,paty,patz
      return
      end
c------------------------------------------------------------
c-------------------------------------------------------------

      subroutine wmgrfpt
      include 'zone.inc'
      read(patlun,rec=patrec,iostat=patio) patare,patper,
     *patlb,patid,patzon,patx,paty,patz
      return
      end
c----------------------------------------------------------
c----------------------------------------------------------
      subroutine otca
      include 'sandag.inc'
      include 'tcov.inc'
      fnamo='tcov/aat.adf'
      open(share='denynone',unit=aatlun,convert='big_endian',
     *file=fnamo,access=
     *'direct',form='unformatted',recl=131,status='old',err=98)
      aatio=0
      go to 99
   98 aatio=1
   99 return
      end
c--------------------------------------------------------------
c--------------------------------------------------------------
      subroutine ottca
      include 'sandag.inc'
      include 'tcov.inc'
      fnamo='ttcov/aat.adf'
C     call opencov(aatlun)
      open(share='denynone',unit=aatlun,convert='big_endian',
     *file=fnamo,access=
     *'direct',form='unformatted',recl=131,status='old',err=98)
      aatio=0
      go to 99
   98 aatio=1
   99 return
      end
c------------------------------------------------------------
c------------------------------------------------------------
      subroutine rtca !read tcov arc attributes
      include 'tcov.inc'
      read(aatlun,rec=aatrec,iostat=aatio) aatarn(2),aatarn(1),
     *lpoly,rpoly,aatlen,aatlb,aatid,aatx(2),aaty(2),aatx(1),aaty(1),
     *aatwho,aattmp(1),aattmp(2),aatplt,aatsph,aatrt,aatlk,aatrts,
     *aatnm,aatxnm(2),aatxnm(1),aattpn(2),aattpn(1),
     *aatcj,aatsta,aatloc,aatlp,aatadt,aatvol,
     *aatppc,aattpc,aatsec,aatdir,aatffc,aatcl,aatosp,aatasp,aatlos,
     *aatdyr,aatdprj,aatyr(1),aatprj(1),aatjur(1),aatfc(1),
     *aathov(1),aattrk(1),aatspd(1),aattspd(1),aatway(1),aatmed(1),
     *aattoll(1,1),aattoll(2,1),aattoll(3,1),
     *aattm(1,1),aattm(2,1),aattm(3,1),
     *aatln(1,1,1),aatln(2,1,1),aatln(3,1,1),aataux(1,1),
     *aatpct(1),aatphf(1),aatcnt(1,1),aattl(1,1),aatrl(1,1),
     *aatll(1,1),aatplc(1),aatat(1),aatgc(1),aattv(1),aatrv(1),
     *aatlv(1),aatstp(1),aattm(1,2),aattm(2,2),aattm(3,2),
     *aatln(1,2,1),aatln(2,2,1),aatln(3,2,1),aataux(2,1),
     *aatpct(2),aatphf(2),aatcnt(2,1),aattl(2,1),aatrl(2,1),
     *aatll(2,1),aatplc(2),aatat(2),aatgc(2),aattv(2),aatrv(2),
     *aatlv(2),aatstp(2),
     *(aatyr(i),aatprj(i),aatjur(i),aatfc(i),
     *aathov(i),aattrk(i),aatspd(i),aattspd(i),aatway(i),aatmed(i),
     *aattoll(1,i),aattoll(2,i),aattoll(3,i),
     *aatln(1,1,i),aatln(2,1,i),aatln(3,1,i),aataux(1,i),
     *aatcnt(1,i),aattl(1,i),aatrl(1,i),aatll(1,i),
     *aatln(1,2,i),aatln(2,2,i),aatln(3,2,i),aataux(2,i),
     *aatcnt(2,i),aattl(2,i),aatrl(2,i),aatll(2,i),i=2,mxtcyr)

      return
      end
c------------------------------------------------------------------
      subroutine wtca !write tcov arc attributes
      include 'tcov.inc'
      write(aatlun,rec=aatrec,iostat=aatio) aatarn(2),aatarn(1),
     *lpoly,rpoly,aatlen,aatlb,aatid,aatx(2),aaty(2),aatx(1),aaty(1),
     *aatwho,aattmp(1),aattmp(2),aatplt,aatsph,aatrt,aatlk,aatrts,
     *aatnm,aatxnm(2),aatxnm(1),aattpn(2),aattpn(1),
     *aatcj,aatsta,aatloc,aatlp,aatadt,aatvol,
     *aatppc,aattpc,aatsec,aatdir,aatffc,aatcl,aatosp,aatasp,aatlos,
     *aatdyr,aatdprj,aatyr(1),aatprj(1),aatjur(1),aatfc(1),
     *aathov(1),aattrk(1),aatspd(1),aattspd(1),aatway(1),aatmed(1),
     *aattoll(1,1),aattoll(2,1),aattoll(3,1),
     *aattm(1,1),aattm(2,1),aattm(3,1),
     *aatln(1,1,1),aatln(2,1,1),aatln(3,1,1),aataux(1,1),
     *aatpct(1),aatphf(1),aatcnt(1,1),aattl(1,1),aatrl(1,1),
     *aatll(1,1),aatplc(1),aatat(1),aatgc(1),aattv(1),aatrv(1),
     *aatlv(1),aatstp(1),aattm(1,2),aattm(2,2),aattm(3,2),
     *aatln(1,2,1),aatln(2,2,1),aatln(3,2,1),aataux(2,1),
     *aatpct(2),aatphf(2),aatcnt(2,1),aattl(2,1),aatrl(2,1),
     *aatll(2,1),aatplc(2),aatat(2),aatgc(2),aattv(2),aatrv(2),
     *aatlv(2),aatstp(2),
     *(aatyr(i),aatprj(i),aatjur(i),aatfc(i),
     *aathov(i),aattrk(i),aatspd(i),aattspd(i),aatway(i),aatmed(i),
     *aattoll(1,i),aattoll(2,i),aattoll(3,i),
     *aatln(1,1,i),aatln(2,1,i),aatln(3,1,i),aataux(1,i),
     *aatcnt(1,i),aattl(1,i),aatrl(1,i),aatll(1,i),
     *aatln(1,2,i),aatln(2,2,i),aatln(3,2,i),aataux(2,i),
     *aatcnt(2,i),aattl(2,i),aatrl(2,i),aatll(2,i),i=2,mxtcyr)
      return
      end
c-------------------------------------------------------
c-------------------------------------------------------
      subroutine otcn
      include 'sandag.inc'
      include 'tcov.inc'
      fnamo='tcov/nat.adf'
      open(share='denynone',unit=natlun,convert='big_endian',
     *file=fnamo,access=
     *'direct',form='unformatted',recl=31,status='old',err=98)
      go to 99
   98 natio=1
   99 return
      end
c---------------------------------------------------------
c---------------------------------------------------------
      subroutine ottcn
      include 'sandag.inc'
      include 'tcov.inc'
      fnamo='ttcov/nat.adf'
      open(share='denynone',unit=natlun,convert='big_endian',
     *file=fnamo,access=
     *'direct',form='unformatted',recl=31,status='old',err=98)
      natio=0
      go to 99
   98 natio=1
   99 return
      end
c---------------------------------------------------------
c---------------------------------------------------------
      subroutine rtcn
      include 'tcov.inc'
      read(natlun,rec=natrec,iostat=natio) aatlb,natlb,natid,
     *natxnm(1),natxnm(2),natnty,nattmp,natx,naty,natz,natiuc,
     *natsph,nattpn,natyr(1),natprj(1),natjur(1),natcnt(1),natcyc,
     *natyr(2),natprj(2),natjur(2),natcnt(2),nattap,
     *nattrn,natfz4,natfz6,natfz7,
     *nattyr(1),natstp(1),natprk(1),nattt(1),natdwl(1),
     *nattyr(2),natstp(2),natprk(2),nattt(2),natdwl(2)
      return
      end
c----------------------------------------------------------
c----------------------------------------------------------
      subroutine wtcn
      include 'tcov.inc'
      write(natlun,rec=natrec,iostat=natio) aatlb,natlb,natid,
     *natxnm(1),natxnm(2),natnty,nattmp,natx,naty,natz,natiuc,
     *natsph,nattpn,natyr(1),natprj(1),natjur(1),natcnt(1),natcyc,
     *natyr(2),natprj(2),natjur(2),natcnt(2),nattap,
     *nattrn,natfz4,natfz6,natfz7,
     *nattyr(1),natstp(1),natprk(1),nattt(1),natdwl(1),
     *nattyr(2),natstp(2),natprk(2),nattt(2),natdwl(2)
      return
      end
c----------------------------------------------------------
c-----------------------------------------------------------
      subroutine otctrs
	use dflib
      include 'sandag.inc'
      include 'tcov.inc'
      integer namlen, namfl
	fnami='tcov/transit.sec'
      open(share='denynone',unit=seclun,convert='big_endian',
     *file=fnami,access=
     *'direct',form='unformatted',recl=50,status='old',err=2)
      secio=0
      go to 99
    2 call getarg(2,sname)
c	sname='o:\lta\test2'
      namlen=len_trim(sname)
	namfl=len_trim(fnami)
      write(fnamo,20002) sname,fnami
      write(0,20009) fnamo
20002 format(a<namlen>,'\covs\',a<namfl>)	
	print *, namlen,namfl
	print *,fnamo
      write(0,20009) fnamo
20009 format(a80)
      open(share='denynone',unit=seclun,convert='big_endian',
     *file=fnamo,access=
     *'direct',form='unformatted',recl=50,status='old',err=98)
      secio=0
      go to 99
   98 secio=1
   99 return
      end
c---------------------------------------------------------------
c--------------------------------------------------------------
      subroutine ottctrs
      include 'sandag.inc'
      include 'tcov.inc'
      fnamo='ttcov/transit.sec'
      open(share='denynone',unit=seclun,convert='big_endian',
     *file=fnamo,access=
     *'direct',form='unformatted',recl=50,status='old',err=98)
      secio=0
      go to 99
   98 secio=1
   99 return
      end
c--------------------------------------------------------------
c--------------------------------------------------------------
      subroutine ottrcs
      include 'sandag.inc'
      include 'tcov.inc'
      fnamo='ttrcov/transit.sec'
      open(share='denynone',unit=seclun,convert='big_endian',
     *file=fnamo,access=
     *'direct',form='unformatted',recl=50,status='old',err=98)
      secio=0
      go to 99
   98 secio=1
   99 return
      end
c--------------------------------------------------------------
c--------------------------------------------------------------
      subroutine rtctrs
      include 'tcov.inc'
      read(seclun,rec=secrec,iostat=secio)
     *rlink,alink,secmes(2),secmes(1),secpos(2),secpos(1),
     *seclb,secid,sectm2,sectm4,
     *secic,secoc,secsic,secsoc,seci2
      return
      end
c-------------------------------------------------------------
c-------------------------------------------------------------
      subroutine wtctrs
      include 'tcov.inc'
      write(seclun,rec=secrec,iostat=secio)
     *rlink,alink,secmes(2),secmes(1),secpos(2),secpos(1),
     *seclb,secid,sectm2,sectm4,
     *secic,secoc,secsic,secsoc,seci2
      return
      end
c------------------------------------------------------------
c------------------------------------------------------------
      subroutine otctus
      include 'sandag.inc'
      include 'tcov.inc'
      fnamo='tcov/turns.sec'
      open(share='denynone',unit=seclun,convert='big_endian',
     *file=fnamo,access=
     *'direct',form='unformatted',recl=8,status='old',err=98)
      secio=0
      go to 99

   98 secio=1
   99 return
      end
c--------------------------------------------------------------

      subroutine rtctus
      include 'tcov.inc'
      read(seclun,rec=secrec,iostat=secio)
     *rlink,alink,secmes(2),secmes(1),secpos(2),secpos(1),seclb,secid
      return
      end
c--------------------------------------------------------------
      subroutine wtctus
      include 'tcov.inc'
      write(seclun,rec=secrec,iostat=secio)
     *rlink,alink,secmes(2),secmes(1),secpos(2),secpos(1),seclb,secid
      return
      end
c---------------------------------------------------------------
c---------------------------------------------------------------
      subroutine otcea
      include 'sandag.inc'
      include 'tcov.inc'
      fnamo='tcoved/aat.adf'
      open(share='denynone',unit=aatlun,convert='big_endian',
     *file=fnamo,access=
     *'direct',form='unformatted',recl=131,status='old',err=98)
    9 aatio=0
      go to 99
   98 aatio=1
   99 return
      end
c-----------------------------------------------------------------
c-----------------------------------------------------------------
      subroutine otcen
      include 'sandag.inc'
      include 'tcov.inc'
      fnamo='tcoved/nat.adf'
      open(share='denynone',unit=natlun,convert='big_endian',
     *file=fnamo,access=
     *'direct',form='unformatted',recl=31,status='old',err=98)
    9 natio=0
      go to 99
   98 natio=1
   99 return
      end
c---------------------------------------------------------------
c---------------------------------------------------------------
      subroutine otcetrr
      include 'sandag.inc'
      include 'tcov.inc'
      fnamo='tcoved\transit.rat'
      open(share='denynone',unit=ratlun,convert='big_endian',
     *file=fnamo,access=
     *'direct',form='unformatted',recl=2,status='old',err=98)
      go to 9
    9 ratio=0
      go to 99
   98 ratio=1

   99 return
      end
c----------------------------------------------------------------
c-----------------------------------------------------------------
      subroutine ottctrr
      include 'sandag.inc'
      include 'tcov.inc'
      fnamo='ttcov/transit.rat'
      open(share='denynone',unit=ratlun,convert='big_endian',
     *file=fnamo,access=
     *'direct',form='unformatted',recl=2,status='old',err=98)
      go to 9
    9 ratio=0
      go to 99
   98 ratio=1
   99 return
      end
c-----------------------------------------------------------------
c-----------------------------------------------------------------
      subroutine ottrcr
      include 'sandag.inc'
      include 'tcov.inc'
      fnamo='ttrcov/transit.rat'
      open(share='denynone',unit=ratlun,convert='big_endian',
     *file=fnamo,access=
     *'direct',form='unformatted',recl=2,status='old',err=98)
      go to 9
    9 ratio=0
      go to 99
   98 ratio=1
   99 return
      end
c----------------------------------------------------------------
c----------------------------------------------------------------
      subroutine rtctrr
      include 'tcov.inc'
      read(ratlun,rec=ratrec,iostat=ratio) ratlb,ratid
      return
      end
c----------------------------------------------------------------
      subroutine wtctrr
      include 'tcov.inc'
      write(ratlun,rec=ratrec,iostat=ratio) ratlb,ratid
      return
      end
c----------------------------------------------------------------

c----------------------------------------------------------------
      subroutine otcetrs
      include 'sandag.inc'
      include 'tcov.inc'
      fnamo='tcoved/transit.sec'
      open(share='denynone',unit=seclun,convert='big_endian',
     *file=fnamo,access=
     *'direct',form='unformatted',recl=50,status='old',err=98)
    9 secio=0
      go to 99
   98 secio=1
   99 return
      end
c-------------------------------------------------------------
c----------------------------------------------------------------
      subroutine ofca1
	use dflib
      include 'sandag.inc'
      include 'tcov.inc'
      fnami='fwycov1/aat.adf'
      open(share='denynone',unit=aatlun,convert='big_endian',
     *file=fnami,access=
     *'direct',form='unformatted',recl=101,status='old',err=2)
      aatio=0
      go to 99
    2 call getarg(1,pname)
      namlen=len_trim(pname)
	namfl=len_trim(fnami)
	print *, namlen,namfl
      write(fnamo,20001) pname,fnami
	print *,fnamo
      write(0,20009) fnamo
20001 format(a<namlen>,'\',a<namfl>)
      open(share='denynone',unit=aatlun,convert='big_endian',
     *file=fnamo,access=
     *'direct',form='unformatted',recl=101,status='old',err=98)
      aatio=0
20009 format(a80)
      go to 99
   98 aatio=1
   99 return
      end
c---------------------------------------------------------------
c----------------------------------------------------------------
      subroutine ofca2
	use dflib
      include 'sandag.inc'
      include 'tcov.inc'
      fnami='fwycov2/aat.adf'
      open(share='denynone',unit=aatlun,convert='big_endian',
     *file=fnami,access=
     *'direct',form='unformatted',recl=101,status='old',err=2)
      aatio=0
      go to 99
    2 call getarg(1,pname)
      namlen=len_trim(pname)
	namfl=len_trim(fnami)
	print *, namlen,namfl
      write(fnamo,20001) pname,fnami
	print *,fnamo
      write(0,20009) fnamo
20001 format(a<namlen>,'\',a<namfl>)
      open(share='denynone',unit=aatlun,convert='big_endian',
     *file=fnamo,access=
     *'direct',form='unformatted',recl=101,status='old',err=98)
      aatio=0
20009 format(a80)
      go to 99
   98 aatio=1
   99 return
      end
c---------------------------------------------------------------
      subroutine osbebfca1
      use dflib
      include 'sandag.inc'
      include 'tcov.inc'
      fnami='sbebfwycov1/aat.adf'
      open(share='denynone',unit=aatlun,convert='big_endian',
     *file=fnami,access=
     *'direct',form='unformatted',recl=101,status='old',err=2)
      aatio=0
      go to 99
    2 call getarg(1,pname)
      namlen=len_trim(pname)
	namfl=len_trim(fnami)
	print *, namlen,namfl
      write(fnamo,20001) pname,fnami
	print *,fnamo
      write(0,20009) fnamo
20001 format(a<namlen>,'\',a<namfl>)
      open(share='denynone',unit=aatlun,convert='big_endian',
     *file=fnamo,access=
     *'direct',form='unformatted',recl=101,status='old',err=98)
      aatio=0
20009 format(a80)
      go to 99
   98 aatio=1
   99 return
      end
c---------------------------------------------------------------
c----------------------------------------------------------------
      subroutine osbebfca2
      use dflib
      include 'sandag.inc'
      include 'tcov.inc'
      fnami='sbebfwycov2/aat.adf'
      open(share='denynone',unit=aatlun,convert='big_endian',
     *file=fnami,access=
     *'direct',form='unformatted',recl=101,status='old',err=2)
      aatio=0
      go to 99
    2 call getarg(1,pname)
      namlen=len_trim(pname)
	namfl=len_trim(fnami)
	print *, namlen,namfl
      write(fnamo,20001) pname,fnami
	print *,fnamo
      write(0,20009) fnamo
20001 format(a<namlen>,'\',a<namfl>)
      open(share='denynone',unit=aatlun,convert='big_endian',
     *file=fnamo,access=
     *'direct',form='unformatted',recl=101,status='old',err=98)
      aatio=0
20009 format(a80)
      go to 99
   98 aatio=1
   99 return
      end

c----------------------------------------------------------------
      subroutine ofcp1
      use dflib
      include 'sandag.inc'
      include 'zone.inc'
      fnami='fwypnt1/pat.adf'
      open(share='denynone',unit=patlun,convert='big_endian',
     *file=fnami,access=
     *'direct',form='unformatted',recl=43,status='old',err=2)
      patio=0
      go to 99
    2 call getarg(1,pname)
      namlen=len_trim(pname)
	namfl=len_trim(fnami)
	print *, namlen,namfl
      write(fnamo,20001) pname,fnami
	print *,fnamo
      write(0,20009) fnamo
20001 format(a<namlen>,'\',a<namfl>)
      open(share='denynone',unit=patlun,convert='big_endian',
     *file=fnamo,access=
     *'direct',form='unformatted',recl=43,status='old',err=98)
      patio=0
20009 format(a80)
      go to 99
   98 patio=1
   99 return
      end
c---------------------------------------------------------------
c----------------------------------------------------------------
      subroutine ofcp2
	use dflib
      include 'sandag.inc'
      include 'zone.inc'
      fnami='fwypnt2/pat.adf'
      open(share='denynone',unit=patlun,convert='big_endian',
     *file=fnami,access=
     *'direct',form='unformatted',recl=43,status='old',err=2)
      patio=0
      go to 99
    2 call getarg(1,pname)
      namlen=len_trim(pname)
	namfl=len_trim(fnami)
	print *, namlen,namfl
      write(fnamo,20001) pname,fnami
	print *,fnamo
      write(0,20009) fnamo
20001 format(a<namlen>,'\',a<namfl>)
      open(share='denynone',unit=patlun,convert='big_endian',
     *file=fnamo,access=
     *'direct',form='unformatted',recl=43,status='old',err=98)
      patio=0
20009 format(a80)
      go to 99
   98 patio=1
   99 return
      end
c---------------------------------------------------------------

      subroutine rfcp
      include 'tcov.inc'
      include 'zone.inc'
      read(patlun,rec=patrec,iostat=patio) patare,patper,patlb,patid,
     *aatudj,aatadj,aattmp,aatsph,aatrt,aatlk,aatnm,aatxnm(2),aatxnm(1),
     *aatcj,aatsta,aatloc,aatadt,aatvol,aatppc,aattpc,
     *aatdir,aatffc,aatcl,aatasp,aatyr(1),aatprj(1),aatjur(1),
     *aatfc(1),aathov(1),aatspd(1),aatway(1),aatmed(1),
     *aatln(1,1,1),aatln(2,1,1),aatln(3,1,1),aatvla(1),aatvlp(1),
     *aatln(1,2,1),aatln(2,2,1),aatln(3,2,1),aatvla(2),aatvlp(2)
      return
      end
c-------------------------------------------------------------
c---------------------------------------------------------------
      subroutine wfcp
      include 'tcov.inc'
      include 'zone.inc'
      write(patlun,rec=patrec,iostat=patio) patare,patper,patlb,patid,
     *aatudj,aatadj,aattmp,aatsph,aatrt,aatlk,aatnm,aatxnm(2),aatxnm(1),
     *aatcj,aatsta,aatloc,aatadt,aatvol,aatppc,aattpc,
     *aatdir,aatffc,aatcl,aatasp,aatyr(1),aatprj(1),aatjur(1),
     *aatfc(1),aathov(1),aatspd(1),aatway(1),aatmed(1),
     *aatln(1,1,1),aatln(2,1,1),aatln(3,1,1),aatvla(1),aatvlp(1),
     *aatln(1,2,1),aatln(2,2,1),aatln(3,2,1),aatvla(2),aatvlp(2)
      return
      end
c-------------------------------------------------------------
c---------------------------------------------------------------
      subroutine rhca !read hwycov arc attributes
      include 'tcov.inc'
      read(aatlun,rec=aatrec,iostat=aatio) aatarn(2),aatarn(1),
     *lpoly,rpoly,aatlen,aatlb,aatid,aatqid,aatcc,aatudj,aatadj,aattmp,
     *aatplt,aatsph,aatrt,aatlk,aatnm,aatxnm(2),aatxnm(1),aattpn(2),
     *aattpn(1),aatcj,aatsta,aatloc,aatlp,aatadt,aatvol,aatppc,aattpc,
     *aatsec,aatdir,aatffc,aatcl,aatasp,aatyr(1),aatprj(1),aatjur(1),
     *aatfc(1),aathov(1),aattrk(1),aatspd(1),aattspd(1),aatway(1),
     *aatmed(1),aatcst,aattoll(1,1),aattoll(2,1),aattoll(3,1),
     *aatln(1,1,1),aatln(2,1,1),aatln(3,1,1),aataux(1,1),
     *aatpct(1),aatphf(1),aatcnt(1,1),
     *aattl(1,1),aatrl(1,1),aatll(1,1),aattlb(1),aatrlb(1),aatllb(1),
     *aatgc(1),aatplc(1),aatlc(1,1),aatlc(2,1),aatlc(3,1),
     *aatxc(1,1),aatxc(2,1),aatxc(3,1),aathc(1,1),aathc(2,1),aathc(3,1),
     *aatlt(1,1),aatlt(2,1),aatlt(3,1),aatxt(1,1),aatxt(2,1),aatxt(3,1),
     *aattc(1),aatvla(1),aatvlp(1),aatls(1),
     *aatln(1,2,1),aatln(2,2,1),aatln(3,2,1),aataux(2,1),
     *aatpct(2),aatphf(2),aatcnt(2,1),
     *aattl(2,1),aatrl(2,1),aatll(2,1),aattlb(2),aatrlb(2),aatllb(2),
     *aatgc(2),aatplc(2),aatlc(1,2),aatlc(2,2),aatlc(3,2),
     *aatxc(1,2),aatxc(2,2),aatxc(3,2),aathc(1,2),aathc(2,2),aathc(3,2),
     *aatlt(1,2),aatlt(2,2),aatlt(3,2),aatxt(1,2),aatxt(2,2),aatxt(3,2),
     *aattc(2),aatvla(2),aatvlp(2),aatls(2)
      return
      end
c-------------------------------------------------------------
      subroutine whca !write hwycov arc attributes
      include 'tcov.inc'
      write(aatlun,rec=aatrec,iostat=aatio) aatarn(2),aatarn(1),
     *lpoly,rpoly,aatlen,aatlb,aatid,aatqid,aatcc,aatudj,aatadj,aattmp,
     *aatplt,aatsph,aatrt,aatlk,aatnm,aatxnm(2),aatxnm(1),aattpn(2),
     *aattpn(1),aatcj,aatsta,aatloc,aatlp,aatadt,aatvol,aatppc,aattpc,
     *aatsec,aatdir,aatffc,aatcl,aatasp,aatyr(1),aatprj(1),aatjur(1),
     *aatfc(1),aathov(1),aattrk(1),aatspd(1),aattspd(1),aatway(1),
     *aatmed(1),aatcst,aattoll(1,1),aattoll(2,1),aattoll(3,1),
     *aatln(1,1,1),aatln(2,1,1),aatln(3,1,1),aataux(1,1),
     *aatpct(1),aatphf(1),aatcnt(1,1),
     *aattl(1,1),aatrl(1,1),aatll(1,1),aattlb(1),aatrlb(1),aatllb(1),
     *aatgc(1),aatplc(1),aatlc(1,1),aatlc(2,1),aatlc(3,1),
     *aatxc(1,1),aatxc(2,1),aatxc(3,1),aathc(1,1),aathc(2,1),aathc(3,1),
     *aatlt(1,1),aatlt(2,1),aatlt(3,1),aatxt(1,1),aatxt(2,1),aatxt(3,1),
     *aattc(1),aatvla(1),aatvlp(1),aatls(1),
     *aatln(1,2,1),aatln(2,2,1),aatln(3,2,1),aataux(2,1),
     *aatpct(2),aatphf(2),aatcnt(2,1),
     *aattl(2,1),aatrl(2,1),aatll(2,1),aattlb(2),aatrlb(2),aatllb(2),
     *aatgc(2),aatplc(2),aatlc(1,2),aatlc(2,2),aatlc(3,2),
     *aatxc(1,2),aatxc(2,2),aatxc(3,2),aathc(1,2),aathc(2,2),aathc(3,2),
     *aatlt(1,2),aatlt(2,2),aatlt(3,2),aatxt(1,2),aatxt(2,2),aatxt(3,2),
     *aattc(2),aatvla(2),aatvlp(2),aatls(2)
      return
      end
c----------------------------------------------------------------
      subroutine ohcn
      use dflib
      include 'sandag.inc'
      include 'tcov.inc'
      fnami='hwycov/nat.adf'
      open(share='denynone',unit=natlun,convert='big_endian',
     *file=fnami,access=
     *'direct',form='unformatted',recl=20,status='old',err=98)
      natio=0
      go to 99
    2 call getarg(1,pname)
      namlen=len_trim(pname)
      namfl=len_trim(fnami)
      print *, namlen,namfl
      write(fnamo,20001) pname,fnami
      print *,fnamo
      write(0,20009) fnamo
20001 format(a<namlen>,'\',a<namfl>)
      open(share='denynone',unit=natlun,convert='big_endian',
     *file=fnamo,access=
     *'direct',form='unformatted',recl=20,status='old',err=98)
      natio=0
      go to 99
20009 format(a80)
   98 natio=1
   99 return
      end
c---------------------------------------------------------------
      subroutine onofwyhcn
      use dflib
      include 'sandag.inc'
      include 'tcov.inc'
      fnami='nofwycov/nat.adf'
c       open(share='denynone',unit=natlun,convert='big_endian',
c      *file=fnami,access=
c      *'direct',form='unformatted',recl=20,status='old',err=98)
c       natio=0
c       go to 99
    2 call getarg(1,pname)
      namlen=len_trim(pname)
      namfl=len_trim(fnami)
      print *, namlen,namfl
      write(fnamo,20001) pname,fnami
      print *,fnamo
      write(0,20009) fnamo
20001 format(a<namlen>,'\',a<namfl>)
      open(share='denynone',unit=natlun,convert='big_endian',
     *file=fnamo,access=
     *'direct',form='unformatted',recl=20,status='old',err=98)
      natio=0
      go to 99
20009 format(a80)
   98 natio=1
   99 return
      end
c---------------------------------------------------------------
      subroutine ohcatdz
	use dflib
      include 'sandag.inc'
      include 'tcov.inc'
      fnami='hwycovtdz/aat.adf'
      open(share='denynone',unit=aatlun,convert='big_endian',
     *file=fnami,access=
     *'direct',form='unformatted',recl=101,status='old',err=2)
      aatio=0
      go to 99
    2 call getarg(1,pname)
      namlen=len_trim(pname)
	namfl=len_trim(fnami)
	print *, namlen,namfl
      write(fnamo,20001) pname,fnami
	print *,fnamo
      write(0,20009) fnamo
20001 format(a<namlen>,'\',a<namfl>)
      open(share='denynone',unit=aatlun,convert='big_endian',
     *file=fnamo,access=
     *'direct',form='unformatted',recl=101,status='old',err=98)
      aatio=0
20009 format(a80)
      go to 99
   98 aatio=1
   99 return
      end
c------------------------------------------------------------------
      subroutine ohca
	use dflib
      include 'sandag.inc'
      include 'tcov.inc'
      fnami='hwycov/aat.adf'
      open(share='denynone',unit=aatlun,convert='big_endian',
     *file=fnami,access=
     *'direct',form='unformatted',recl=101,status='old',err=2)
      aatio=0
      go to 99
    2 call getarg(1,pname)
      namlen=len_trim(pname)
	namfl=len_trim(fnami)
	print *, namlen,namfl
      write(fnamo,20001) pname,fnami
	print *,fnamo
      write(0,20009) fnamo
20001 format(a<namlen>,'\',a<namfl>)
      open(share='denynone',unit=aatlun,convert='big_endian',
     *file=fnamo,access=
     *'direct',form='unformatted',recl=101,status='old',err=98)
      aatio=0
20009 format(a80)
      go to 99
   98 aatio=1
   99 return
      end
c-----------------------------------------------------------------
      subroutine ohca1
	use dflib
      include 'sandag.inc'
      include 'tcov.inc'
      fnami='hwycov1/aat.adf'
      open(share='denynone',unit=aatlun,convert='big_endian',
     *file=fnami,access=
     *'direct',form='unformatted',recl=101,status='old',err=2)
      aatio=0
      go to 99
    2 call getarg(1,pname)
      namlen=len_trim(pname)
	namfl=len_trim(fnami)
	print *, namlen,namfl
      write(fnamo,20001) pname,fnami
	print *,fnamo
      write(0,20009) fnamo
20001 format(a<namlen>,'\',a<namfl>)
      open(share='denynone',unit=aatlun,convert='big_endian',
     *file=fnamo,access=
     *'direct',form='unformatted',recl=101,status='old',err=98)
      aatio=0
20009 format(a80)
      go to 99
   98 aatio=1
   99 return
      end
c-----------------------------------------------------------------
       subroutine ohca2
	use dflib
      include 'sandag.inc'
      include 'tcov.inc'
      fnami='hwycov2/aat.adf'
      open(share='denynone',unit=aatlun,convert='big_endian',
     *file=fnami,access=
     *'direct',form='unformatted',recl=101,status='old',err=2)
      aatio=0
      go to 99
    2 call getarg(1,pname)
      namlen=len_trim(pname)
	namfl=len_trim(fnami)
	print *, namlen,namfl
      write(fnamo,20001) pname,fnami
	print *,fnamo
      write(0,20009) fnamo
20001 format(a<namlen>,'\',a<namfl>)
      open(share='denynone',unit=aatlun,convert='big_endian',
     *file=fnamo,access=
     *'direct',form='unformatted',recl=101,status='old',err=98)
      aatio=0
20009 format(a80)
      go to 99
   98 aatio=1
   99 return
      end
c-----------------------------------------------------------------
      subroutine rhcn
      include 'tcov.inc'
      read(natlun,rec=natrec,iostat=natio) aatlb,natlb,natid,
     *natxnm(1),natxnm(2),nattmp,natx,naty,natz,natiuc,
     *natsph,nattpn,natyr(1),natjur(1),natcnt(1)
      return
      end
c---------------------------------------------------------------
      subroutine rnofwyhcn
      include 'tcov.inc'
      read(natlun,rec=natrec,iostat=natio) aatlb,natlb,natid,
     *natxnm(1),natxnm(2),nattmp,natx,naty,natz,natiuc,
     *natsph,nattpn,natyr(1),natjur(1),natcnt(1)
      return
      end
c---------------------------------------------------------------
      subroutine whcn
      include 'tcov.inc'
      write(natlun,rec=natrec,iostat=natio) aatlb,natlb,natid,
     *natxnm(1),natxnm(2),nattmp,natx,naty,natz,natiuc,
     *natsph,nattpn,natyr(1),natjur(1),natcnt(1)
      return
      end
c----------------------------------------------------------------
      subroutine rhct
      include 'tcov.inc'
      read(tatlun,rec=tatrec,iostat=tatio) tatlb,tatlb1,tatlb2,
     *tatazi,tatang,tatid1,tatid2
      return
      end
c---------------------------------------------------------------
      subroutine whct
      include 'tcov.inc'
      write(tatlun,rec=tatrec,iostat=tatio) tatlb,tatlb1,tatlb2,
     *tatazi,tatang,tatid1,tatid2
      return
      end
c---------------------------------------------------------------
      subroutine ohctus
      include 'sandag.inc'
      include 'tcov.inc'
      fnamo='hwycov/turns.sec'
      open(share='denynone',unit=seclun,convert='big_endian',
     *file=fnamo,access=
     *'direct',form='unformatted',recl=8,status='old',err=98)
      secio=0
      go to 99
   98 secio=1
   99 return
      end
c--------------------------------------------------------------
      subroutine rhctus
      include 'tcov.inc'
      read(seclun,rec=secrec,iostat=secio)
     *rlink,alink,secmes(2),secmes(1),secpos(2),secpos(1),seclb,secid
      return
      end
c-------------------------------------------------------------
      subroutine whctus
      include 'tcov.inc'
      write(seclun,rec=secrec,iostat=secio)
     *rlink,alink,secmes(2),secmes(1),secpos(2),secpos(1),seclb,secid
      return
      end
c--------------------------------------------------------------
      subroutine otrca
      use dflib
      include 'sandag.inc'
      include 'tcov.inc'
      fnami='trcov/aat.adf'
      open(share='denynone',unit=aatlun,convert='big_endian',
     *file=fnami,access=
     *'direct',form='unformatted',recl=66,status='old',err=2)
      aatio=0
      go to 99
    2 call getarg(1,pname)
      namlen=len_trim(pname)
	namfl=len_trim(fnami)
	print *, namlen,namfl
      write(fnamo,20001) pname,fnami
	print *,fnamo
      write(0,20009) fnamo
20001 format(a<namlen>,'\',a<namfl>)
      open(share='denynone',unit=aatlun,convert='big_endian',
     *file=fnamo,access=
     *'direct',form='unformatted',recl=66,status='old',err=98)
      aatio=0
      go to 99
20009 format(a80)
   98 aatio=1
   99 return
      end
c-------------------------------------------------------------
      subroutine rtrca
      include 'tcov.inc'
      read(aatlun,rec=aatrec,iostat=aatio) aatarn(2),aatarn(1),
     *lpoly,rpoly,aatlen,aatlb,aatid,aattvl(1),aattvl(2),
     *aatx(2),aaty(2),aatx(1),aaty(1),aattmp,aatsph,aatrt,
     *aatlk,aatnm,aatxnm(2),aatxnm(1),aattpn(2),aattpn(1),
     *aatdir,aatosp,aatyr(1),aatjur(1),aatfc(1),aathov(1),
     *aatspd(1),aatway(1),aatmed(1),aatfwylen,
     *aatln(1,1,1),aatln(2,1,1),aatln(3,1,1),aataux(1,1),
     *aatcnt(1,1),aattl(1,1),aatrl(1,1),aatll(1,1),
     *aattm(1,1),aattm(2,1),aattm(3,1),aatstp(1),
     *aatln(1,2,1),aatln(2,2,1),aatln(3,2,1),aataux(2,1),
     *aatcnt(2,1),aattl(2,1),aatrl(2,1),aatll(2,1),
     *aattm(1,2),aattm(2,2),aattm(3,2),aatstp(1),aattrt,
     *aatmm,aatlrz,aatcrz
      return
      end
c-------------------------------------------------------------
      subroutine wtrca
      include 'tcov.inc'
      write(aatlun,rec=aatrec,iostat=aatio) aatarn(2),aatarn(1),
     *lpoly,rpoly,aatlen,aatlb,aatid,aattvl(1),aattvl(2),
     *aatx(2),aaty(2),aatx(1),aaty(1),aattmp,aatsph,aatrt,
     *aatlk,aatnm,aatxnm(2),aatxnm(1),aattpn(2),aattpn(1),
     *aatdir,aatosp,aatyr(1),aatjur(1),aatfc(1),aathov(1),
     *aatspd(1),aatway(1),aatmed(1),aatfwylen,
     *aatln(1,1,1),aatln(2,1,1),aatln(3,1,1),aataux(1,1),
     *aatcnt(1,1),aattl(1,1),aatrl(1,1),aatll(1,1),
     *aattm(1,1),aattm(2,1),aattm(3,1),aatstp(1),
     *aatln(1,2,1),aatln(2,2,1),aatln(3,2,1),aataux(2,1),
     *aatcnt(2,1),aattl(2,1),aatrl(2,1),aatll(2,1),
     *aattm(1,2),aattm(2,2),aattm(3,2),aatstp(1),aattrt,
     *aatmm,aatlrz,aatcrz
      return
      end
c------------------------------------------------------------
c subroutine to be called in transCAD
c since there is no way to get the directory path string
c getarg() is the way to record the string of directory path and 
c write out to fnamo with the file name with the full path 
      subroutine otrcn
      use dflib
	include 'sandag.inc'
      include 'tcov.inc'
      integer namlen, namfl 
      fnami='trcov/nat.adf'
      open(share='denynone',unit=natlun,convert='big_endian',
     *file=fnami,access=
     *'direct',form='unformatted',recl=29,status='old',err=2)
      natio=0
      print *,fnami
      go to 99
    2 call getarg(1,pname)
      namlen=len_trim(pname)
	namfl=len_trim(fnami)
	print *, namlen,namfl
      write(fnamo,20001) pname,fnami
	print *,fnamo
      write(0,20009) fnamo
20001 format(a<namlen>,'\',a<namfl>)
      open(share='denynone',unit=natlun,convert='big_endian',
     *file=fnamo,access=
     *'direct',form='unformatted',recl=29,status='old',err=98)
      natio=0
20009 format(a80)
      go to 99
   98 natio=1
   99 return
      end
c------------------------------------------------------------

      subroutine rtrcn
      include 'tcov.inc'
      read(natlun,rec=natrec,iostat=natio) aatlb,natlb,natid,
     *natxnm(1),natxnm(2),nattmp,natx,naty,natz,natiuc,
     *natsph,nattpn,natyr(1),natjur(1),natcnt(1),nattap,
     *nattrn,natfz4,natfz6,natfz7,
     *natstp(1),natprk(1),nattt(1),
     *natstp(2),natprk(2),natvol
      return
      end
c-----------------------------------------------------------
      subroutine wtrcn
      include 'tcov.inc'
      write(natlun,rec=natrec,iostat=natio) aatlb,natlb,natid,
     *natxnm(1),natxnm(2),nattmp,natx,naty,natz,natiuc,
     *natsph,nattpn,natyr(1),natjur(1),natcnt(1),nattap,
     *nattrn,natfz4,natfz6,natfz7,
     *natstp(1),natprk(1),nattt(1),
     *natstp(2),natprk(2),natvol
      return
      end
c-----------------------------------------------------------
c-----------------------------------------------------------
      subroutine otrcs

      use dflib
      include 'sandag.inc'
      include 'tcov.inc'
      integer namlen, namfl
      fnami='trcov/transit.sec'
      open(share='denynone',unit=seclun,convert='big_endian',
     *file=fnami,access=
     *'direct',form='unformatted',recl=28,status='old',err=2)
      secio=0
      print *,fnami
      go to 99
    2 call getarg(1,pname)
      namlen=len_trim(pname)
	namfl=len_trim(fnami)
	print *, namlen,namfl
      write(fnamo,20001) pname,fnami
      write(0,20009) fnamo
20001 format(a<namlen>,'\',a<namfl>)
      open(share='denynone',unit=seclun,convert='big_endian',
     *file=fnamo,access=
     *'direct',form='unformatted',recl=28,status='old',err=98)
	secio=0
20009 format(a80)
      go to 99
   98 secio=1
   99 return
      end
c------------------------------------------------------------
      subroutine rtrcs
      include 'tcov.inc'
      read(seclun,rec=secrec,iostat=secio)
     *rlink,alink,secmes(2),secmes(1),secpos(2),secpos(1),
     *seclb,secid,sectm2,sectm4,
     *secarn(2),secarn(1),sectpn(2),sectpn(1),
     *secstp(2),secstp(1),secnm,secxnm(2),secxnm(1),seci2
      return
      end
c-----------------------------------------------------------
      subroutine wtrcs
      include 'tcov.inc'
      write(seclun,rec=secrec,iostat=secio)
     *rlink,alink,secmes(2),secmes(1),secpos(2),secpos(1),
     *seclb,secid,sectm2,sectm4,
     *secarn(2),secarn(1),sectpn(2),sectpn(1),
     *secstp(2),secstp(1),secnm,secxnm(2),secxnm(1),seci2
      return
      end
c--------------------------------------------------------
c----------------------------------------------------------
      subroutine otrcr
      use dflib
	include 'sandag.inc'
      include 'tcov.inc'
      integer namlen, namfl
      fnami='trcov/transit.rat'
      open(share='denynone',unit=ratlun,convert='big_endian',
     *file=fnami,access=
     *'direct',form='unformatted',recl=23,status='old',err=2)
      print *,fnami 
      ratio=0
      go to 99
    2 call getarg(1,pname)
      namlen=len_trim(pname)
	namfl=len_trim(fnami)
	print *, namlen,namfl
      write(fnamo,20001) pname,fnami
      print *, fnamo
20001 format(a<namlen>,'\',a<namfl>)
      open(share='denynone',unit=ratlun,convert='big_endian',
     *file=fnamo,access=
     *'direct',form='unformatted',recl=23,status='old',err=98)
      ratio=0
20009 format(a80)
      go to 99
   98 ratio=1
   99 return
      end
c--------------------------------------------------------
      subroutine rtrcr
      include 'tcov.inc'
      read(ratlun,rec=ratrec,iostat=ratio) ratlb,ratid,
     *ratco,ratmod,ratlin,rathwy(1),rathwy(2),rathwy(3),
     *rathwy(4),ratnhr,ratorg,ratdes,ratvia
      return
      end
c----------------------------------------------------------
      subroutine wtrcr
      include 'tcov.inc'
      write(ratlun,rec=ratrec,iostat=ratio) ratlb,ratid,
     *ratco,ratmod,ratlin,rathwy(1),rathwy(2),rathwy(3),
     *rathwy(4),ratnhr,ratorg,ratdes,ratvia
      return
      end
c-----------------------------------------------------------
c-----------------------------------------------------------
      subroutine owba
	use dflib  
      include 'sandag.inc'   
      include 'tcov.inc'   
	fnami='walkbar/arc.adf'
c     if(iyear.gt.2000) fnami='walkbarfy/arc.adf'
      open(share='denynone',unit=aatlun,convert='big_endian',
     *file=fnami,access=
     *'direct',form='unformatted',recl=1,status='old',err=2)
      print *,fnami
      go to 9
    2 fnami='../covs/walkbar/arc.adf' 
      open(share='denynone',unit=aatlun,convert='big_endian',
     *file=fnami,access=
     *'direct',form='unformatted',recl=1,status='old',err=3)
      print *,fnami
    9 aatio=0
      go to 99
    3 fnami='walkbar/arc.adf'
      call getarg(1,pname)
      namlen=len_trim(pname)
	namfl=len_trim(fnami)
	print *, namlen,namfl
      write(fnamo,20001) pname,fnami
	print *,fnamo
      write(0,20009) fnamo
20001 format(a<namlen>,'\',a<namfl>)
      open(share='denynone',unit=aatlun,convert='big_endian',
     *file=fnamo,access=
     *'direct',form='unformatted',recl=1,status='old',err=4)     
      aatio=0
      go to 99
20009 format(a80)
    4 fnami='walkbar/arc.adf'
      call getarg(2,sname)
      namlen=len_trim(sname)
      write(fnamo,20002) sname,fnami
20002 format(a<namlen>,'\covs\',a<namfl>)	
      open(share='denynone',unit=aatlun,convert='big_endian',
     *file=fnamo,access=
     *'direct',form='unformatted',recl=1,status='old',err=98) 
      write(0,20009) fnamo	
      aatio=0
      go to 99
   98 aatio=1
   99 return
      end
c---------------------------------------------------------------------------
c---------------------------------------------------------------------------
c	parse property file
c	Wu Sun (March 27, 2008) wsu@sandag.org
c
c	punit: proptery file unit number
c	propertyName: given property name (string)
c	propertyValue: returned property value, return 'null' string if property 
c     not found, or sandag.properties doesn't exist
c---------------------------------------------------------------------------
      subroutine getProperty(punit,propertyName, propertyValue)
      use dflib
      include 'sandag.inc'
c     declare global variables
      integer punit,temp1,temp2
c     important****: the argument must be delcared as flexible length use *
      character (len=*)::propertyName
      character (len=80):: propertyValue
c     declare local variables
      character (len=80):: prop,propname,proppath
      integer position,proplen

      propertyValue='null'

c     open property info file
      fnami='properties.info'
      facsmode='sequential'
      call openoutput(lunerr,facsmode)

      proppath='sandag.properties'
      open(unit=punit,file=proppath,status='old',iostat=istat)

c     search sandag.properties from various locations
      if(istat==0) then
		write(lunerr,101) 'DOS environment,found sandag.properties' 
		goto 5
      else
		proppath='..\data\sandag.properties'
		open(unit=punit,file=proppath,status='old',iostat=istat)
		if(istat==0) then
			write(lunerr,101) 'DOS environment,
     *found sandag.properties in data dir'
			goto 5
		else
			write(lunerr,101) 'could not find sandag.properties in DOS 
     *environment!'
		endif
	endif

c	start looking in TransCAD environment
	write(lunerr,101) 'Now searching TransCAD environment!'
      call getarg(1,pname)
	call getarg(2,sname)

	if(len_trim(pname)==0.OR.len_trim(sname)==0) then
		write(lunerr,101) 'Running in DOS environment, failed finding 
     *sandag.properties!'
		write(lunerr,101) 'Check your sandag.properties location!'
	    call exit(-1)
	else
		proppath=trim(pname)//'\sandag.properties'
		open(unit=punit,file=proppath,status='old',iostat=istat)
		if(istat==0) then
			write(lunerr,101) 'TransCAD environment,  
     *sandag.properties found in alt dir'
			goto 5
		else
			proppath=trim(sname)//'\data\sandag.properties'
			open(unit=punit,file=proppath,status='old',
     *iostat=istat)
			if(istat==0) then
				write(lunerr,101) 'TransCAD environment, 
     *sandag.properties found in data dir'
				goto 5
			else
				write(lunerr,101) 'Running in TransCAD environment, 
     * failed finding sandag.properties!'
				write(lunerr,101) 'Check sandag.properties location!'	
				call exit(-2)
			endif
		endif
      endif

      write(lunerr,101) proppath

c     if sandag.properties found, read records until end of file
    5 read(punit,*,end=100) prop
      if(index(prop,"#")==1) then
      go to 5
      else
      position=index(prop,"=")
      proplen=len(trim(prop))
      propname=prop(1:position-1)
      temp1=index(propname,propertyName)
      temp2=index(propertyName,trim(propname))
      if(temp1==1.AND.temp2==1) then
      propertyValue=prop(position+1:proplen)
      go to 100
      else  
      go to 5
      endif
      endif
101   format(a80)
100   close(punit)
      close(lunerr)
c     print*, 'propertyName=',propertyName,'propertyVal=',propertyValue
      return
      end

c---------------------------------------------------------------------------
c	Given a separator, parse string to array
c	Wu Sun (May 5, 2008) wsu@sandag.org
c	str: string to be parsed
c     separator: separator, e.g. "|", can't be "," or " "
c	parsedStr: parsed string array
c	count: number of elements in array
c---------------------------------------------------------------------------
	subroutine parseStr(str,separator,parsedStr,count)
c	important****: the argument must be delcared as flexible length use *
	character (len=*)::str
	character (len=*)::separator
	character*80 parsedStr(40)
	character*200 workstr
	integer*4 position,i,strlen,count

	strlen=len(str)
	i=1
	workstr=str
c	get all elements except last one
	do while (index(workstr,separator)>0)
		position=index(workstr,separator)
		parsedStr(i)=workstr(1:position-1)
		workstr=workstr(position+1:strlen)
		i=i+1
	end do
c     get last element
	parsedStr(i)=trim(workstr)

	count=i
	return
	end

c---------------------------------------------------------------------------
c	Convertion between string and integer and real
c	Wu Sun (May 2, 2008) wsu@sandag.org
c	Following 4 subrountines convert between integer, real and string
c---------------------------------------------------------------------------
      subroutine strToInt(str, intVal)
c	important****: the argument must be delcared as flexible length use *
	character (len=*)::str
	integer intValue
      read (str,'(I20)') intVal
	return
	end

      subroutine strToReal(str, realVal)
c	important****: the argument must be delcared as flexible length use *
	character (len=*)::str
	real*8 realVal
      read (str,'(f20.3)') realVal
	return
	end

	subroutine intToStr(intVal, str)
c	important****: the argument must be delcared as flexible length use *
	character (len=*)::str
	integer intVal
      write(str,'(I20)') intVal
	str=trim(adjustl(str))
	return
	end

	subroutine realToStr(realVal, str)
c	important****: the argument must be delcared as flexible length use *
	character (len=*)::str
	real*8 realVal
      write(str,'(f20.3)') realVal
	return
	end

c-------------------------------------------------------------------------------------------------
c	Cross reference using a lookup table
c	Wu Sun (July 2, 2008) wsu@sandag.org
c	Arguments:
c	tableName: lookkup table file (txt file) with full path
c	punit:	   read unit for lookup table
c	inval:	   input value
c	outval:	   cross reference found for inval
c	incol:	   column position in lookup table for inval
c	outcol:	   column position in lookup table for outval
c	separator: column separator in lookup table file, currently not working for ","," " and tab.
c	status:	   lookup status, 0:found correspondance, -1:correspondance not found
c-------------------------------------------------------------------------------------------------
      subroutine lookup(tableName,punit,inval,outval,incol,outcol,
     *separator,status)
	use dflib
      include 'sandag.inc'
c     declare global variables
	integer punit,inval,outval,incol,outcol,currentval,count,status

c	important****: the argument must be delcared as flexible length use *
	character (len=*)::tableName,separator
	character *80 parsedStr(40),lookuprec,invalStr

c	initialize status to -1 (failure status)
	status=-1
c	open error file
c      fnami='sandagsub.err'
c	facsmode='sequential'
c	call openoutput(lunerr,facsmode)

c	open lookup table
	open(share='denynone',unit=punit,file=tableName,status='old',
     *iostat=istat)

c	read records until end of file
	if(istat==0) then
		!read titile line
	    read(punit,*,end=100) lookuprec
		!start reading content lines
    5		read(punit,*,end=100) lookuprec
		call parseStr(lookuprec,separator,parsedStr,count)
		call strToInt(parsedStr(incol),currentval)
		if(inval==currentval) then
			call strToInt(parsedStr(outcol),outval)
			!update status to 0 (success status)
			status=0
			go to 100
		else
			go to 5
		endif
	else
c		write(lunerr,101) 'Failed openning '//tableName
	endif
100	close(punit)
c	close(lunerr)
c	write to error file if no correspondance found for input val
c	if(status==-1) then
c		call intToStr(inval,invalStr)
c		write(lunerr,101) 'failed looking up for '//invalStr
c	endif
101   format(a80)
	return
	end
c-------------------------------------------------------------------------   
c   Ziying Ouyang (Jan 2010) zou@sandag.org
c   given year of cost data 
c   given base cost year (baseCostYear) from 'sandag.properties' file
c   return the CPI adjustment factor
c   arguments: 
c   yrCost: year of the cost
c   cpiFctor: CPI adjustment factor
c-------------------------------------------------------------------------
      subroutine getCPIFctor(yrCost,cpiFctor)
      use dflib
      include 'sandag.inc'
      integer*4 iyr,baseYear,yrCost
      real*8 cpi,cpilast,cpiBase, cpiCurrt,cpiFctor
      character*80 chead,bCostYr
      data lunerr2, lunin/1,2/
	   
      fnami='properties.info'
      facsmode='sequential'
      call openoutput(lunerr2,facsmode)
      call getProperty(9998,'baseCostYear',bCostYr)
      call strToInt(bCostYr,baseYear)
      cpi=0.0
      fnami='cpi.csv'
      call opendata(lunin)
      if(istat.ne.0) then
        write(lunerr2,10001)'cpi.csv file not found'
        close(lunerr2)
        go to 300
      end if
      read(lunin,10001) chead
  298 read(lunin,10002,iostat=istat) iyr,cpi
      if(istat.ne.0) go to 299  
      if (baseYear.eq.iyr)cpiBase=cpi
c year of the cost matches with the CPI year   
      if (yrCost.eq.iyr)cpiCurrt=cpi 
	cpilast=cpi       
      go to 298  
  299 close(lunin)
C    for future years, take the most current year CPI
      if (cpiCurrt.eq.0)cpiCurrt=cpilast
      if (cpiBase.gt.0.and.cpiCurrt.gt.0) then
       cpiFctor=cpiBase/cpiCurrt
      else
       cpiFctor=1.0             
      endif
  300 return
10001 format(a80)
10002 format(i5,f6.2)
      end	
