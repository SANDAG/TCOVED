      integer*4 mxzn,mxext,aprtzn,mxpu,mxput
      integer*4 mxmgra,mxsra,mxadty,mxfc,mxtca,mxtcn,mxid,mxpsra
      integer*4 mxtpn,mxhcn,mxhca,mxtrcn,mxtrca,mxtap,mxtrn,mexzn
      integer*4 mxbsta,mxsta,mxzn9,mxcj,mxcsta,mxnode,mxzn10,mxzn12
      parameter (mxzn=4996,mxext=12,aprtzn=3152,mxpu=10,mxput=11,
     *mxmgra=23014,mxsra=63,mxadty=24,mxfc=14,mxtca=60000,mxagg=2000,
     *mxtcn=35000,mxid=60000,mxtpn=35000,mxhcn=35000,mxhca=60000,
     *mxtrcn=20000,mxtrca=30000,mxtap=2500,mxtrn=20000,mexzn=4246,
     *mxnode=32001,mxzn10=4605,mxzn12=4682,
     *mxbsta=5999,mxsta=1999,mxzn9=4252,mxcj=21,mxcsta=219999,mxpsra=81,
     *mxmoda=10,mxmodt=7,mxstop=10,
     *i1aprt=3522,i3aprt=3511,i2aprt=3494,m2aprt=21625,
     *i1rcs=3085,i2rcs=652,i3rcs=657,i4rcs=1354,
     *i5rcs=1353,i6rcs=1827,i7rcs=2133,i8rcs=2141,i9rcs=2698,
     *i10rcs=2998,i11rcs=2998,i12rcs=3452,i13rcs=3798,i14rcs=4208,
     *i15rcs=4339,
     *i132nd=4054,i232nd=4067,i332nd=4075,i432nd=4123,
     *i532nd=4075,i632nd=4067,i1cc=3789,
     *i1ucsd=2084,i2ucsd=2097,
     *i3ucsd=2085,i4ucsd=2116,i5ucsd=2123,i6ucsd=2109,
     *mxtdz=2000,mxtrid=71000,mxcon=20)
      common /sddt/ cdate,ctime
      common /sdnm/ fnami,fnamo,pname,sname,ename
      common /sdhd/ cheadi,cheado
      common /stat/ istat
      logical errf
      integer*2 io(500),origin,table,dummy,idata(80)
      integer*4 recnum,istat
      character*1 eject,cheadi(80),cheado(80),adata(80)
      character*3 tpstat
      character*8 ctime,cdate
      character*80 phead,fnami,fnamo,pname,sname,facsmode,ename
