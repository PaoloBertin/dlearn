subroutine qdct5(ishist,sout  ,mat   ,ien   ,d     ,dl    ,                                                                        &
                 x     ,xl    ,det   ,shl   ,shg   ,r     ,                                                                        &
                 shgbar,w     ,b     ,strain,c     ,stress,                                                                        &
                 pstrn ,pstrs ,work  ,                                                                                             &
                 nsout ,nen   ,ndof  ,ned   ,nsd   ,nesd  ,                                                                        &
                 nrowsh,nint  ,neg   ,iopt  ,ibbar ,nrowb ,                                                                        &
                 nee   ,nstr  ,nsout1)
!
!.... program to calculate and store element time-histories for the
!        four-node quadrilateral, elastic continuum element
!
     implicit double precision (a-h,o-z)
!
!.... deactivate above card(s) for single-precision operation
!
     real sout
     logical lquad
     dimension ishist(3,1),sout(nsout1,1),mat(1),ien(nen,1),d(ndof,1),                                                             &
               dl(ned,1),x(nsd,1),xl(nesd,1),det(1),shl(nrowsh,nen,1),                                                             &
               shg(nrowsh,nen,1),r(1),shgbar(3,nen,1),w(1),b(nrowb,1),                                                             &
               strain(1),c(nrowb,nrowb,1),stress(1),pstrn(1),pstrs(1),                                                             &
               work(1)
     common /hplotc/ nplpts,locplt,time

     sout(1,locplt) = real(time)

     do 300 i=1,nsout

        nel   = ishist(1,i)
        intpt = ishist(2,i)
        ncomp = ishist(3,i)
    
        m = mat(nel)
        call local(ien(1,nel),d,dl,nen,ndof,ned)
        call local(ien(1,nel),x,xl,nen,nsd,nesd)
        lquad = .true.
        if (ien(3,nel).eq.ien(4,nel)) lquad = .false.
        call qdcshg(xl,det,shl,shg,nint,nel,neg,lquad)
    
        if (iopt.eq.2) then
    
            do 100 l=1,nint
                r(l) = rowdot(shg(nrowsh,1,l),xl,nrowsh,nesd,nen)
                det(l) = det(l)*r(l)
100         continue
    
        endif
    
        if (ibbar.eq.1) call meansh(shgbar,w,det,r,shg,nen,nint,iopt,nesd,nrowsh)
    
        ! calculate stress, strain and principal values
        call qdcstr(shg(1,1,intpt),shgbar,b,r(intpt),dl,strain,c(1,1,m),                                                           &
                    stress,pstrn,pstrs,nrowsh,nesd,nrowb,ibbar,nen,ned,                                                            &
                    nee,nstr,iopt)
    
        do 200 j=1,4
            work(j     ) = stress(j)
            work(j +  4) = pstrs(j)
            work(j +  8) = strain(j)
            work(j + 12) = pstrn(j)
    200 continue
    
        sout(i+1,locplt) = real(work(ncomp))
    
 300 continue

     return
     end
