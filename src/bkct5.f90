subroutine bkct5(ishist,sout  ,mat   ,ien   ,d     ,dl    ,                                                                        &
                 x     ,xl    ,det   ,shl   ,shg   ,                                                                               &
                 shgbar,w     ,b     ,strain,c     ,stress,                                                                        &
                 pstrn ,pstrs ,work  ,                                                                                             &
                 nsout ,nen   ,ndof  ,ned   ,nsd   ,nesd  ,                                                                        &
                 nrowsh,nint  ,neg   ,ibbar ,nrowb ,                                                                               &
                 nee   ,nstr  ,nsout1)
    !.... subroutine to calculate and store element time-histories for the
    !     eight-node brick, elastic element
    !
    implicit double precision (a-h,o-z)
    
    ! deactivate above card(s) for single-precision operation
    real sout
    dimension ishist(3,1),sout(nsout1,1),mat(1),ien(nen,1),d(ndof,1),                                                              &
              dl(ned,1),x(nsd,1),xl(nesd,1),det(1),shl(nrowsh,nen,1),                                                              &
              shg(nrowsh,nen,1),shgbar(nrowsh,nen,1),w(1),b(nrowb,1),                                                              &
              strain(1),c(nrowb,nrowb,1),stress(1),pstrn(1),pstrs(1),                                                              &
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
        call bkcshg(xl,det,shl,shg,nint,nel,neg)

        if (ibbar.eq.1) call meansh(shgbar,w,det,r,shg,nen,nint,0,nesd,nrowsh)

        ! calculate stress, strain and principal values
        call bkcstr(shg(1,1,intpt),shgbar,b,dl,strain,c(1,1,m),                                                                    &
                        stress,pstrn,pstrs,nrowsh,nesd,nrowb,ibbar,nen,ned,                                                        &
                        nee,nstr)

        do 200 j=1,6
            work(j     ) = stress(j)
            work(j +  6) = pstrs(j)
            work(j + 12) = strain(j)
            work(j + 18) = pstrn(j)
200     continue

        sout(i+1,locplt) = real(work(ncomp))

300 continue

    return
    
end subroutine bkct5

