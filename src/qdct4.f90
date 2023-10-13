subroutine qdct4(mat   ,ien   ,d     ,dl    ,x     ,xl    ,                                                                        &
                 det   ,shl   ,shg   ,xint  ,r     ,shgbar,                                                                        &
                 w     ,b     ,strain,c     ,stress,pstrn ,                                                                        &
                 pstrs ,                                                                                                           &
                 nint  ,numel ,nen   ,ndof  ,ned   ,nsd   ,                                                                        &
                 nesd  ,nrowsh,neg   ,iopt  ,ibbar ,nrowb ,                                                                        &
                 nee   ,nstr  )
    !
    !.... program to calculate and print stress, strain and
    !        principal values for the four-node quadrilateral,
    !        elastic continuum element
    
    implicit double precision (a-h,o-z)
    
    !.... deactivate above card(s) for single-precision operation
    logical lquad
    dimension mat(1),ien(nen,1),d(ndof,1),dl(ned,1),x(nsd,1),                                                                      &
              xl(nesd,1),det(1),shl(nrowsh,nen,1),shg(nrowsh,nen,1),                                                               &
              xint(nesd,1),r(1),shgbar(3,1),w(1),b(nrowb,1),strain(1),                                                             &
              c(nrowb,nrowb,1),stress(1),pstrn(1),pstrs(1)
    common /consts/ zero,pt1667,pt25,pt5,one,two,three,four,five
    
    nntot = 16
    nn = 0
    
    do 300 nel=1,numel
        
        m = mat(nel)
        call local(ien(1,nel),d,dl,nen,ndof,ned)
        call local(ien(1,nel),x,xl,nen,nsd,nesd)
        lquad = .true.
        if (ien(3,nel).eq.ien(4,nel)) lquad = .false.
        call qdcshg(xl,det,shl,shg,nint,nel,neg,lquad)
        
        !.... calculate coordinates of integration points
        do 100 l=1,nint
            xint(1,l) = rowdot(shg(nrowsh,1,l),xl(1,1),nrowsh,nesd,nen)
            xint(2,l) = rowdot(shg(nrowsh,1,l),xl(2,1),nrowsh,nesd,nen)
            if (iopt.eq.2) then
                r(l) = xint(1,l)
                det(l) = det(l)*r(l)
            endif
100     continue
        
        if (ibbar.eq.1) call meansh(shgbar,w,det,r,shg,nen,nint,iopt,nesd,nrowsh)
        
        ! loop over integration points
        do 200 l=1,nint

            !.... calculate stress, strain and principal values
            call qdcstr(shg(1,1,l),shgbar,b,r(l),dl,strain,c(1,1,m),stress,                                                        &
                        pstrn,pstrs,nrowsh,nesd,nrowb,ibbar,nen,ned,nee,nstr,iopt)
            
            ! print stress, strain and principal values
            call prts2d(xint(1,l),stress,pstrs,strain,pstrn,nn,nntot,neg,nel,l)
200     continue
        
300 continue
    
    return
end subroutine qdct4
