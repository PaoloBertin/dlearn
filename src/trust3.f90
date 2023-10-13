subroutine trust3(mat   ,ien   ,dpred ,dl    ,vpred ,vl    ,                                                                       &
                  a     ,al    ,rdampk,rdampm,rho   ,grav  ,                                                                       &
                  elresf,x     ,xl    ,det   ,shl   ,shg   ,                                                                       &
                  g1    ,work  ,area  ,w     ,eleffm,b     ,                                                                       &
                  strain,c     ,dmat  ,stress,brhs  ,lm    ,                                                                       &
                  xs    ,                                                                                                          &
                  numel ,ned   ,nen   ,ndof  ,ldyn  ,nee   ,                                                                       &
                  imass ,nesd  ,lfbody,nsd   ,nint  ,nrowsh,                                                                       &
                  neg   ,nrowb )
    !
    !.... program to calculate residual-force vector for the
    !        three-dimensional, elastic truss element and
    !        assemble into the global right-hand-side vector
    !
    implicit double precision (a-h,o-z)
    !.... deactivate above card(s) for single-precision operation
    
    logical ldyn,formma,formkd,zeroal,zerodl,zerog,lnode3
    dimension mat(1),ien(nen,1),dpred(ndof,1),dl(ned,1),vpred(ndof,1),                                                             &
              vl(ned,1),a(ndof,1),al(ned,1),rdampk(1),rdampm(1),                                                                   &
              rho(1),grav(1),elresf(1),x(nsd,1),xl(nesd,1),det(1),                                                                 &
              shl(nrowsh,nen,1),shg(nrowsh,nen,1),g1(1),work(1),                                                                   &
              area(1),w(1),eleffm(nee,1),b(nrowb,1),strain(1),                                                                     &
              c(nrowb,nrowb,1),dmat(nrowb,1),stress(1),brhs(1),                                                                    &
              lm(ned,nen,1),xs(nesd,1)
    common /consts/ zero,pt1667,pt25,pt5,one,two,three,four,five
    
    do 500 nel=1,numel
        
        formma = .false.
        formkd = .false.
        m = mat(nel)
        
        !.... note: for static analysis mpdprd = mpd, hence reference
        !           to array "dpred" will access the contents of array "d".
        call local(ien(1,nel),dpred,dl,nen,ndof,ned)
        if (ldyn) then
            
            call local(ien(1,nel),vpred,vl,nen,ndof,ned)
            call local(ien(1,nel),a,al,nen,ndof,ned)
            
            do 200 j=1,nen
                
                do 100 i=1,ned
                    dl(i,j) = dl(i,j) + rdampk(m)*vl(i,j)
                    al(i,j) = al(i,j) + rdampm(m)*vl(i,j)
100             continue
                
200         continue
            
            call ztest(al,nee,zeroal)
            if ( (.not.zeroal) .and. (imass.ne.2) .and. (rho(m).ne.zero) ) formma = .true.            
        else            
            call clear(al,nee)            
        endif
        
        call ztest(dl,nee,zerodl)
        if (.not.zerodl) formkd = .true.
        call ztest(grav,nesd,zerog)
        
        if ((.not.zerog) .and. (lfbody.ne.0) .and. (rho(m).ne.zero) .and. (imass.ne.2)) then
        formma = .true.
        do 400 i=1,ned
            work(i) = grav(i)*g1(lfbody)
            
            do 300 j=1,nen
                al(i,j) = al(i,j) - work(i)
300         continue
400     continue
        
    endif
    
    if (formma.or.formkd) then
        
        call clear(elresf,nee)
        call local(ien(1,nel),x,xl,nen,nsd,nesd)
        lnode3 = .true.
        if (nen.eq.3 .and. ien(2,nel).eq.ien(3,nel)) lnode3 = .false.
        call trushg(xl,det,shl,shg,xs,nen,nint,nel,neg,lnode3)
        
        if (formma) then
            
            !.......... form inertial and/or body force
            constm = - area(m)*rho(m)
            call contma(shg,xl,w,det,al,eleffm,work,elresf,constm,imass,                                                           &
                        nint,nrowsh,nesd,nen,ned,nee)
        endif
        
        if (formkd) then
            
            ! form internal force
            constk = - area(m)
            call truskd(w,det,shg,xs,xl,b,dl,strain,c(1,1,m),dmat,                                                                 &
                        stress,work,elresf,constk,nen,nint,nrowsh,                                                                 &
                        nesd,nrowb,nee)
        endif
        
        call addrhs(brhs,elresf,lm(1,1,nel),nee)
        
    endif
    
500 continue

return

end subroutine trust3
