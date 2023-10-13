subroutine qdct3(mat   ,ien   ,dpred ,dl    ,vpred ,vl    ,                                                                        &
                 a     ,al    ,rdampk,rdampm,rho   ,grav  ,                                                                        &
                 elresf,x     ,xl    ,det   ,shl   ,shg   ,                                                                        &
                 r     ,g1    ,work  ,th    ,w     ,eleffm,                                                                        &
                 shgbar,b     ,strain,c     ,stress,brhs  ,                                                                        &
                 lm    ,ielno ,iside ,press ,shear ,                                                                               &
                 numel ,ned   ,nen   ,ndof  ,ldyn  ,nee   ,                                                                        &
                 imass ,nesd  ,lfbody,nsd   ,nint  ,nrowsh,                                                                        &
                 neg   ,iopt  ,nrowb ,nstr  ,ibbar , nsurf,                                                                        &
                 lfsurf)
    !
    !.... program to calculate residual-force vector for the
    !     four-node quadrilateral, elastic continuum element and
    !     assemble into the global right-hand-side vector
    
    implicit double precision (a-h,o-z)
    
    !.... deactivate above card(s) for single-precision operation
    logical ldyn,formma,formkd,zeroal,zerodl,zerog,lquad
    dimension mat(1),ien(nen,1),dpred(ndof,1),dl(ned,1),vpred(ndof,1),                                                             &
              vl(ned,1),a(ndof,1),al(ned,1),rdampk(1),rdampm(1),                                                                   &
              rho(1),grav(1),elresf(1),x(nsd,1),xl(nesd,1),det(1),                                                                 &
              shl(nrowsh,nen,1),shg(nrowsh,nen,1),r(1),g1(1),work(1),                                                              &
              th(1),w(1),eleffm(nee,1),shgbar(3,1),b(nrowb,1),                                                                     &
              strain(1),c(nrowb,nrowb,1),stress(1),brhs(1),                                                                        &
              lm(ned,nen,1),ielno(1),iside(1),press(2,1),shear(2,1)
    common /consts/ zero,pt1667,pt25,pt5,one,two,three,four,five
    
    do 600 nel=1,numel
        
        formma = .false.
        formkd = .false.
        m = mat(nel)
        
        !.... note: for static analysis mpdprd = mpd, hence reference to
        !           array "dpred" will access the contents of array "d".
        call local(ien(1,nel),dpred,dl,nen,ndof,ned)
        if (ldyn) then
            
            call local(ien(1,nel),vpred,vl,nen,ndof,ned)
            call local(ien(1,nel),a,al,nen,ndof,ned)
            
            do 200 j=1,nen
                !
                do 100 i=1,ned
                    dl(i,j) = dl(i,j) + rdampk(m)*vl(i,j)
                    al(i,j) = al(i,j) + rdampm(m)*vl(i,j)
100             continue
                !
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
                temp = grav(i)*g1(lfbody)
                
                do 300 j=1,nen
                    al(i,j) = al(i,j) - temp
300             continue
            
400         continue
        
        endif
    
        if (formma.or.formkd) then
            
            call clear(elresf,nee)
            call local(ien(1,nel),x,xl,nen,nsd,nesd)
            lquad = .true.
            if (ien(3,nel).eq.ien(4,nel)) lquad = .false.
            call qdcshg(xl,det,shl,shg,nint,nel,neg,lquad)
        
            if (iopt.eq.2) then
                do 500 l=1,nint
                    r(l) = rowdot(shg(nrowsh,1,l),xl,nrowsh,nesd,nen)
                    det(l) = det(l)*r(l)
500             continue
            endif
        
            if (formma) then
            
                ! form inertial and/or body force
                constm = - th(m)*rho(m)
                call contma(shg,xl,w,det,al,eleffm,work,elresf,constm,imass,nint,nrowsh,nesd,nen,ned,nee)
            endif
        
            if (formkd) then
                
                ! form internal force
                constk = - th(m)
                call qdckd(shgbar,w,det,r,shg,b,dl,strain,c(1,1,m),stress,                                                         &
                           work,elresf,constk,ibbar,nen,nint,iopt,nrowsh,                                                          &
                           nesd,nrowb,nee,nstr)
            endif
        
            call addrhs(brhs,elresf,lm(1,1,nel),nee)
        
        endif
    
600 continue

    ! form surface force
    ! note: assembly of surface loads is performed inside qdcsuf

    if ( (nsurf.gt.0) .and. (lfsurf.gt.0) ) call qdcsuf(ielno,ien,x,xl,iside,mat,th,press,shear,elresf,                            &
                                                        brhs,lm,g1(lfsurf),nsurf,nen,nsd,nesd,ned,nee,iopt)

    return

    end subroutine qdct3
