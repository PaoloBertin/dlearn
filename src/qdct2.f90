subroutine qdct2(eleffm,ien   ,x     ,xl    ,mat   ,det   ,                                                                        &
                 shl   ,shg   ,r     ,rdampm,rdampk,th    ,                                                                        &
                 rho   ,w     ,work  ,shgbar,b     ,c     ,                                                                        &
                 dmat  ,db    ,alhs  ,idiag ,lm    ,                                                                               &
                 impexp,imass ,numel ,neesq ,nen   ,nsd   ,                                                                        &
                 nesd  ,nint  ,neg   ,nrowsh,ldyn  ,ned   ,                                                                        &
                 iopt  ,ibbar ,nrowb ,nstr  ,nee   )
!
!.... program to calculate effective mass matrix for the
!        four-node quadrilateral, elastic continuum element and
!        assemble into the global left-hand-side matrix
!
!         impexp = 0, implicit time integration
!                = 1, explicit time integration
!
    implicit double precision (a-h,o-z)
!
!.... deactivate above card(s) for single-precision operation
!
    logical ldyn,ldiag,lquad
    dimension eleffm(nee,1),ien(nen,1),x(nsd,1),xl(nesd,1),mat(1),                                                                &
              det(1),shl(nrowsh,nen,1),shg(nrowsh,nen,1),r(1),                                                                    &
              rdampm(1),rdampk(1),th(1),rho(1),w(1),work(1),                                                                      &
              shgbar(3,1),b(nrowb,1),c(nrowb,nrowb,1),dmat(nrowb,1),                                                              &
              db(nrowb,1),alhs(1),idiag(1),lm(ned,nen,1)
    common /coeffs/ coeff1,coeff2,coeff3,coeff4,coeff5,coeff6,                                                                    &
                    coeff7,coeff8,alpha1,beta1 ,gamma1,dt1
    common /consts/ zero,pt1667,pt25,pt5,one,two,three,four,five

    ldiag = .false.
    if ( (impexp.eq.1) .and. (imass.eq.1) ) ldiag = .true.

    do 200 nel=1,numel

        call clear(eleffm,neesq)
        call local(ien(1,nel),x,xl,nen,nsd,nesd)
        m = mat(nel)
        lquad = .true.
        if (ien(3,nel).eq.ien(4,nel)) lquad = .false.
        call qdcshg(xl,det,shl,shg,nint,nel,neg,lquad)

        if (iopt.eq.2) then

            do 100 l=1,nint
                r(l) = rowdot(shg(nrowsh,1,l),xl,nrowsh,nesd,nen)
                det(l) = det(l)*r(l)
100         continue

        endif

        if ( ldyn .and. (imass.ne.2) ) then

!....... form mass matrix
            constm = (one + rdampm(m)*coeff4)*th(m)*rho(m)
            if (constm.ne.zero) call contm(shg,xl,w,det,eleffm,work,                                                               &
                                           constm,imass,nint,nrowsh,nesd,nen,ned,nee,.false.)

        endif

        if ( (.not.ldyn) .or. (impexp.eq.0) ) then

!....... form stiffness matrix
            constk = (coeff4*rdampk(m) + coeff5)*th(m)
            call qdck(shgbar,w,det,r,shg,b,c(1,1,m),dmat,db,eleffm,constk,                                                         &
                      ibbar,nen,nint,iopt,nesd,nrowsh,nrowb,nstr,nee)

        endif

!.... assemble element effective mass matrix into global
!     left-hand-side matrix

        call addlhs(alhs,eleffm,idiag,lm(1,1,nel),nee,ldiag)

200 continue

    return

end subroutine qdct2
