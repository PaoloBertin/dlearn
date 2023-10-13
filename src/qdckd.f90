subroutine qdckd(shgbar,w,det,r,shg,b,dl,strain,c,stress,work,                                                                     &
                 elresf,constk,ibbar,nen,nint,iopt,nrowsh,                                                                         &
                 nesd,nrowb,nee,nstr)
!
!.... program to form internal force ("-k*d") for a continuum element
!        with "nen" nodes
!
!        note: the b-bar option is restricted to the mean-dilatation
!              formulation. to generalize to other formulations,
!              redimension array "shgbar", and replace routines
!              "meansh" and "qdcb".
!
    implicit double precision (a-h,o-z)
!
!.... deactivate above card(s) for single-precision operation
!
    dimension shgbar(3,nen,1),w(1),det(1),r(1),shg(nrowsh,nen,1),                                                                 &
              b(nrowb,1),dl(1),strain(1),c(nrowb,1),stress(1),                                                                    &
              work(1),elresf(1)

    if (ibbar.eq.1) call meansh(shgbar,w,det,r,shg,nen,nint,iopt,nesd,nrowsh)

    !.... loop on integration points
    do 100 l=1,nint
        temp = constk*w(l)*det(l)

        ! set up the strain-displacement matrix
        call qdcb(shg(1,1,l),shgbar,b,r(l),iopt,nrowsh,nrowb,nen,ibbar)

        ! calculate strains
        call multab(b,dl,strain,nrowb,nee,nstr,nee,nstr,1,1)

        ! calculate stresses
        call multab(c,strain,stress,nrowb,nstr,nstr,nstr,nstr,1,1)

        ! calculate element internal force
        call smult(temp,stress,stress,nstr,nstr,nstr,1,1)
        call multab(b,stress,work,nrowb,nstr,nee,nstr,nee,1,2)
        call matadd(elresf,work,elresf,nee,nee,nee,nee,1,1)

100 continue

    return

end subroutine qdckd
