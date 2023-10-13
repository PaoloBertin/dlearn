subroutine qdcsuf(ielno,ien,x,xl,iside,mat,th,press,shear,elresf,                                                                  &
    brhs,lm,fac,nsurf,nen,nsd,nesd,ned,nee,iopt)
!
!.... program to compute consistent surface loads for the
!        four-node quadrilateral, elastic continuum element
!
!        note: two-point gaussian quadrature is employed
!
    implicit double precision (a-h,o-z)
!.... deactivate above card(s) for single-precision operation

    dimension z(2),work(2),ielno(1),ien(nen,1),x(nsd,1),xl(nesd,1),                                                                &
    &          iside(1),mat(1),th(1),press(2,1),shear(2,1),                                                                        &
    &          elresf(ned,1),brhs(1),lm(ned,nen,1)
    common /consts/ zero,pt1667,pt25,pt5,one,two,three,four,five

    z(2) = pt5/sqrt(three)
    z(1) = - z(2)

    do 300 k=1,nsurf
        nel = ielno(k)
        call local(ien(1,nel),x,xl,nen,nsd,nesd)
        call clear(elresf,nee)
        i = iside(k)
        j = i + 1
        if (j.eq.5) j = 1
        dx = xl(1,j) - xl(1,i)
        dy = xl(2,j) - xl(2,i)
        m = mat(nel)
        temp = pt5*fac*th(m)

        do 200 l=1,2
            shi = pt5 - z(l)
            shj = pt5 + z(l)
            p = shi*press(1,k) + shj*press(2,k)
            s = shi*shear(1,k) + shj*shear(2,k)

            if (iopt.eq.2) then
                r = shi*xl(1,i) + shj*xl(1,j)
                p = p*r
                s = s*r
            endif

            work(1) = temp*( - p*dy + s*dx)
            work(2) = temp*(   p*dx + s*dy)

            do 100 n=1,2
                elresf(n,i) = elresf(n,i) + shi*work(n)
                elresf(n,j) = elresf(n,j) + shj*work(n)
100         continue

200     continue

        call addrhs(brhs,elresf,lm(1,1,nel),nee)

300 continue

    return
    
end subroutine qdcsuf
