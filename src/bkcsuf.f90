subroutine bkcsuf(ielno,ien,x,xl,iside,press,elresf,brhs,lm,fac,nsurf,nen,nsd,nesd,ned,nee)
    !
    !     subroutine to compute consistent surface loads for the
    !     eight-node brick, elastic continuum element
    !
    !     note: two by two points gaussian quadrature is employed
    !
    implicit double precision (a-h,o-z)

!.... deactivate above card(s) for single-precision operation
    dimension work(2),ielno(1),ien(nen,1),x(nsd,1),xl(nesd,1),                                                                     &
              iside(1),mat(1),press(4,1),shl(3,4,4),w(4),det(4),                                                                   &
              elresf(ned,1),brhs(1),lm(ned,nen,1),ienl(4,6),sigp(6),                                                               &
              p(3),xlf(3,4),xs(3,3)                                                                                                
    common /consts/ zero,pt1667,pt25,pt5,one,two,three,four,five

    data nint/4/
    data ienl /1,2,3,4 ,                                                                                                           &
               5,6,7,8 ,                                                                                                           &
               1,2,6,5 ,                                                                                                           &
               2,3,7,6 ,                                                                                                           &
               3,4,8,7 ,                                                                                                           &
               4,1,5,8 /
    data sigp /1.,-1.,-1.,-1.,-1.,-1./

    do 300 k=1,nsurf
        nel = ielno(k)
        call local(ien(1,nel),x,xl,nen,nsd,nesd)
        call clear(elresf,nee)

        i = iside(k)
        call local(ienl(1,i),xl,xlf,4,nsd,nesd)

        call qdcshl (shl,w,nint)

        do 200 l=1,nint

            call sufdet (xlf,shl(1,1,l),nel,neg,i,xs,det(l))
            pn = rcdot(shl(3,1,l),press(1,k),3,4)
            pn = pn*fac*sigp(i)*w(l)*det(l)
            p(1)=xs(3,1)*pn
            p(2)=xs(3,2)*pn
            p(3)=xs(3,3)*pn

            do 100 n=1,3
                do 100 j=1,4
                    jj=ienl(j,i)
                    elresf(n,jj) = elresf(n,jj) + shl(3,j,l)*p(n)
100         continue

200     continue
        call addrhs(brhs,elresf,lm(1,1,nel),nee)

300 continue

    return

end subroutine bkcsuf
