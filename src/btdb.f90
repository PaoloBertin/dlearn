subroutine btdb(elstif,b,db,nee,nrowb,nstr)
    !
    !.... program to multiply b(transpose) * db taking account of symmetry
    !        and accumulate into element stiffness matrix
    !
    implicit double precision (a-h,o-z)
    
    !.... deactivate above card(s) for single-precision operation
    dimension elstif(nee,1),b(nrowb,1),db(nrowb,1)
    !
    do 200 j=1,nee
    
        do 100 i=1,j
            elstif(i,j) = elstif(i,j) + coldot(b(1,i),db(1,j),nstr)
100     continue
        
200 continue
    
    return
    
end subroutine btdb
    