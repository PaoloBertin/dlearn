subroutine formlm (id,ien,lm,ndof,ned,nen,numel)
    !
    ! subroutine to form lm array
    !
    dimension id(ndof,1),ien(nen,1),lm(ned,nen,1)
    
    do 300 k=1,numel

        do 200 j=1,nen
            node=ien(j,k)

            do 100 i=1,ned
                lm(i,j,k) = id(i,node)
100         continue

200     continue

300 continue

    return
    
end subroutine formlm
   