subroutine colht(idiag,lm,ned,nen,numel)

    !.... program to compute column heights in global left-hand-side matrix
    !
    dimension idiag(1),lm(ned,nen,1)
    common /colhtc/ neq
    
    do 500 k=1,numel
        min = neq
        do 200 j=1,nen
            
            do 100 i=1,ned
                num = lm(i,j,k)
                if (num.gt.0) min = min0(min,num)
100         continue

200     continue

        do 400 j=1,nen

            do 300 i=1,ned
                num = lm(i,j,k)
                if (num.gt.0) then
                    m = num - min
                    if (m.gt.idiag(num)) idiag(num) = m
                endif
300         continue

400     continue
    
500 continue
    
    return
    
end subroutine colht
    