function lout(i,j)
    !
    !.... program to determine logical switch
    !
    logical lout
    !
    lout = .false.
    if (j.eq.0) return
    if (mod(i,j).eq.0) lout = .true.
    
    return
    
end function lout
    