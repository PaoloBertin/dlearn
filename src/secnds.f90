function secnds (t)
    ! 
    ! Interface for DLEARN program (T.J.R. HUGHES) to get CPU time
    !
    call syscpu(t0) 

    secnds = t0-t
    
    return
    
end function secnds

