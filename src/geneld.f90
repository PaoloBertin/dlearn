subroutine geneld
    !
    ! subroutine to set defaults for element node
    ! and material number generation
    !
    common /genelc/ n,nel(3),incel(3),inc(3)
    
    if (nel(1).eq.0) nel(1) = 1
    if (nel(2).eq.0) nel(2) = 1
    if (nel(3).eq.0) nel(3) = 1
    
    if (incel(1).eq.0) incel(1) = 1
    if (incel(2).eq.0) incel(2) = nel(1)
    if (incel(3).eq.0) incel(3) = nel(1)*nel(2)
    
    if (inc(1).eq.0) inc(1) = 1
    if (inc(2).eq.0) inc(2) = (1+nel(1))*inc(1)
    if (inc(3).eq.0) inc(3) = (1+nel(2))*inc(2)
    
    return
end subroutine geneld
    