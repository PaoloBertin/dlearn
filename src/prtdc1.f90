subroutine prtdc1(i,iname,iadd,ndim1,ndim2,ndim3,ipr)
    !
    !.... program to print memory-pointer information for an array
    !
    character*4 name(2),nelpar,lefths
    dimension iname(2)
    common /iounit/ iin,iout,irsin,irsout
    !ZACE MODIF (RD  6 jan 1987) -----ordre save/data + character name
    save neg
    data nelpar,lefths/'npar','alhs'/
    
    if (i.eq.1) neg = 1
    call intchr (iname(1),name(1))
    call intchr (iname(2),name(2))
    if (name(1).eq.nelpar) then
        write (iout,1000) neg
        neg = neg + 1
    endif
    if (name(1).eq.lefths) write (iout,2000)
    write(iout,3000) i,name(1),name(2),iadd,ndim1,ndim2,ndim3,ipr
    
    return
    
1000 format( /14x,'*****',7x,'begin element group number',i5/' ')
2000 format( /14x,'*****',7x,'end element group data',/' ')
3000 format(14x,i5,7x,2a4,1x,6i10)

end subroutine prtdc1

