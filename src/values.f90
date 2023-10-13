!block data
    !
    !.... program to define output labels and numerical constants
    !
    !implicit double precision (a-h,o-z)

    !.... deactivate above card(s) for single-precision operation
    !character*4 labeld,label1,label2,label3
    !common /consts/ zero,pt1667,pt25,pt5,one,two,three,four,five
    !common /labels/ labeld(3),label1(16),label2(3),label3(24)
    !
    !        labeld(3)  = displacement, velocity and acceleration labels
    !        label1(16) = output labels for element-type 1
    !        label2(3)  = output labels for element-type 2
    !        label2(24) = output labels for element-type 3
    !
    !.... note: add label arrays for any additional elements
    
    ! data zero,pt1667,pt25,pt5 /0.00,0.1666666666666667,0.25,0.50/,                                                                 &
    !      one,two,three,four,five/1.00,2.00,3.00,4.00,5.00/
    
    ! data labeld/'disp','vel ','acc '/
    
    ! data label1/'s 11','s 22','s 12','s 33','ps 1','ps 2',                                                                         &
    !             'tau ','sang','e 11','e 22','g 12','e 33',                                                                         &
    !             'pe 1','pe 2','gam ','eang'/
    
    ! data label2/'strs','forc','strn'/
    
    ! data label3/'s 11','s 22','s 33','s 12','s 23','s 31',                                                                         &
    !             'ps 1','ps 2','ps 3','----','----','----',                                                                         &
    !             'e 11','e 22','e 33','g 12','g 23','g 31',                                                                         &
    !             'pe 1','pe 2','pe 3','----','----','----'/

!end

 
