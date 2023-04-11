program potansiyel_hesaplama
    implicit none
    
    real :: a , b , d , potansiyel , r 
    real , parameter :: dr = 0.5 ! her 0.5 için
    real , parameter :: pi = 3.141593 ! pi sayısı
    
    r = 0.0
    potansiyel = 0.0

    write (*,"(x,a)" , advance = "no") "Ic yaricapi giriniz: " ; read *, a
    write (*, "(x,a)" , advance = "no") "Dis yaricapi giriniz: " ; read *, b
    write (*, "(x,a)" , advance = "no") "Yogunlugu giriniz: " ; read *, d
    print*, "-----------------------"

    write (*, "(6x , a , 8x , a )") "r" , "V(r)"
    print*, "-----------------------"

    do while (r < 10.0)

        if (r < a) then 

            potansiyel = 2 * pi * d * (b**2 - a**2)

        else if (r >= a .and. r <= b ) then
            
            potansiyel = 2 * pi * d * (b**2 - r**2 / 3) - 4 * pi * d * a**3 / 3*r 
        
        else if (r > b) then
            
            potansiyel = 4 * pi * d * (b**3 - a**3) / 3 * r
            
        end if

        r = r + dr

        print 10 , r , potansiyel

    end do

    10 format (2f9.2)

end program potansiyel_hesaplama
