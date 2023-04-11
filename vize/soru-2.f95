program fonksiyon_hesaplama
    implicit none

    real , parameter :: pi = 3.141593 ! pi sayısı
    real :: y = 0.0 , hes_sonuc , j_toplam = 0.0 ! y = son değer
    integer :: x , i , j 

    do x = 1 , 4

        do  i = 1 , 5
            y = y + j_toplam
            do j = 1 , 3
                call hes(real(x),real(i),real(j),hes_sonuc)
                j_toplam = j_toplam + hes_sonuc
            end do
        end do

        write (*,*) "Y = " , y

    end do

    contains

    ! Faktoriyel hesaplayan fonksiyon
    integer function fakt (n)
        integer , intent(in) :: n 
        integer sayac

        fakt = 1 
        do sayac =  1 , n
            fakt = fakt * sayac
        end do

    end function fakt

    ! h1 fonksiyonu
    subroutine hes (sub_x,sub_i,sub_j,carpim)

        real , intent(in) :: sub_x , sub_i , sub_j
        real , intent(out) :: carpim
        integer :: k
        real :: ilk_sonuc  , son_sonuc = 1

        do k = 1 , 3 

            ilk_sonuc = (sub_x**(k-sub_j) / (13 * fakt(k-int(sub_j)))) * exp(sub_x) * tan(sub_x) - (fakt(2*int(sub_i) - int(sub_j)))  
            son_sonuc = son_sonuc * ilk_sonuc
        end do

        son_sonuc = carpim
        
    end subroutine hes

end program fonksiyon_hesaplama
