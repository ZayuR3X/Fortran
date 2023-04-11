program matris_hesaplama
    implicit none
    
    integer :: m , n , i , j , k
    real , allocatable  :: a(:,:) , b(:,:) , c(:,:) , d_carpim(:,:) , toplam(:,:) , iki_ile_carp(:,:)

    ! Matris boyutlarının alımı
    write (*,"(a)") "Matris boyutlarini giriniz(MxN) " ; read *, m,n
    
    ! Dinamik dizileri boyutlandırma
    allocate(a(m,n),b(m,n),c(m,n),d_carpim(m,n),toplam(m,n),iki_ile_carp(m,n))
    
    ! B ve C matrisinin değerlerini alma
    write(*,"(a)") "----------------------------------"
    write (*,"(a)") "B matrisinin degerlerini giriniz"
    write(*,"(a)") "----------------------------------"

    do i = 1 , m
        do j = 1 , n
            write(*,"(a,i1,a,i1,a)" , advance = "no") "Matrisin [", i, ",", j, "] elemanini giriniz: "
            read (*,*) b(i,j)
        end do
    end do

    write(*,"(a)") "----------------------------------"
    write (*,"(a)") "C matrisinin degerlerini giriniz"
    write(*,"(a)") "----------------------------------"
    
    do i = 1 , m
        do j = 1 , n
            write(*,"(a,i1,a,i1,a)" , advance = "no") "Matrisin [", i, ",", j, "] elemanini giriniz: "
            read (*,*) b(i,j)
        end do
    end do

    ! D matrisini hesaplama
    do i = 1 , m
        do j = 1 , n
            d_carpim(i,j) = 0.0
            do k = 1 , n
                d_carpim(i,j) = d_carpim(i,j) + b(i,k) * c(k,j)
            end do
        end do
    end do

    ! B ve C matrisinin toplamı
    do i = 1 , m
        do j = 1 , n
            toplam(i,j) = b(i,j) + c(i,j)
        end do
    end do
    
    ! Toplam matrisin 2 ile çarpılması
    do i = 1 , m
        do j = 1 , n
            iki_ile_carp(i,j) = 2 * toplam(i,j)
        end do
    end do

    ! Son olarak A matrisini bulmak için fark işlemi
    do i = 1 , m 
        do j = 1 , n
            a(i,j) = iki_ile_carp(i,j) - d_carpim(i,j)
        end do
    end do

    ! A matrisini ekrana yazdırma
    write(*,"(a)") "-------- A Matrisi --------"
    do i = 1 , m 
        do j = 1 , n
            write(*,"(7x,a,i1,a,i1,a,f4.2)") "[", i, ",", j, "] : " , a(i,j)
        end do
    end do

    ! deallocate matrisleri serbest bırakır
    deallocate(a,b,c,d_carpim,toplam,iki_ile_carp)

end program matris_hesaplama
