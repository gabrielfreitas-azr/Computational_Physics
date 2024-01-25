      program tarefa1
      
      real*8 :: dmoment(4) = 0.d0
      
      call srand(1)
      
      N = 0

      write(*,*) "Digite o tamanho (N) da distribuição: "
      read(*,*) N
      
      do i = 1, N
      
         x = rand()
         
         dmoment(1) = dmoment(1) + x
         dmoment(2) = dmoment(2) + x**2
         dmoment(3) = dmoment(3) + x**3
         dmoment(4) = dmoment(4) + x**4
      
      end do
      
      write(*,*) "<x> = ", dmoment(1) / real(N, 8)
      write(*,*) "<x^2> = ", dmoment(2) / real(N, 8)
      write(*,*) "<x^3> = ", dmoment(3) / real(N, 8)
      write(*,*) "<x^4> = ", dmoment(4) / real(N, 8)
      
      end program tarefa1
