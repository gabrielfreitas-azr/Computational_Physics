      program tarefa2a
      
      real*8 :: f
       
      real*8 :: h(11) = 0.d0     
      real*8 :: dint_f = 0.d0, x_i = 0.d0
      
      integer*8 :: ia = 0, ib = 1
      
      open(11, file = "tarefa-2a-saida-1.dat")
      
      ! inicializa o vetor com os passos (h)
      
      do i = 2, 12     
         h(i - 1) = 1.d0 / (3.d0 * 2.d0 ** real(i, 8))
      end do
      
      do j = 1, 11
      
         ! loop com os passos
         
         N = (ib - ia) / h(j)
         dint_f = 0.d0
         
         ! percorre o intervalo, aplicando a regra do trapézio
         
         do i = 1, N + 1
         
            x_i = real(ia, 8) +  real(i - 1, 8) * h(j)
            
            dint_f = (f(x_i + h(j)) + f(x_i)) + dint_f
         end do  
         
         dint_f = dint_f * 0.5d0 * h(j)
         
         write(11,*) dint_f
         
      end do
      
      close(11)
 
      end program tarefa2a
      
      function f(x)
      
      real*8 :: f
      real*8 :: x
      real*8 :: pi = dacos(-1.d0)
      
      f = dexp(-x) * dcos(2.d0 * pi * x)
      
      return 
      end function f
