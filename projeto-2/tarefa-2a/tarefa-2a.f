      program tarefa2a
      
      integer*4, parameter :: N = 1000
      real*8 :: sumx = 0.e0, sumx2 = 0.e0
      
      open(11, file = "tarefa-2a-saida-1.dat")
      call srand(1)
      
      M = 0
      p = 0.5e0
      
      write(*,*) "Número de andarilhos (M): "
      read(*,*) M
      
      do i = 1, M
      
         ix = 0
         
         do j = 1, N 
             
            if (rand().ge.p) then
               ix = ix + 1
            else
               ix = ix - 1
            end if
            
         end do
         
         write(11,'(I7)') ix
         
         sumx = sumx + ix
         sumx2 = sumx2 + ix**2
      end do
      
      write(*,*) "<x> = ", sumx / real(M, 4)
      write(*,*) "<x^2> = ", sumx2 / real(M, 4)
      
      close(11)
      
      end program tarefa2a
