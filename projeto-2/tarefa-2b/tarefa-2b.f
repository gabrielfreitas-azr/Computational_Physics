      program tarefa2b
      
      integer*8, parameter :: N = 1000
      integer*8 :: ix(3) 
      real*8 :: p(3), sumx(3) = 0.e0, sumx2(3) = 0.e0

      open(11, file = "tarefa-2b-saida-1.dat")
      open(12, file = "tarefa-2b-saida-2.dat")
      open(13, file = "tarefa-2b-saida-3.dat")

      call srand(1)
      
      M = 0
      
      p(1) = 1.e0 / 3.e0
      p(2) = 1.e0 / 4.e0
      p(3) = 1.e0 / 5.e0
      
      write(*,*) "Número de andarilhos (M): "
      read(*,*) M
      
      do i = 1, M
      
         ix(1) = 0
         ix(2) = 0
         ix(3) = 0
         
         do j = 1, N 
             
            rand_x = rand()
            
            do k = 1, 3           
               if (rand_x.ge.(1.e0 - p(k))) then
                  ix(k) = ix(k) + 1
               else
                  ix(k) = ix(k) - 1
               end if
            end do
            
         end do
         
         write(11,'(I7)') ix(1)
         write(12,'(I7)') ix(2)
         write(13,'(I7)') ix(3)   
         
         do k = 1, 3           
            sumx(k) = sumx(k) + ix(k)
            sumx2(k) = sumx2(k) + ix(k)**2
         end do      
         
      end do
      
      do k = 1, 3       
      
         write(*,*) "Para p = ", p(k)    
         write(*,*) "<x> = ", sumx(k) / real(M, 4)
         write(*,*) "<x^2> = ", sumx2(k) / real(M, 4)
      
      end do 
      
      close(11)
      close(12)
      close(13)
      
      end program tarefa2b
