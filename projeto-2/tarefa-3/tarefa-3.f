      program tarefa3

      integer*8 :: ix = 0, iy = 0
      real*8 :: dmeanx = 0.d0, dmeany = 0.d0, delta = 0.d0

      open(11, file = "tarefa-3-saida-1.dat") 
      
      call srand(1)
      
      M = 100000
      N = 0
      
      write(*,*) "Número de passos (N): "
      read(*,*) N
      
      do i = 1, M 
      
         ix = 0
         iy = 0
 
         do j = 1, N
             
            rand_x = rand()
            
            if (rand_x.le.(0.25d0)) then
               iy = iy + 1
            else if (rand_x.le.(0.5d0)) then
               iy = iy - 1           
            else if (rand_x.le.(0.75d0)) then
               ix = ix + 1
            else 
                ix = ix - 1       
            end if
            
         end do
         
         dmeanx = dmeanx + ix
         dmeany = dmeany + iy
         delta = delta + ix**2 + iy**2
         
         write(11,'(2I7)') ix, iy  
         
      end do
      
      dmeanx = dmeanx / real(M, 8)
      dmeany = dmeany / real(M, 8)
      delta = delta / real(M, 8)
      
      write(*,*) "<x> = ", dmeanx
      write(*,*) "<y> = ", dmeany
      write(*,*) "<delta^2> = ", delta - dmeanx**2 - dmeany**2
      
      close(11)

      end program tarefa3
