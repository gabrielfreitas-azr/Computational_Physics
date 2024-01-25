      program tarefa4
      
      integer, parameter :: N = 1000
      integer, parameter :: M = 1000
      integer*8 :: ipos(M, 2) = 0
      
      open(11, file = "tarefa-4-saida-1.dat") 
      call srand(1)
      
      do i = 1, N
 
         do j = 1, M
            rand_x = rand()
            
            if (rand_x.le.(0.25d0)) then
               ipos(j, 2) = ipos(j, 2) + 1
            else if (rand_x.le.(0.5d0)) then
               ipos(j, 2) = ipos(j, 2) - 1          
            else if (rand_x.le.(0.75d0)) then
               ipos(j, 1) = ipos(j, 1) + 1
            else 
               ipos(j, 1) = ipos(j, 1) - 1       
            end if           
         end do
         
         S = 0.e0
         
         do ix = -N, N-5, 5
            do iy = -N, N-5, 5
            
               and = 0.e0
               
               do k = 1, M
               
                  x_and = ipos(k, 1)
                  y_and = ipos(k, 2)
                  
                  if(((ix.le.x_and).and.(x_and.le.(ix + 5)))
     *            .and.((iy.le.y_and).and.(y_and.le.(iy + 5)))) then
     
                     and = and + 1.e0
                     
                  end if             
               end do   
               
               if (and.ne.(0.e0)) then

                  pi = and / real(M, 8)
                  S = S - pi * log(pi)
               end if   
            end do
         end do  
         
         write(11,*) i , S
      end do
      
      close(11)

      end program tarefa4
