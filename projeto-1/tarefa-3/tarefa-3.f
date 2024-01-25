      program tarefa3
   
      real*8 :: values(500000)
      real*8 :: vtemp = 0
      integer :: i, n, m
      
      open(unit = 10, file = "tarefa-3-entrada-1.in")
      open(unit = 11, file = "tarefa-3-saida-1.dat")
      
      n = 0
      
      do i = 1, 500000
         read(10,*, end = 1) values(i)
         n = n + 1
      end do
1     continue

      write(*,*) "n = ", n
      write(11,*) n
      
      write(*,*) "m = ?"
      read(*,*) m
         
      do i = 1, m
         do j = (n - 1), 1, -1
            
            if (values(j + 1).lt.values(j)) then
            
               vtemp = values(j)
               values(j) = values(j + 1)
               values(j + 1) = vtemp
            
            end if
         end do
      end do
      
      
      do i = 1, n
         write(11,*) values(i)
      end do
      
      close(10)
      close(11)
      
      end program tarefa3
