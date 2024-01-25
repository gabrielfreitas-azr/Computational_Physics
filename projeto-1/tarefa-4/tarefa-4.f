      program tarefa4
     
      integer, parameter :: max_n = 10000
      integer :: n, icount = 0
      logical :: b(max_n) = .TRUE.
      
      read(*,*) n
     
      do i = 2, n
         j = 2
         do while((i * j).le.max_n)
            b(i * j) = .FALSE.
            j = j + 1
         end do
      end do
      
      do i = 1, n
         if (b(i)) then
            write(*,*) i
            icount = icount + 1
         end if
      end do
      
      write(*,*) icount
      end program tarefa4
      
