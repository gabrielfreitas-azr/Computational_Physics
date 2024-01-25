      program tarefa8a
      
      real*8 :: R, fun_gamma, vd_formula = 0.d0
      real*8, parameter :: pi = acos(-1.d0)
      integer*8 :: id
      
      open(unit = 10, file = "tarefa-8a-saida-1.dat")
      
      read(*,*) R, id
      
      write(10,*) R
      
      do i = 0,id

         x = real(i, 8)/2.d0 + 1.d0
         fun_gamma = 1.d0
         
         do while(x.ne.(0.5d0).and.x.ne.(1.d0))
            fun_gamma = (x - 1.d0) * fun_gamma
            x = x - 1.d0
         end do
         
         if (x.eq.(0.5d0)) then
            fun_gamma = fun_gamma * dsqrt(pi)
         end if
         
         vd_formula = pi**(real(i, 8) / 2.d0)* R**real(i, 8) / fun_gamma
      
         write(10,*) i, vd_formula
      end do
      
      close(10)
     
      end program tarefa8a

      
      
      
      
