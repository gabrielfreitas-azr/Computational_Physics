      program tarefa1

      real*8 :: a, b, c, delta, x1, x2

      write(*,*) "Coeficientes do polinômio: ax^2 + bx + c = 0"
      read(*,*) a, b, c 

      delta = b**2 - 4 * a * c

      if (delta .lt. 0) then

         write(*,*) "Nenhuma solução nos reais."
         
      else
         x1 = (-b + sqrt(delta)) / (2 * a)
         write(*,*) "x = ", x1
              
         if (delta .gt. 0) then

            x2 = (-b - sqrt(delta)) / (2 * a)
            write(*,*) "x' = ", x2   
               
         end if
      end if

      end program tarefa1
      
