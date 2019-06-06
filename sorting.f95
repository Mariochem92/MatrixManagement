!This subroutine allows the sorting of a matrix. If v==1 it sorts from the smallest to the largest value
!if v==2 it sorts from the largest value to the smallest
!rsort determines whic column to sort according to
!The matrix A is charactherized by a number of columns c and rows r



               subroutine sort(A,r,c,rsort,v,B)
               implicit none
               integer :: r,c,irow,krow,rsort,v
               Double precision :: A(r,c), buf(c), B(r,c)

                          if (v==1) then
                                do irow = 1, r
                                krow = minloc( A( irow:r, rsort ), dim=1 ) +irow - 1
                                buf( : )     = A( irow, : )
                                A( irow, : ) = A( krow, : )
                                A( krow, : ) = buf( : )
                                 end do

                           B=A

                         end if

                          if (v==2) then
                                do irow = 1, r
                                krow = maxloc( A( irow:r, rsort ), dim=1 ) +irow- 1
                                buf( : )     = A( irow, : )
                                A( irow, : ) = A( krow, : )
                                A( krow, : ) = buf( : )
                                 end do

                           B=A

                         end if


                 
                   end subroutine
                 
