!-----------------------------------------------------------
! input ...
! A(r,c) - array of coefficients for matrix A
! r,c - dimension
!rsort - determines which column to sort according to
!v - determines the sorting verse, if v==1 it sorts from the smallest to the largest value if v==2 it sorts from the largest value to the smallest
! output ...
! B(r,c) - inverse matrix of A
! comments ...
! the original matrix A(r,c) will be destroyed 
! during the calculation
!===========================================================



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
                 
