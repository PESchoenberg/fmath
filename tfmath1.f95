!> =============================================================================
!> 
!> tfmath1.f95
!> 
!> =============================================================================
!> 
!>  Copyright (C) 2018 - 2020  Pablo Edronkin (pablo.edronkin at yahoo.com)
!> 
!>    This program is free software: you can redistribute it and/or modify
!>    it under the terms of the GNU Lesser General Public License as published
!>    by the Free Software Foundation, either version 3 of the License, or
!>    (at your option) any later version.
!> 
!>    This program is distributed in the hope that it will be useful,
!>    but WITHOUT ANY WARRANTY; without even the implied warranty of
!>    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!>    GNU Lesser General Public License for more details.
!> 
!>    You should have received a copy of the GNU Lesser General Public License
!>    along with this program.  If not, see <https://www.gnu.org/licenses/>.
!> 
!> =============================================================================
!> 
!> Test program for fmath* functions.
!>
!> Compilation and linking:
!> - gfortran -std='gnu' tfmath1.f95 fmath1.o fmath2.o fmath3.o fmath4.o fmath5.o -o tfmath1 -O3 -march=native -Wall -Wextra -fopenmp
!>
!> or (if you want to use MPICH with this)
!>
!> - mpif90 -std='gnu' tfmath1.f95 fmath1.o fmath2.o fmath3.o fmath4.o fmath5.o -o tfmath1_mpi -O3 -march=native -Wall -Wextra -fopenmp
!>
!> Execution:
!> - ./tfmath1
!>
!> or (if you compiled it for use with MPICH)
!>
!> - mpiexec -np [n] ./tfmath1_mpi (n = number of processes to use, recommended
!>  n = 1 in this case, because you might get errors otherwise, at least it the
!>  present version of fmath).
!> 
program tfmath1

    use fmath1
    use fmath2
    use fmath3
    use fmath4
    use fmath5
    implicit none
    
    ! Setting a platform-independent floating point precision.
    integer, parameter :: tmath1_p1 = selected_real_kind( 10,300 )
    
    integer:: a, opt
    real( kind = tmath1_p1 ) :: x
    
    a = 1
    x = 1.0
    opt = 1

    do while ( opt > 0 )
    
        call system("clear")
        call RspFComment( "Ready to begin available fmath tests. Select your option" ) 
        call RspFComment( " 0 - Exit" )
        call RspFComment( " 1 - fmath1" )
        call RspFComment( " 2 - fmath2" )
        call RspFComment( " 3 - fmath3" )
        call RspFComment( " 4 - fmath4" )
        call RspFComment( " 5 - fmath5" )
        read(*,*) opt
        
        call system("clear")
        select case (opt)
            case (0)
                call RspFComment( "Program tfmath finished normally. Bye!" )           
            case (1)
                call RspFTestFmath1All()       
            case (2)
                call RspFTestFmath2All()       
            case (3)
                call RspFTestFmath3All()       
            case (4)
                call RspFTestFmath4All()       
            case (5)
                call RspFTestFmath5All()
            case default
               call RspFCommentEn( "You selected an option not implemented. Please, try again..." )
        end select    
    
    end do
    
end program

