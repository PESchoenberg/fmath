!> =============================================================================
!> 
!> fmath4.f95 - Support and general math functions and subroutines.
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
!> Compilation:
!> - ./cfmath.sh or 
!> - mpif90 -std='gnu' -c fmath4.f95 -O3 -march=native -Wall -Wextra -fopenmp 
!>
module fmath4

    implicit none

    ! Setting a platform-independent floating point precision.
    integer, parameter :: fmath4_p1 = selected_real_kind( 10,300 )
    
    !------------------------------------------------------------------------------
    contains

        !------------------------------------------------------------------------------
        ! Misc functions.


        !> Draws a simple line in the console.
        !>
        subroutine RspFLine()

            implicit none

            write(*,*) "--------------------------------------------------------------"

        end subroutine


        !> "Not implemented yet" place holder. Puts a standard message on console.
        !>
        subroutine RspFNiy()

            implicit none

            call RspFComment( 'Test not implemented yet...' )
            read(*,*)
            
        end subroutine


        !> Writes a comment on console.
        !>
        !> Arguments:
        !> - p_t: text to be written.
        !>
        subroutine RspFComment( p_t )
        
            implicit none
            character( len = * ) :: p_t
            
            write(*,*) p_t
            write(*,*)
            
        end subroutine


        !> Places a comment and asks for [enter].
        !>
        !> Arguments:
        !> - p_t: text to be written.
        !>
        subroutine RspFCommentEn( p_t )
        
            implicit none
            
            character( len = * ) :: p_t
            
            call RspFComment( p_t )
            call RspfComment( "Press [enter] to continue..." )
            read(*,*)
            
        end subroutine


        !------------------------------------------------------------------------------
        ! Math functions.

        !> Calculates the nth root of a number.
        !>
        !> Arguments:
        !> - p_n:
        !> - p_r:
        !>
        !> Output:
        !> - The nth root of a number (p_n**(/p_r)).
        !>
        pure real( kind = fmath4_p1 ) function RspFNthRoot( p_n, p_r )

            implicit none

            real( kind = fmath4_p1 ), intent(in) :: p_n, p_r
            
            RspFNthRoot = p_n**( 1.0 / p_r )
            
        end function


        !> Calculates the radius of the nth circle of a Pappus chain, given that the radius of the 
        !> first circle r = ac / ab.
        !>
        !> Arguments:
        !> - p_ac: diameter of cicle 2, being a the point on which the inversion is centered.
        !> - p_ab: diameter of the first circle.
        !> - p_n: nth order.
        !>
        !> Output:
        !> - Radius of nth circle.
        !>
        !> Sources:
        !> - Adapted from rosettacode.org by Pablo Edronkin.
        !> - https://en.wikipedia.org/wiki/Pappus_chain
        !> - https://en.wikipedia.org/wiki/Arbelos
        !>
        real( kind = fmath4_p1 ) function RspFPappusChainRadiiGenerator( p_ac, p_ab, p_n )
        
            implicit none

            real( kind = fmath4_p1 ) :: p_ac, p_ab, p_n, r

            r = p_ac / p_ab
            RspFPappusChainRadiiGenerator = ( (1 - r) * r ) / ( 2 * ( p_n**2 * ( (1 - r)**2 ) + r ) )

            return
        end function 
 

        !> Generates a Mersenne number M = 2^p_pow - 1.
        !>  
        !> Arguments:
        !> - p_pow: power; should be <= 1023. 
        !>
        !> Output:
        !> - Mersenne number.
        !>
        !> Sources:
        !> - https://en.wikipedia.org/wiki/Mersenne_prime
        !>
        real( kind = fmath4_p1 ) function RspFMersenneNumberGenerator( p_pow )
        
            implicit none

            real( kind = fmath4_p1 ) :: p_pow

            RspFMersenneNumberGenerator = 2.0**p_pow - 1

            return
        end function 
        
 
        !> Maps p_x to the interval defined by [p_minr, p_maxr] for data in the range [p_mind, p_maxd].
        !>
        !> Arguments:
        !> - p_x: value to normalize.
        !> - p_minr: lower threshold of the range interval. 
        !> - p_maxr: higher threshold of the range interval.
        !> - p_mind: lower threshold of the data range.
        !> - p_maxd: higher threshold of the data range.
        !>
        !> Output:
        !> - Normalized value.
        !>
        !> Sources:
        !> - https://www.youtube.com/watch?v=SrjX2cjM3Es&list=PLea0WJq13cnDXK34DuEHacBR3rgAVJZkZ        
        !>
        real( kind = fmath4_p1 ) function RspFNormalize2MinMaxInterval( p_x , p_minr, p_maxr, p_mind, p_maxd )
        
            implicit none

            real( kind = fmath4_p1 ) :: p_x , p_minr, p_maxr, p_mind, p_maxd

            RspFNormalize2MinMaxInterval = ( ( ( p_x - p_mind ) * ( p_maxr - p_minr ) ) / ( p_maxd- p_mind ) ) + p_minr

            return
        end function


        !> Computes growth according to the Solow - Swan economic growth model at a given time instance.
        !> 
        !> Arguments:
        !> - p_l: labour.
        !> - p_k: capital
        !> - p_a: technology
        !> - p_e: elasticity, ( 0 < a < 1 ).
        !>
        !> Output:
        !> - Growth. 
        !>
        real( kind = fmath4_p1 ) function RspFSolowSwanModel( p_l, p_k, p_a, p_e )
        
            implicit none

            real( kind = fmath4_p1 ) :: p_l, p_k, p_a, p_e

            RspFSolowSwanModel = ( p_k**p_e ) * ( ( p_a * p_l )**( 1 - p_e ) )

            return
        end function


        !> Denormalizes p_y from a mapped interval defined by [p_minr, p_maxr] to a data range 
        !> defined by range [p_mind, p_maxd].
        !>
        !> Arguments:
        !> - p_x: value to denormalize.
        !> - p_minr: lower threshold of the range interval. 
        !> - p_maxr: higher threshold of the range interval.
        !> - p_mind: lower threshold of the data range.
        !> - p_maxd: higher threshold of the data range.
        !>
        !> Output:
        !> - Denormalized value.
        !>
        !> Sources:
        !> - Adapted from rosettacode.org by Pablo Edronkin.        
        !> - See RspFNormalize2MinMaxInterval(...).
        !>
        !> NEEDS TESTING.
        !> 
        real( kind = fmath4_p1 ) function RspFDenormalizeFromMinMaxInterval( p_y , p_minr, p_maxr, p_mind, p_maxd )
        
            implicit none

            real( kind = fmath4_p1 ) :: p_y , p_minr, p_maxr, p_mind, p_maxd

            RspFDenormalizeFromMinMaxInterval = ( ( ( p_y - p_minr ) * ( p_maxd- p_mind ) ) / ( p_maxr - p_minr ) ) + p_mind

            return
        end function


        !> Calulates Z according to the Euler-Riemann Zeta function; bare-bones implementation.
        !>
        !> Arguments:
        !> - p_s: Riemann s value. 
        !> - p_i: iterations
        !>
        !> Output:
        !> - Z value.
        !>
        !> Sources:
        !> - Adapted from rosettacode.org by Pablo Edronkin.        
        !> - https://en.wikipedia.org/wiki/Riemann_zeta_function
        !> - http://mathworld.wolfram.com/RiemannZetaFunction.html
        !>
        !> NEEDS TESTING AND CONVERSION TO COMPLEX NUMBERS.
        !> 
        real( kind = fmath4_p1 ) function RspFZeta( p_s, p_i )
        
            implicit none

            real( kind = fmath4_p1 ) :: p_s, z, x
            integer :: p_i, n

            x = 0.0
            z = 0.0
            if ( (p_s - (-1) < epsilon(x) ) .or. (p_s - (-13) < epsilon(x) ) ) then
                z = -1/12
            else if ( p_s - 0.0 < epsilon(x) ) then
                z = -1/2
            else if ( p_s - 1/2 < epsilon(x) ) then
                z = -1.4603545;
            else if ( p_s - 2.0 < epsilon(x) ) then  
                z = 1.645
            else if ( p_s - 3.0 < epsilon(x) ) then
                z = 1.202
            else if ( p_s - 4.0 < epsilon(x) ) then
                z = 1.0823
            else
                n = 1
                do n = 1, p_i
                    z = z + ( n**(-p_s) );
                end do
            end if
            RspFZeta = z

            return
        end function


        !> If condition p_cond is fulfilled, returns p_num3, otherwise returns p_num4.
        !> 
        !> Arguments:
        !> - p_cond: condition that should be fulfilled, wwritten as a string:
        !>     - "==" equal.
        !>     - "<" less than.
        !>     - ">" greater than.
        !>     - "<=" less or equal than.
        !>     - ">=" greater or equal than.
        !>     - "abs" compares the equality of both absolute values.
        !>     - "opp" oposite numbers.
        !>     - "recip" reciprocal numbers.
        !>     - "++" both positive.
        !>     - "--" both negative.
        !>     - "00" both zero.
        !>     - "+-" first positive and second negative.
        !>     - "-+" first negative and second positive.
        !>     - "0+" first zero, second positive.
        !>     - "+0" first positive, second zero.
        !>     - "0-" first zero, second negative.
        !>     - "-0" first negative, second zero.
        !> - p_num1: number to be compared.
        !> - p_num2: number that the function should not return.
        !> - p_num3: result returned if p_num1 = p_num2.
        !> - p_num4: result if p_num1 != p_num2
        !> 
        !> Output:
        !> - Either p_num3 or p_num4.
        !> 
        pure real( kind = fmath4_p1 ) function RspFRoicf( p_cond, p_num1, p_num2, p_num3, p_num4 )
        
            implicit none

            real( kind = fmath4_p1 ) :: x
            real( kind = fmath4_p1 ), intent(in) :: p_num1, p_num2, p_num3, p_num4
            character, intent(in) :: p_cond
            logical :: cond_f

            cond_f = .false.
            x = 0.0

            ! Equal. 
            if( p_cond == "==" ) then
                if ( p_num1 - p_num2 < epsilon(x) ) then
                    cond_f = .true.
                end if
            end if

            ! Less than. 
            if( p_cond == "<" ) then
                if ( p_num2 - p_num1 > epsilon(x) ) then
                    cond_f = .true.
                end if
            end if

            ! Greater than. 
            if( p_cond == ">" ) then
                if ( p_num1 - p_num2 > epsilon(x) ) then
                    cond_f = .true.
                end if
            end if
    
            ! Less or equal than. 
            if( p_cond == "<=" ) then
                if ( p_num2 - p_num1 >= epsilon(x) ) then
                    cond_f = .true.
                end if
            end if

            ! Greater or equal than. 
            if( p_cond == ">=" ) then
                if ( p_num1 - p_num2 >= epsilon(x) ) then
                    cond_f = .true.
                end if
            end if

            ! Have the same absolute value. 
            if( p_cond == "abs" ) then
                if  ( abs( p_num1 ) - abs( p_num2 ) < epsilon(x) ) then
                    cond_f = .true.
                end if
            end if

            ! Opposite numbers. 
            if( p_cond == "opp" ) then
                if ( p_num1 - ( ( -1.0) * p_num2 ) < epsilon(x) ) then
                    cond_f = .true.
                end if
            end if

            ! Recip. 
            if( p_cond == "recip" ) then
                if ( ( p_num1 * ( 1 / p_num2 ) ) < ( 1 + epsilon(x) ) ) then
                    cond_f = .true.
                end if
            end if

            ! Both positive. 
            if( p_cond == "++" ) then
                if ( ( p_num1 > 0 + epsilon(x) ) .and. ( p_num2 > 0 + epsilon(x) ) ) then
                    cond_f = .true.
                end if
            end if

            ! Both negative. 
            if( p_cond == "--" ) then
                if ( ( p_num1 < 0 - epsilon(x) ) .and. ( p_num2 < 0 - epsilon(x) ) ) then
                    cond_f = .true.
                end if
            end if

            ! Both zero. 
            if( p_cond == "00" ) then
                if ( ( abs(p_num1) < 0 + epsilon(x) ) .and. ( abs(p_num2) < 0 + epsilon(x) ) ) then
                    cond_f = .true.
                end if
            end if    

            ! First zero, second positive. 
            if( p_cond == "0+" ) then

                if ( ( abs(p_num1) < 0 + epsilon(x) ) .and. ( abs(p_num2) > 0 + epsilon(x) ) ) then
                    cond_f = .true.
                end if
            end if

            ! First zero, second negative. 
            if( p_cond == "0-" ) then
                if ( ( abs(p_num1) < 0 + epsilon(x) ) .and. ( p_num2 < 0 - epsilon(x) ) ) then
                    cond_f = .true.
                end if
            end if

            ! First positive, second zero. 
            if( p_cond == "+0" ) then
                if ( ( p_num1 > 0 + epsilon(x) ) .and. ( abs(p_num2) < 0 + epsilon(x) ) ) then
                    cond_f = .true.
                end if
            end if

            ! First negative, second zero. 
            if( p_cond == "-0" ) then
                if ( ( p_num1 < 0 - epsilon(x) ) .and. ( abs(p_num2) < 0 + epsilon(x) ) ) then
                    cond_f = .true.
                end if
            end if

            ! Return.
            if ( cond_f .eqv. .true. ) then
                RspFRoicf = p_num3;
            else
                RspFRoicf = p_num4;
            end if

            return
        end function


        !> Put a value within a range.
        !>
        !> Arguments:
        !> - p_min: lower limit of the range. 
        !> - p_max: higher limit of the range.
        !> - p_val: value to be tested against the range.
        !>
        !> Output:
        !> - Value within the range.
        !>
        !> Sources:
        !> - http://dlib.net/dlib/algs.h.html#put_in_range
        !>
        pure real( kind = fmath4_p1 ) function RspFPutInRange( p_min, p_max, p_val )
        
            implicit none

            real( kind = fmath4_p1 ) :: res
            real( kind = fmath4_p1 ), intent(in) :: p_min, p_max, p_val

            res = p_val
            res = RspFRoicf( "<", res, p_min, p_min, res )
            res = RspFRoicf( ">", res, p_max, p_max, res )

            RspFPutInRange = res

            return
        end function 


        !> Puts a value within a range if p_pir is 1.0.
        !>
        !> Arguments:
        !> - p_pir: 1.0 to put in range, any other number otherwise.
        !> - p_min: Lowlr limit of the range.
        !> - p_max: higher limit of the range.
        !> - p_w: value to be ranged. 
        !>
        !> Output:
        !> - Adapted from rosettacode.org by Pablo Edronkin.        
        !> - Value ranged if p_pir is 1, original value otherwise.
        !>
        pure real( kind = fmath4_p1 ) function RspFPir( p_pir, p_min, p_max, p_w )
        
            implicit none

            real( kind = fmath4_p1 ), intent(in) :: p_pir, p_min, p_max, p_w

            if ( abs( p_pir ) - 1.0 < epsilon( abs( p_pir ) ) ) then
                RspFPir = RspFPutInRange( p_min, p_max, p_w );
            else
                RspFPir = p_w;
            end if

            return
        end function


        !> Calculates the tolerance value of real rounding and comparisons on a given system.
        !> 
        !> Arguments:
        !> - p_x: value.
        !>
        !> Output:
        !> - Epsilon value.
        !>
        !> Sources:
        !> - Adapted from rosettacode.org by Pablo Edronkin.        
        !> - http://jules-lsm.github.io/coding_standards/guidelines/fp_arithmetic.html
        !> 
        pure real( kind = fmath4_p1 ) function RspFTol( p_x )
        
            implicit none

            real( kind = fmath4_p1 ), intent(in) :: p_x

            RspFTol = epsilon( p_x ) 

            return
        end function


        !> Performs operation of p_op to all elements of array p_vec from element p_min to element p_max
        !> successively and as a series. For example, in the case of a serial sum, the function will
        !> yield p_vec(0) + p_vec(1) + ... p_vec(n).
        !>
        !> Arguments:
        !> - p_cond: operation to be applied, as per the following list:
        !>      - "++": summation.
        !>      - "--": substraction.
        !>      - "**": multiplication.
        !>      - "//": division.
        !>      - "am": aritmetic mean.
        !>      - "gm": geometric mean.
        !>      - "hm": harmonic mean.        
        !> - p_vec: array of real numbers.
        !> - p_vec_len: number of elements contained in p_vec.
        !> - p_min: ordinal of the first element of the sub array to which p_op will be applied.
        !> - p_max: ordinal of the last element of the sub array to which p_op will be applied.
        !>
        !> Output:
        !> - The result of p_cond applied to the relevant elements of p_vec succesively; it returns 0.0 if p_cond is not recognized.
        !> 
        !> Sources:
        !> - Adapted from rosettacode.org by Pablo Edronkin.        
        !> - http://www.mathcs.emory.edu/~cheung/Courses/561/Syllabus/6-Fortran/array4.html
        !> - https://en.wikipedia.org/wiki/Mean
        !>
        pure real( kind = fmath4_p1 ) function RspFSerialOp( p_cond, p_vec, p_vec_len, p_min, p_max )
        
            implicit none

            integer :: x1, x2
            integer, intent(in) :: p_vec_len, p_min, p_max
            real( kind = fmath4_p1 ) :: res, x3
            real( kind = fmath4_p1 ), dimension(p_vec_len), intent(in) :: p_vec
            character(2), intent(in) :: p_cond

            x2 = (p_max + 1) - p_min
            x3 = 1.00 / x2
            
            if (p_cond  == "**") then
                res = 1.0
            else if (p_cond == "//") then
               res = 1.0
            else if (p_cond == "gm") then
                res = 1.0
            else
                res = 0.0
            end if
            
            do x1 = p_min, p_max

                ! Sum.
                if ( p_cond == "++" ) then
                    res = res + p_vec(x1)
                 endif

                ! Arithmetic mean.
                if (p_cond == "am") then
                    res = res + p_vec(x1)
                endif

                ! Harmonic mean.
                if (p_cond == "hm") then
                    res = res + ( 1 / p_vec(x1) )
                endif
                
                ! Substraction.
                if ( p_cond == "--" ) then
                    res = res - p_vec(x1)
                endif
                
                ! Multiplication.
                if ( p_cond == "**" ) then
                    res = res * p_vec(x1)
                endif

                ! Geometric mean.
                if ( p_cond == "gm" ) then
                    res = res * p_vec(x1)
                endif
                
                ! Division.
                if ( p_cond == "//" ) then
                    res = res / p_vec(x1)
                endif

            end do

            ! Mean calculation, aritmetic.
            if (p_cond == "am") then
                !res = res / ((p_max + 1) - p_min)
                res = res / x2
            end if

            ! Mean calculation, geometric.
            if (p_cond == "gm") then
                res = res ** x3
            end if

            ! Mean calculation, harmonic.
            if (p_cond == "hm") then
                res = x2 / res
            end if
            
            !Result            
            RspFSerialOp = res 

            return
        end function


        !> Calculate the step size for numerical integration using equal step methods.
        !>
        !> Arguments:
        !> p_steps: number of steps required.
        !> p_ll: lower limit of the integration.
        !> p_ulL upper limit of the integral.
        !>
        !> Output:
        !> - Step size.
        !>
        !> Sources:
        !> - MHJ_Ch7_integration.pdf
        !> - http://www.bu.edu/tech/support/research/training-consulting/online-tutorials/mpi/example1-2/
        !>
        pure real( kind = fmath4_p1 ) function RspFStepSize( p_steps, p_ll, p_ul )
        
            implicit none

            real( kind = fmath4_p1 ), intent(in) :: p_steps, p_ll, p_ul

            RspFStepSize = ( p_ul - p_ll ) / p_steps

            return
        end function 

        
        !> Create a vector with random numbers; normal distribution.
        !>
        !> Arguments:
        !> - p_rows: number of elements that the vector shall have.
        !> 
        !> Output:
        !> - Real numbers between [0:1)
        !>
        function RspFRndDistNormal( p_rows )
        
            implicit none

            integer, intent(in) :: p_rows
            real( kind = fmath4_p1 ), dimension(p_rows) :: RspFRndDistNormal

            call random_number(RspFRndDistNormal)
            
            return
        end function


        !> Create a vector with evenly-distributed numbers using quasi random generation.
        !>
        !> Arguments:
        !> - p_rows: number of elements that the vector shall have.
        !> 
        !> Output:
        !> - Real numbers between [0:1]
        !>
        pure function RspFRndDistEven( p_rows )
        
            implicit none

            integer ::  x1
            integer, intent(in) :: p_rows
            real( kind = fmath4_p1 ) :: n_sum, step, a
            real( kind = fmath4_p1 ), dimension(p_rows) :: RspFRndDistEven

            a = 1.0
            step = a / p_rows
            n_sum = 0
            do x1 = 1, p_rows
                RspFRndDistEven(x1) = n_sum
                n_sum = n_sum + step
                if (n_sum > 1.0) then
                    n_sum = 1.0
                endif
            end do 

            return
        end function
        

        !> Numeric integration using a variant of the Monte Carlo method. This version uses no pointers 
        !> and needs only a matrix of function results and a selected number of iterations with which 
        !> the function will test results for each input value of the results matrix provided. The 
        !> area of the functions is then calculated based on the number of tests made corresponding to
        !> each x value that provide random y results below the y value for the actual function. 
        !>
        !> Arguments:
        !> - p_rm: random method used.
        !>      - "no": normal.
        !>      - "ev": even.
        !> - p_rows: number of steps used to solve the integral (row length of p_xy).
        !> - p_xy: 2D matrix of type p_xy(x,y) containing values for x and the corresponding y solved 
        !>   using a user - defined integral f(x).
        !> - p_iter: number of iterations.
        !> - p_x1: min x value.
        !> - p_x2: max x value.
        !> - p_y1: min y value.
        !> - p_y2: max y value.
        !>
        !> Output:
        !> - Value of the integral defined between p_xy(1, 1) and p_xy(p_rows, 1).
        !>
        !> Sources:
        !> - https://stackoverflow.com/questions/8612466/how-to-alias-a-function-name-in-fortran?noredirect=1&lq=1
        !> - http://ocw.uci.edu/upload/files/mae10_w2011_lecture13.pdf
        !> - https://en.wikipedia.org/wiki/Monte_Carlo_method
        !> - https://en.wikipedia.org/wiki/Monte_Carlo_algorithm
        !> - https://gcc.gnu.org/onlinedocs/gfortran/RANDOM_005fSEED.html
        !>
        !> NEEDS TESTING.
        !>
        real( kind = fmath4_p1 ) function RspFNumericIntegration1( p_rm, p_rows, p_xy, p_iter, p_x1, p_x2, p_y1, p_y2 )
        
            implicit none

            integer :: x1, x2, counted_y, total_y
            integer, intent(in) :: p_rows, p_iter
            real( kind = fmath4_p1 ) :: y_span, area_fun, xy(p_iter), a
            real( kind = fmath4_p1 ), intent(in) :: p_x1, p_x2, p_y1, p_y2, p_xy(p_rows,2)
            character(2), intent(in) :: p_rm

            !Some initial stuff.
            a = 1.0
            counted_y = 0
            total_y = p_iter * p_rows
            y_span = p_y2 - p_y1
                       
            !Calculate random values for xy()
            if ( p_rm .eq. "ev" ) then
                xy = RspFRndDistEven( p_rows )
            else
                call random_seed()
                xy = RspFRndDistNormal( p_rows )

                !Put the random numbers within the range [p_y1:p_y2]
                !do x1 = 1, p_iter
                !   xy(x1) = p_y1 + ( y_span * xy(x1) )
                !end do
                
            end if

            !For each x value in p_xy, perform certain tasks.
            do x1 = 1, p_rows
           
                !Generate p_iter random y points.
                do x2 = 1, p_iter
                    
                    !For each random y point if it is greater than the y for current p_xy point, add one to counter.
                    if ( xy(x2) .le. p_xy(x1,2) ) then
                        counted_y = counted_y + 1
                    endif 
                    
                end do
                
            end do

            !For each x in p_xy we just generated p_iter random numbers and compared then to the
            !y value of p_xy element corresponding to x. A proportion of all those random y values
            !was counted as higher or lowwer than the p_xy y value. Therefore we can calculate how many 
            !points lie within the area below the function curve and thus, estimate the integral.

            !Area used by the function.
            area_fun = ( counted_y / total_y ) * a
            
            RspFNumericIntegration1 = area_fun * RspFAreaTot( p_x1, p_x2, p_y1, p_y2 )
            
            return
        end function 


        !> Numeric integration using the Newton-Cotes equal step quadrature method (trapezoidal rule); 
        !> This version uses no pointers.
        !>
        !> Arguments:
        !> - p_rows: number of steps used to solve the integral (row length of p_xy).
        !> - p_xy: 2D matrix of type p_xy(x,y) containing values for x and the corresponding y solved 
        !> using a user - defined integral f(x).
        !>  - x: defined from [1:p_rows], contains the x input for which the user-defined function
        !>  f was solved.
        !>  - y: contains the result of f(x) 
        !>
        !> Output:
        !> - Value of the integral defined between p_xy(1, 1) and p_xy(p_rows, 1).
        !>
        !> Sources:
        !> - http://www.phy.ohiou.edu/~elster/phys5071/extras/MHJ_Ch7_integration.pdf
        !> - http://www.bu.edu/tech/support/research/training-consulting/online-tutorials/mpi/example1-2/
        !>
        pure real( kind = fmath4_p1 ) function RspFNumericIntegration2( p_rows, p_xy )
        
            implicit none

            integer :: j
            integer, intent(in) :: p_rows
            real( kind = fmath4_p1 ), intent(in) :: p_xy( p_rows, 2)
            real( kind = fmath4_p1 ) :: trap_sum, step, rows

            rows = p_rows
            step = RspFStepSize( rows, p_xy( 1, 1 ), p_xy( p_rows, 1 ) )
            trap_sum = 0.0
            do j = 1, p_rows
                trap_sum = trap_sum + p_xy( j, 2 )
            end do

            RspFNumericIntegration2 = ( trap_sum + ( p_xy( 1, 2 ) / 2 ) + ( p_xy( p_rows, 2 ) / 2 ) ) * step

            return
        end function 


        !> Calculates the total area of a 2D rectangle given four coordinates that inscribe it.
        !>
        !> Arguments:
        !> - p_x1: min x value.
        !> - p_x2: max x value.
        !> - p_y1: min y value.
        !> - p_y2: max y value.
        !>
        !> Output:
        !> -  Area of the rectangle.
        !>
        pure real( kind = fmath4_p1 ) function RspFAreaTot( p_x1, p_x2, p_y1, p_y2 )
        
            implicit none

            real( kind = fmath4_p1 ), intent(in) :: p_x1, p_x2, p_y1, p_y2

            RspFAreaTot = ( p_x2 - p_x1 ) * ( p_y2 - p_y1 )

            return
        end function 


        !> Finds if a point in a plane is within the rectangle defined by the coordinates passed
        !> as arguments.
        !>
        !> Arguments:
        !> - p_x1: min x value.
        !> - p_x2: max x value.
        !> - p_y1: min y value.
        !> - p_y2: max y value.
        !> - p_px: x coordinate of point.
        !> - p_py: y coordinate of point.
        !>
        !> Output:
        !> -  1 if point is inside rectangle, 0 otherwise.        
        !>
        pure real( kind = fmath4_p1 ) function RspFIsInsideRectangle( p_x1, p_x2, p_y1, p_y2, p_px, p_py )
        
            implicit none

            real( kind = fmath4_p1 ), intent(in) :: p_x1, p_x2, p_y1, p_y2, p_px, p_py
            
            if ( ( ( p_px <= p_x2 ) .and. ( p_px >= p_x1 ) ) .and. ( ( p_py <= p_y2 ) .and. ( p_py >= p_y1 ) ) ) then
                RspFIsInsideRectangle = 1.0
            else
                RspFIsInsideRectangle = 0.0
            endif

            return
        end function 


        !> Calculates the factorial of the absolute value of input number.
        !>
        !> Arguments:
        !> - p_x1: number.
        !>
        !> Output:
        !> -  Area of the rectangle.
        !>
        pure recursive real( kind = fmath4_p1 ) function RspFFact( p_x1 ) result(res)
        
            implicit none

            real( kind = fmath4_p1 ), intent(in) :: p_x1

            if ( abs(p_x1) <= 1 ) then
               res = 1
            else
               res = p_x1 * RspFFact(p_x1 - 1)
            end if
            
            return
        end function 


        !> Calculates the inverse exponent of a number using 1 / p_x1**p_x2.
        !>
        !> Arguments:
        !> - p_x1: base.
        !> - p_x2: exponent.
        !>
        !> Output:
        !> -  Area of the rectangle.
        !>
        pure real( kind = fmath4_p1 ) function RspFInvExp( p_x1, p_x2 )
        
            implicit none

            real( kind = fmath4_p1 ), intent(in) :: p_x1, p_x2

            RspFInvExp = 1 / ( p_x1**p_x2 )

            return
        end function 


        !> Calculates exp(p_x1, p_x2) + p_x3.
        !>
        !> Arguments:
        !> - p_x1: base.
        !> - p_x2: exponent.
        !> - p_x3: summand.
        !>
        pure real( kind = fmath4_p1 ) function RspFS1( p_x1, p_x2, p_x3 )
        
            implicit none

            real( kind = fmath4_p1 ), intent(in) :: p_x1, p_x2, p_x3

            RspFS1 = p_x1**p_x2 + p_x3

            return
        end function         


        !> Performs p_x1**p_x2 [p_cond] p_x3**p_x4
        !>
        !> Arguments:
        !> - p_cond: operation to be applied, as per the following list:
        !>      - "+": summation.
        !>      - "-": substraction.
        !>      - "*": multiplication.
        !>      - "/": division.       
        !> - p_x1: first operand.
        !> - p_x2: second operand.
        !> - p_x3: third operand.
        !> - p_x4: fourth operand.        
        !>
        !> Output:
        !> - Res = p_x1**p_x2 [p_cond] p_x3**p_x4
        !>
        pure real( kind = fmath4_p1 ) function RspFS2( p_cond, p_x1, p_x2, p_x3, p_x4 )
        
            implicit none

            real( kind = fmath4_p1 ), intent(in) :: p_x1, p_x2, p_x3, p_x4
            real( kind = fmath4_p1 ) :: res, x12, x34
            character(1), intent(in) :: p_cond

            x12 = p_x1 ** p_x2
            x34 = p_x3 ** p_x4
            
            if (p_cond  == "+") then
               res = x12 + x34
            else if (p_cond == "-") then
               res = x12 - x34
            else if (p_cond == "*") then
               res = x12 * x34
            else if (p_cond == "/") then
               res = x12 / x34               
            else
               res = 0.0
            end if
                       
            !Result            
            RspFS2 = res 

            return
        end function        

        
        !==============================================================================
        ! Test.
        

        !> This function contains all tests for fmath4 functions and subroutines.
        !>
        !> Output:
        !> - Test results for fmath4.
        !>       
        subroutine RspFTestFmath4All()
            
            implicit none

            call system("clear")
            call RspFTestFmath41()
            write(*,*)
            call RspFTestFmath42()
            write(*,*)
            call RspFTestFmath43()
            write(*,*)
            call RspFTestFmath44()
            write(*,*)
            call RspFTestFmath45()
            write(*,*)
            call RspFTestFmath46()
            
        end subroutine


        !> Test for fmath4 subroutines and functions.
        !> - RspFNthRoot(...)
        !>
        !> Output:
        !> - Test results for fmath4.
        !>       
        subroutine RspFTestFmath41()
            
            implicit none
            
            real( kind = fmath4_p1 ) :: n, r
            integer :: x1
            
            n = 1024.00
            call RspFLine()
            call RspFCommentEn( "Begin RspFTestFmath41" )
            call RspFComment( "Testing RspFNthRoot(...)" ) 
              
            do x1 = 1,10
                r = x1 * 1.0
                write(*,*) n, "**(1.0/", r, ") = ", RspFNthRoot( n, r )
                write(*,*) " "
            end do
                        
            call RspFCommentEn( "End RspFTestFmath41" )
            
        end subroutine


        !> Test for fmath4 subroutines and functions.
        !> - RspFPappusChainRadiiGenerator(...)
        !>
        !> Output:
        !> - Test results for fmath4.
        !>       
        subroutine RspFTestFmath42()
            
            implicit none

            real( kind = fmath4_p1 ) :: a, b, c
            integer :: x1
            
            a = 2
            b = 1
            call RspFLine()
            call RspFCommentEn( "Begin RspFTestFmath42" )
            call RspFComment( "Testing RspFPappusChainRadiiGenerator(...)" ) 
              
            do x1 = 1,10
                c = x1 * 1.0
                write(*,*) "a =   ", a
                write(*,*) "b =   ", b
                write(*,*) "x1 =  ", x1
                write(*,*) "res = ", RspFPappusChainRadiiGenerator( a, b, c )
                write(*,*)
            end do
                        
            call RspFCommentEn( "End RspFTestFmath42" )
            
        end subroutine


        !> Test for fmath4 subroutines and functions.
        !> - RspFMersenneNumberGenerator(...)
        !> - RspFNormalize2MinMaxInterval(...)
        !> - RspFSolowSwanModel(...)
        !> - RspFDenormalizeFromMinMaxInterval(...)
        !> - RspFZeta(...)
        !> - RspFRoicf(...)
        !> - RspFPutInRange(...)
        !> - RspFPir(...)
        !> - RspFTol(...)
        !> - RspFSerialOp(...)
        !> - RspFStepSize(...)
        !> - RspFRndDistNormal(...)
        !> - RspFRndDistEven(...)
        !> - 
        !> Output:
        !> - Test results for fmath4.
        !>       
        subroutine RspFTestFmath43()
            
            implicit none

            integer :: x1, n
            real( kind = fmath4_p1 ) :: a
            real( kind = fmath4_p1 ), dimension(10) :: vec
            
            a = 1.0
            n = 10
            
            call RspFLine()
            call RspFCommentEn( "Begin RspFTestFmath43" )

            call RspFComment( "Testing RspFMersenneNumberGenerator(...)" ) 
            do x1 = 1,n
                write(*,*) a * x1
                write(*,*) RspFMersenneNumberGenerator( a * x1 )
                write(*,*)      
            end do

            call RspFComment( "Testing RspFNormalize2MinMaxInterval(...)" ) 
            do x1 = 1,n
                write(*,*) a * x1, a * 0.0, a * 1.0, a * 0.0, a * 10.0
                write(*,*) RspFNormalize2MinMaxInterval( a * x1, a * 0.0, a * 1.0, a * 0.0, a * 10.0 )
                write(*,*)      
            end do

            call RspFComment( "Testing RspFSolowSwanModel(...)" ) 
            do x1 = 1,n
                write(*,*) a * 1.0, a * 2.0, a * 3.0, a * x1
                write(*,*) RspFSolowSwanModel( a * 1.0, a * 2.0, a * 3.0, a * x1 )
                write(*,*)      
            end do

            call RspFComment( "Testing RspFDenormalizeFromMinMaxInterval(...)" ) 
            do x1 = 1,n
                write(*,*) a * x1, a * 0.0, a * 1.0, a * 0.0, a * 10.0
                write(*,*) RspFDenormalizeFromMinMaxInterval( a * x1, a * 0.0, a * 1.0, a * 0.0, a * 10.0 )
                write(*,*)      
            end do

            call RspFComment( "Testing RspFZeta(...)" ) 
            do x1 = 1,n
                write(*,*) a * x1, 10
                write(*,*) RspFZeta( a * x1, 10 )
                write(*,*)      
            end do            

            call RspFComment( "Testing RspFRoicf(...)" ) 
            do x1 = 1,n
                write(*,*) "<", a * x1, a * 5.0, a * 0.0, a * 1.0
                write(*,*) RspFRoicf( "<", a * x1, a * 5.0, a * 0.0, a * 1.0 )
                write(*,*)      
            end do

            call RspFComment( "Testing RspFPutInRange(...)" ) 
            do x1 = 1,n
                write(*,*) "<", a * x1
                write(*,*) RspFPutInRange( a * 1.0, a * 5.0, a * x1 )
                write(*,*)      
            end do

            call RspFComment( "Testing RspFPir(...)" ) 
            do x1 = 1,n
                write(*,*) a * x1
                write(*,*) RspFPir( a * 1.0, a * 2.0, a * 5.0, a * x1 )
                write(*,*)      
            end do

            call RspFComment( "Testing RspFTol(...)" ) 
            do x1 = 1,n
                write(*,*) a * x1
                write(*,*) RspFTol( a * 1.0 )
                write(*,*)      
            end do
          
            call RspFComment( "Testing RspFSerialOp(...)" ) 
            do x1 = 1,n
                vec(x1) = x1 * a
            end do
            do x1 = 1,n
                write(*,*) x1, vec(x1)
            end do
            write(*,*) 
            write(*,*) " ++ = ", RspFSerialOp( "++", vec, n, 1, 8 )
            write(*,*) " -- = ", RspFSerialOp( "--", vec, n, 1, 8 )
            write(*,*) " ** = ", RspFSerialOp( "**", vec, n, 1, 8 )
            write(*,*) " // = ", RspFSerialOp( "//", vec, n, 1, 8 )
            write(*,*) " am = ", RspFSerialOp( "am", vec, n, 1, 8 )
            write(*,*) " gm = ", RspFSerialOp( "gm", vec, n, 1, 8 )
            write(*,*) " hm = ", RspFSerialOp( "hm", vec, n, 1, 8 )
            write(*,*)

            call RspFComment( "Testing RspFStepSize(...)" ) 
            do x1 = 1,n
                write(*,*) a * x1, n * a
                write(*,*) RspFStepSize( a * x1, a * 0.00, a * n )
                write(*,*)      
            end do

            call RspFComment( "Testing RspFRndDistNormal(...)" )
            vec = RspFRndDistNormal( n )
            do x1 = 1,n
                write(*,*) vec(x1)
                write(*,*)      
            end do

            call RspFComment( "Testing RspFRndDistEven(...)" )
            vec = RspFRndDistEven( n )
            do x1 = 1,n
                write(*,*) vec(x1)
                write(*,*)      
            end do
                        
            call RspFCommentEn( "End RspFTestFmath43" )
            
        end subroutine


        !> Test for fmath4 subroutines and functions.
        !>
        !> - RspFNumericIntegration1(...)
        !> - RspFNumericIntegration2(...)
        !>
        !> Output:
        !> - Test results for fmath4.
        !>       
        subroutine RspFTestFmath44()
            
            implicit none

            integer :: x1, n
            real( kind = fmath4_p1 ) :: a, b            
            real( kind = fmath4_p1 ), dimension(100,2) :: xy

            a = 1.0
            b = 100.00
            n = 100
            
            call RspFLine()
            call RspFCommentEn( "Begin RspFTestFmath44" )

            !Create matrix for integrals
            do x1 = 1,n
               xy(x1,1) = x1
               xy(x1,2) = x1
            end do
            
            call RspFComment( "Testing RspFNumericIntegration1(...)" )
            write(*,*)
            call RspFComment( "2D matrix with sample data:" )
            do x1 = 1,n
               write(*,*) xy(x1,1), xy(x1,2)
            end do
            write(*,*)
            write(*,*) "no", n, "xy", n, a * 0.0, a * b, a * 0.0, a * b
            write(*,*)
            write(*,*) "Area = ", RspFNumericIntegration1( "no", n, xy, n * n, a * 0.0, a * b, a * 0.0, a * b )
            write(*,*)
            write(*,*) "ev", n, "xy", n, a * 0.0, a * 1.0, a * 0.0, a * 1.0
            write(*,*)
            write(*,*) "Area = ", RspFNumericIntegration1( "ev", n, xy, n * n, a * 0.0, a * b, a * 0.0, a * b )
            write(*,*) 

            call RspFComment( "Testing RspFNumericIntegration2(...)" )
            write(*,*)
            call RspFComment( "2D matrix with sample data:" )
            do x1 = 1,n
               write(*,*) xy(x1,1), xy(x1,2)
            end do 
            write(*,*) n
            write(*,*) "Area = ", RspFNumericIntegration2( n, xy )           
            write(*,*)
            
            call RspFCommentEn( "End RspFTestFmath44" )

        end subroutine


        !> Test for fmath4 subroutines and functions.
        !>
        !> - RspFAreaTot(...)  
        !> - RspFIsInsideRectangle(...)
        !>
        !> Output:
        !> - Test results for fmath4.
        !>       
        subroutine RspFTestFmath45()
            
            implicit none

            integer :: x1, n, y1
            real( kind = fmath4_p1 ) :: a
            
            a = 1.0
            n = 10
            
            call RspFLine()
            call RspFCommentEn( "Begin RspFTestFmath45" )

            call RspFComment( "Testing RspFAreaTot(...)" ) 
            do x1 = 1,n
                y1 = x1
                write(*,*) ((a * x1 * 2) - ( a * x1)), " x ", ((a * y1 * 2) - ( a * y1)) 
                write(*,*) RspFAreaTot( a * x1, a * x1 * 2, a * y1, a * y1 * 2 )
                write(*,*)      
            end do

            call RspFComment( "Testing RspFIsInsideRectangle(...)" ) 
            do x1 = 1,n
                y1 = x1
                write(*,*) a * 2, a * 7, a * 2, a * 7, a * x1 
                write(*,*) "Res = ", RspFIsInsideRectangle( a * 2, a * 7, a * 2, a * 7, a * x1, a * y1 )
                write(*,*)                
            end do

            call RspFComment( "Testing RspFFact(...)" ) 
            do x1 = 1,n
                write(*,*) a * x1
                write(*,*) "Res = ", RspFFact( a * x1 )
                write(*,*)                
            end do

            call RspFComment( "Testing RspFInvExp(...)" ) 
            do x1 = 1,n
                write(*,*) a * x1
                write(*,*) "Res = ", RspFInvExp( a * x1, ((a * x1) - 1) )
                write(*,*)                
            end do
           
            call RspFCommentEn( "End RspFTestFmath45" )
            
        end subroutine        


        !> Test for fmath4 subroutines and functions.
        !>
        !> - RspFS1(...)  
        !> - RspFS2(...)
        !>
        !> Output:
        !> - Test results for fmath4.
        !>       
        subroutine RspFTestFmath46()
            
            implicit none

            integer :: x1, n
            real( kind = fmath4_p1 ) :: a
            
            a = 1.0
            n = 10
            
            call RspFLine()
            call RspFCommentEn( "Begin RspFTestFmath46" )

            call RspFComment( "Testing RspFS1(...)" ) 
            do x1 = 1,n
                write(*,*) a * x1
                write(*,*) "Res = ", RspFS1( a * x1, a * x1, a * x1 )
                write(*,*)                
            end do

            call RspFComment( "Testing RspFS2(\'+\'...)" ) 
            do x1 = 1,n
                write(*,*) a * x1
                write(*,*) "Res = ", RspFS2( "+", a * x1, a * x1, a * x1, a * x1 )
                write(*,*)                
            end do

            call RspFComment( "Testing RspFS2(\'-\'...)" ) 
            do x1 = 1,n
                write(*,*) a * x1
                write(*,*) "Res = ", RspFS2( "-", a * x1, a * x1, a * x1, a * x1 )
                write(*,*)                
            end do

            call RspFComment( "Testing RspFS2(\'*\'...)" ) 
            do x1 = 1,n
                write(*,*) a * x1
                write(*,*) "Res = ", RspFS2( "*", a * x1, a * x1, a * x1, a * x1 )
                write(*,*)                
            end do

            call RspFComment( "Testing RspFS2(\'/\'...)" ) 
            do x1 = 1,n
                write(*,*) a * x1
                write(*,*) "Res = ", RspFS2( "/", a * x1, a * x1, a * x1, a * x1 )
                write(*,*)                
            end do
            
            call RspFCommentEn( "End RspFTestFmath46" )
            
        end subroutine

        
end module

