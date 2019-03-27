!> =============================================================================
!> 
!> fmath3.f95 - Physical constants.
!> 
!> =============================================================================
!> 
!>  Copyright (C) 2018 - 2019  Pablo Edronkin (pablo.edronkin at yahoo.com)
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
!> - mpif90 -std='gnu' -c fmath3.f95 -O3 -march=native -Wall -Wextra -fopenmp 
!>
module fmath3

    use fmath4
    implicit none

    ! Setting a platform-independent floating point precision.
    integer, parameter :: fmath3_p1 = selected_real_kind( 10,300 )
    
    !------------------------------------------------------------------------------
    contains

        !> Returns the value of a requested constant.
        !>
        !> Arguments:
        !> - p_k: passed as literal strings (i.e. 'Em' for Earth's mass).
        !>  - G: Gravitational constant expressed in N * m**2 * kg**-2.
        !>  - c: Speed of light in km/s.
        !>  - Em: Earth's mass in kg.
        !>  - Sm: Sun's mass in kg.
        !>  - ak: auinkm, km per astronomical unit.
        !>  - pk: psinkm, km per parsec.
        !>  - sy: Seconds per year.
        !>  - sl: Stability limit for Lagrangian points L3 and L4.
        !>
        !> Output:
        !> - A numeric value representing the constant in question.
        !>
        !> Sources:
        !> - [1]- http://www.astro.wisc.edu/~dolan/constants.html
        !> - [2]- https://en.wikipedia.org/wiki/Planetary_mass
        !> - [3]- https://es.wikipedia.org/wiki/Unidad_astron%C3%B3mica
        !> - [4]- https://en.wikipedia.org/wiki/Solar_radius
        !> - [5]- https://en.wikipedia.org/wiki/Astronomical_constant
        !> - [6]- http://astronomyonline.org/Science/SmallAngleFormula.asp
        !> - [7]- https://en.wikipedia.org/wiki/Luminosity
        !> - [8]- https://en.wikipedia.org/wiki/Lagrangian_point
        !> - [9]- https://en.wikipedia.org/wiki/Observable_universe
        !> - [10]- https://en.wikibooks.org/wiki/Introduction_to_Astrophysics/Stars/Luminosity 
        !>
        !> NEEDS TESTING.
        !>
        pure real( kind = fmath3_p1 ) function RspFConst( p_k )
        
            implicit none
            
            real( kind = fmath3_p1 ) :: res
            character( len = * ), intent(in) :: p_k
            
            res = 0.0

            !------------------------------------------------------------------------------
            ! Time constants. 
           
            ! Seconds per year.
            if ( p_k == "sy" ) then
                res = 3.1538E007   
            endif            

            ! Seconds per minute.
            if ( p_k == "f_t_sxm" ) then
                res = 60   
            endif
            
            ! Minutes per hour.
            if ( p_k == "f_t_mxh" ) then
                res = 60   
            endif

            ! Hours per day.
            if ( p_k == "f_t_hxd" ) then
                res = 24   
            endif
            
            ! Days per week.
            if ( p_k == "f_t_dxw" ) then
                res = 7   
            endif

            ! Weeks per month.
            if ( p_k == "f_t_wxmo" ) then
                res = 4   
            endif

            ! Months per year.
            if ( p_k == "f_t_moxy" ) then
                res = 12   
            endif

            ! Hours per day.
            if ( p_k == "f_t_wxy" ) then
                res = 52   
            endif

            ! Days per year.
            if ( p_k == "f_t_dxy" ) then
                res = 356   
            endif

            ! Years per decade.
            if ( p_k == "f_t_yxde" ) then
                res = 10   
            endif
            
            ! Decades per century.
            if ( p_k == "f_t_dexc" ) then
                res = 10   
            endif
        

            !------------------------------------------------------------------------------
            ! Physics constants. 
            
            ! Gravitational constant in Newtons [1].
            if ( p_k == "G" ) then
                res = 6.6725985E-011
            endif
            
            ! Chandrasekhar limit.
            if ( p_k == "Ch" ) then
                res = 1.4
            endif

            ! Speed of light [1].
            if ( p_k == "c" ) then
                res = 299792.458   
            endif
            
            ! Mass of Earth.
            if ( p_k == "Em" ) then
                res = 5.97219E24   
            endif

            ! Mass of Jupiter [2].
            if ( p_k == "Jm" ) then
                res = 1.89852E27   
            endif
            
            ! Mass of the Sun.
            if ( p_k == "Sm" ) then
                res = 1.9891E30   
            endif

            ! Mass of the universe (kg) [9].
            if ( p_k == "Um" ) then
                !res = 10.0E53   
            endif
            
            ! Mean eq radius of Earth [1].
            if ( p_k == "Er" ) then
                res = 6378   
            endif

            ! Mean eq radius of Jupiter [Jr].
            if ( p_k == "Jr" ) then
                res = 71400   
            endif
            
            ! Mean eq radius of the Sun [4].
            if ( p_k == "Sr" ) then
                res = 6.955E5   
            endif

            ! Radius of the universe (km) [9].
            if ( p_k == "Ur" ) then
                res = 8.8E23   
            endif
            
            ! auinkm [3].
            if ( p_k == "ak" ) then
                res = 149597870.7   
            endif

            ! psinkm.
            if ( p_k == "pk" ) then
                res = 3.08567758E013   
            endif

            ! Arcseconds per radian (radinarcs) [6].
            if ( p_k == "ra" ) then
                res = 206265   
            endif

            ! Stefan-Boltzmann constant. W * m**-2 * K**-4 [7].
            if ( p_k == "SB" ) then
                res = 5.670373E8   
            endif

            ! Stability of Lagrangian L3 and L4.
            if ( p_k == "sl" ) then
                res = 24.96   
            endif

            !------------------------------------------------------------------------------
            ! Plank units. 

            ! Planck time (s).
            if ( p_k == "tP" ) then
                res = 5.39121E-44 
            endif

            ! Planck length (m).
            if ( p_k == "lP" ) then
                res = 1.61624E-35   
            endif

            ! Planck mass (kg).
            if ( p_k == "mP" ) then
                res = 2.17645E-8   
            endif

            ! Planck charge (C).
            if ( p_k == "cP" ) then
                res = 1.8755459E-18   
            endif

            ! Planck temperature (K).
            if ( p_k == "TP" ) then
                res = 1.41679E32   
            endif

            !------------------------------------------------------------------------------
            ! Aerodynamics

            ! R const of air.
            if ( p_k == "R" ) then
                res = 287.00 ! 287.00J/KgK   
            endif
            
            ! Rho const of air at sea level.
            if ( p_k == "r0" ) then
                res = 1.225 ! Pascal   
            endif
            
            ! Sound speed at sea level.
            if ( p_k == "ss" ) then
                res =340.29 ! m/s   
            endif
            
            ! Ratio of specific heat of air.
            if ( p_k == "Ua" ) then
                res = 1.4 ! Dimensionless   
            endif
            
            ! Gravity acceleration on Earth.
            if ( p_k == "gE" ) then
                res = 9.80665 ! m/(s**2)   
            endif
            
            ! Molar mass air.
            if ( p_k == "Ma" ) then
                res = 28.97 ! g/mol   
            endif
            
            ! Molar mass hidrogen.
            if ( p_k == "Mh" ) then
                res = 2.0 ! g/mol   
            endif
            
            ! Molar mass helium.
            if ( p_k == "Me" ) then
                res = 4.0 ! g/mol   
            endif
            
            ! Molar mass nitrogen.
            if ( p_k == "Mn" ) then
                res = 28.0 ! g/mol   
            endif
            
            ! Surface temperature.
            if ( p_k == "T0" ) then
                res = 288.15 ! 288.15/K   
            endif
            
            ! Surface pressure.
            if ( p_k == "P0" ) then
                res = 101325 !Pa   
            endif
            ! Gamma factor for Mach calculation.
            if ( p_k == "gf" ) then
                res = 1.4   
            endif

            ! Return results.
            RspFConst = res

            return
        end function



        !==============================================================================
        ! Test.


        !> This function contains all tests for fmath3 functions and subroutines.
        !>
        !> Output:
        !> - Test results for fmath3.
        !>       
        subroutine RspFTestFmath3All()
            
            implicit none

            call system("clear")
            call RspFTestFmath31()
            write(*,*)
            
        end subroutine

        !> Test 1 for fmath3 subroutines and functions.
        !>
        !> Output:
        !> - Test results for fmath3.
        !>       
        subroutine RspFTestFmath31()
            
            implicit none
            
            integer :: x1
            real( kind = fmath3_p1 ) :: res
            character(10) :: v

            call RspFLine()            
            call RspFCommentEn( "Begin RspFTestFmath31" )
            call RspFComment( "Testing RspFConst(...)" )
            do x1 = 1,44

                if ( x1 == 1) then
                    v = "sy"   
                else if ( x1 == 2 ) then            
                    v = "f_t_sxm"
                else if ( x1 == 3 ) then 
                    v = "f_t_mxh"
                else if ( x1 == 4 ) then 
                    v = "f_t_hxd"
                else if ( x1 == 5 ) then 
                    v = "f_t_dxw"
                else if ( x1 == 6 ) then 
                    v = "f_t_wxmo"
                else if ( x1 == 7 ) then 
                    v = "f_t_moxy"
                else if ( x1 == 9 ) then 
                    v = "f_t_wxy"
                else if ( x1 == 9 ) then 
                    v = "f_t_dxy"
                else if ( x1 == 10 ) then 
                    v = "f_t_yxde"
                else if ( x1 == 11 ) then 
                    v = "f_t_dexc"
                else if ( x1 == 12 ) then 
                    v = "G"
                else if ( x1 == 13 ) then 
                    v = "Ch"
                else if ( x1 == 14 ) then 
                    v = "c"
                else if ( x1 == 15 ) then 
                    v = "Em"
                else if ( x1 == 16 ) then 
                    v = "Jm"
                else if ( x1 == 17 ) then 
                    v = "Sm"
                else if ( x1 == 18 ) then 
                    v = "Um"
                else if ( x1 == 19 ) then 
                    v = "Er"
                else if ( x1 == 20 ) then 
                    v = "Jr"
                else if ( x1 == 21 ) then 
                    v = "Sr"
                else if ( x1 == 22 ) then 
                    v = "Ur"
                else if ( x1 == 23 ) then 
                    v = "ak"
                else if ( x1 == 24 ) then 
                    v = "pk"
                else if ( x1 == 25 ) then 
                    v = "ra"
                else if ( x1 == 26 ) then 
                    v = "SB"
                else if ( x1 == 27 ) then 
                    v = "sl"
                else if ( x1 == 28 ) then 
                    v = "tP"
                else if ( x1 == 29 ) then 
                    v = "lP"
                else if ( x1 == 30 ) then 
                    v = "mP"
                else if ( x1 == 31 ) then 
                    v = "cP"
                else if ( x1 == 32 ) then 
                    v = "TP"
                else if ( x1 == 33 ) then 
                    v = "R"
                else if ( x1 == 34 ) then 
                    v = "r0"
                else if ( x1 == 35 ) then 
                    v = "ss"
                else if ( x1 == 36 ) then
                    v = "Ua"
                else if ( x1 == 37 ) then 
                    v = "gE"
                else if ( x1 == 38 ) then 
                    v = "Ma"
                else if ( x1 == 39 ) then 
                    v = "Mh"
                else if ( x1 == 40 ) then 
                    v = "Me"
                else if ( x1 == 41 ) then 
                    v = "Mn"
                else if ( x1 == 42 ) then 
                    v = "T0"
                else if ( x1 == 43 ) then 
                    v = "P0"
                else if ( x1 == 44 ) then 
                    v = "gf"                    
                end if 
                
                res = RspFConst( v )
                write (*,*) v, " = ", res 
            
            end do
            call RspFCommentEn( "End RspFTestFmath31" )
            
        end subroutine
    
end module

