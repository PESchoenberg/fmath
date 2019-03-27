!> =============================================================================
!> 
!> fmath5.f95 - Aerospace engineering functions and subroutines.
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
!> Modified:
!> - Jul 5 2018.
!>
!> Compilation:
!> - ./cfmath.sh or 
!> - mpif90 -std='gnu' -c fmath5.f95 -O3 -march=native -Wall -Wextra -fopenmp -fdec-math
!>
module fmath5

    use fmath3
    use fmath4

    implicit none

    ! Setting a platform-independent floating point precision.
    integer, parameter :: fmath5_p1 = selected_real_kind( 10,300 )
    
    !------------------------------------------------------------------------------
    contains


        !> Atmospheric calculator 3.
        !>
        !> Arguments:
        !> - p_alt: altitude.
        !>
        !> Output:
        !> - An array containing:
        !> - [1] p_alt
        !> - [2] t
        !> - [3] P
        !> - [4] rho
        !>
        pure function RspFAtmocal3( p_alt )
        
            implicit none

            real( kind = fmath5_p1 ), dimension(4) :: RspFAtmocal3, res, tt
            real( kind = fmath5_p1 ), dimension(5) :: pp
            real( kind = fmath5_p1 ) :: g, t, p, r, rho, a, b
            real( kind = fmath5_p1 ), intent(in) :: p_alt

            g = RspFConst("gE") !!
            t = RspFConst("T0") !!
            p = RspFConst("P0") !!
            r = RspFConst("R") !!
            rho = RspFConst("r0") !!
            pp(1:5) = 0
            tt(1:4) = 0

            !TROPOSHERE
            a = -.0065 !Lapse rate
            b = -g / (a * r)
            if ( p_alt >= 11000 ) then
                tt(1) = t + a * ( 11000 ) 
                pp(1) = p * (tt(1) / t )**b 
            else if ( ( p_alt >= 0 ) .and. ( p_alt < 11000 ) ) then 
                tt(1) = t + a * ( p_alt ) 
                pp(1) = p * ( tt(1) / t )**b 
                rho = pp(1) / ( r * tt(1) ) 
                t = tt(1);
                p = pp(1);    
            end if

            !STRATOSPHERE
            a = 0.0
            b = -g / (a * r)
            if ( p_alt >= 20000 ) then
                tt(2) = tt(1) + a * (9000) 
                pp(2) = pp(1) * exp( -g / ( r * tt(1)) * 9000) 
            else if( p_alt > 11000 ) then
                tt(2) = tt(1) + a * ( p_alt - 11000 ) 
                pp(2) = pp(1) * exp( -g / ( r * tt(1) ) * ( p_alt - 11000) ) 
                rho = pp(2) / ( r * tt(2) )   
                t = tt(2);
                p = pp(2);  
            end if

            !MESOSPHERE
            a = 0.001;
            b = -g / (a * r) 
            if ( p_alt >= 32000 ) then
                tt(3) = tt(2) + a * ( 12000 )
                pp(3) = pp(2) * ( tt(3) / tt(2) )**b 
            else if ( p_alt > 20000 ) then
                tt(3) = tt(2) + a * ( p_alt - 20000 )
                pp(3) = pp(2) * ( tt(3) / tt(2) )**b
                rho = pp(3) / ( r * tt(3) )
                t = tt(3)
                p = pp(3)    
            end if

            !TERMOSPHERE
            a = 0.0028
            b = -g / (a * r) 
            if ( p_alt >= 47000 ) then
                tt(4) = tt(3) + a * ( 15000 ) 
                pp(4) = pp(3) * ( tt(4) / tt(3) )**b 
            else if ( p_alt > 32000 ) then
                tt(4) = tt(3) + a * ( p_alt - 32000 ) 
                pp(4) = pp(3) * ( tt(4) / tt(3) )**b 
                rho = pp(4) / ( r * tt(4) )
                t = tt(4)
                p = pp(4)  
            end if
            if ( p_alt > 47000 ) then
                pp(5) = pp(4) * exp(-g / ( r * tt(4) ) * ( p_alt - 47000 ) ) 
                rho = pp(5) / ( r * tt(4) ) 
                t = tt(4)
                p = pp(5)
            endif

            res(1) = p_alt
            res(2) = t
            res(3) = p
            res(4) = rho

            RspFAtmocal3 = res

            return
        end function 


        !> Estimates the required bank angle given a certain true air speed in knots.
        !>
        !> Arguments:
        !> - p_ts_kts: true airspeed in knots. 
        !>
        !> Output:
        !> - Estimated bank angle.
        !>
        !> Sources:
        !> - https://en.wikipedia.org/wiki/Standard_rate_turn
        !>
        real( kind = fmath5_p1 ) function RspFBankAngleEstimated( p_tas_kts )
        
            implicit none

            real( kind = fmath5_p1 ) :: p_tas_kts

            RspFBankAngleEstimated = ( p_tas_kts / 10.0 ) + 5.0

            return
        end function 


        !> Calculates the circumferential stress on a circular fuselage.
        !>
        !> Arguments:
        !> - p_in: prespsure inside the fuselage. 
        !> - p_out: pressure outside the fuselage. 
        !> - p_r: interior radius of the fuselage.
        !> - p_t: thickness of the fuselage skin.
        !>
        !> Output:
        !> - Circumferential or hub stress.
        !>
        pure real( kind = fmath5_p1 ) function RspFCircumferentialStress( p_in, p_out, p_r, p_t )
        
            implicit none

            real( kind = fmath5_p1 ), intent(in) :: p_in, p_out, p_r, p_t

            RspFCircumferentialStress = ( (p_in - p_out) * p_r ) / p_t

            return
        end function 


        !> Simple proportional controller. 
        !>
        !> Arguments:
        !> - p_pir: 1 to put in range, 0 otherwise.
        !> - p_min: lower limit of the range.
        !> - p_max: higher limit of the range.
        !> - p_kp: proportional gain.
        !> - p_de: instantaneous error.
        !>
        !> Output:
        !> - P value that should be passed to the controller.
        !> 
        !> Sources:
        !> - https://en.wikipedia.org/wiki/PID_controller
        !>
        pure real( kind = fmath5_p1 ) function RspFPidP1( p_pir, p_min, p_max, p_kp, p_de )
        
            implicit none

            real( kind = fmath5_p1 ), intent(in) :: p_pir, p_min, p_max, p_kp, p_de

            RspFPidP1 = RspFPir( p_pir, p_min, p_max, ( p_kp * p_de ) )

            return
        end function 

        
        !> Simple integral controller. 
        !>
        !> Arguments:
        !> - p_pir: 1 to put in range, 0 otherwise.
        !> - p_min: lower limit of the range.
        !> - p_max: higher limit of the range.
        !> - p_ki: proportional gain.
        !> - p_ai: integral up to iteration - 1
        !> - p_de: instantaneous error.
        !> - p_dt: time variation.
        !>
        !> Output:
        !> - I Value up to present interation that should be passed to a PID controller.
        !>
        !> Sources:
        !> - https://en.wikipedia.org/wiki/PID_controller
        !>
        !> NEEDS TESTING.
        !>
        pure real( kind = fmath5_p1 ) function RspFPidI1( p_pir, p_min, p_max, p_ki, p_ai, p_de, p_dt )
        
            implicit none

            real( kind = fmath5_p1 ), intent(in) :: p_pir, p_min, p_max, p_ki, p_ai, p_de, p_dt

            RspFPidI1 = RspFPir( p_pir, p_min, p_max, ( p_ki * ( p_ai + (p_de * p_dt) ) ) )

            return
        end function 


        !> Simple derivative controller. 
        !>
        !> Arguments:
        !> - p_pir: 1 to put in range, 0 otherwise.
        !> - p_min: lower limit of the range.
        !> - p_max: higher limit of the range.
        !> - p_kd: proportional gain.
        !> - p_de: error variation.
        !> - p_dt: time variation.
        !>
        !> Output:
        !> - D Value that should be passed to a PID controller.
        !>
        !> Sources:
        !> - https://en.wikipedia.org/wiki/PID_controller
        !>
        !> NEEDS TESTING.
        !>
        pure real( kind = fmath5_p1 ) function RspFPidD1( p_pir, p_min, p_max, p_kd, p_de, p_dt )
        
            implicit none

            real( kind = fmath5_p1 ), intent(in) :: p_pir, p_min, p_max, p_kd, p_de, p_dt

            RspFPidD1 = RspFPir( p_pir, p_min, p_max, ( p_kd * ( p_de / p_dt ) ) )

            return
        end function 


        !> Equation of state.
        !>
        !> Arguments:
        !> - p_density: in kg/(m**3)
        !> - p_temperature: environmental temperature, in Kelvins.
        !> - p_r: specific gas constant. Is the universal gas law R divided by the gas' molar mass. 
        !>  In the case of air, it will be 287.00 J/KgK
        !>
        !> Output:
        !> - Pressure, in Pa.
        !>
        pure real( kind = fmath5_p1 ) function RspFEquationOfState( p_density, p_r, p_temperature )
        
            implicit none

            real( kind = fmath5_p1 ), intent(in) :: p_density, p_r, p_temperature

            RspFEquationOfState = p_density * p_R * p_temperature

            return
        end function 


        !> Calculates the speed of sound in a gas wiwth given parameters.
        !>
        !> Arguments:
        !> - p_omega: ratio of specific heat for the gas.
        !> - p_r: R const of gas, in J/KgK.
        !> - p_t: temperature of the gas, in K. 
        !>
        !> Output:
        !> - Sound speed under given conditions, in m/s.
        !>
        pure real( kind = fmath5_p1 ) function RspFSoundSpeed( p_omega, p_r, p_t )
        
            implicit none

            real( kind = fmath5_p1 ), intent(in) :: p_omega, p_r, p_t

            RspFSoundSpeed = sqrt( ( p_omega * p_r * p_t) )

            return
        end function
    

        !> Calculates the safety factor of an aircraft based on its loads.
        !>
        !> Arguments:
        !> - p_limit_load: load after which no remaining damage is allowed.
        !> - p_ultimate_load: load after which failure may occur after three seconds.
        !>
        !> Output:
        !> - Safety factor. Dimension-less number.
        !>
        pure real( kind = fmath5_p1 ) function RspFSafetyFactor( p_ultimate_load, p_limit_load )
        
            implicit none

            real( kind = fmath5_p1 ), intent(in) :: p_ultimate_load, p_limit_load

            RspFSafetyFactor = p_ultimate_load / p_limit_load

            return
        end function 


        !> Calculates the lift force of a balloon.
        !> 
        !> Arguments
        !> - p_density_atm: atmospheric density.
        !> - p_volume_disp: displaced volume.
        !> - p_delta: can take the following values:
        !>   - temp_atm / temp_gas: temperature of the surrounding gas atmosphere divided by the temperature of the gas inside the balloon.
        !>   - mass_atm / mass_gas: molar mass of the surrounding atmosphere divided by the molar mass of the gas in the balloon.
        !>   - density_atm / density_gas: density of the surrounding atmosphere divided by the density of the gas in the balloon.
        !> - p_g: Gravity acceleration.
        !> - p_payload: Payload of the balloon.
        !>
        !> Output:
        !> - Lift force of the balloon.
        !>
        pure real( kind = fmath5_p1 ) function RspFLiftBalloon( p_density_atm, p_volume_disp, p_delta, p_g, p_payload )
        
            implicit none

            real( kind = fmath5_p1 ), intent(in) :: p_density_atm, p_volume_disp, p_delta, p_g, p_payload

            RspFLiftBalloon = p_density_atm * p_volume_disp * p_g * (1 - p_delta ) - p_payload

            return
        end function


        !> Calculates the load factor of an aircraft. If the aircraft is turning while maintaining 
        !> altitude L/W should be expressed as 1/cos(phi), being phi the bank angle.
        !>
        !> Arguments:
        !> - p_lift: lift force
        !> - p_weight: weight of the aircraft.
        !>
        !> Output:
        !> - Load factor.
        !>
        !> Sources:
        !> - https://en.wikipedia.org/wiki/Load_factor_(aeronautics)
        !>
        pure real( kind = fmath5_p1 ) function RspFLoadFactor( p_lift, p_weight )
        
            implicit none

            real( kind = fmath5_p1 ), intent(in) :: p_lift, p_weight

            RspFLoadFactor = p_lift / p_weight

            return
        end function 


        !> Calculates the longitudinal stress on a fuselage.
        !>
        !> Arguments:
        !> - p_in: pressure inside the fuselage. 
        !> - p_out: pressure outside the fuselage. 
        !> - p_R: interior radius of the fuselage.
        !> - p_t: thickness of the fuselage skin.
        !>
        !> Output:
        !> - Longitudinal stress.
        !>
        pure real( kind = fmath5_p1 ) function RspFLongitudinalStress( p_in, p_out, p_r, p_t )
        
            implicit none

            real( kind = fmath5_p1 ), intent(in) :: p_in, p_out, p_r, p_t

            RspFLongitudinalStress = RspFCircumferentialStress(p_in, p_out, p_r, p_t) / 2

            return
        end function 


        !> Calculates the Mach angle of air shock waves.
        !>
        !> Arguments:
        !> - p_m: mach number. 
        !>
        !> Output:
        !> - Mach angle, in degrees.
        !>
        !> Sources:
        !> - http://www.personal.psu.edu/jhm/f90/lectures/7.html 
        !>
        !> NEEDS TESTING, POSSIBLE ASIN - ASIND ISSUE.
        pure real( kind = fmath5_p1 ) function RspFMachNumber( p_m )
        
            implicit none

            real( kind = fmath5_p1 ), intent(in) :: p_m

            RspFMachNumber = asin( 1 / p_m )

            return
        end function 


        !> Calculates the mach number of an aircraft.
        !>
        !> Arguments:                        
        !> - p_tas: TAS (true airspeed) of aircraft.
        !> - p_t: temperature in Kelvin at the altitude in which the aircraft is flying.
        !>
        !> Output:
        !> - Mach number.
        !>
        !> Sources:
        !> - https://en.wikipedia.org/wiki/Mach_number
        !>
        pure real( kind = fmath5_p1 ) function RspFMachNumber1( p_tas, p_t )
        
            implicit none

            real( kind = fmath5_p1 ), intent(in) :: p_tas, p_t

            RspFMachNumber1 = p_tas / ( sqrt( ( RspFConst("gf") * RspFConst("R") * p_t ) ) )

            return
        end function 
        
        
        !> Calculates the Mach number of an object moving in a gas.
        !>
        !> Arguments:
        !> - p_u: flow velocity in m/s.
        !> - p_omega: ratio of specific heat for the gas.
        !> - p_r: R const of gas, in J/KgK.
        !> - p_t: temperature of the gas, in K. 
        !>
        !> Output:
        !> - Mach number.
        !>
        !> Sources:
        !> - https://en.wikipedia.org/wiki/Mach_number
        !>
        pure real( kind = fmath5_p1 ) function RspFMachNumber2( p_u, p_omega, p_r, p_t )
        
            implicit none

            real( kind = fmath5_p1 ), intent(in) :: p_u, p_omega, p_r, p_t

            RspFMachNumber2 = p_u / RspFSoundSpeed( p_omega, p_r, p_t )

            return
        end function 


        !> Calculates a specific property of a given material.
        !>
        !> Arguments:
        !> - p_property: property of the material.
        !> - p_density: density of the material. 
        !>
        !> Output:
        !> - Specific property of the material.
        !>
        pure real( kind = fmath5_p1 ) function RspFMaterialSpecificProperty( p_property, p_density )
        
            implicit none

            real( kind = fmath5_p1 ), intent(in) :: p_property, p_density

            RspFMaterialSpecificProperty = p_property / p_density

            return
        end function 


        !> Calculates the strain of a bar of material.
        !>
        !> Arguments:
        !> - p_dl: variation in length after force is applied.
        !> - p_l: length of the bar of material at T0. 
        !>
        !> Output:
        !> - Strain force.
        !>
        pure real( kind = fmath5_p1 ) function RspFMaterialStrain( p_dl, p_l )
        
            implicit none

            real( kind = fmath5_p1 ), intent(in) :: p_dl, p_l

            RspFMaterialStrain = p_dl / p_l

            return
        end function
        

        !> Calculates the stress of a bar of material.
        !>
        !> Arguments:
        !> - p_force: applied force.
        !> - p_area: area over which p_force is applied. 
        !>
        !> Output:
        !> - Stress force.       
        !>
        pure real( kind = fmath5_p1 ) function RspFMaterialStress( p_force, p_area )
        
            implicit none

            real( kind = fmath5_p1 ), intent(in) :: p_force, p_area

            RspFMaterialStress = p_force / p_area

            return
        end function
        

        !> Gives the maximum amount of lift per volume of air for a balloon based on mass and gravity 
        !> acceleration.
        !> 
        !> Arguments:
        !> - p_m: mass or weight of the gas.
        !> - p_g: gravity acceleration.
        !> 
        !> Output:
        !> - Lift per volume of air.
        !>
        pure real( kind = fmath5_p1 ) function RspFMaxAmountLiftPerVolume( p_m, p_g )
        
            implicit none

            real( kind = fmath5_p1 ), intent(in) :: p_m, p_g

            RspFMaxAmountLiftPerVolume = p_m * p_g

            return
        end function


        !> PID controler, parallel, non-interacting variant.
        !>
        !> Arguments:
        !> - p_pir: 3 for both levels, 2 for MV level, 1 for term level. By default, 
        !> the function does not put in range values, i.e. assumes 00.
        !> - p_min: lower limit of the normalization range.
        !> - p_max: higher limit of the normalization range.
        !> - p_kp: proportional gain, proportional term.
        !> - p_ki: proportional gain, intergration term.
        !> - p_kd: proportional gain, derivative term.
        !> - p_de: instantaneous error.
        !> - p_dt: time step (delta t).
        !>
        !> Output:
        !> - MV: Manipulated variable, either normalized to [p_min, p_max] or raw.
        !>
        !> Sources:
        !> - https://en.wikipedia.org/wiki/PID_controller
        !>
        !> NEEDS TESTING.
        !>
        pure real( kind = fmath5_p1 ) function RspFPid2( p_pir, p_min, p_max, p_kp, p_ki, p_kd, p_de, p_dt )
        
            implicit none

            integer, intent(in) :: p_pir 
            real( kind = fmath5_p1 ), intent(in) :: p_min, p_max, p_kp, p_ki, p_kd, p_de, p_dt
            real( kind = fmath5_p1 ) :: ai, mv, piru, pirl

            ai = 0

            ! Set normalization levels
            if ( p_pir == 3 ) then
                piru = 1.0
                pirl = 1.0
            else if ( p_pir == 2 ) then
                piru = 1.0
                pirl = 0.0
            else if ( p_pir == 1 ) then
                piru = 0.0
                pirl = 1.0
            else
                piru = 0.0
                pirl = 0.0
            end if

            ! Calculate correcting terms and the manipulated variable
            mv = RspFPidP1( pirl, p_min, p_max, p_kp, p_de ) + &
                RspFPidI1( pirl, p_min, p_max, p_ki, ai, p_de, p_dt ) + &
                RspFPidD1( pirl, p_min, p_max, p_kd, p_de, p_dt )        
                
            if ( piru - 1.0 < epsilon(piru) ) then
                RspFPid2 = RspFPir( piru, p_min, p_max, mv )
            else
                RspFPid2 = mv
            endif

            return
        end function 
    

        !==============================================================================
        ! Test.


        !> This function contains all tests for fmath5 functions and subroutines.
        !>
        !> Output:
        !> - Test results for fmath5.
        !>       
        subroutine RspFTestFmath5All()
            
            implicit none

            call system("clear")
            call RspFTestFmath51()
            write(*,*)
            call RspFTestFmath52()
            write(*,*)
            call RspFTestFmath53()
            write(*,*)
            call RspFTestFmath54()
            write(*,*)
            
        end subroutine


        !> Test for fmath5 subroutines and functions.
        !> - RspFatmocal3() at various altitudes.
        !>
        !> Output:
        !> - Test results for fmath5.
        !>       
        subroutine RspFTestFmath51()
            
            implicit none
            
            real( kind = fmath5_p1 ) :: alt
            real( kind = fmath5_p1 ), dimension(4) :: res
            
            alt = 0.0
            call RspFLine()
            call RspFCommentEn( "Begin RspFTestFmath51" )
            call RspFComment( "Testing RspFAtmocal3(...)" )                       
            do while ( alt <= 100000.0 )
                
                res = RspFAtmocal3(alt)
                alt =  alt + 1000.0
                write (*,*) "Alt = ", res(1)
                write (*,*) "t   = ", res(2)
                write (*,*) "P   = ", res(3)
                write (*,*) "Rho = ", res(4)
                write (*,*)
                
            end do 
            call RspFCommentEn( "End RspFTestFmath51" )
             
        end subroutine


        !> Test for fmath5 subroutines and functions.
        !>
        !> Output:
        !> - Test results for fmath5.
        !>       
        subroutine RspFTestFmath52()
            
            implicit none
            
            real( kind = fmath5_p1 ) :: n
            integer :: x1
            
            call RspFLine()
            call RspFCommentEn( "Begin RspFTestFmath52" )
            call RspFComment( "Testing RspFBankAngleEstimated(...)" )
            
            n = 0
            do x1 = 1,25
                write(*,*) "tas = ", n, " - bank angle = ", RspFBankAngleEstimated( n )
                n = n + 100
            end do
            
            call RspFCommentEn( "End RspFTestFmath52" )
             
         end subroutine RspFTestFmath52


        !> Test for fmath5 subroutines and functions.
        !>
        !> - RspFCircumferentialStress(...)
        !> - RspFPidP1(...)
        !> - RspFPidI1(...)
        !> - RspFPidD1(...)
        !> - RspFEquationOfState(...)
        !> - RspFSoundSpeed(...)
        !> - RspFSafetyFactor(...)
        !> - RspFLiftBalloon(...)
        !> - RspFLoadFactor(...) 
        !> 
        !> Output:
        !> - Test results for fmath5.
        !>       
        subroutine RspFTestFmath53()
            
            implicit none

            integer :: x1, n, y1
            real( kind = fmath5_p1 ) :: a
            
            a = 1.0
            n = 10
            
            call RspFLine()
            call RspFCommentEn( "Begin RspFTestFmath53" )

            call RspFComment( "RspFCircumferentialStress(...)" ) 
            do x1 = 1,n
                y1 = x1
                write(*,*) a * x1, a * x1 / 2
                write(*,*) a * y1, a * y1 * 0.001
                write(*,*) RspFCircumferentialStress( a * x1, a * x1 / 2, a * y1, a * y1 * 0.001 )
                write(*,*)      
            end do

            call RspFComment( "RspFPidP1(...)" ) 
            do x1 = 1,n
                y1 = x1
                write(*,*) 1.00 * a, -4.00 * a, 4.00 * a, a * x1 * 2, a * 0.01
                write(*,*) "Res1 = ", RspFPidP1( 1.00 * a, -4.00 * a, 4.00 * a, a * x1 * 2, a * 0.01 )                
                write(*,*) "Res2 = ", RspFPidP1( 0.00 * a, -4.00 * a, 4.00 * a, a * x1 * 2, a * 0.01 )
                write(*,*)      
            end do

            call RspFComment( "RspFPidI1(...)" ) 
            do x1 = 1,n
                y1 = x1
                write(*,*) 1.00 * a, -1.00 * a, 1.00 * a, x1 * 0.9 * a, x1 * a, 0.9 * a, 1.00 * a
                write(*,*) "Res1 = ", RspFPidI1( 0.00 * a, -1.00 * a, 1.00 * a, x1 * 0.9 * a, x1 * a, 0.9 * a, 1.00 * a )
                write(*,*) "Res2 = ", RspFPidI1( 1.00 * a, -1.00 * a, 1.00 * a, x1 * 0.9 * a, x1 * a, 0.9 * a, 1.00 * a )
                write(*,*)      
            end do

            call RspFComment( "RspFPidD1(...)" ) 
            do x1 = 1,n
                y1 = x1
                write(*,*) 1.00 * a, -1.00 * a, 1.00 * a, x1 * 0.9 * a, x1 * a, x1 * 0.9 * a
                write(*,*) "Res1 = ", RspFPidD1( 0.00 * a, -1.00 * a, 1.00 * a, x1 * 0.9 * a, x1 * a, x1 * 0.9 * a  )
                write(*,*) "Res2 = ", RspFPidD1( 1.00 * a, -1.00 * a, 1.00 * a, x1 * 0.9 * a, x1 * a, x1 * 0.9 * a  )
                write(*,*)      
            end do

            call RspFComment( "RspFEquationOfState(...)" ) 
            do x1 = 1,n
                y1 = x1
                write(*,*) 1.00 * a, x1 * 100 * a, x1 * 0.9 * a
                write(*,*) "Res = ", RspFEquationOfState( 1.00 * a, x1 * 100 * a, x1 * 0.9 * a )
                write(*,*)      
            end do
            
            call RspFComment( "RspFSoundSpeed(...)" ) 
            do x1 = 1,n
                y1 = x1
                write(*,*) x1 * a, x1 * 0.9 * a, x1 * 100 * a
                write(*,*) "Res = ", RspFSoundSpeed( x1 * a, x1 * 0.9 * a, x1 * 100 * a )
                write(*,*)      
            end do

            call RspFComment( "RspFSafetyFactor(...)" ) 
            do x1 = 1,n
                y1 = x1
                write(*,*) x1 * a, x1 ** x1
                write(*,*) "Res = ", RspFSafetyFactor( x1 * a, (a * x1) ** x1 )
                write(*,*)      
            end do

            call RspFComment( "RspFLiftBalloon(...)" ) 
            do x1 = 1,n
                write(*,*) x1 * a / 100, x1 * a * 0.9, x1 * a * 0.5, RspFConst("gE") * a, x1 * a
                write(*,*) "Res = ", RspFLiftBalloon( x1 * a / 100, x1 * a * 0.9, x1 * a * 0.5, RspFConst("gE") * a, x1 * a )
                write(*,*)
            end do
            
            call RspFComment( "RspFLoadFactor(...)" ) 
            do x1 = 1,n
                y1 = x1
                write(*,*) x1 * a, a * x1 * x1
                write(*,*) "Res = ", RspFLoadFactor( x1 * a, a * x1 * x1 )
                write(*,*)      
            end do
            
            call RspFCommentEn( "End RspFTestFmath53" )
            
         end subroutine RspFTestFmath53


        !> Test for fmath5 subroutines and functions.
        !>
        !> - RspFLongitudinalStress(...)
        !> - RspFMachNumber(...)
        !> - RspFMachNumber1(...)
        !> - RspFMachNumber2(...)
        !> - RspFMaterialSpecificProperty(...)
        !> - RspFMaterialStrain(...)
        !> - RspFMaterialStress(...)
        !> - RspFMaxAmountLiftPerVolume(...)
        !> - RspFPid2(...)
        !> 
        !> Output:
        !> - Test results for fmath5.
        !>       
        subroutine RspFTestFmath54()
            
            implicit none

            integer :: x1, n, y1
            real( kind = fmath5_p1 ) :: a
            
            a = 1.0
            n = 10
            
            call RspFLine()
            call RspFCommentEn( "Begin RspFTestFmath54" )

            call RspFComment( "RspFLongitudinalStress(...)" ) 
            do x1 = 1,n
                y1 = x1
                write(*,*) x1 * a, a / x1, a * 10, a * 0.001
                write(*,*) "Res = ", RspFLongitudinalStress( x1 * a, a / x1, a * 10, a * 0.001 )
                write(*,*)      
            end do

            call RspFComment( "RspFMachNumber(...)" ) 
            do x1 = 1,n
                y1 = x1
                write(*,*) x1 * a
                write(*,*) "Res = ", RspFMachNumber( x1 * a )
                write(*,*)      
            end do
            
            call RspFComment( "RspFMachNumber1(...)" ) 
            do x1 = 1,n
                y1 = ( 273 / x1 )
                write(*,*) x1 * a * 100
                write(*,*) "Res = ", RspFMachNumber1( x1 * a * 100, y1 * a )
                write(*,*)      
            end do

            call RspFComment( "RspFMachNumber2(...)" ) 
            do x1 = 1,n
                y1 = ( 273 / x1 )
                write(*,*) x1 * a * 100, 0.5 * a, 0.75 * a, y1 * a
                write(*,*) "Res = ", RspFMachNumber2( x1 * a * 100, 0.5 * a, 0.75 * a, y1 * a )
                write(*,*)      
            end do

            call RspFComment( "RspFMaterialSpecificProperty(...)" ) 
            do x1 = 1,n
                y1 = ( 273 / x1 )
                write(*,*) x1 * a * 100, y1 * a
                write(*,*) "Res = ", RspFMaterialSpecificProperty( x1 * a * 100, y1 * a )
                write(*,*)      
            end do

            call RspFComment( "RspFMaterialStrain(...)" ) 
            do x1 = 1,n
                y1 = ( 273 / x1 )
                write(*,*) x1 * a * 100, y1 * a
                write(*,*) "Res = ", RspFMaterialStrain( x1 * a * 100, y1 * a )
                write(*,*)      
            end do

            call RspFComment( "RspFMaterialStress(...)" ) 
            do x1 = 1,n
                y1 = ( 273 / x1 )
                write(*,*) x1 * a * 100, y1 * a
                write(*,*) "Res = ", RspFMaterialStress( x1 * a * 100, y1 * a )
                write(*,*)      
            end do

            call RspFComment( "RspFMaxAmountLiftPerVolume(...)" ) 
            do x1 = 1,n
                y1 = ( 273 / x1 )
                write(*,*) x1 * a * 100, RspFConst("gE") * a
                write(*,*) "Res = ", RspFMaxAmountLiftPerVolume( x1 * a * 100, RspFConst("gE") * a )
                write(*,*)      
            end do

            call RspFComment( "RspFPid2(...)" ) 
            do x1 = 1,n
                y1 = x1
                write(*,*) 3, -1.00 * a, 1.00 * a, x1 * 0.9 * a, x1 * 0.8 * a, x1 * 0.7 * a, 0.1 * a, 1.0 * a
                write(*,*) "Res1 = ", RspFPid2( 1, -1.00 * a, 1.00 * a, x1 * 0.9 * a, x1 * 0.8 * a, &
                     x1 * 0.7 * a, 0.1 * a, 1.0 * a )
                write(*,*) "Res2 = ", RspFPid2( 2, -1.00 * a, 1.00 * a, x1 * 0.9 * a, x1 * 0.8 * a, &
                     x1 * 0.7 * a, 0.1 * a, 1.0 * a )
                write(*,*) "Res3 = ", RspFPid2( 3, -1.00 * a, 1.00 * a, x1 * 0.9 * a, x1 * 0.8 * a, &
                     x1 * 0.7 * a, 0.1 * a, 1.0 * a )
                write(*,*)      
            end do
               
            call RspFCommentEn( "End RspFTestFmath54" )
            
         end subroutine RspFTestFmath54
    
end module

