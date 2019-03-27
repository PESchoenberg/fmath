!> =============================================================================
!> 
!> fmath3.f95 - Implementing conversion functions.
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
!> - mpif90 -c fmath1.f95
!> - mpif90 -std='gnu' -c fmath1.f95 -O3 -march=native -Wall -Wextra -fopenmp 
!>
module fmath1

    use fmath3

    implicit none

    ! Setting a platform-independent floating point precision.
    integer, parameter :: fmath1_p1 = selected_real_kind( 10,300 )

    !------------------------------------------------------------------------------
    ! Time    
    real( kind = fmath1_p1 ), parameter :: f_i2c = 10.00                   ! Millenia to centuries.
    real( kind = fmath1_p1 ), parameter :: f_c2d = 10.00                   ! Centuries to decades.
    real( kind = fmath1_p1 ), parameter :: f_k2y = 10.00                   ! Decades to years.
    real( kind = fmath1_p1 ), parameter :: f_y2d = 365.00                  ! Years to days.
    real( kind = fmath1_p1 ), parameter :: f_d2h = 24.00                   ! Days to hours.
    real( kind = fmath1_p1 ), parameter :: f_h2m = 60.00                   ! Hours to minutes.
    real( kind = fmath1_p1 ), parameter :: f_m2s = 60.00                   ! Minutes to seconds.
    real( kind = fmath1_p1 ), parameter :: f_w2d = 7.00                    ! Weeks to days.
    real( kind = fmath1_p1 ), parameter :: f_m2w = 4.00                    ! Months to weeks.
    real( kind = fmath1_p1 ), parameter :: f_y2m = 12.00                   ! Years to months.

    !------------------------------------------------------------------------------
    ! Metric - Length    
    real( kind = fmath1_p1 ), parameter :: f_km2hm = 10.00                 ! Kilometers to hectometers.
    real( kind = fmath1_p1 ), parameter :: f_hm2am = 10.00                 ! Hectometers to decameters.
    real( kind = fmath1_p1 ), parameter :: f_am2me = 10.00                 ! Decameters to meters.
    real( kind = fmath1_p1 ), parameter :: f_me2dm = 10.00                 ! Meters to decimeters.
    real( kind = fmath1_p1 ), parameter :: f_dm2cm = 10.00                 ! Decimeters to centimeters.
    real( kind = fmath1_p1 ), parameter :: f_cm2mm = 10.00                  ! Centimeters to millimeters.
    real( kind = fmath1_p1 ), parameter :: f_mm2um = 1000.00               ! Millimeters to micrometers.
    real( kind = fmath1_p1 ), parameter :: f_um2nm = 1000.00               ! Micrometers to nanometers.
    real( kind = fmath1_p1 ), parameter :: f_nm2pm = 1000.00               ! Nanometer to picometer.
    real( kind = fmath1_p1 ), parameter :: f_pm2fm = 1000.00               ! Picometer to femtometer.
    real( kind = fmath1_p1 ), parameter :: f_fm2am = 1000.00               ! Femtometer to attometer.
    real( kind = fmath1_p1 ), parameter :: f_am2zm = 1000.00               ! Attometer to zeptometer.
    real( kind = fmath1_p1 ), parameter :: f_zm2ym = 1000.00               ! Zeptometer to yoctometer.

    !Metric - Volume
    real( kind = fmath1_p1 ), parameter :: f_kl2hl = 10.00                 ! Kiloliter to hectoliter
    real( kind = fmath1_p1 ), parameter :: f_hl2al = 10.00                 ! Hectoliter to decaliter
    real( kind = fmath1_p1 ), parameter :: f_al2li = 10.00                 ! Decaliter to liter.
    real( kind = fmath1_p1 ), parameter :: f_li2dl = 10.00                 ! Liter to deciliter.
    real( kind = fmath1_p1 ), parameter :: f_dl2cl = 10.00                 ! Deciliter to centiliter.
    real( kind = fmath1_p1 ), parameter :: f_cl2ml = 10.00                 ! Centiliter to milliliter.
    real( kind = fmath1_p1 ), parameter :: f_ml2ul = 1000.00               ! Milliliter to microliter.
    real( kind = fmath1_p1 ), parameter :: f_ul2nl = 1000.00               ! Microliter to nanoliter.
    real( kind = fmath1_p1 ), parameter :: f_nl2pl = 1000.00               ! Nanoliter to picoliter.
    real( kind = fmath1_p1 ), parameter :: f_pl2fl = 1000.00               ! Picoliter to femtoliter.
    real( kind = fmath1_p1 ), parameter :: f_fl2al = 1000.00               ! Femtoliter to attoliter.
    real( kind = fmath1_p1 ), parameter :: f_al2zl = 1000.00               ! Attoliter to zeptoliter.
    real( kind = fmath1_p1 ), parameter :: f_zl2yl = 1000.00               ! Zeptoliter to yoctoliter.
    real( kind = fmath1_p1 ), parameter :: f_cm2li = 1000.00                ! Cubic meter to liter.

    !Metric - Mass
    real( kind = fmath1_p1 ), parameter :: f_mt2kg = 1000.00               ! Metric ton to kilogram.
    real( kind = fmath1_p1 ), parameter :: f_kg2hg = 10.00                 ! Kilogram to hectogram.
    real( kind = fmath1_p1 ), parameter :: f_hg2ag = 10.00                 ! Hectogram to decagram.
    real( kind = fmath1_p1 ), parameter :: f_ag2gr = 10.00                 ! Decagram to gram.
    real( kind = fmath1_p1 ), parameter :: f_gr2dg = 10.00                 ! Gram to decigram.
    real( kind = fmath1_p1 ), parameter :: f_dg2cg = 10.00                 ! Decigram to centigram.
    real( kind = fmath1_p1 ), parameter :: f_cg2mg = 10.00                 ! Centigram to milligram.
    real( kind = fmath1_p1 ), parameter :: f_mg2ug = 1000.00               ! Milligram to microgram.
    real( kind = fmath1_p1 ), parameter :: f_ug2ng = 1000.00               ! Microgram to nanogram.
    real( kind = fmath1_p1 ), parameter :: f_ng2pg = 1000.00               ! Nanogram to picogram.
    real( kind = fmath1_p1 ), parameter :: f_pg2fg = 1000.00               ! Picogram to femtogram.
    real( kind = fmath1_p1 ), parameter :: f_fg2ag = 1000.00               ! Femtogram to attogram.
    real( kind = fmath1_p1 ), parameter :: f_ag2zg = 1000.00               ! Attogram to zeptogram.
    real( kind = fmath1_p1 ), parameter :: f_zg2yg = 1000.00               ! Zeptogram to yoctogram.

    !------------------------------------------------------------------------------
    ! Imperial - Length
    real( kind = fmath1_p1 ), parameter :: f_lm2yd = 1769.00               ! Land mile to yards.
    real( kind = fmath1_p1 ), parameter :: f_nm2yd = 2025.37               ! Nautical mile to yards.
    real( kind = fmath1_p1 ), parameter :: f_yd2ft = 3.00                  ! Yard to feet.
    real( kind = fmath1_p1 ), parameter :: f_ft2in = 12.00                 ! Feet to inches.

    ! Imperial - Volume
    real( kind = fmath1_p1 ), parameter :: f_ug2ig = 0.832674              ! US gallon to imperial gallon.
    real( kind = fmath1_p1 ), parameter :: f_ug2uq = 4.00                  ! US gallon to US quarter.
    real( kind = fmath1_p1 ), parameter :: f_uq2up = 2.00                  ! US quarter to US pint.
    real( kind = fmath1_p1 ), parameter :: f_ug2uc = 15.7725               ! US gallon to US cup.
    real( kind = fmath1_p1 ), parameter :: f_ug2us = 256.00                ! US gallon to US spoon.
    real( kind = fmath1_p1 ), parameter :: f_ug2ut = 768.00                ! US gallon to US teaspoon.
    real( kind = fmath1_p1 ), parameter :: f_ug2uz = 128.00                ! US gallon to US liquid ounce.
    real( kind = fmath1_p1 ), parameter :: f_ug2cf = 0.133681              ! US gallon to cubic feet.
    real( kind = fmath1_p1 ), parameter :: f_cf2ci = 1728.00               ! Cubic feet to cubic inch.
    real( kind = fmath1_p1 ), parameter :: f_ig2iq = 4.00                  ! Imperial gallon to imperial quarter.
    real( kind = fmath1_p1 ), parameter :: f_iq2ip = 2.00                  ! Imperial quarter to imperial pint.
    real( kind = fmath1_p1 ), parameter :: f_ig2ic = 16.00                 ! Imperial gallon to imperial cup.
    real( kind = fmath1_p1 ), parameter :: f_ig2is = 256.00                ! Imperial gallon to imperial spoon.
    real( kind = fmath1_p1 ), parameter :: f_ig2it = 768.00                ! Imperial gallon to imperial teaspoon.
    real( kind = fmath1_p1 ), parameter :: f_ig2iz = 128.00                ! Imperial gallon to imperial liquid ounce.

    ! Imperial - Mass
    real( kind = fmath1_p1 ), parameter :: f_lb2oz = 16.00                 ! Pounds to Ounces.
    real( kind = fmath1_p1 ), parameter :: f_lb2st = 0.0714286             ! Pounds to stones.
    real( kind = fmath1_p1 ), parameter :: f_lb2ts = 0.0005                ! Pounds to Short tons.
    real( kind = fmath1_p1 ), parameter :: f_lb2tl = 0.000446429           ! Pounds to Long tons.

    !------------------------------------------------------------------------------
    ! Conversion - Imperial - Metric - Length
    real( kind = fmath1_p1 ), parameter :: f_me2yd = 1.09361               ! Meter to yard

    ! Conversion - Imperial - Metric - Volume
    real( kind = fmath1_p1 ), parameter :: f_li2ug = 0.264172              ! Liter to US gallon.

    ! Conversion Metric - Imperial - Mass
    real( kind = fmath1_p1 ), parameter :: f_kg2lb = 2.20462               ! Kilogram to pound.

    !------------------------------------------------------------------------------
    ! Conversion - Pressure - Various

    real( kind = fmath1_p1 ), parameter :: f_ba2mb = 1000.00000            ! Bar to millibar.
    real( kind = fmath1_p1 ), parameter :: f_pa2n2 = 1.00000000            ! Pascals to N/m2.    
    real( kind = fmath1_p1 ), parameter :: f_pa2ps = 0.000145038           ! Pascals to psi.
    real( kind = fmath1_p1 ), parameter :: f_pa2tr = 0.00750062            ! Pascals to torr.
    real( kind = fmath1_p1 ), parameter :: f_pa2ba = 5.0E-005              ! Pascals to bar.
    real( kind = fmath1_p1 ), parameter :: f_pa2am = 9.8692E-006           ! Pascals to physical atm.
    real( kind = fmath1_p1 ), parameter :: f_pa2at = 1.0197162129779E-005  ! Pascals to technical atm.    
    real( kind = fmath1_p1 ), parameter :: f_pa2k2 = 1.01972E-005          ! Pascals to kg/cm2.
    real( kind = fmath1_p1 ), parameter :: f_pa2hg = 0.00750064            ! Pascals to mmHg.
    real( kind = fmath1_p1 ), parameter :: f_pa2ho = 0.00010197            ! Pascals to meters of H2O.    
    real( kind = fmath1_p1 ), parameter :: f_pa2ap = 0.10000000            ! Pascals to decopascals.
    real( kind = fmath1_p1 ), parameter :: f_ap2hp = 0.10000000            ! Decapascals to hectopascals.
    real( kind = fmath1_p1 ), parameter :: f_hp2kp = 0.10000000            ! Hectopascals to kilopascals.
    real( kind = fmath1_p1 ), parameter :: f_kp2mp = 0.00100000            ! Kilopascals to megapascals.    
    real( kind = fmath1_p1 ), parameter :: f_mp2gp = 0.00100000            ! Megapascals to gigapascals.    
    real( kind = fmath1_p1 ), parameter :: f_pa2mp = 1000.00000            ! Pascals to millipascals.
    real( kind = fmath1_p1 ), parameter :: f_mp2up = 1000.00000            ! Mllipascals to micropascals.                            
    real( kind = fmath1_p1 ), parameter :: f_pa2pz = 0.00100000            ! Pascals to pieze.

    !------------------------------------------------------------------------------
    contains


        !==============================================================================
        ! Derived

        !------------------------------------------------------------------------------
        ! Aerodynamic functions.


        !> Converts equivalent airspeed to true airspeed.
        !>
        !> Arguments:
        !> - p_n: Quantity to convert; equivalent airspeed in m/s.
        !> - p_density_at_altitude: density of the air at the desired altitude. In Kg/m**3.
        !>
        !> Output:
        !> - Converted magnitude. True airspeed.
        !>
        pure real( kind = fmath1_p1 ) function RspFEas2Tas( p_n, p_density_at_altitude )
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n, p_density_at_altitude
            RspFEas2Tas = sqrt( ( RspFConst("r0") / p_density_at_altitude) ) * p_n
    
            return
        end function


        !> Conversion of feet to meters.
        !>
        !> Arguments:
        !> - p_n: quantity to convert, in ft.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvFeet2Meters( p_n )
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n
            
            RspFConvFeet2Meters = RspFConvYards2Meters( RspFConvFeet2Yards(p_n) )
    
            return
        end function        


        !> Conversion of meters to feet.
        !>
        !> Arguments:
        !> - p_n: quantity to convert, in m.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvMeters2Feet( p_n )
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n
            
            RspFConvMeters2Feet = RspFConvYards2Feet( RspFConvMeters2Yards( p_n ) )
    
            return
        end function 


        !> Converts a speed expressed in km/h to knots.
        !>
        !> Arguments:
        !> - p_n: quantity to convert, in km/h.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvKmH2Kts( p_n )
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in)  :: p_n
            
            RspFConvKmH2Kts = p_n / RspFConst("kt")
    
            return
        end function   


        !> Converts a speed expressed in knots to km/h.
        !>
        !> Arguments:
        !> - p_n: quantity to convert, in kts.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvKts2KmH( p_n )
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n
  
            RspFConvKts2KmH = p_n * RspFConst("kt")

            return
        end function  


        !> Converts a speed expressed in km/h to m/s.
        !>
        !> Arguments:
        !> - p_n: quantity to convert, in km/h.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvKmH2MS( p_n )
        
            implicit none

            real( kind = fmath1_p1 ), intent(in) :: p_n
            real( kind = fmath1_p1 ) :: x

            x = 1.0
            RspFConvKmH2MS = p_n * RspFConvDecameters2Meters( RspFConvHectometers2Decameters&
            ( RspFConvKilometers2Hectometers( x ) ) ) / ( RspFConst( "f_t_sxm" ) * RspFConst&
            ( "f_t_mxh" ) )

            return
        end function 


        !> Converts a speed expressed in m/s to km/h.
        !>
        !> Arguments:
        !> - p_n: quantity to convert, in m/s.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvMS2KmH( p_n )
        
            implicit none

            real( kind = fmath1_p1 ), intent(in) :: p_n
            real( kind = fmath1_p1 ) :: x

            x = 1.0
            RspFConvMS2KmH = p_n * ( RspFConst( "f_t_sxm" ) * RspFConst( "f_t_mxh" ) ) / &
            RspFConvDecameters2Meters( RspFConvHectometers2Decameters( &
            RspFConvKilometers2Hectometers( x ) ) ) 

            return
        end function


        !> Calculates the TAS (flow velocity) of an object with a given mach number and sound speed.
        !>
        !> Arguments:
        !> - p_m: mach number.
        !> - p_c: sound speed.
        !>
        !> Output:
        !> - Flow velocity of moving object in m/s.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvMach2MS( p_m, p_c ) 

            implicit none  
	          
            real( kind = fmath1_p1 ), intent(in) :: p_m, p_c
            
            RspFConvMach2MS = p_m * p_c

            return
        end function


        !> Calculates the velocity in terms of Mach of an object with a given velocity in m/s and
        !> sound speed.
        !>
        !> Arguments:
        !> - p_v: velocity, in m/s.
        !> - p_c: sound speed.
        !>
        !> Output:
        !> - Flow velocity of moving object in Mach.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvMS2Mach( p_v, p_c ) 

            implicit none  
	          
            real( kind = fmath1_p1 ), intent(in) :: p_v, p_c

            RspFConvMS2Mach = p_v / p_c

            return
        end function

        !==============================================================================
        ! Base
        
        !------------------------------------------------------------------------------
        ! Time functions.


        !> Conversion of seconds to minutes.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvSeconds2Minutes( p_n ) result(res)
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n 
            res = RspFUconv( p_n, ( 1 / f_m2s ) )
    
            return
        end function


        !> Conversion of minutes to seconds.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvMinutes2Seconds( p_n ) result(res)
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n 
            res = RspFUconv( p_n, f_m2s )

            return
        end function


        !> Conversion of minutes to hours.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvMinutes2Hours( p_n ) result(res)
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n 
            res = RspFUconv( p_n, ( 1 / f_h2m ) )
            
            return
        end function


        !> Conversion of hours to minutes.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvHours2Minutes( p_n ) result(res)
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n 
            res = RspFUconv( p_n, f_h2m )
            
            return
        end function


        !> Conversion of hours to days.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvHours2Days( p_n ) result(res)
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n 
            res = RspFUconv( p_n, ( 1 / f_d2h ) )
            
            return
        end function


        !> Conversion of days to hours.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvDays2Hours( p_n ) result(res)
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n 
            res = RspFUconv( p_n, f_d2h )
            
            return
        end function


        !> Conversion of days to weeks.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>        
        pure real( kind = fmath1_p1 ) function RspFConvDays2Weeks( p_n ) result(res)
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n 
            res = RspFUconv( p_n, ( 1 / f_w2d ) )
            
            return
        end function


        !> Conversion of weeks to days.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvWeeks2Days( p_n ) result(res)
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n 
            res = RspFUconv( p_n, f_w2d )
            
            return
        end function


        !> Conversion of weeks to months.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvWeeks2Months( p_n ) result(res)
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n
            res = RspFUconv( p_n, ( 1 / f_m2w ) )
            
            return
        end function


        !> Conversion of months to weeks.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvMonths2Weeks( p_n ) result(res)
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n 
            res = RspFUconv( p_n, f_m2w )
            
            return
        end function


        !> Conversion of months to years.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvMonths2Years( p_n ) result(res)
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n 
            res = RspFUconv( p_n, ( 1 / f_y2m ) )
            
            return
        end function


        !> Conversion of years to months.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvYears2Months( p_n ) result(res)
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n 
            res = RspFUconv( p_n, f_y2m )
            
            return
        end function


        !> Conversion of days to years.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvDays2Years( p_n ) result(res)
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n
            res = RspFUconv( p_n, ( 1 / f_y2d ) )
            
            return
        end function


        !> Conversion of years to days.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvYears2Days( p_n ) result(res)
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n 
            res = RspFUconv( p_n, f_y2d )
            
            return
        end function


        !> Conversion of years to decades.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvYears2Decades( p_n ) result(res)
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n
            res = RspFUconv( p_n, ( 1 / f_k2y ) )
            
            return
        end function


        !> Conversion of decades to years.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvDecades2Years( p_n ) result(res)
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n
            res = RspFUconv( p_n, f_k2y )
            
            return
        end function


        !> Conversion of decades to centuries.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvDecades2Centuries( p_n ) result(res)
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n 
            res = RspFUconv( p_n, ( 1 / f_c2d ) )
            
            return
        end function


        !> Conversion of centuries to decades.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvCenturies2Decades( p_n ) result(res)
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n 
            res = RspFUconv( p_n, f_c2d )
            
            return
        end function


        !> Conversion of centuries to millenia.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvCenturies2Millenia( p_n ) result(res)
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n
            res = RspFUconv( p_n, ( 1 / f_i2c ) )
            
            return
        end function


        !> Conversion of millenia to centuries.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvMillenia2Centuries( p_n ) result(res)
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n 
            res = RspFUconv( p_n, f_i2c )
            
            return
        end function

        !------------------------------------------------------------------------------
        ! Metric - length


        !> Conversion of hectometers to kilometers.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvHectometers2Kilometers( p_n ) result(res)
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n
            res = RspFUconv( p_n, ( 1 / f_km2hm ) )
            
            return
        end function


        !> Conversion of kilometers to hectometers.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvKilometers2Hectometers( p_n ) result(res)
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n 
            res = RspFUconv( p_n, f_km2hm )
            
            return
        end function


        !> Conversion of decameters to hectometers.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvDecameters2Hectometers( p_n ) result(res)
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n
            res = RspFUconv( p_n, ( 1 / f_hm2am ) )
            
            return
        end function


        !> Conversion of hectometers to decameters.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvHectometers2Decameters( p_n ) result(res)
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n 
            res = RspFUconv( p_n, f_hm2am )
            
            return
        end function


        !> Conversion of decameters to hectometers.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvMeters2Decameters( p_n ) result(res)
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n 
            res = RspFUconv( p_n, ( 1 / f_am2me ) )
            
            return
        end function


        !> Conversion of hectometers to decameters.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvDecameters2Meters( p_n ) result(res)
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n 
            res = RspFUconv( p_n, f_am2me )
            
            return
        end function


        !> Conversion of decimeters to meters.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvDecimeters2Meters( p_n ) result(res)
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n 
            res = RspFUconv( p_n, ( 1 / f_me2dm ) )
            
            return
        end function


        !> Conversion of meters to decimeters.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvMeters2Decimeters( p_n ) result(res)
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n 
            res = RspFUconv( p_n, f_me2dm )
            
            return
        end function


        !> Conversion of centimeters to decimeters.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvCentimeters2Decimeters( p_n ) result(res)
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n
            res = RspFUconv( p_n, ( 1 / f_dm2cm ) )
            
            return
        end function


        !> Conversion of decimeters to centimeters.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvDecimeters2Centimeters( p_n ) result(res)
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n
            res = RspFUconv( p_n, f_dm2cm )
            
            return
        end function


        !> Conversion of millimiters to centimeters.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvMillimeters2Centimeters( p_n ) result(res)
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n
            res = RspFUconv( p_n, ( 1 / f_cm2mm ) )
            
            return
        end function


        !> Conversion of centimeters to millimiters.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvCentimeters2Millimeters( p_n ) result(res)
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n 
            res = RspFUconv( p_n, f_cm2mm )
            
            return
        end function


        !> Conversion of micrometers to millimeters.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvMicrometers2Millimeters( p_n ) result(res)
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n
            res = RspFUconv( p_n, ( 1 / f_mm2um ) )
            
            return
        end function


        !> Conversion of millimiters to micrometers.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvMillimeters2Micrometers( p_n ) result(res)
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n
            res = RspFUconv( p_n, f_mm2um )
            
            return
        end function

                
        !> Conversion of nanometers to micrometers.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvNanometers2Micrometers( p_n ) result(res)
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n
            res = RspFUconv( p_n, ( 1 / f_um2nm ) )
            
            return
        end function


        !> Conversion of micrometers to nanometers.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvMicrometers2Nanometers( p_n ) result(res)
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n
            res = RspFUconv( p_n, f_um2nm )
            
            return
        end function        


        !> Conversion of picometers to nanometers.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvPicometers2Nanometers( p_n ) result(res)
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n
            res = RspFUconv( p_n, ( 1 / f_nm2pm ) )
            
            return
        end function


        !> Conversion of nanometers to picometers.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvNanometers2Picometers( p_n ) result(res)
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n 
            res = RspFUconv( p_n, f_nm2pm )
            
            return
        end function 


        !> Conversion of femtometers to picometers.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvFemtometers2Picometers( p_n ) result(res)
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n
            res = RspFUconv( p_n, ( 1 / f_pm2fm ) )
            
            return
        end function


        !> Conversion of picometers to femtometers.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvPicometers2Femtometers( p_n ) result(res)
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n
            res = RspFUconv( p_n, f_pm2fm )
            
            return
        end function 


        !> Conversion of attometers to femtometers.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvAttometers2Femtometers( p_n ) result(res)
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n
            res = RspFUconv( p_n, ( 1 / f_fm2am ) )
            
            return
        end function


        !> Conversion of femtometers to attometers.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvFemtometers2Attometers( p_n ) result(res)
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n 
            res = RspFUconv( p_n, f_fm2am )
            
            return
        end function 


        !> Conversion of zeptometers to attometers.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvZeptometers2Attometers( p_n ) result(res)
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n
            res = RspFUconv( p_n, ( 1 / f_am2zm ) )
            
            return
        end function


        !> Conversion of attometers to zeptometers.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvAttometers2Zeptometers( p_n ) result(res)
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n 
            res = RspFUconv( p_n, f_am2zm )
            
            return
        end function


        !> Conversion of yoctometers to zeptometers.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvYoctometers2Zeptometers( p_n ) result(res)
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n 
            res = RspFUconv( p_n, ( 1 / f_zm2ym ) )
            
            return
        end function


        !> Conversion of zeptometers to yoctometers.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvZeptometers2Yoctometers( p_n ) result(res)
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n 
            res = RspFUconv( p_n, f_zm2ym )
            
            return
        end function


        !------------------------------------------------------------------------------
        ! Metric - Volume


        !> Conversion of hectoliters to kiloliters.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvHectoliters2Kiloliters( p_n ) result(res)
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n
            res = RspFUconv( p_n, ( 1 / f_kl2hl ) )
            
            return
        end function


        !> Conversion of kiloliters to hectoliters.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvKiloliters2Hectoliters( p_n ) result(res)
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n 
            res = RspFUconv( p_n, f_kl2hl )
            
            return
        end function


        !> Conversion of decaliters to hectoliters.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvDecaliters2Hectoliters( p_n ) result(res)
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n 
            res = RspFUconv( p_n, ( 1 / f_hl2al ) )
            
            return
        end function


        !> Conversion of hectoliters to decaliters.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvHectoliters2Decaliters( p_n ) result(res)
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n 
            res = RspFUconv( p_n, f_hl2al )
            
            return
        end function


        !> Conversion of liters to decaliters.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvLiters2Decaliters( p_n ) result(res)
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n 
            res = RspFUconv( p_n, ( 1 / f_al2li ) )
            
            return
        end function


        !> Conversion of decaliters to liters.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvDecaliters2Liters( p_n ) result(res)
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n 
            res = RspFUconv( p_n, f_al2li )
            
            return
        end function

              
        !> Conversion of deciliters to liters.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvDeciliters2Liters( p_n ) result(res)
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n
            res = RspFUconv( p_n, ( 1 / f_li2dl ) )
            
            return
        end function


        !> Conversion of liters to deciliters.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvLiters2Deciliters( p_n ) result(res)
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n
            res = RspFUconv( p_n, f_li2dl )
            
            return
        end function        


        !> Conversion of centiliters to deciliters.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvCentiliters2Deciliters( p_n ) result(res)
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n
            res = RspFUconv( p_n, ( 1 / f_dl2cl ) )
            
            return
        end function


        !> Conversion of deciliters to centiliters.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvDeciliters2Centiliters( p_n ) result(res)
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n
            res = RspFUconv( p_n, f_dl2cl )
            
            return
        end function


        !> Conversion of milliliters to centiliters.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvMilliliters2Centiliters( p_n ) result(res)
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n
            res = RspFUconv( p_n, ( 1 / f_cl2ml ) )
            
            return
        end function


        !> Conversion of centiliters to milliliters.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvCentiliters2Milliliters( p_n ) result(res)
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n 
            res = RspFUconv( p_n, f_cl2ml )
            
            return
        end function


        !> Conversion of microliters to milliliters.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvMicroliters2Milliliters( p_n ) result(res)
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n 
            res = RspFUconv( p_n, ( 1 / f_ml2ul ) )
            
            return
        end function


        !> Conversion of milliliters to microliters.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvMilliliters2Microliters( p_n ) result(res)
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n 
            res = RspFUconv( p_n, f_ml2ul )
            
            return
        end function

                
        !> Conversion of nanoliters to microliters.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvNanoliters2Microliters( p_n ) result(res)
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n
            res = RspFUconv( p_n, ( 1 / f_ul2nl ) )
            
            return
        end function


        !> Conversion of microliters to nanoliters.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvMicroliters2Nanoliters( p_n ) result(res)
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n 
            res = RspFUconv( p_n, f_ul2nl )
            
            return
        end function        


        !> Conversion of picoliters to nanoliters.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvPicoliters2Nanoliters( p_n ) result(res)
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n
            res = RspFUconv( p_n, ( 1 / f_nl2pl ) )
            
            return
        end function


        !> Conversion of nanoliters to picoliters.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvNanoliters2Picoliters( p_n ) result(res)
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n 
            res = RspFUconv( p_n, f_nl2pl )
            
            return
        end function 


        !> Conversion of femtoliters to picoliters.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvFemtoliters2Picoliters( p_n ) result(res)
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n
            res = RspFUconv( p_n, ( 1 / f_pl2fl ) )
            
            return
        end function


        !> Conversion of picoliters to femtoliters.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvPicoliters2Femtoliters( p_n ) result(res)
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n 
            res = RspFUconv( p_n, f_pl2fl )
            
            return
        end function


        !> Conversion of attoliters to femtoliters.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvAttoliters2Femtoliters( p_n ) result(res)
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n
            res = RspFUconv( p_n, ( 1 / f_fl2al ) )
            
            return
        end function


        !> Conversion of femtoliters to attoliters.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvFemtoliters2Attoliters( p_n ) result(res)
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n
            res = RspFUconv( p_n, f_fl2al )
            
            return
        end function


        !> Conversion of attoliters to femtoliters.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvZeptoliters2Attoliters( p_n ) result(res)
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n
            res = RspFUconv( p_n, ( 1 / f_al2zl ) )
            
            return
        end function


        !> Conversion of femtoliters to attoliters.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvAttoliters2Zeptoliters( p_n ) result(res)
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n
            res = RspFUconv( p_n, f_al2zl )
            
            return
        end function


        !> Conversion of yoctoliters to zeptoliters.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvYoctoliters2Zeptoliters( p_n ) result(res)
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n 
            res = RspFUconv( p_n, ( 1 / f_zl2yl ) )
            
            return
        end function


        !> Conversion of zeptoliters to yoctoliters.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvZeptoliters2Yoctoliters( p_n ) result(res)
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n 
            res = RspFUconv( p_n, f_zl2yl )
            
            return
        end function


        !> Conversion of liters to cubic meters.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvLiters2CubicMeters( p_n ) result(res)
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n
            res = RspFUconv( p_n, ( 1 / f_cm2li ) )
            
            return
        end function


        !> Conversion of cubic meters to liters.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvCubicMeters2Liters( p_n ) result(res)
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n
            res = RspFUconv( p_n, f_cm2li )
            
            return
        end function


        !------------------------------------------------------------------------------
        !Metric - Mass

        
        !> Conversion of kilograms to metric tons.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvKilograms2MetricTons( p_n ) result(res)
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n 
            res = RspFUconv( p_n, ( 1 / f_mt2kg ) )
            
            return
        end function


        !> Conversion of metric tons to kilograms.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvMetricTons2Kilograms( p_n ) result(res)
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n 
            res = RspFUconv( p_n, f_mt2kg )
            
            return
        end function


        !> Conversion of hectograms to kilograms.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvHectograms2Kilograms( p_n ) result(res)
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n
            res = RspFUconv( p_n, ( 1 / f_kg2hg ) )
            
            return
        end function


        !> Conversion of kilograms to hectograms.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvKilograms2Hectograms( p_n ) result(res)
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n 
            res = RspFUconv( p_n, f_kg2hg )
            
            return
        end function


        !> Conversion of decagrams to hectograms.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvDecagrams2Hectograms( p_n ) result(res)
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n 
            res = RspFUconv( p_n, ( 1 / f_hg2ag ) )
            
            return
        end function


        !> Conversion of hectograms to decagrams.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvHectograms2Decagrams( p_n ) result(res)
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n 
            res = RspFUconv( p_n, f_hg2ag )
            
            return
        end function


        !> Conversion of grams to decagrams.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvGrams2Decagrams( p_n ) result(res)
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n 
            res = RspFUconv( p_n, ( 1 / f_ag2gr ) )
            
            return
        end function


        !> Conversion of decagrams to grams.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvDecagrams2Grams( p_n ) result(res)
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n
            res = RspFUconv( p_n, f_ag2gr )
            
            return
        end function


        !> Conversion of decigrams to grams.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvDecigrams2Grams( p_n ) result(res)
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n
            res = RspFUconv( p_n, ( 1 / f_gr2dg ) )
            
            return
        end function


        !> Conversion of grams to decigrams.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvGrams2Decigrams( p_n ) result(res)
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n 
            res = RspFUconv( p_n, f_gr2dg )
            
            return
        end function


        !> Conversion of centigrams to decigrams.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvCentigrams2Decigrams( p_n ) result(res)
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n
            res = RspFUconv( p_n, ( 1 / f_dg2cg ) )
            
            return
        end function


        !> Conversion of decigrams to centigrams.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvDecigrams2Centigrams( p_n ) result(res)
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n 
            res = RspFUconv( p_n, f_dg2cg )
            
            return
        end function


        !> Conversion of milligrams to centigrams.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvMilligrams2Centigrams( p_n ) result(res)
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n 
            res = RspFUconv( p_n, ( 1 / f_cg2mg ) )
            
            return
        end function


        !> Conversion of centigrams to milligrams.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvCentigrams2Milligrams( p_n ) result(res)
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n 
            res = RspFUconv( p_n, f_cg2mg )
            
            return
        end function


        !> Conversion of micrograms to milligrams.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvMicrograms2Milligrams( p_n ) result(res)
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n
            res = RspFUconv( p_n, ( 1 / f_mg2ug ) )
            
            return
        end function


        !> Conversion of milligrams to micrograms.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvMilligrams2Micrograms( p_n ) result(res)
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n 
            res = RspFUconv( p_n, f_mg2ug )
            
            return
        end function


        !> Conversion of nanograms to micrograms.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvNanograms2Micrograms( p_n ) result(res)
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n
            res = RspFUconv( p_n, ( 1 / f_ug2ng ) )
            
            return
        end function


        !> Conversion of micrograms to nanograms.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvMicrograms2Nanograms( p_n ) result(res)
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n 
            res = RspFUconv( p_n, f_ug2ng )
            
            return
        end function


        !> Conversion of picograms to nanograms.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvPicograms2Nanograms( p_n ) result(res)
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n 
            res = RspFUconv( p_n, ( 1 / f_ng2pg ) )
            
            return
        end function


        !> Conversion of nanograms to picograms.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvNanograms2Picograms( p_n ) result(res)
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n
            res = RspFUconv( p_n, f_ng2pg )
            
            return
        end function


        !> Conversion of femtograms to picograms.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvFemtograms2Picograms( p_n ) result(res)
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n 
            res = RspFUconv( p_n, ( 1 / f_pg2fg ) )
            
            return
        end function


        !> Conversion of picograms to femtograms.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvPicograms2Femtograms( p_n ) result(res)
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n 
            res = RspFUconv( p_n, f_pg2fg )
            
            return
        end function


        !> Conversion of attograms to femtograms.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvAttograms2Femtograms( p_n ) result(res)
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n 
            res = RspFUconv( p_n, ( 1 / f_fg2ag ) )
            
            return
        end function


        !> Conversion of femtograms to attograms.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvFemtograms2Attograms( p_n ) result(res)
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n 
            res = RspFUconv( p_n, f_fg2ag )
            
            return
        end function


        !> Conversion of zeptograms to attograms.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvZeptograms2Attograms( p_n ) result(res)
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n
            res = RspFUconv( p_n, ( 1 / f_ag2zg ) )
            
            return
        end function


        !> Conversion of attograms to zeptograms.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvAttograms2Zeptograms( p_n ) result(res)
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n
            res = RspFUconv( p_n, f_ag2zg )
            
            return
        end function


        !> Conversion of yoctograms to zeptograms.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvYoctograms2Zeptograms( p_n ) result(res)
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n
            res = RspFUconv( p_n, ( 1 / f_zg2yg ) )
            
            return
        end function


        !> Conversion of zeptograms to yoctograms.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvZeptograms2Yoctograms( p_n ) result(res)
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n 
            res = RspFUconv( p_n, f_zg2yg )
            
            return
        end function


        !------------------------------------------------------------------------------
        ! Metric to imperial functions.


        !> Conversion of yards to meters.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvYards2Meters( p_n ) result(res)
            implicit none
            real( kind = fmath1_p1 ), intent(in) :: p_n 
            res = RspFUconv( p_n, ( 1 / f_me2yd ) )
            
            return
        end function


        !> Conversion of meters to yards.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvMeters2Yards( p_n ) result(res)
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n
            res = RspFUconv( p_n, f_me2yd )
            
            return
        end function


        !> Conversion of US gallons to liters.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvUsgal2Liters( p_n ) result(res)
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n
            res = RspFUconv( p_n, ( 1 / f_li2ug ) )
            
            return
        end function


        !> Conversion of liters to US gallons.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvLiters2Usgal( p_n ) result(res)
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n 
            res = RspFUconv( p_n, f_li2ug )
            
            return
        end function


        !> Conversion of pounds to kg.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvPounds2Kilograms( p_n ) result(res)
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n 
            res = RspFUconv( p_n, ( 1 / f_kg2lb ) )
            
            return
        end function


        !> Conversion of kg to pounds.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvKilograms2Pounds( p_n ) result(res)
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n
            res = RspFUconv( p_n, f_kg2lb )
            
            return
        end function

        
        !------------------------------------------------------------------------------
        ! Imperial - Length


        !> Conversion of yards to land miles.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvYards2LandMiles( p_n ) result(res)
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n
            res = RspFUconv( p_n, ( 1 / f_lm2yd ) )
            
            return
        end function


        !> Conversion of land miles to yards.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvLandMiles2Yards( p_n ) result(res)
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n 
            res = RspFUconv( p_n, f_lm2yd )
            
            return
        end function


        !> Conversion of yards to nautical miles.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvYards2NauticalMiles( p_n ) result(res)
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n
            res = RspFUconv( p_n, ( 1 / f_nm2yd ) )
            
            return
        end function


        !> Conversion of nautical miles to yards.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvNauticalMiles2Yards( p_n ) result(res)
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n 
            res = RspFUconv( p_n, f_nm2yd )
            
            return
        end function


        !> Conversion of feet to yards.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvFeet2Yards( p_n ) result(res)
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n
            res = RspFUconv( p_n, ( 1 / f_yd2ft ) )
            
            return
        end function


        !> Conversion of yards to feet.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvYards2Feet( p_n ) result(res)
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n 
            res = RspFUconv( p_n, f_yd2ft )
            
            return
        end function


        !> Conversion of inches to feet.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvInches2Feet( p_n ) result(res)
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n 
            res = RspFUconv( p_n, ( 1 / f_ft2in ) )
            
            return
        end function


        !> Conversion of feet to inches.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvFeet2Inches( p_n ) result(res)
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n 
            res = RspFUconv( p_n, f_ft2in )
            
            return
        end function


        !------------------------------------------------------------------------------
        ! Imperial - Volume


        !> Conversion of imperial to US gallons.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvImperialGallons2UsGallons( p_n ) result(res)
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n 
            res = RspFUconv( p_n, ( 1 / f_ug2ig ) )
            
            return
        end function


        !> Conversion of US to imperial gallons.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvUsGallons2ImperialGallons( p_n ) result(res)
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n 
            res = RspFUconv( p_n, f_ug2ig )
            
            return
        end function

        
        !> Conversion of US Quarters to US gallons.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvUsQuarters2UsGallons( p_n ) result(res)
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n 
            res = RspFUconv( p_n, ( 1 / f_ug2uq ) )
            
            return
        end function


        !> Conversion of US gallons to US Quarters.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvUsGallons2UsQuarters( p_n ) result(res)
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n
            res = RspFUconv( p_n, f_ug2uq )
            
            return
        end function


        !> Conversion of US pint to US quarter.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvUsPints2UsQuarters( p_n ) result(res)
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n
            res = RspFUconv( p_n, ( 1 / f_uq2up ) )
            
            return
        end function


        !> Conversion of US quarter to US pint.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvUsQuarters2UsPints( p_n ) result(res)
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n 
            res = RspFUconv( p_n, f_uq2up )
            
            return
        end function


        !> Conversion of US cups to US gallons.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvUsCups2UsGallons( p_n ) result(res)
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n 
            res = RspFUconv( p_n, ( 1 / f_ug2uc ) )
            
            return
        end function


        !> Conversion of US gallons to US cups.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvUsGallons2UsCups( p_n ) result(res)
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n 
            res = RspFUconv( p_n, f_ug2uc )
            
            return
        end function


        !> Conversion of US Spoons to US gallons.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvUsSpoons2UsGallons( p_n ) result(res)
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n 
            res = RspFUconv( p_n, ( 1 / f_ug2us ) )
            
            return
        end function


        !> Conversion of US gallons to US Spoons.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvUsGallons2UsSpoons( p_n ) result(res)
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n 
            res = RspFUconv( p_n, f_ug2us )
            
            return
        end function


        !> Conversion of US tea Spoons to US gallons.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvUsTeaSpoons2UsGallons( p_n ) result(res)
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n
            res = RspFUconv( p_n, ( 1 / f_ug2ut ) )
            
            return
        end function


        !> Conversion of US gallons to US tea Spoons.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvUsGallons2UsTeaSpoons( p_n ) result(res)
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n 
            res = RspFUconv( p_n, f_ug2ut )
            
            return
        end function


        !> Conversion of US liquid Ounces to US gallons.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvUsLiquidOunces2UsGallons( p_n ) result(res)
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n 
            res = RspFUconv( p_n, ( 1 / f_ug2uz ) )
            
            return
        end function


        !> Conversion of US gallons to US liquid Ounces.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvUsGallons2UsLiquidOunces( p_n ) result(res)
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n
            res = RspFUconv( p_n, f_ug2uz )
            
            return
        end function


        !> Conversion of cubic feet to US gallons.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvCubicFeet2UsGallons( p_n ) result(res)
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n 
            res = RspFUconv( p_n, ( 1 / f_ug2cf ) )
            
            return
        end function


        !> Conversion of US gallons to cubic feet.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvUsGallons2CubicFeet( p_n ) result(res)
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n 
            res = RspFUconv( p_n, f_ug2cf )
            
            return
        end function


        !> Conversion of cubic inches to cubic feet.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvCubicInches2CubicFeet( p_n ) result(res)
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n 
            res = RspFUconv( p_n, ( 1 / f_cf2ci ) )
            
            return
        end function


        !> Conversion of cubic feet to cubic inches.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvCubicFeet2CubicInches( p_n ) result(res)
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n 
            res = RspFUconv( p_n, f_cf2ci )
            
            return
        end function


        !> Conversion of imperial Quarters to imperial gallons.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvImperialQuarters2ImperialGallons( p_n ) result(res)
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n 
            res = RspFUconv( p_n, ( 1 / f_ig2iq ) )
            
            return
        end function


        !> Conversion of imperial gallons to imperial Quarters.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvImperialGallons2ImperialQuarters( p_n ) result(res)
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n 
            res = RspFUconv( p_n, f_ig2iq )
            
            return
        end function


        !> Conversion of imperial pint to imperial quarter.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvImperialPints2ImperialQuarters( p_n ) result(res)
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n
            res = RspFUconv( p_n, ( 1 / f_iq2ip ) )
            
            return
        end function


        !> Conversion of imperial quarter to imperial pint.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvImperialQuarters2ImperialPints( p_n ) result(res)
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n 
            res = RspFUconv( p_n, f_iq2ip )
            
            return
        end function


        !> Conversion of imperial cups to imperial gallons.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvImperialCups2ImperialGallons( p_n ) result(res)
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n 
            res = RspFUconv( p_n, ( 1 / f_ig2ic ) )
            
            return
        end function


        !> Conversion of imperial gallons to imperial cups.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvImperialGallons2ImperialCups( p_n ) result(res)
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n 
            res = RspFUconv( p_n, f_ig2ic )
            
            return
        end function


        !> Conversion of imperial Spoons to imperial gallons.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvImperialSpoons2ImperialGallons( p_n ) result(res)
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n 
            res = RspFUconv( p_n, ( 1 / f_ig2is ) )
            
            return
        end function


        !> Conversion of imperial gallons to imperial Spoons.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvImperialGallons2ImperialSpoons( p_n ) result(res)
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n 
            res = RspFUconv( p_n, f_ig2is )
            
            return
        end function


        !> Conversion of imperial tea Spoons to imperial gallons.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvImperialTeaSpoons2ImperialGallons( p_n ) result(res)
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n
            res = RspFUconv( p_n, ( 1 / f_ig2it ) )
            
            return
        end function


        !> Conversion of imperial gallons to imperial tea Spoons.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvImperialGallons2ImperialTeaSpoons( p_n ) result(res)
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n
            res = RspFUconv( p_n, f_ig2it )
            
            return
        end function


        !> Conversion of imperial liquid Ounces to imperial gallons.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvImperialLiquidOunces2ImperialGallons( p_n ) result(res)
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n 
            res = RspFUconv( p_n, ( 1 / f_ig2iz ) )
            
            return
        end function


        !> Conversion of imperial gallons to imperial liquid Ounces.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvImperialGallons2ImperialLiquidOunces( p_n ) result(res)
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n 
            res = RspFUconv( p_n, f_ig2iz )
            
            return
        end function


        !------------------------------------------------------------------------------
        ! Imperial - Mass

        
        !> Conversion of Ounces to pounds.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvOunces2Pounds( p_n ) result(res)
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n
            res = RspFUconv( p_n, ( 1 / f_lb2oz ) )
            
            return
        end function


        !> Conversion of pounds to Ounces.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvPounds2Ounces( p_n ) result(res)
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n 
            res = RspFUconv( p_n, f_lb2oz )
            
            return
        end function


        !> Conversion of stones to pounds.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvStones2Pounds( p_n ) result(res)
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n
            res = RspFUconv( p_n, ( 1 / f_lb2st ) )
            
            return
        end function


        !> Conversion of pounds to stones.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvPounds2Stones( p_n ) result(res)
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n 
            res = RspFUconv( p_n, f_lb2st )
            
            return
        end function


        !> Conversion of Short tons to pounds.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvShortTons2Pounds( p_n ) result(res)
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n 
            res = RspFUconv( p_n, ( 1 / f_lb2ts ) )
            
            return
        end function


        !> Conversion of pounds to Short tons.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvPounds2ShortTons( p_n ) result(res)
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n
            res = RspFUconv( p_n, f_lb2ts )
            
            return
        end function


        !> Conversion of Long tons to pounds.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvLongTons2Pounds( p_n ) result(res)
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n 
            res = RspFUconv( p_n, ( 1 / f_lb2tl ) )
            
            return
        end function


        !> Conversion of pounds to Long tons.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvPounds2LongTons( p_n ) result(res)
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n
            res = RspFUconv( p_n, f_lb2tl )
            
            return
        end function


        !------------------------------------------------------------------------------
        ! Conversion - Pressure - Various


        !> Conversion of millibars to bars.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvMillibars2Bars( p_n ) result(res)
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n 
            res = RspFUconv( p_n, ( 1 / f_ba2mb ) )
            
            return
        end function


        !> Conversion of bars to millibars.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvBars2Millibars( p_n ) result(res)
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n 
            res = RspFUconv( p_n, f_ba2mb )
            
            return
        end function


        !> Conversion of newtons per square meter to pascals.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvNewtonsPerSquareMeters2Pascals( p_n ) result(res)
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n 
            res = RspFUconv( p_n, ( 1 / f_pa2n2 ) )
            
            return
        end function


        !> Conversion of pascals to newtons per square meter.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvPascals2NewtonsPerSquareMeters( p_n ) result(res)
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n 
            res = RspFUconv( p_n, f_pa2n2 )
            
            return
        end function


        !> Conversion of psi to pascals.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvPsi2Pascals( p_n ) result(res)
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n 
            res = RspFUconv( p_n, ( 1 / f_pa2ps ) )
            
            return
        end function


        !> Conversion of pascals to psi.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvPascals2Psi( p_n ) result(res)
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n 
            res = RspFUconv( p_n, f_pa2ps )
            
            return
        end function


        !> Conversion of torr to pascals.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvTorr2Pascals( p_n ) result(res)
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n 
            res = RspFUconv( p_n, ( 1 / f_pa2tr ) )
            
            return
        end function


        !> Conversion of pascals to torr.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvPascals2Torr( p_n ) result(res)
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n 
            res = RspFUconv( p_n, f_pa2tr )
            
            return
        end function


        !> Conversion of bar to pascals.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvBars2Pascals( p_n ) result(res)
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n 
            res = RspFUconv( p_n, ( 1 / f_pa2ba ) )
            
            return
        end function


        !> Conversion of pascals to bar.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvPascals2Bars( p_n ) result(res)
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n 
            res = RspFUconv( p_n, f_pa2ba )
            
            return
        end function


        !> Conversion of physical atmospheres to pascals.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvPhysicalAtmospheres2Pascals( p_n ) result(res)
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n
            res = RspFUconv( p_n, ( 1 / f_pa2am ) )
            
            return
        end function

        !> Conversion of pascals to physicalAtmospheres.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvPascals2PhysicalAtmospheres( p_n ) result(res)
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n 
            res = RspFUconv( p_n, f_pa2am )
            
            return
        end function


        !> Conversion of technical atmospheres to pascals.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvTechnicalAtmospheres2Pascals( p_n ) result(res)
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n
            res = RspFUconv( p_n, ( 1 / f_pa2at ) )
            
            return
        end function


        !> Conversion of pascals to technicalAtmospheres.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvPascals2TechnicalAtmospheres( p_n ) result(res)
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n 
            res = RspFUconv( p_n, f_pa2at )
            
            return
        end function


        !> Conversion of kg/cm2 to pascals.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvKgOverCm22Pascals( p_n ) result(res)
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n 
            res = RspFUconv( p_n, ( 1 / f_pa2k2 ) )
            
            return
        end function


        !> Conversion of pascals to kg/cm2.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvPascals2KgOverCm2( p_n ) result(res)
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n 
            res = RspFUconv( p_n, f_pa2k2 )
            
            return
        end function


        !> Conversion of mmHg to pascals.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvMmhg2Pascals( p_n ) result(res)
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n 
            res = RspFUconv( p_n, ( 1 / f_pa2hg ) )
            
            return
        end function


        !> Conversion of pascals to mmHg.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvPascals2Mmhg( p_n ) result(res)
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n 
            res = RspFUconv( p_n, f_pa2hg )
            
            return
        end function


        !> Conversion of meters of water to pascals.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvMetersOfWater2Pascals( p_n ) result(res)
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n 
            res = RspFUconv( p_n, ( 1 / f_pa2ho ) )
            
            return
        end function


        !> Conversion of pascals to meters of water.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvPascals2MetersOfWater( p_n ) result(res)
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n 
            res = RspFUconv( p_n, f_pa2ho )
            
            return
        end function


        !> Conversion of decapascals to pascals.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvDecapascals2Pascals( p_n ) result(res)
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n 
            res = RspFUconv( p_n, ( 1 / f_pa2ap ) )
            
            return
        end function


        !> Conversion of pascals to decapascals.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvPascals2Decapascals( p_n ) result(res)
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n 
            res = RspFUconv( p_n, f_pa2ap )
            
            return
        end function


        !> Conversion of hectopascals to decapascals.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvHectopascals2Decapascals( p_n ) result(res)
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n 
            res = RspFUconv( p_n, ( 1 / f_ap2hp ) )
            
            return
        end function


        !> Conversion of decapascals to hectopascals.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvDecapascals2Hectopascals( p_n ) result(res)
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n 
            res = RspFUconv( p_n, f_ap2hp )
            
            return
        end function


        !> Conversion of kilopascals to hectopascals.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvKilopascals2Hectopascals( p_n ) result(res)
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n 
            res = RspFUconv( p_n, ( 1 / f_hp2kp ) )
            
            return
        end function


        !> Conversion of hectopascals to kilopascals.
        !>
        !> Arguments:
        !> - p_n: quantity to convert.
        !>
        !> Output:
        !> - Converted magnitude.
        !>
        pure real( kind = fmath1_p1 ) function RspFConvHectopascals2Kilopascals( p_n ) result(res)
        
            implicit none
            
            real( kind = fmath1_p1 ), intent(in) :: p_n 
            res = RspFUconv( p_n, f_hp2kp )
            
            return
        end function


        !==============================================================================
        ! Test.


        !> This function contains all tests for fmath1 functions and subroutines.
        !>
        !> Output:
        !> - Test results for fmath1.
        !>       
        subroutine RspFTestFmath1All()
            
            implicit none

            call system("clear")
            call RspFTestFmath11()
            
        end subroutine


        !> Test for fmath1 subroutines and functions.
        !>
        !> - RspFTestTime(...)
        !> - RspFTestMetricImperial(...)
        !> - RspFTestMetricLength(...)
        !> - RspFTestMetricVolume(...)
        !> - RspFTestMetricMass(...)
        !> - RspFTestImperialLength(...)
        !> - RspFTestImperialVolume(...)
        !> - Testing RspFTestImperialMass(...)
        !> - Testing RspFTestPressureVarious(...)
        !>
        !> Output:
        !> - Test results for fmath1.
        !>       
        subroutine RspFTestFmath11()
            
            implicit none

            call RspFLine()
            call RspFCommentEn( "Begin RspFTestFmath11" )
            call RspFComment( "Testing RspFTestTime(...)" ) 
            call RspFTestTime()
            write(*,*)
            call RspFComment( "Testing RspFTestMetricImperial(...)" )
            call RspFTestMetricImperial()
            write(*,*)
            call RspFComment( "Testing RspFTestMetricLength(...)" )
            call RspFTestMetricLength()
            write(*,*)
            call RspFComment( "Testing RspFTestMetricVolume(...)" )
            call RspFTestMetricVolume()
            write(*,*)
            call RspFComment( "Testing RspFTestMetricMass(...)" )
            call RspFTestMetricMass()
            write(*,*)
            call RspFComment( "Testing RspFTestImperialLength(...)" )
            call RspFTestImperialLength()
            write(*,*)
            call RspFComment( "Testing RspFTestImperialVolume(...)" )
            call RspFTestImperialVolume()
            write(*,*)
            call RspFComment( "Testing RspFTestImperialMass(...)" )
            call RspFTestImperialMass()
            write(*,*)
            call RspFComment( "Testing RspFTestPressureVarious(...)" )
            call RspFTestPressureVarious()
            write(*,*)   
            call RspFCommentEn( "End RspFTestFmath11" ) 
                       
        end subroutine


        !> Test for fmath1 subroutines and functions.
        !>
        !> Output:
        !> - Test results for fmath1.
        !>       
        subroutine RspFTestFmath12()
            
            implicit none

            call RspFLine()
            call RspFComment( "Begin RspFTestFmath12" )
            call RspFComment( "Testing RspFNthRoot(...)" ) 
            call RspFCommentEn( "End RspFTestFmath12" )            
        end subroutine


        !>Shows a list of results for time conversion functions.
        !>
        subroutine RspFTestTime()

            implicit none
	        
            real( kind = fmath1_p1 ) :: pin, pout

            pin = 1.00
            call RspFComment( 'Time units conversion test.' )

            pout = RspFConvSeconds2Minutes( pin )
            write(*,*) pin,' sec = ', pout, 'min.'

            pout = RspFConvMinutes2Seconds( pin )
            write(*,*) pin,' min = ', pout, 'sec.'

            pout = RspFConvMinutes2Hours( pin )
            write(*,*) pin,' min = ', pout, 'hs.'

            pout = RspFConvHours2Minutes( pin )
            write(*,*) pin,' hs  = ', pout, 'min'

            pout = RspFConvHours2Days( pin )
            write(*,*) pin,' hs  = ', pout, 'ds'

            pout = RspFConvDays2Hours( pin )
            write(*,*) pin,' ds  = ', pout, 'hs'

            pout = RspFConvDays2Weeks( pin )
            write(*,*) pin,' ds  = ', pout, 'ws'

            pout = RspFConvWeeks2Days( pin )
            write(*,*) pin,' ws  = ', pout, 'ds'

            pout = RspFConvWeeks2Months( pin )
            write(*,*) pin,' w  = ', pout, 'm'

            pout = RspFConvMonths2Weeks( pin )
            write(*,*) pin,' m  = ', pout, 'w'

            pout = RspFConvMonths2Years( pin )
            write(*,*) pin,' m  = ', pout, 'y'

            pout = RspFConvYears2Months( pin )
            write(*,*) pin,' y  = ', pout, 'm'

            pout = RspFConvDays2Years( pin )
            write(*,*) pin,' d  = ', pout, 'y'

            pout = RspFConvYears2Days( pin )
            write(*,*) pin,' y  = ', pout, 'd'

            pout = RspFConvYears2Decades( pin )
            write(*,*) pin,' y  = ', pout, 'k'

            pout = RspFConvDecades2Years( pin )
            write(*,*) pin,' k  = ', pout, 'y'

            pout = RspFConvDecades2Centuries( pin )
            write(*,*) pin,' d  = ', pout, 'c'

            pout = RspFConvCenturies2Decades( pin )
            write(*,*) pin,' c  = ', pout, 'd'

            pout = RspFConvCenturies2Millenia( pin )
            write(*,*) pin,' c  = ', pout, 'i'

            pout = RspFConvMillenia2Centuries( pin )
            write(*,*) pin,' i  = ', pout, 'c'

        end subroutine


        !>Shows a list of results for metric - imperial conversion functions.
        !>
        subroutine RspFTestMetricImperial()

            implicit none
	        
            real( kind = fmath1_p1 ) :: pin, pout

            pin = 1.00
            call RspFComment( 'Metric - imperial units conversion test.' )

            pout = RspFConvYards2Meters( pin )
            write(*,*) pin,' yd = ', pout, 'm.'

            pout = RspFConvMeters2Yards( pin )
            write(*,*) pin,' m = ', pout, 'yd.'

            pout = RspFConvUsgal2Liters( pin )
            write(*,*) pin,' uga = ', pout, 'l.'

            pout = RspFConvLiters2Usgal( pin )
            write(*,*) pin,' l = ', pout, 'uga.'

            pout = RspFConvPounds2Kilograms( pin )
            write(*,*) pin,' lb = ', pout, 'kg.'

            pout = RspFConvKilograms2Pounds( pin )
            write(*,*) pin,' kg = ', pout, 'lb.'

        end subroutine


        !>Shows a list of results for metric length conversion functions.
        !>
        subroutine RspFTestMetricLength()

            implicit none
	        
            real( kind = fmath1_p1 ) :: pin, pout

            pin = 1.00
            call RspFComment( 'Metric - length units conversion test.' )

            pout = RspFConvHectometers2Kilometers( pin )
            write(*,*) pin,' hm = ', pout, 'km.'
            
            pout = RspFConvKilometers2Hectometers( pin )
            write(*,*) pin,' km = ', pout, 'hm.'
            
            pout = RspFConvDecameters2Hectometers( pin )
            write(*,*) pin,' am = ', pout, 'hm.'                        

            pout = RspFConvHectometers2Decameters( pin )
            write(*,*) pin,' hm = ', pout, 'am.'
            
            pout = RspFConvMeters2Decameters( pin )
            write(*,*) pin,' me = ', pout, 'am.'
            
            pout = RspFConvDecameters2Meters( pin )
            write(*,*) pin,' am = ', pout, 'me.'   
            
            pout = RspFConvDecimeters2Meters( pin )
            write(*,*) pin,' dm = ', pout, 'me.'
            
            pout = RspFConvMeters2Decimeters( pin )
            write(*,*) pin,' me = ', pout, 'dm.'                                             

            pout = RspFConvCentimeters2Decimeters( pin )
            write(*,*) pin,' cm = ', pout, 'dm.'
            
            pout = RspFConvDecimeters2Centimeters( pin )
            write(*,*) pin,' dm = ', pout, 'cm.'
                        
            pout = RspFConvMillimeters2Centimeters( pin )
            write(*,*) pin,' mm = ', pout, 'cm.'
            
            pout = RspFConvCentimeters2Millimeters( pin )
            write(*,*) pin,' cm = ', pout, 'mm.'            

            pout = RspFConvMicrometers2Millimeters( pin )
            write(*,*) pin,' um = ', pout, 'mm.'
            
            pout = RspFConvMillimeters2Micrometers( pin )
            write(*,*) pin,' mm = ', pout, 'um.' 

            pout = RspFConvNanometers2Micrometers( pin )
            write(*,*) pin,' nm = ', pout, 'um.'
            
            pout = RspFConvMicrometers2Nanometers( pin )
            write(*,*) pin,' um = ', pout, 'nm.'

            pout = RspFConvPicometers2Nanometers( pin )
            write(*,*) pin,' pm = ', pout, 'nm.'
            
            pout = RspFConvNanometers2Picometers( pin )
            write(*,*) pin,' nm = ', pout, 'pm.'
                        
            pout = RspFConvFemtometers2Picometers( pin )
            write(*,*) pin,' fm = ', pout, 'pm.'
            
            pout = RspFConvPicometers2Femtometers( pin )
            write(*,*) pin,' pm = ', pout, 'fm.'            

            pout = RspFConvAttometers2Femtometers( pin )
            write(*,*) pin,' am = ', pout, 'fm.'
            
            pout = RspFConvFemtometers2Attometers( pin )
            write(*,*) pin,' fm = ', pout, 'am.'
                        
            pout = RspFConvZeptometers2Attometers( pin )
            write(*,*) pin,' zm = ', pout, 'am.'
            
            pout = RspFConvAttometers2Zeptometers( pin )
            write(*,*) pin,' am = ', pout, 'zm.'
                        
            pout = RspFConvYoctometers2Zeptometers( pin )
            write(*,*) pin,' ym = ', pout, 'zm.'
            
            pout = RspFConvZeptometers2Yoctometers( pin )
            write(*,*) pin,' zm = ', pout, 'ym.'                        
            
        end subroutine


        !>Shows a list of results for metric volume conversion functions.
        !>
        subroutine RspFTestMetricVolume()

            implicit none
	        
            real( kind = fmath1_p1 ) :: pin, pout

            pin = 1.00
            call RspFComment( 'Metric - volume units conversion test.' )

            pout = RspFConvHectoliters2Kiloliters( pin )
            write(*,*) pin,' hl = ', pout, 'kl.'
            
            pout = RspFConvKiloliters2Hectoliters( pin )
            write(*,*) pin,' kl = ', pout, 'hl.'
                        
            pout = RspFConvDecaliters2Hectoliters( pin )
            write(*,*) pin,' al = ', pout, 'hl.'
            
            pout = RspFConvHectoliters2Decaliters( pin )
            write(*,*) pin,' hl = ', pout, 'al.'            

            pout = RspFConvLiters2Decaliters( pin )
            write(*,*) pin,' li = ', pout, 'al.'
            
            pout = RspFConvDecaliters2Liters( pin )
            write(*,*) pin,' al = ', pout, 'li.'
                        
            pout = RspFConvDeciliters2Liters( pin )
            write(*,*) pin,' dl = ', pout, 'li.'
            
            pout = RspFConvLiters2Deciliters( pin )
            write(*,*) pin,' li = ', pout, 'dl.'
                        
            pout = RspFConvCentiliters2Deciliters( pin )
            write(*,*) pin,' cl = ', pout, 'dl.'
            
            pout = RspFConvDeciliters2Centiliters( pin )
            write(*,*) pin,' dl = ', pout, 'cl.'
                        
            pout = RspFConvMilliliters2Centiliters( pin )
            write(*,*) pin,' ml = ', pout, 'cl.'
            
            pout = RspFConvCentiliters2Milliliters( pin )
            write(*,*) pin,' cl = ', pout, 'ml.'
                        
            pout = RspFConvMicroliters2Milliliters( pin )
            write(*,*) pin,' ul = ', pout, 'ml.'
            
            pout = RspFConvMilliliters2Microliters( pin )
            write(*,*) pin,' ml = ', pout, 'ul.' 
                       
            pout = RspFConvNanoliters2Microliters( pin )
            write(*,*) pin,' nl = ', pout, 'ul.'
            
            pout = RspFConvMicroliters2Nanoliters( pin )
            write(*,*) pin,' ul = ', pout, 'nl.'
                        
            pout = RspFConvPicoliters2Nanoliters( pin )
            write(*,*) pin,' pl = ', pout, 'nl.'
            
            pout = RspFConvNanoliters2Picoliters( pin )
            write(*,*) pin,' nl = ', pout, 'pl.'
                        
            pout = RspFConvFemtoliters2Picoliters( pin )
            write(*,*) pin,' fl = ', pout, 'pl.'
            
            pout = RspFConvPicoliters2Femtoliters( pin )
            write(*,*) pin,' pl = ', pout, 'fl.'
                        
            pout = RspFConvAttoliters2Femtoliters( pin )
            write(*,*) pin,' al = ', pout, 'fl.'
            
            pout = RspFConvFemtoliters2Attoliters( pin )
            write(*,*) pin,' fl = ', pout, 'al.'
            
            pout = RspFConvZeptoliters2Attoliters( pin )
            write(*,*) pin,' zl = ', pout, 'al.'
            
            pout = RspFConvAttoliters2Zeptoliters( pin )
            write(*,*) pin,' al = ', pout, 'zl.'
            
            pout = RspFConvYoctoliters2Zeptoliters( pin )
            write(*,*) pin,' yl = ', pout, 'zl.'
            
            pout = RspFConvZeptoliters2Yoctoliters( pin )
            write(*,*) pin,' zl = ', pout, 'yl.'
            
            pout = RspFConvLiters2CubicMeters( pin )
            write(*,*) pin,' li = ', pout, 'm3.'
            
            pout = RspFConvCubicMeters2Liters( pin )
            write(*,*) pin,' m3 = ', pout, 'li.'            
            
        end subroutine


        !>Shows a list of results for metric mass conversion functions.
        !>
        subroutine RspFTestMetricMass()

            implicit none
	        
            real( kind = fmath1_p1 ) :: pin, pout

            pin = 1.00
            call RspFComment( 'Metric - mass units conversion test.' )

            pout = RspFConvKilograms2MetricTons( pin )
            write(*,*) pin,' kg = ', pout, 'mt.'
            
            pout = RspFConvMetricTons2Kilograms( pin )
            write(*,*) pin,' mt = ', pout, 'kg.'

            pout = RspFConvHectograms2Kilograms( pin )
            write(*,*) pin,' hg = ', pout, 'kg.'
            
            pout = RspFConvKilograms2Hectograms( pin )
            write(*,*) pin,' kg = ', pout, 'hg.'
                        
            pout = RspFConvDecagrams2Hectograms( pin )
            write(*,*) pin,' ag = ', pout, 'hg.'
            
            pout = RspFConvHectograms2Decagrams( pin )
            write(*,*) pin,' hg = ', pout, 'ag.'                        

            pout = RspFConvGrams2Decagrams( pin )
            write(*,*) pin,' gr = ', pout, 'ag.'
            
            pout = RspFConvDecagrams2Grams( pin )
            write(*,*) pin,' ag = ', pout, 'gr.'

            pout = RspFConvDecigrams2Grams( pin )
            write(*,*) pin,' dg = ', pout, 'gr.'
            
            pout = RspFConvGrams2Decigrams( pin )
            write(*,*) pin,' gr = ', pout, 'dg.'
                        
            pout = RspFConvCentigrams2Decigrams( pin )
            write(*,*) pin,' cg = ', pout, 'dg.'
            
            pout = RspFConvDecigrams2Centigrams( pin )
            write(*,*) pin,' dg = ', pout, 'cg.'            

            pout = RspFConvMilligrams2Centigrams( pin )
            write(*,*) pin,' mg = ', pout, 'cg.'
            
            pout = RspFConvCentigrams2Milligrams( pin )
            write(*,*) pin,' cg = ', pout, 'mg.'
                        
            pout = RspFConvMicrograms2Milligrams( pin )
            write(*,*) pin,' ug = ', pout, 'mg.'
            
            pout = RspFConvMilligrams2Micrograms( pin )
            write(*,*) pin,' mg = ', pout, 'ug.'            

            pout = RspFConvNanograms2Micrograms( pin )
            write(*,*) pin,' ng = ', pout, 'ug.'
            
            pout = RspFConvMicrograms2Nanograms( pin )
            write(*,*) pin,' ug = ', pout, 'ng.'
                        
            pout = RspFConvPicograms2Nanograms( pin )
            write(*,*) pin,' pg = ', pout, 'ng.'
            
            pout = RspFConvNanograms2Picograms( pin )
            write(*,*) pin,' ng = ', pout, 'pg.' 
                       
            pout = RspFConvFemtograms2Picograms( pin )
            write(*,*) pin,' fg = ', pout, 'pg.'
            
            pout = RspFConvPicograms2Femtograms( pin )
            write(*,*) pin,' pg = ', pout, 'fg.'
                        
            pout = RspFConvAttograms2Femtograms( pin )
            write(*,*) pin,' ag = ', pout, 'fg.'
            
            pout = RspFConvFemtograms2Attograms( pin )
            write(*,*) pin,' fg = ', pout, 'ag.'
                        
            pout = RspFConvZeptograms2Attograms( pin )
            write(*,*) pin,' zg = ', pout, 'ag.'
            
            pout = RspFConvAttograms2Zeptograms( pin )
            write(*,*) pin,' ag = ', pout, 'zg.'
                        
            pout = RspFConvYoctograms2Zeptograms( pin )
            write(*,*) pin,' yg = ', pout, 'zg.'
            
            pout = RspFConvZeptograms2Yoctograms( pin )
            write(*,*) pin,' zg = ', pout, 'yg.'

        end subroutine


        !>Shows a list of results for imperial length conversion functions.
        !>
        subroutine RspFTestImperialLength()

            implicit none
	        
            real( kind = fmath1_p1 ) :: pin, pout

            pin = 1.00
            call RspFComment( 'Imperial - length units conversion test.' )

            pout = RspFConvYards2LandMiles( pin )
            write(*,*) pin,' yd = ', pout, 'lm.'
            
            pout = RspFConvLandMiles2Yards( pin )
            write(*,*) pin,' lm = ', pout, 'yd.'
                        
            pout = RspFConvYards2NauticalMiles( pin )
            write(*,*) pin,' yd = ', pout, 'nm.'
            
            pout = RspFConvNauticalMiles2Yards( pin )
            write(*,*) pin,' nm = ', pout, 'yd.'            

            pout = RspFConvFeet2Yards( pin )
            write(*,*) pin,' ft = ', pout, 'yd.'
            
            pout = RspFConvYards2Feet( pin )
            write(*,*) pin,' yd = ', pout, 'ft.'
            
            pout = RspFConvInches2Feet( pin )
            write(*,*) pin,' in = ', pout, 'ft.'
            
            pout = RspFConvFeet2Inches( pin )
            write(*,*) pin,' ft = ', pout, 'in.'             

        end subroutine

        !>Shows a list of results for imperial volume conversion functions.
        !>
        subroutine RspFTestImperialVolume()

            implicit none
	        
            real( kind = fmath1_p1 ) :: pin, pout

            pin = 1.00
            call RspFComment( 'Imperial - volume units conversion test.' )

            pout = RspFConvImperialGallons2UsGallons( pin )
            write(*,*) pin,' ig = ', pout, 'ug.'
            
            pout = RspFConvUsGallons2ImperialGallons( pin )
            write(*,*) pin,' ug = ', pout, 'ig.'

            pout = RspFConvUsQuarters2UsGallons( pin )
            write(*,*) pin,' uq = ', pout, 'ug.'
            
            pout = RspFConvUsGallons2UsQuarters( pin )
            write(*,*) pin,' ug = ', pout, 'uq.'            

            pout = RspFConvUsPints2UsQuarters( pin )
            write(*,*) pin,' up = ', pout, 'uq.'
            
            pout = RspFConvUsQuarters2UsPints( pin )
            write(*,*) pin,' uq = ', pout, 'up.'

            pout = RspFConvUsCups2UsGallons( pin )
            write(*,*) pin,' uc = ', pout, 'ug.'
            
            pout = RspFConvUsGallons2UsCups( pin )
            write(*,*) pin,' ug = ', pout, 'uc.'
            
            pout = RspFConvUsSpoons2UsGallons( pin )
            write(*,*) pin,' us = ', pout, 'ug.'
            
            pout = RspFConvUsGallons2UsSpoons( pin )
            write(*,*) pin,' ug = ', pout, 'us.' 

            pout = RspFConvUsTeaSpoons2UsGallons( pin )
            write(*,*) pin,' ut = ', pout, 'ug.'
            
            pout = RspFConvUsGallons2UsTeaSpoons( pin )
            write(*,*) pin,' ug = ', pout, 'ut.'
            
            pout = RspFConvUsLiquidOunces2UsGallons( pin )
            write(*,*) pin,' uz = ', pout, 'ug.'
            
            pout = RspFConvUsGallons2UsLiquidOunces( pin )
            write(*,*) pin,' ug = ', pout, 'uz.'
            
            pout = RspFConvCubicFeet2UsGallons( pin )
            write(*,*) pin,' cf = ', pout, 'ug.'
            
            pout = RspFConvUsGallons2CubicFeet( pin )
            write(*,*) pin,' ug = ', pout, 'cf.'
                        
            pout = RspFConvCubicInches2CubicFeet( pin )
            write(*,*) pin,' ci = ', pout, 'cf.'
            
            pout = RspFConvCubicFeet2CubicInches( pin )
            write(*,*) pin,' cf = ', pout, 'ci.'                                     

            pout = RspFConvImperialQuarters2ImperialGallons( pin )
            write(*,*) pin,' iq = ', pout, 'ig.'
            
            pout = RspFConvImperialGallons2ImperialQuarters( pin )
            write(*,*) pin,' ig = ', pout, 'iq.'

            pout = RspFConvImperialPints2ImperialQuarters( pin )
            write(*,*) pin,' ip = ', pout, 'iq.'
            
            pout = RspFConvImperialQuarters2ImperialPints( pin )
            write(*,*) pin,' iq = ', pout, 'ip.'
            pout = RspFConvImperialCups2ImperialGallons( pin )
            write(*,*) pin,' ic = ', pout, 'ig.'
            
            pout = RspFConvImperialGallons2ImperialCups( pin )
            write(*,*) pin,' ig = ', pout, 'ic.'

            pout = RspFConvImperialSpoons2ImperialGallons( pin )
            write(*,*) pin,' is = ', pout, 'ig.'
            
            pout = RspFConvImperialGallons2ImperialSpoons( pin )
            write(*,*) pin,' ig = ', pout, 'is.'

            pout = RspFConvImperialTeaSpoons2ImperialGallons( pin )
            write(*,*) pin,' it = ', pout, 'ig.'
            
            pout = RspFConvImperialGallons2ImperialTeaSpoons( pin )
            write(*,*) pin,' ig = ', pout, 'it.'

            pout = RspFConvImperialLiquidOunces2ImperialGallons( pin )
            write(*,*) pin,' iz = ', pout, 'ig.'
            
            pout = RspFConvImperialGallons2ImperialLiquidOunces( pin )
            write(*,*) pin,' ig = ', pout, 'iz.'

        end subroutine


        !>Shows a list of results for imperial mass conversion functions.
        !>
        subroutine RspFTestImperialMass()

            implicit none
	        
            real( kind = fmath1_p1 ) :: pin, pout

            pin = 1.00
            call RspFComment( 'Imperial - mass units conversion test.' )

            pout = RspFConvOunces2Pounds( pin )
            write(*,*) pin,' oz = ', pout, 'lb.'
            
            pout = RspFConvPounds2Ounces( pin )
            write(*,*) pin,' lb = ', pout, 'oz.'

            pout = RspFConvStones2Pounds( pin )
            write(*,*) pin,' st = ', pout, 'lb.'
            
            pout = RspFConvPounds2Stones( pin )
            write(*,*) pin,' lb = ', pout, 'st.'

            pout = RspFConvShortTons2Pounds( pin )
            write(*,*) pin,' ts = ', pout, 'lb.'
            
            pout = RspFConvPounds2ShortTons( pin )
            write(*,*) pin,' lb = ', pout, 'ts.'

            pout = RspFConvLongTons2Pounds( pin )
            write(*,*) pin,' tl = ', pout, 'lb.'
            
            pout = RspFConvPounds2LongTons( pin )
            write(*,*) pin,' lb = ', pout, 'tl.'

        end subroutine


        !>Shows a list of results for pressure various conversion functions.
        !>
        subroutine RspFTestPressureVarious()

            implicit none
	        
            real( kind = fmath1_p1 ) :: pin, pout

            pin = 1.00
            call RspFComment( 'Various - pressure units conversion test.' )

            pout = RspFConvMillibars2Bars( pin )
            write(*,*) pin,' mb = ', pout, 'ba.'
            
            pout = RspFConvBars2Millibars( pin )
            write(*,*) pin,' ba = ', pout, 'mb.'
                        
            pout = RspFConvNewtonsPerSquareMeters2Pascals( pin )
            write(*,*) pin,' n2 = ', pout, 'pa.'
            
            pout = RspFConvPascals2NewtonsPerSquareMeters( pin )
            write(*,*) pin,' pa = ', pout, 'n2.'
            
            pout = RspFConvPsi2Pascals( pin )
            write(*,*) pin,' ps = ', pout, 'pa.'
            
            pout = RspFConvPascals2Psi( pin )
            write(*,*) pin,' pa = ', pout, 'ps.'
                        
            pout = RspFConvTorr2Pascals( pin )
            write(*,*) pin,' tr = ', pout, 'pa.'
            
            pout = RspFConvPascals2Torr( pin )
            write(*,*) pin,' pa = ', pout, 'tr.'                                    

            pout = RspFConvBars2Pascals( pin )
            write(*,*) pin,' ba = ', pout, 'pa.'
            
            pout = RspFConvPascals2Bars( pin )
            write(*,*) pin,' pa = ', pout, 'ba.'

            pout = RspFConvPhysicalAtmospheres2Pascals( pin )
            write(*,*) pin,' am = ', pout, 'pa.'
            
            pout = RspFConvPascals2PhysicalAtmospheres( pin )
            write(*,*) pin,' pa = ', pout, 'am.'
            
            pout = RspFConvTechnicalAtmospheres2Pascals( pin )
            write(*,*) pin,' at = ', pout, 'pa.'
            
            pout = RspFConvPascals2TechnicalAtmospheres( pin )
            write(*,*) pin,' pa = ', pout, 'at.'

            pout = RspFConvKgOverCm22Pascals( pin )
            write(*,*) pin,' k2 = ', pout, 'pa.'
            
            pout = RspFConvPascals2KgOverCm2( pin )
            write(*,*) pin,' pa = ', pout, 'k2.'

            pout = RspFConvMmhg2Pascals( pin )
            write(*,*) pin,' hg = ', pout, 'pa.'
           
            pout = RspFConvPascals2Mmhg( pin )
            write(*,*) pin,' pa = ', pout, 'hg.'

            pout = RspFConvMetersOfWater2Pascals( pin )
            write(*,*) pin,' ho = ', pout, 'pa.'
           
            pout = RspFConvPascals2MetersOfWater( pin )
            write(*,*) pin,' pa = ', pout, 'ho.'

            pout = RspFConvDecapascals2Pascals( pin )
            write(*,*) pin,' ap = ', pout, 'pa.'
           
            pout = RspFConvPascals2Decapascals( pin )
            write(*,*) pin,' pa = ', pout, 'ap.'
                        
            pout = RspFConvHectopascals2Decapascals( pin )
            write(*,*) pin,' hp = ', pout, 'ap.'
           
            pout = RspFConvDecapascals2Hectopascals( pin )
            write(*,*) pin,' ap = ', pout, 'hp.'
                        
            pout = RspFConvKilopascals2Hectopascals( pin )
            write(*,*) pin,' kp = ', pout, 'hp.'
           
            pout = RspFConvHectopascals2Kilopascals( pin )
            write(*,*) pin,' hp = ', pout, 'kp.'

        end subroutine


        !------------------------------------------------------------------------------
        ! Misc functions.


        !> Universal conversor
        !>
        !> Arguments:
        !> - p_from.
        !> - p_factor.
        !>        
        pure real( kind = fmath1_p1 ) function RspFUconv( p_from, p_factor ) 

            implicit none  
	          
            real( kind = fmath1_p1 ), intent(in) :: p_from, p_factor
            
            RspFUconv = p_from * p_factor

            return
        end function

end module

