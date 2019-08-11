!> =============================================================================
!> 
!> fmath2.f95 - Astrophysics.
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
!> Sources:
!> - http://annefou.github.io/Fortran/modules/modules.html
!> - https://en.wikipedia.org/wiki/List_of_algorithms
!>
!> Compilation:
!> - ./cfmath.sh or  
!> - mpif90 -std='gnu' -c fmath2.f95 -O3 -march=native -Wall -Wextra -fopenmp 
!>
module fmath2

    use fmath4
    use fmath3

    implicit none

    ! Setting a platform-independent floating point precision.
    integer, parameter :: fmath2_p1 = selected_real_kind( 10,300 )

    !------------------------------------------------------------------------------
    contains

        
        !> Calculates angular momentum based on the propotionality of the moment 
        !> of inertia and angular velocity.
        !>
        !> Arguments:
        !> - p_i: Moment of inertia.
        !> - p_w: Angular velocity.
        !>
        !> Output:
        !> - Angular momentum.
        !>
        !> Sources:
        !> - https://en.wikipedia.org/wiki/Angular_momentum
        !>
        pure real( kind = fmath2_p1 ) function RspFAngularMomentum1( p_i, p_w )
        
            implicit none
            
            real( kind = fmath2_p1 ), intent(in) :: p_i, p_w
            
            RspFAngularMomentum1 = p_i * p_w
        
            return
        end function


        !> Calculates angular momentum based on tangential speed; Tangential speed is considered 
        !> as equivalent to speed in linear momentum.
        !>
        !> Arguments:
        !> - p_r: Radius of rotation.
        !> - p_m: Mass.
        !> - p_v: Tangential speed at radius p_r.
        !>
        !> Output:
        !> - Angular momentum.
        !>
        !> Sources:
        !> - https://en.wikipedia.org/wiki/Angular_momentum
        !>
        pure real( kind = fmath2_p1 ) function RspFAngularMomentum2( p_r, p_m, p_v )
        
            implicit none
            real( kind = fmath2_p1 ), intent(in) :: p_r, p_m, p_v
            
            RspFAngularMomentum2 = p_r * RspFLinearMomentum1( p_m, p_v )
        
            return
        end function


        !> Calculates linear momentum taking into account the Lorentz factor. Will cause a divide by 
        !> zero error at p_v = c .
        !>
        !> Arguments:
        !> - p_m: Rest mass.
        !> - p_v: Velocity at the center of mass.
        !>
        !> Output:
        !> - Linear momentum.
        !>
        !> Sources:
        !> - https://en.wikipedia.org/wiki/Momentum
        !> 
        pure real( kind = fmath2_p1 ) function RspFLinearMomentum1( p_m, p_v )
        
            implicit none
            
            real( kind = fmath2_p1 ), intent(in) :: p_m, p_v
            
            RspFLinearMomentum1 = p_m * p_v * RspFLorentzFactor( p_v )
            
            return
        end function
        
        
        !> Calculates the Lorentz factor. 
        !>
        !> Arguments:
        !> - p_v_km: relative velocity between intertial frames of reference, in kms/s.
        !>
        !> Returns:
        !> - G: Gamma, which is the Lorentz factor.
        !>
        !> Sources:
        !> - https://en.wikipedia.org/wiki/Lorentz_factor 
        !>   
        pure real( kind = fmath2_p1 ) function RspFLorentzFactor( p_v_km )
        
            implicit none
            
            real( kind = fmath2_p1 ), intent(in) :: p_v_km
            
            RspFLorentzFactor = 1 / sqrt( ( 1 - ( ( RspFBetaVelocity( p_v_km ) )**2 ) ) )
            
            return
        end function
        

        !> Calculates the ratio between a given velocity and the speed of light in the vacuum. 
        !>
        !> Arguments:
        !> - p_v_km: velocity, in km/s.
        !>
        !> Output:
        !> - b: beta velocity.
        !>
        !> Sources:
        !> - https://en.wikipedia.org/wiki/Beta_(velocity)
        !>        
        pure real( kind = fmath2_p1 ) function RspFBetaVelocity( p_v_km )
        
            implicit none
            
            real( kind = fmath2_p1 ), intent(in) :: p_v_km
            
            RspFBetaVelocity = p_v_km / RspFConst("c")
            
            return
        end function
        

        !> Calculates the eccentricity of an elliptical galaxy or similar body.
        !>
        !> Arguments:
        !> - p_axis_a_ps: Semi major axis (a) in parsecs.
        !> - p_axis_b_ps: Semi minor axis (b) in parsecs.
        !>
        !> Output:
        !> - Eccentricity.
        !>
        pure real( kind = fmath2_p1 ) function RspFApparentEllipticity( p_axis_a_ps, p_axis_b_ps )
        
            implicit none
            
            real( kind = fmath2_p1 ), intent(in) :: p_axis_a_ps, p_axis_b_ps
            
            RspFApparentEllipticity = 1 - ( p_axis_b_ps / p_axis_a_ps )
            
            return
        end function        
        
        
        !> Calculates the apparent recessional velocity of a redshifted galaxy.
        !>
        !> Arguments:
        !> - p_z: cosmological redshift.
        !> - p_c: speed of light.
        !>
        !> Output:
        !> - ARV.
        !>
        pure real( kind = fmath2_p1 ) function RspFApparentRecessionalVelocity( p_z, p_c ) 

            implicit none
            
            real( kind = fmath2_p1 ), intent(in) :: p_z, p_c
            
            RspFApparentRecessionalVelocity = p_z * p_c

            return
        end function        


        !> Calculates the approximate distance L from a body with mass p_mass2_kg at which Lagrange 
        !> points L1 and L2 are located by calculating its Hill radius assuming a negligible 
        !> eccentricity.
        !>
        !> Arguments:
        !> - p_mass_m1_kg: mass of the bigger (more massive) body of the system.
        !> - p_mass_m2_kg: mass of the lesser body.
        !> - p_a: semi major axis of the orbit of object 1 around object 2. Can use any unit of 
        !>  length. The result will be expressed in those units.
        !>
        !> Output:
        !> - Radius at which points L1 and L2 are located (separated 180 degrees) on the line between 
        !>  bodies with masses p_mass_m1 and p_mass_m2 according to this general distribution (not to 
        !>  scale):
        !>
        !>                            (L4)
        !>
        !>
        !>
        !> (L3) --------- (M1) --------- (L1) --- (M2) --- (L2)
        !>
        !>
        !>
        !>                            (L5)
        !>
        !> Sources:
        !> - https://en.wikipedia.org/wiki/Lagrangian_point
        !>
        !> NEEDS TESTING
        !>
        pure real( kind = fmath2_p1 ) function RspFApproximateLagrangianL1L2( p_mass1_kg, p_mass2_kg, p_a ) 

            implicit none

            real( kind = fmath2_p1 ) :: b
            real( kind = fmath2_p1 ), intent(in) :: p_mass1_kg, p_mass2_kg, p_a

            b = 0.0 
            RspFApproximateLagrangianL1L2 = RspFHillSphere(p_mass1_kg, p_mass2_kg, b, p_a);
            
            return
        end function
        

        !> Calculates the approximate distance L from a body with mass p_mass2_kg at which Lagrange point L3 
        !> is located by calculating its Hill radius assuming a negligible eccentricity.
        !>
        !> Arguments:
        !> - p_mass_m1_kg: Mass of the bigger (more massive) body of the system.
        !> - p_mass_m2_kg: Mass of the lesser body.
        !> - p_a: Semi major axis of the orbit of object 1 around object 2. Can use any unit of 
        !>  length. The result will be expressed in those units.
        !>
        !> Output:
        !> - Radius at which points L3 is located according to this general distribution (not to scale):
        !>
        !>                            (L4)
        !>
        !>
        !>
        !> (L3) --------- (M1) --------- (L1) --- (M2) --- (L2)
        !>
        !>
        !>
        !>                            (L5)
        !>
        !> Sources:
        !> - https://en.wikipedia.org/wiki/Lagrangian_point
        !>
        !> NEEDS TESTING
        !>
        pure real( kind = fmath2_p1 ) function RspFApproximateLagrangianL3( p_mass1_kg, p_mass2_kg, p_a )
        
            implicit none
            
            real( kind = fmath2_p1 ), intent(in) :: p_mass1_kg, p_mass2_kg, p_a
            
            RspFApproximateLagrangianL3 = p_a * ( 2 * ( 5 * p_mass2_kg ) / ( 1 * p_mass1_kg ))

            return
        end function


        !> Finds out if Lagrangian points L3 or L4 in a system of two orbiting massive bodies are 
        !> stable or not depending on the ratio between both masses, which has to be greather than 
        !> the predefined stability limit sl.
        !>
        !> Arguments:
        !> - p_mass1: mass of the main body of the system.
        !> - p_mass2: mass of the satellite.
        !>
        !> Output:
        !> - 1.0 (true) if L3 and L4 are stable, 0.0 (false) otherwise.
        !>
        !> Sources:
        !> - https://en.wikipedia.org/wiki/Lagrangian_point
        !>
        pure real( kind = fmath2_p1 ) function RspFAreLagrangianPointsL3L4Stable( p_mass1, p_mass2 )
        
            implicit none
            
            real( kind = fmath2_p1 ), intent(in) :: p_mass1, p_mass2

            if ( RspFMassRatio( p_mass1, p_mass2 ) > RspFConst("sl") ) then  
                RspFAreLagrangianPointsL3L4Stable = 1.0
            else
                RspFAreLagrangianPointsL3L4Stable = 0.0
            end if

            return
        end function


        !> Calculates the ratio between two masses.
        !>
        !> Arguments:
        !> - p_mass1: Primary mass.
        !> - p_mass2: Secondary mass.
        !>
        !> Output:
        !> - Ratio between both masses.
        !>
        pure real( kind = fmath2_p1 ) function RspFMassRatio( p_mass1, p_mass2 )
        
            implicit none
            
            real( kind = fmath2_p1 ), intent(in) :: p_mass1, p_mass2

            RspFMassRatio = p_mass1 / p_mass2

            return
        end function

    
        !> Approximates the Roche lobe as a sphere of the same volume using the Eggleton formula.
        !>
        !> Arguments:
        !> - p_mass1: mass of the major body.
        !> - p_mass2: mass of the minor body.
        !> - p_a: orbital separation of the system.
        !>
        !> Output:
        !> - Approximate radius of the sphere of same volume as the Roche radius.
        !>
        !> Sources:
        !> - https://en.wikipedia.org/wiki/Roche_lobe
        !> - http://cronodon.com/SpaceTech/Roche_Potential.html
        !>
        !> NEEDS TESTING.
        !>
        pure real( kind = fmath2_p1 ) function RspFApproximateRocheLobeEggleton( p_mass1, p_mass2, p_a )
        
            implicit none

            real( kind = fmath2_p1 ) :: b, q1, q2
            real( kind = fmath2_p1 ), intent(in) :: p_mass1, p_mass2, p_a
        
            q1 = p_mass1 / p_mass2
            q2 = q1**( 2 / 3 )
            b = 3.0
            RspFApproximateRocheLobeEggleton = p_a * ( (0.49 * q2 ) / ( ( 0.6 * q2 ) + log( 1 + RspFNthroot( q1 , b ) ) ) )
            
            return
        end function


        !> Calculates an approxiamte value for the radius of the Hill sphere of an object of p_mass2_kg 
        !> orbiting around an object of p_mass1_kg.
        !>
        !> Arguments:
        !> - p_mass1_kg: mass of the bigger (more massive) body of the system.
        !> - p_mass2_kg: mass of the lesser body.
        !> - p_e: eccentricity of the orbit.
        !> - p_a: semi major axis of the orbit of object 1 around object 2. Can use any unit of 
        !>  length. The result will be expressed in those units.
        !>
        !> Output:
        !> - Approximated radius of the Hill sphere around the lesser body.
        !>
        !> Sources:
        !> - https://en.wikipedia.org/wiki/Hill_sphere
        !> - https://en.wikipedia.org/wiki/Roche_lobe
        !> - https://en.wikipedia.org/wiki/Roche_limit
        !>
        !> NEEDS TESTING
        !>
        pure real( kind = fmath2_p1 ) function RspFHillSphere( p_mass1_kg, p_mass2_kg, p_e, p_a )

            implicit none

            real( kind = fmath2_p1 ) :: b
            real( kind = fmath2_p1 ), intent(in) :: p_mass1_kg, p_mass2_kg, p_e, p_a

            b = 3.0
            RspFHillSphere = p_a * ( 1 - p_e ) * RspFNthroot( ( p_mass2_kg / ( 3 * p_mass1_kg ) ), b )

            return
        end function


        !> Returns the distance in km of an object.
        !>
        !> Arguments:
        !> - p_parallax: Parallax measured in arcseconds.
        !>
        !> Output:
        !> - Distance to the object, measured in km.
        !>
        pure real( kind = fmath2_p1 ) function RspFArcsec2Km( p_parallax )

            implicit none

            real( kind = fmath2_p1 ), intent(in) :: p_parallax

            RspFArcsec2Km = RspFPs2Km( RspFArcsec2Ps( p_parallax ) );    
            
            return
        end function


        !> pe_arcsec_to_ps (p_parallax) returns the distance in parsecs of an object.
        !>
        !> Arguments:
        !> - p_parallax: Parallax, in arcseconds.
        !>
        !> Output:
        !> - Distance to the object, expressed in parsecs.
        !>
        pure real( kind = fmath2_p1 ) function RspFArcsec2Ps( p_parallax )
        
            implicit none

            real( kind = fmath2_p1 ), intent(in) :: p_parallax
            
            RspFArcsec2Ps = 1 / p_parallax;
            
            return
        end function


        !> Converts a distance expressed in parsecs to kilometers.
        !>
        !> Arguments:
        !> - p_distance_in_ps: Distance to the target, in parsecs.
        !>
        !> Output:
        !> - Distance to the target expressed in kilometers.
        !>
        pure real( kind = fmath2_p1 ) function RspFPs2Km( p_distance_in_ps )
        
            implicit none

            real( kind = fmath2_p1 ), intent(in) :: p_distance_in_ps        

            RspFPs2Km = RspFOther2Km( p_distance_in_ps, RspFConst("pk") )
            
            return
        end function


        !> Converts a distance expressed in other units of longitude or distance to kilometres.
        !>        
        !> Arguments:
        !> - p_distance_in_other: distance expressed in other measure.
        !> - p_otherinkm: other distance expressed in km.
        !>
        !> Output:
        !> - Distance in km.
        !>        
        pure real( kind = fmath2_p1 ) function RspFOther2Km( p_distance_in_other, p_otherinkm )
        
            implicit none

            real( kind = fmath2_p1 ), intent(in) :: p_distance_in_other, p_otherinkm

            RspFOther2Km = p_distance_in_other * p_otherinkm
            
            return
        end function


        !> Returns the distance in light years of an object.
        !>
        !> Arguments:
        !> - p_parallax: Parallax, in arcseconds.
        !>
        !> Output:
        !> - Distance to the object, expressed in light years.
        !>        
        pure real( kind = fmath2_p1 ) function RspFArcsec2Ly( p_parallax )
        
            implicit none

            real( kind = fmath2_p1 ), intent(in) :: p_parallax
  
            RspFArcsec2Ly = RspFKm2Ly( RspFArcsec2Km( p_parallax ) )
  
            return
        end function


        !> Converts a distance expressed in km to light years.
        !> 
        !> Arguments:
        !> - p_distance_in_km: Distance to the target expressed in km.
        !> 
        !> Output:
        !> - Distance in light years.
        !>       
        pure real( kind = fmath2_p1 ) function RspFKm2Ly( p_distance_in_km )
        
            implicit none

            real( kind = fmath2_p1 ), intent(in) :: p_distance_in_km
            
            RspFKm2Ly = RspFKm2Other( p_distance_in_km, ( RspFConst("sy") * RspFConst("c") )  )
           
            return
        end function                    


        !> Converts a distance expressed in km to another unit of measure or distance.
        !> 
        !> Arguments:
        !> - p_distance_in_km: Distance to the target expressed in km.
        !> - P_otherinkm: Selected unit of length or distance expressed in km.
        !> 
        !> Output:
        !> - Distance to the target in the selected unit of length.
        !>       
        pure real( kind = fmath2_p1 ) function RspFKm2Other( p_distance_in_km, p_otherinkm )
        
            implicit none

            real( kind = fmath2_p1 ), intent(in) :: p_distance_in_km, p_otherinkm

            RspFKm2Other = p_distance_in_km / p_otherinkm

            return
        end function
        

        !> Returns the astronomical (geometric) albedo of a celestial body.
        !>
        !> Arguments:
        !> - p_absolute_magnitude: absolute magnitude of CB.
        !> - p_diameter_km: diameter of the CB, in km.
        !>
        !> Output:
        !> - Albedo of CB.
        !>
        !> Sources:
        !> - [1] https://en.wikipedia.org/wiki/Albedo
        !>
        !> NEEDS TESTING
        !>      
        pure real( kind = fmath2_p1 ) function RspFAstronomicalAlbedo( p_absolute_magnitude, p_diameter_km )
        
            implicit none

            real( kind = fmath2_p1 ), intent(in) :: p_absolute_magnitude, p_diameter_km

            RspFAstronomicalAlbedo = ( ( 1329.0 * (10.0 ** ( ( (-1.0) * p_absolute_magnitude ) / 5.0 ) ) ) / p_diameter_km )**2.0;

            return
        end function

        
        !> Converts a distance expressed in AU to kilometres.
        !> 
        !> Arguments:
        !> - p_distance_in_au: Distance measured in astronomical units.
        !> 
        !> Output:
        !> - Distance expressed in kilometers.
        !>    
        pure real( kind = fmath2_p1 ) function RspFAu2Km( p_distance_in_au )
        
            implicit none

            real( kind = fmath2_p1 ), intent(in) :: p_distance_in_au

            RspFAu2Km = RspFOther2Km( p_distance_in_au, RspFConst("ak") )

            return
        end function


        !> Calculates the Semi-major axis of the primary's orbit arbarycenter of the system based  
        !> on the distance between the centers ofound the two masses and the values of those masses.
        !> 
        !> Arguments:
        !> - p_a: distance between the centers of the two bodies (semi major axis of the system).
        !> - p_mass1: Mass of the first body.
        !> - p_mass2: Mass of the second body.
        !> 
        !> Output:
        !> - Semi-major axis of the primary's orbit around the barycenter.
        !>
        !> Sources:
        !> - https://en.wikipedia.org/wiki/Barycenter
        !>     
        pure real( kind = fmath2_p1 ) function RspFBarycenter( p_a, p_mass1, p_mass2 )
        
          implicit none
          
            real( kind = fmath2_p1 ), intent(in) :: p_a, p_mass1, p_mass2

            RspFBarycenter = p_a * RspFRatioBeta (p_mass1, p_mass2)

            return
        end function


        !> Finds the result of a = p_q1 / (p_q1 + p_q2).
        !> 
        !> Arguments:
        !> - p_q1
        !> - p_q1
        !> 
        !> Output:
        !> - Ratio alpha.
        !>    
        pure real( kind = fmath2_p1 ) function RspFRatioAlpha( p_q1, p_q2 )
        
            implicit none

            real( kind = fmath2_p1 ), intent(in) :: p_q1, p_q2

            RspFRatioAlpha = p_q1 / (p_q1 + p_q2)

            return
        end function


        !> Finds the result of b = p_q2 / (p_q1 + p_q2)
        !> 
        !> Arguments:
        !> - p_q1
        !> - p_q2
        !> 
        !> Output:
        !> - b.
        !>    
        pure real( kind = fmath2_p1 ) function RspFRatioBeta( p_q1, p_q2 )
        
            implicit none

            real( kind = fmath2_p1 ), intent(in) :: p_q1, p_q2

            RspFRatioBeta = p_q2 / ( p_q1 + p_q2 )

            return
        end function


        !> Calculates data for a simple model of the black body.
        !> 
        !> Arguments:
        !> - p_t: T in Kelvin units.
        !> - p_w: wavelength [0.01:0.01:5.0] microns sweep over a range of wavelengths.
        !> 
        !> Output:
        !> - Result in W/m2/um x 1E8 .
        !>  
        !> Sources:
        !> - MATLAB/Octave/Freemat
        !>    
        pure real( kind = fmath2_p1 ) function RspFBlackBodySimpleModel( p_t, p_w )
          
            implicit none

            real( kind = fmath2_p1 ), intent(in) :: p_t, p_w

            RspFBlackBodySimpleModel = 3.742 / ( ( p_w**5 ) * ( exp( 1.439E4 / ( p_w * p_t ) ) * ( -1.0 ) ) )

            return
        end function
  

        !> Returns the distance in parsecs to a star using the distance modulus m - M as a parameter. 
        !> This version of the equation requires the user to enter the result of the rest between 
        !> apparent and absolute magnitudes.
        !> 
        !> Arguments:
        !> - p_distance_modulus: value of the difference between apparent and absolute magnitudes.
        !> 
        !> Output:
        !> - Distance expressed in parsecs.
        !>  
        !> Sources:
        !> - 
        !>    
        pure real( kind = fmath2_p1 ) function RspFDm2Ps( p_distance_modulus )
        
            implicit none

            real( kind = fmath2_p1 ), intent(in) :: p_distance_modulus

            RspFDm2Ps = 10**( ( p_distance_modulus / 5 ) + 1 )

            return
        end function


        !> Calculates the Doppler factor of a source relative to the observer.
        !> 
        !> Arguments:
        !> - p_v: Velocity, needed to calculate the beta velocity or speed of an object relative 
        !>  to the speed of light, in km/s. 
        !> 
        !> Output:
        !> - Ratio fs/fo
        !>  
        !> Sources:
        !> - https://en.wikipedia.org/wiki/Relativistic_Doppler_effect
        !> - https://en.wikipedia.org/wiki/Beta_(velocity)
        !> - https://en.wikipedia.org/wiki/Hubble%27s_law
        !>    
        pure real( kind = fmath2_p1 ) function RspFDopplerFactor( p_v )
        
            implicit none

            real( kind = fmath2_p1 ) ::  beta
            real( kind = fmath2_p1 ), intent(in) :: p_v

            beta = RspFBetaVelocity( p_v )
            RspFDopplerFactor = sqrt( ( ( 1 + beta ) / ( 1 - beta ) ) )

            return
        end function
  

        !> Estimates the number of probable civilizations in our galaxy that might have a sufficient 
        !> degree of technology that would allow them to communicate. In other words, it guesses how 
        !> many people we might call to chat in our place in the universe. This is the original form 
        !> of the Drake equation.
        !>
        !> Arguments:
        !> - p_r: average rate of star formation in the galaxy, per year.
        !> - p_fo: fraction of star systems with planets.
        !> - p_ne: average number of life-supporting planets per system in those systems with planets.
        !> - p_fl: fraction of those p_ne planets that actually develop life. 
        !> - p_fi: fraction of those P_fl planets that develop civilizations.
        !> - p_fc: fraction of hose p_fi civilizations that developed communications technologies.
        !> - p_l: length of time, in years over which such civilizations broadcast their messages.
        !>
        !> Output:
        !> - Number of probable communicative civilizations in the Milky Way.
        !>
        !> Sources:
        !> - https://en.wikipedia.org/wiki/Drake_equation
        !>    
        pure real( kind = fmath2_p1 ) function RspFDrakeEquation1( p_r, p_fo, p_ne, p_fl, p_fi, p_fc, p_l )
        
            implicit none

            real( kind = fmath2_p1 ), intent(in) :: p_r, p_fo, p_ne, p_fl, p_fi, p_fc, p_l

            RspFDrakeEquation1 = p_r * p_fo * p_ne * p_fl * p_fi * p_fc * p_l

            return
        end function  


        !> Estimates the number of probable civilizations in our galaxy that might have a sufficient 
        !> degree of technology that would allow them to communicate. In other words, it guesses how 
        !> many people we might call to chat in our place in the universe. This is the Jul 2013 
        !> Popular Science variant of the Drake equation that includes the Dalek variable.
        !>
        !> Arguments:
        !> - p_r: average rate of star formation in the galaxy, per year.
        !> - p_fo: fraction of star systems with planets.
        !> - p_ne: average number of life-supporting planets per system in those systems with planets.
        !> - p_fl: fraction of those p_ne planets that actually develop life. 
        !> - p_fi: fraction of those P_fl planets that develop civilizations.
        !> - p_fc: fraction of hose p_fi civilizations that developed communications technologies.
        !> - p_l: length of time, in years over which such civilizations broadcast their messages.
        !> - p_fd: Dalek factor. Number of civilizations that can survive an alien attack.
        !>
        !> Output:
        !> - Number of probable communicative civilizations in the Milky Way.
        !>   
        !> Sources:
        !> - https://en.wikipedia.org/wiki/Drake_equation
        !>    
        pure real( kind = fmath2_p1 ) function RspFDrakeEquation2( p_r, p_fo, p_ne, p_fl, p_fi, p_fc, p_l, p_fd )
        
            implicit none

            real( kind = fmath2_p1 ), intent(in) :: p_r, p_fo, p_ne, p_fl, p_fi, p_fc, p_l, p_fd

            RspFDrakeEquation2 = p_r * p_fo * p_ne * p_fl * p_fi * p_fc * p_l * p_fd

            return
        end function  
        

        !> Equation of state of a perfect fluid.
        !>
        !> Arguments:
        !> - p_pressure: Force applied perpendicularly to a surface per unit area, expressed in 
        !>  kg / (m * s**2).
        !> - p_energy_density: amount of energy stored in a given system or region of space per 
        !>  unit volume or mass, expressed in kg / (m * s**2).
        !>
        !> Output
        !> - Dimentionless ratio.
        !>
        !> Sources:
        !> - [1] https://en.wikipedia.org/wiki/Equation_of_state_(cosmology)
        !> - [2] https://en.wikipedia.org/wiki/Pressure
        !> - [3] https://en.wikipedia.org/wiki/Energy_density
        !>    
        pure real( kind = fmath2_p1 ) function RspFStateEquation( p_pressure, p_energy_density )
        
            implicit none

            real( kind = fmath2_p1 ), intent(in) :: p_pressure, p_energy_density

            RspFStateEquation = p_pressure / p_energy_density

            return
        end function  
        

        !> Calculates the escape velocity for an object from a sphericaly simmetrical, non-rotating 
        !> massive body of mass m and a given radius.
        !>
        !> Arguments:
        !> - p_m_mass_kg: mass m, in kg.
        !> - p_radius_km: radius of the object whose mass is M.
        !>
        !> Output:
        !> - Escape velocity, expressed in km/s.
        !>
        !> NEEDS CHECKING TO ACCOUNT FOR RELATIVISTIC EFFECTS.
        !>    
        pure real( kind = fmath2_p1 ) function RspFEscapeVelocity( p_m_mass_kg, p_radius_km )
        
            implicit none

            real( kind = fmath2_p1 ), intent(in) :: p_m_mass_kg, p_radius_km

            RspFEscapeVelocity = ( 2.0 * ( RspFStandardGravitationalParameter( p_m_mass_kg ) / p_radius_km ) )**(0.5)

            return
        end function  
        

        !> Returns the standard gravitational parameter of a celestial body, measured in km**3 * s**-2.
        !>
        !> Arguments:
        !> - p_mass_kg: mass of the object in question, expressed in kg.
        !>
        !> Output: 
        !> - Standard gratitational parameter of a celestial body, measured  in km**3 * s**-2.
        !>    
        pure real( kind = fmath2_p1 ) function RspFStandardGravitationalParameter( p_mass_kg )

            implicit none

            real( kind = fmath2_p1 ), intent(in) :: p_mass_kg

            RspFStandardGravitationalParameter = ( RspFConst("G") * p_mass_kg ) / 10**9

            return
        end function  
        

        !> Calculates the radius of the theoretical event horizon of a non-rotating object.
        !>
        !> Arguments:
        !> - p_mass_kg: Mass of the spherial, non-rotating object, in kg.
        !> 
        !> Output:
        !> - Radius of the event horizon, defined as the radius from which the escape velocity 
        !>  equals the speed of light, expressed in km.
        !>    
        pure real( kind = fmath2_p1 ) function RspFFindEventHorizon( p_mass_kg )
        
            implicit none

            real( kind = fmath2_p1 ), intent(in) :: p_mass_kg

            ! Escape velocity Ve should equal speed light in the case of an event horizon.
            RspFFindEventHorizon = 2.0 * ( RspFStandardGravitationalParameter( p_mass_kg ) / ( RspFConst("c") )**2 )

            return
        end function
        

        !> Returns the gravitational binding energy ratio of a non uniform spherical system. 
        !>
        !> Arguments:
        !> - p_mass_kg: mass of a body, in kg.
        !> - p_radus_km: radius of the pody of mass p_mass_kg, in km.
        !>
        !> Output:
        !> - Gravitational binding energy of a body of given mass and radius, expressed in Joules.
        !>
        !> Sources:
        !> - [1] https://en.wikipedia.org/wiki/Gravitational_binding_energy        
        !>
        !> NEEDS TESTING.
        !>    
        pure real( kind = fmath2_p1 ) function RspFGravitationalBindingEnergyNus( p_mass_kg,p_radius_km )
        
            implicit none

            real( kind = fmath2_p1 ), intent(in) :: p_mass_kg,p_radius_km

            RspFGravitationalBindingEnergyNus = ( RspFConst("c")**2 * p_mass_kg ) / ( p_radius_km - ( 738313.0 * p_mass_kg ) )

            return
        end function


        !> Returns the gravitational binding energy of a uniform spherical system of uniform density.
        !>
        !> Arguments:
        !> - p_mass_kg: mass of a body, in kg.
        !> - p_radus_km: radius of the pody of mass p_mass_kg, in km.
        !>
        !> Output:
        !> - U: Gravitational binding energy of a body of given mass and radius, expressed in 
        !>  Joules. Convention is U positive.
        !>
        !> Sources:
        !> - [1] https://en.wikipedia.org/wiki/Gravitational_binding_energy
        !>
        !> NEEDS TESTING.
        !>    
        pure real( kind = fmath2_p1 ) function RspFGravitationalBindingEnergyUs( p_mass_kg, p_radius_km )
        
            implicit none

            real( kind = fmath2_p1 ), intent(in) :: p_mass_kg, p_radius_km

            RspFGravitationalBindingEnergyUs = ( 3.0 * RspFConst("G") * ( p_mass_kg )**2 ) / (5.0 * p_radius_km * 1000.0)

            return
        end function


        !> Given two masses orbiting slowly, compared to the speed of light at a certain distance, 
        !> this function calculates the energy that such a system gives away.
        !>
        !> Arguments:
        !> - p_mass1_kg: mass of the first body, in kg.
        !> - p_mass2_kg: mass of the second body, in kg.
        !> - p_r_km: distance separating the two bodies.
        !>
        !> Output:
        !> - Energy that the described system gives away.
        !>
        !> Sources:
        !> - https://en.wikipedia.org/wiki/Gravitational_wave
        !>
        !> NEEDS TESTING.
        !>    
        pure real( kind = fmath2_p1 ) function RspFGravitationalWave( p_mass1_kg, p_mass2_kg, p_r_km )
        
            implicit none

            real( kind = fmath2_p1 ), intent(in) :: p_mass1_kg, p_mass2_kg, p_r_km

            RspFGravitationalWave = ( -1.0 ) * ( 32.0 / 5.0 ) * ( ( RspFConst("G")**4 ) / &
            ( RspFConst("c")**5 ) ) * ( ( ( ( p_mass1_kg * p_mass2_kg )**2) * ( p_mass1_kg + &
            p_mass2_kg ) ) / ( p_r_km**5 ) )

            return
        end function


        !> Finds out if a body of a given mass and radius is a black hole based on the calculation 
        !> of its escape velocity. If VES >= c, given the parameters passed, the object is a black hole.
        !>
        !> Arguments:
        !> - p_M_mass_kg: mass m, in kg.
        !> - p_radius_km: radius of the object whose mass is m.
        !>
        !> Output:
        !> - 0 if the body in question is not a BH, 1 if it is.
        !>    
        pure real( kind = fmath2_p1 ) function RspFIsBlackHole( p_m_mass_kg, p_radius_km )
        
            implicit none

            real( kind = fmath2_p1 ), intent(in) :: p_m_mass_kg, p_radius_km

            if ( ( RspFEscapeVelocity(p_M_mass_kg, p_radius_km) ) >= ( RspFConst("c") ) ) then
                RspFIsBlackHole = 1.0
            else
                RspFIsBlackHole = 0.0
            endif
            
            return
        end function


        !> Finds out if a given mass is beyond the Chandrasekhar limit or not.
        !>
        !> Arguments:
        !> p_mass_kg: mass of the object in question, expressed in kilograms.
        !>
        !> Output:
        !> - 1.0 if true, 0.0 if false.
        !>   
        pure real( kind = fmath2_p1 ) function RspFIsOverChandrasekharLimit( p_mass_kg )
        
            implicit none

            real( kind = fmath2_p1 ), intent(in) :: p_mass_kg

            if ( p_mass_kg > ( RspFConst("Ch") * RspFConst("Sm") ) ) then
                RspFIsOverChandrasekharLimit = 1.0
            else
                RspFIsOverChandrasekharLimit = 0.0
            endif

            return
        end function


        !> Converts a mass given in kilograms to its equivalent value measured in Earth mass units.
        !>
        !> Arguments:
        !> - p_mass_kg: mass in kilograms
        !>
        !> Output:
        !> - Mass expressed in Earth mass units.
        !>   
        real( kind = fmath2_p1 ) function RspFKg2Em( p_mass_kg )
        
            implicit none

            real( kind = fmath2_p1 ) :: p_mass_kg

            RspFKg2Em = p_mass_kg / RspFConst("Em")

            return
        end function
  

        !> Converts a mass given in kilograms to its equivalent value measured in solar mass units.
        !>
        !> Arguments:
        !> - p_mass_kg: mass in kilograms
        !>
        !> Output:
        !> - Mass expressed in solar mass units.
        !>   
        real( kind = fmath2_p1 ) function RspFKg2Sm( p_mass_kg )
        
            implicit none

            real( kind = fmath2_p1 ) :: p_mass_kg

            RspFKg2Sm = p_mass_kg / RspFConst("Sm")

            return
        end function


        !> Calculates the kinetic energy of a moving object.
        !>
        !> Arguments:
        !> - p_mass: mass of the moving object
        !> - p_velocity: Velocity of the object.
        !>
        !> Output:
        !> - Kinetic energy.
        !>   
        real( kind = fmath2_p1 ) function RspFKineticEnergy( p_mass, p_velocity )
        
            implicit none

            real( kind = fmath2_p1 ) :: p_mass, p_velocity

            RspFKineticEnergy = ( 0.5 * p_mass ) * p_velocity**2

            return
        end function


        !> Converts a distance expressed in km to astronomical units.
        !>
        !> Arguments:
        !> - p_distance_in_km: Disance to the target expressed in km.
        !>
        !> Output:
        !> - Distance in astronomical units.
        !>   
        real( kind = fmath2_p1 ) function RspFKm2Au( p_distance_in_km )
        
            implicit none

            real( kind = fmath2_p1 ) :: p_distance_in_km

            RspFKm2Au = RspFKm2Other( p_distance_in_km, RspFConst("ak") )

            return
        end function


        !> Converts a distance expressed in km to multiples of Earths mean radius.
        !>
        !> Arguments:
        !> - p_distance_in_km: Distance expressed in km.
        !>
        !> Output:
        !> - Distance as a multiple of Earths radius.
        !>   
        real( kind = fmath2_p1 ) function RspFKm2Er( p_distance_in_km )
        
            implicit none

            real( kind = fmath2_p1 ) :: p_distance_in_km

            RspFKm2Er = RspFKm2Other( p_distance_in_km, RspFConst("Er") )

            return
        end function


        !> Converts a distance expressed in km to parsecs.
        !>
        !> Arguments:
        !> - p_distance_in_km: ddisance to the target expressed in km.
        !>
        !> Output:
        !> - Distance in parsecs.
        !>
        real( kind = fmath2_p1 ) function RspFKm2Ps( p_distance_in_km )
        
            implicit none

            real( kind = fmath2_p1 ) :: p_distance_in_km

            RspFKm2Ps = RspFKm2Other( p_distance_in_km, RspFConst("pk") )

            return
        end function


        !> Calculates luminosity of an ideal black body using the Stefan-Botzmann constant.
        !>
        !> Arguments:
        !> - p_area_m: area, in square meters.
        !> - p_temperature_k: temperature, measured in degrees Kelvin.
        !>
        !> Output:
        !> - Luminosity.
        !>
        !> Sources:
        !> - https://en.wikibooks.org/wiki/Introduction_to_Astrophysics/Stars/Luminosity
        !>
        !> NEEDS TESTING.
        !>
        real( kind = fmath2_p1 ) function RspFLuminosity1( p_area_m, p_temperature_k )
        
            implicit none

            real( kind = fmath2_p1 ) :: p_area_m, p_temperature_k

            RspFLuminosity1 = RspFConst("SB") * p_area_m * p_temperature_k**4

            return
        end function


        !> Converts a distance in light years to kilometers.
        !>
        !> Arguments:
        !> - p_distance_in_ly: distance tot he target measured in light years.
        !>
        !> Output:
        !> - Distance to the target expressed in kilometers.
        !>
        real( kind = fmath2_p1 ) function RspFLy2Km( p_distance_in_ly )
        
            implicit none

            real( kind = fmath2_p1 ) :: p_distance_in_ly

            RspFLy2Km = RspFOther2Km( p_distance_in_ly, ( RspFConst("sy") * RspFConst("c") ) )

            return
        end function


        !> Converts a distance in light years to parsecs.
        !>
        !> Arguments:
        !> - p_distance_in_ly: distance tot he target measured in light years.
        !>
        !> Output:
        !> - Distance to the target expressed in parsecs.
        !>
        real( kind = fmath2_p1 ) function RspFLy2Ps( p_distance_ly )
        
            implicit none

            real( kind = fmath2_p1 ) :: p_distance_ly

            RspFLy2Ps = RspFKm2Ps( RspFLy2Km( p_distance_ly ) )

            return
        end function


        !> Returns the distance in parsecs to a star using the distance modulus m - M as a 
        !> parameter. This version of the equation requires the user to enter the apparent and 
        !> absolute magnitudes of the star in question.
        !>
        !> Arguments:
        !> - p_apparent_magnitude: apparent magnitude of the target.
        !> - p_absolute_magnitude: absolute magnitude of the target.
        !>
        !> Output:
        !> - Distance measured in ligth years.
        !>
        real( kind = fmath2_p1 ) function RspFMagnitudes2Ly( p_apparent_magnitude, p_absolute_magnitude )
        
            implicit none

            real( kind = fmath2_p1 ) :: p_apparent_magnitude, p_absolute_magnitude

            RspFMagnitudes2Ly = RspFPs2Ly( RspFDm2Ps( p_apparent_magnitude - p_absolute_magnitude ) )

            return
        end function


        !> Returns the distance in parsecs to a star using the distance modulus m - M as a 
        !> parameter. This version of the equation requires the user to enter the apparent and 
        !> absolute magnitudes of the star.
        !>
        !> Arguments:
        !> - p_apparent_magnitude: Apparent magnitude of the target.
        !> - p_absolute_magnitude: Absolute magnitude of the target.
        !>
        !> Output:
        !> - Distance measured in parsecs.
        !>
        real( kind = fmath2_p1 ) function RspFMagnitudes2Ps( p_apparent_magnitude, p_absolute_magnitude )
        
            implicit none

            real( kind = fmath2_p1 ) :: p_apparent_magnitude, p_absolute_magnitude

            RspFMagnitudes2Ps = RspFDm2Ps( p_apparent_magnitude - p_absolute_magnitude )

            return
        end function


        !> Returns the Schwarzchild radius for an object.
        !>
        !> Arguments:
        !> - p_mass_kg: Mass of the object expressed in kg.
        !>
        !> Output:
        !> - Schwarzschild radius of the object of p_mass_kg, expressed in km.
        !>
        real( kind = fmath2_p1 ) function RspFMass2SchwarzschildRadius( p_mass_kg )
        
            implicit none

            real( kind = fmath2_p1 ) :: p_mass_kg

            RspFMass2SchwarzschildRadius = ( ( 2.0 * RspFConst("G") * p_mass_kg ) / ( RspFConst("c") * 1000.0 )**2 ) / 1000.0

            return
        end function


        !> Calculates the matter density parameter for the reformulated Friedman equation using the 
        !> Hubble parameter. The universe will be flat if k = 0 or if:
        !>
        !> - p_d_crit = (3 * H**2) / (8 * pi * G)
        !>
        !> - If output = 1 => Universe is flat, static.
        !> - If output < 1 => Hyperbolic universe => Expands indefinitely.
        !> - If output > 1 => Closed universe => Will collapse.
        !>
        !> Arguments:
        !> - p_d_1: observed density
        !> - p_d_crit: critical density.
        !>
        !> Output
        !> - Matter density parameter value.
        !>
        real( kind = fmath2_p1 ) function RspFMatterDensityParameter( p_d1, p_d_crit )
        
            implicit none

            real( kind = fmath2_p1 ) :: p_d1, p_d_crit

            RspFMatterDensityParameter = p_d1 / p_d_crit

            return
        end function


        !> Calculates the time average angular velocity as an orbiting boddy progresses along its 
        !> elliptical orbit around a central body.
        !>
        !> Arguments:
        !> - p_mass1_kg: Mass of the main body, in kg.
        !> - p_mass2_kg: Mass of the satellite, in kg.
        !> - p_a_km: semi major axis of the orbit, in km.
        !>
        !> Output:
        !> - Mean motion of an orbiting body in rad/s.
        !>
        !> Sources:
        !> - https://en.wikipedia.org/wiki/Mean_motion
        !>
        !> NEEDS TESTING.
        !>
        real( kind = fmath2_p1 ) function RspFMeanMotion( p_mass1_kg, p_mass2_kg, p_a_km )
        
            implicit none

            real( kind = fmath2_p1 ) :: p_mass1_kg, p_mass2_kg, p_a_km

            RspFMeanMotion = sqrt( ( RspFConst("G") * ( p_mass1_kg + p_mass2_kg ) / (10.0**9) ) / ( ( p_a_km )**3 ) )

            return
        end function


        !> Converts a distance expressed in parsecs to light years.
        !>
        !> Arguments:
        !> - p_distance_in_ps: distance to the target, in parsecs.
        !>
        !> Output:
        !> - Distance to the target expressed in light years.
        !> 
        real( kind = fmath2_p1 ) function RspFPs2Ly( p_distance_ps )
        
            implicit none

            real( kind = fmath2_p1 ) :: p_distance_ps

            RspFPs2Ly = RspFKm2Ly( RspFPs2Km( p_distance_ps ) )

            return
        end function


        !> Calculates the ration between the actual radius and the Schwarzschild radius of a 
        !> non-rotating object.
        !>
        !> Arguments:
        !> - p_mass_kg: Mass of the object, in kg.
        !> - p_radius_km: Radius of the object, in km.
        !>
        !> Output:
        !> - Ratio between the actual and Schwarzschild radii of an object. If Rr>=1 then the object 
        !> must be a black hole.
        !> 
        real( kind = fmath2_p1 ) function RspFRadius2SchwarzschildRatio( p_mass_kg, p_radius_km )
        
            implicit none

            real( kind = fmath2_p1 ) :: p_mass_kg, p_radius_km

            RspFRadius2SchwarzschildRatio = RspFMass2SchwarzschildRadius( p_mass_kg ) / p_radius_km

            return
        end function


        !> Calculates the cosmological doppler redshift in an expanding Big Bang universe (Friedmann–Lemaître–Robertson–Walker model).
        !>
        !> Arguments:
        !> - p_z_obs: redshift now (Observed wavelength). 
        !> - p_z_init: redshift then (Initial wavelength).
        !>
        !> Output:
        !> - Redshift.
        !>
        !> Sources
        !> - https://en.wikipedia.org/wiki/Redshift
        !> - https://en.wikipedia.org/wiki/Friedmann%E2%80%93Lema%C3%AEtre%E2%80%93Robertson%E2%80%93Walker_metric
        !>
        !> NEEDS TESTING.
        !> 
        real( kind = fmath2_p1 ) function RspFRedshiftFlrw( p_z_obs, p_z_init )
        
            implicit none

            real( kind = fmath2_p1 ) :: p_z_obs, p_z_init

            RspFRedshiftFlrw = ( p_z_obs / p_z_init ) - 1.0

            return
        end function
        

        !> Calculates the relativistic doppler redshift in a Minkowski (flat) spacetime, for motion 
        !> completely radial.
        !>
        !> Arguments:
        !> - p_v_km: Velocity in km/s
        !>
        !> Output:
        !> - Redshift.
        !>
        !> Resoures
        !> - https://en.wikipedia.org/wiki/Redshift        
        !>
        !> NEEDS TESTING.
        !> 
        real( kind = fmath2_p1 ) function RspFRedshiftRdrm( p_v_km )
        
            implicit none

            real( kind = fmath2_p1 ) :: p_v_km, beta

            beta = RspFBetaVelocity( p_v_km )
            RspFRedshiftRdrm = sqrt( ( ( 1 + beta ) / ( 1 - beta ) ) ) - 1
            
            return
        end function
        

        !> Calculates the Roche limit for a primary 1 and a satellite 2, which is considered to be 
        !> solid. Units can be chosen by the user but should be selected consistently.
        !>
        !> Arguments:
        !> - p_radius_1: radius of the primary body.
        !> - p_density_1: density of the primary body.
        !> - p_density_2: density of the satellite.
        !>
        !> Output:
        !> - Roche limit. 
        !>
        !> Sources:
        !> - https://en.wikipedia.org/wiki/Roche_limit
        !>
        !> Note:
        !> - The results in this case are only an approximation. More factors should probably be taken 
        !> into  account.        
        !> 
        real( kind = fmath2_p1 ) function RspFRocheLimitFluid( p_radius_1, p_density_1, p_density_2 )
        
            implicit none

            real( kind = fmath2_p1 ) :: p_radius_1, p_density_1, p_density_2, p_a

            p_a = 3.0
            RspFRocheLimitFluid = 2.455 * p_radius_1 * RspFNthroot( ( p_density_1 / p_density_2 ), p_a )

            return
        end function
        

        !> Calculates the Roche limit for a primary 1 and a satellite 2, which is considered to be 
        !> solid. Units can be chosen by the user but should be selected consistently.
        !>
        !> Arguments:
        !> - p_radius_2: Radius of the secondary body.
        !> - p_mass_1: Mass of the primary body.
        !> - p_mass_2: Mass of the satellite.
        !>
        !> Output:
        !> - Roche limit. 
        !>
        !> Sources:
        !> - https://en.wikipedia.org/wiki/Roche_limit
        !>
        !> Note:
        !> - The results in this case are only an approximation. More factors should probably be 
        !> taken into account.        
        !> 
        real( kind = fmath2_p1 ) function RspFRocheLimitSolid( p_radius_2, p_mass_1, p_mass_2 )
        
            implicit none

            real( kind = fmath2_p1 ) :: p_radius_2, p_mass_1, p_mass_2, p_a 

            p_a = 3.0
            RspFRocheLimitSolid = 1.26 * p_radius_2 * RspFNthroot( ( p_mass_1 / p_mass_2 ), p_a )

            return
        end function
        

        !> Given light received at present time with a redshipf p_z, according to the 
        !> Friedmann–Lemaître–Robertson–Walker metric, it calculates the scale factor a at the time 
        !> the light was emitted.
        !>
        !> Arguments:
        !> - p_z: redshift.
        !>
        !> Output:
        !> - Scale factor.
        !>
        !> Sources: 
        !> - https://en.wikipedia.org/wiki/Scale_factor_(cosmology)
        !> - https://en.wikipedia.org/wiki/Friedmann%E2%80%93Lema%C3%AEtre%E2%80%93Robertson%E2%80%93Walker_metric
        !> - https://en.wikipedia.org/wiki/Redshift        
        !>
        !> NEEDS TESTING.
        !>  
        real( kind = fmath2_p1 ) function RspFScaleFactor( p_z )
        
            implicit none

            real( kind = fmath2_p1 ) :: p_z

            RspFScaleFactor = 1.0 + ( 1.0 / p_z )

            return
        end function


        !> Converts a number of seconds into the equivalent time measured in years.
        !>
        !> Arguments:
        !> - p_time_in_seconds
        !>
        !> Output:
        !> - Number of years.
        !>
        real( kind = fmath2_p1 ) function RspFSeconds2Years( p_time_in_seconds )
        
            implicit none

            real( kind = fmath2_p1 ) :: p_time_in_seconds

            RspFSeconds2Years = p_time_in_seconds / RspFConst("sy")

            return
        end function


        !> Finds the diameter of a distance object using the small angle formula.
        !>
        !> Arguments:
        !> - p_angular_size_arcsec: observed angular size at p_distance_km.
        !> - p_distance_km: distance to the object in km.
        !>
        !> Output:
        !> - Diameter of the object, expressed in km.
        !>
        real( kind = fmath2_p1 ) function RspFSmallAngle2Diameter( p_angular_size_arcsec, p_distance_km )
        
            implicit none

            real( kind = fmath2_p1 ) :: p_angular_size_arcsec, p_distance_km

            RspFSmallAngle2Diameter = (p_angular_size_arcsec * p_distance_km) / RspFConst("ra")

            return
        end function


        !> Converts a mass given in solar mass units to its equivalent value measured in kilograms.
        !>
        !> Arguments:
        !> - p_mass_sm: mass expressed in solar mass units.
        !>
        !> Output:
        !> - Mass expressed in kilograms.
        !> 
        real( kind = fmath2_p1 ) function RspFSm2Kg( p_mass_sm )
        
            implicit none

            real( kind = fmath2_p1 ) :: p_mass_sm

            RspFSm2Kg = p_mass_sm * RspFConst("sm")

            return
        end function


        !> Calculates the rate of energy radiation per area unit in a black body.
        !>
        !> Arguments:
        !> - p_t_k: Termodynamic temperature of the BB. Surface temperature of the black body, in 
        !>  degrees Kelvin.
        !>
        !> Output:
        !> - Radiant excitance, radiant flux emitted per unit area, expressed in W / (m**2). 
        !>
        !> Sources:
        !> - https://en.wikibooks.org/wiki/Introduction_to_Astrophysics/Laws_and_Formulae
        !> - https://en.wikipedia.org/wiki/Stefan%E2%80%93Boltzmann_law
        !> - https://en.wikipedia.org/wiki/Radiant_exitance
        !> - http://www.newport.com/Laws-of-Radiation/381843/1033/content.aspx
        !>
        real( kind = fmath2_p1 ) function RspFStefanLaw( p_t_k )
        
            implicit none

            real( kind = fmath2_p1 ) :: p_t_k

            RspFStefanLaw = RspFConst("SB") * ( p_t_k**4 )

            return
        end function


        !> Calculates the surface gravity in a spherically-symmetric celestial body; that is, the 
        !> gravitational acceleration experienced at its surface. Results are returned as multiples 
        !> of the proper values for planet Earth.
        !>
        !> Arguments:
        !> - p_radius_in_km: mean radius of the celestial object, in km.
        !> - p_mass_kg: mass of the object, in kg.
        !>
        !> Output: 
        !> - Surface gravitational acceleration as a multiple of the surface gravity of Earth.
        !>
        !> Sources:
        !> - https://en.wikipedia.org/wiki/Surface_gravity
        !>
        real( kind = fmath2_p1 ) function RspFSurfaceGravity1( p_radius_in_km, p_mass_kg )
        
            implicit none

            real( kind = fmath2_p1 ) :: p_radius_in_km, p_mass_kg

            RspFSurfaceGravity1 = RspFKg2Em( p_mass_kg ) / ( RspFKm2Er( p_radius_in_km ) )**2

            return
        end function


        !> Calculates the surface gravity in a spherically-symmetric celestial body; that is, the 
        !> gravitational acceleration experienced at its surface. 
        !>
        !> Arguments:
        !> - p_radius_in_km: mean radius of the celestial object, in km.
        !> - p_mass_kg: mass of the object, in kg.
        !>
        !> Output:
        !> - g: Surface gravitational acceleration on the surface.
        !>
        !> Sources:
        !> - https://en.wikipedia.org/wiki/Surface_gravity
        !>
        real( kind = fmath2_p1 ) function RspFSurfaceGravity2( p_radius_in_km, p_mass_kg )
        
            implicit none

            real( kind = fmath2_p1 ) :: p_radius_in_km, p_mass_kg

            RspFSurfaceGravity2 = RspFConst("G") * p_mass_kg / p_radius_in_km**2

            return
        end function


        !> RspFYorkTime - York time as defined by Alcubierre (1994).
        !>
        !> Arguments:
        !> - p_xs: xs.
        !> - p_vs: vs.
        !> - p_rs: rs.
        !> - p_df: df.
        !> - p_drs: drs.
        !>                   
        !> Sources:
        !> - 1: "The Alcubierre Warp Drive in Higher Dimensional Spacetime",
        !>   H. G. White 1 and E. W. Davis, Inst. for Advanced Studies at
        !>   Austin.
        !>   - 1.1: p 2.
        !>
        !> Note:
        !> -  NEEDS TESTING.
        !>        
        real( kind = fmath2_p1 ) function RspFYorkTime( p_xs, p_vs, p_rs, p_df, p_drs )
        
            implicit none

            real( kind = fmath2_p1 ) :: p_vs, p_xs, p_rs, p_df, p_drs            

            !> [1.1] 
            RspFYorkTime = (p_vs / RspFConst("c")) * (p_xs / p_rs) * (p_df / p_drs)
            
            return
        end function
        

        !> RspFMetricAlcubierre1994 - Alcubierre metric, 1994.
        !>
        !> Sources:
        !> - 1: "The Alcubierre Warp Drive in Higher Dimensional Spacetime",
        !>   H. G. White 1 and E. W. Davis, Inst. for Advanced Studies at
        !>   Austin.
        !>   - 1.1: p 2.
        !>
        !> Output:
        !> - ds**2 (see sources and metric specification).
        !>
        !> Note:
        !> -  NEEDS TESTING.
        !>
        real( kind = fmath2_p1 ) function RspFMetricAlcubierre1994( p_dt, p_dx, p_dy, p_dz, p_vst, p_frs )
        
            implicit none

            real( kind = fmath2_p1 ) :: p_dt, p_dx, p_dy, p_dz, p_vst, p_frs

            !> Alcubierre metric [1.1]
            RspFMetricAlcubierre1994 = (((-1) * RspFConst("c")**2) * p_dt**2) + (p_dx - (p_vst * p_frs * p_dt ))**2 + &
                 p_dy**2 + p_dz**2
            
            return
        end function

        
        !> RspFSpacetimeExpansionBoostWhite2003 - Field equation for spacetione expansion Boost, according to White (2003).
        !>
        !> Arguments:
        !> - p_vs: vs.
        !> - p_frs: frs.
        !>
        !> Sources:
        !> - 1: "The Alcubierre Warp Drive in Higher Dimensional Spacetime",
        !>   H. G. White 1 and E. W. Davis, Inst. for Advanced Studies at
        !>   Austin.
        !>   - 1.2: p 3.
        !>
        !> Note:
        !> -  NEEDS TESTING.
        !>        
        real( kind = fmath2_p1 ) function RspFSpacetimeExpansionBoostWhite2003 ( p_vs, p_frs )
        
            implicit none

            real( kind = fmath2_p1 ) :: p_vs, p_frs
            
            !> See [1.2]
            RspFSpacetimeExpansionBoostWhite2003 = cosh( 0.5 * ( log( abs( 1 - ( ( p_vs / RspFConst("c") )**2 * p_frs**2 )))))
            
            return
        end function
        
        
        !==============================================================================
        ! Test.
  

        !> This function contains all tests for fmath2 functions and subroutines.
        !>
        !> Output:
        !> - Test results for fmath2.
        !>       
        subroutine RspFTestFmath2All()
            
            implicit none

            call system("clear")
            call RspFTestFmath21()
            write(*,*)
            call RspFTestFmath22()
            write(*,*)
            call RspFTestFmath23()
            write(*,*)
            call RspFTestFmath24()
            write(*,*)
            call RspFTestFmath25()
            write(*,*)
            call RspFTestFmath26()
            write(*,*)
            call RspFTestFmath27()
            write(*,*)
            
        end subroutine

  
        !> Test for fmath2 subroutines and functions.
        !>
        !> - RspFAngularMomentum1(...)
        !> - RspFAngularMomentum2(...)
        !> - RspFLinearMomentum1(...)
        !> - RspFLorentzFactor(...)
        !> - RspFBetaVelocity(...)
        !> - RspFApparentEllipticity(...)
        !> - RspFApparentRecessionalVelocity(...)
        !> - RspFApproximateLagrangianL1L2(...)
        !> - RspFApproximateLagrangianL3(...)
        !> - RspFAreLagrangianPointsL3L4Stable(...)
        !> - RspFMassRatio(...)
        !>
        !> Output:
        !> - Test results for fmath2.
        !>       
        subroutine RspFTestFmath21()
            
            implicit none

            real( kind = fmath4_p1 ) :: n, a, b, c
            integer :: x1
            
            n = 100
            a = 1.0
            b = RspFConst("ak")
            c = RspFConst("c")
            
            call RspFLine()
            call RspFCommentEn( "Begin RspFTestFmath21" )
            
            call RspFComment( "Testing RspFAngularMomentum1(...)" ) 
            do x1 = 1,10
                write(*,*) x1
                write(*,*) n
                write(*,*) RspFAngularMomentum1( ( x1 * a ) , n )
                write(*,*)       
            end do
 
            call RspFComment( "Testing RspFAngularMomentum2(...)" )              
            do x1 = 1,10
                write(*,*) x1
                write(*,*) n
                write(*,*) RspFAngularMomentum2( ( x1 * a ), n, ( x1 / n ) )
                write(*,*)       
            end do
 
            call RspFComment( "Testing RspFLinearMomentum1(...)" )              
            do x1 = 1,10
                write(*,*) x1
                write(*,*) n
                write(*,*) RspFLinearMomentum1( n,  ( x1 * a ) )
                write(*,*)       
            end do

            call RspFComment( "Testing RspFLorentzFactor(...)" )              
            do x1 = 1,10
                write(*,*) x1
                write(*,*) n
                write(*,*) RspFLorentzFactor( x1 * a * 100000.0 )
                write(*,*)       
            end do

            call RspFComment( "Testing RspFBetaVelocity(...)" )              
            do x1 = 1,10
                write(*,*) x1
                write(*,*) RspFBetaVelocity( x1 * a )
                write(*,*)       
            end do

            call RspFComment( "Testing RspFApparentEllipticity(...)" )              
            do x1 = 1,10
                write(*,*) x1
                write(*,*) (a / x1)
                write(*,*) RspFApparentEllipticity( x1 * a, a / x1 )
                write(*,*)       
            end do

            call RspFComment( "Testing RspFApparentRecessionalVelocity(...)" )              
            do x1 = 1,10
                write(*,*) x1
                write(*,*) c
                write(*,*) RspFApparentRecessionalVelocity( x1 * a, c )
                write(*,*)       
            end do

            call RspFComment( "Testing RspFApproximateLagrangianL1L2(...)" )              
            do x1 = 1,10
                write(*,*) x1, " ", RspFConst("Sm"), " ", RspFConst("Em")
                write(*,*) b * x1
                write(*,*) RspFApproximateLagrangianL1L2( RspFConst("Sm"), RspFConst("Em"), b * x1 )
                write(*,*)       
            end do

            call RspFComment( "Testing RspFApproximateLagrangianL3(...)" )              
            do x1 = 1,10
                write(*,*) x1, " ", RspFConst("Sm"), " ", RspFConst("Em")
                write(*,*) b * x1
                write(*,*) RspFApproximateLagrangianL3( RspFConst("Sm"), RspFConst("Em"), b * x1 )
                write(*,*)       
            end do

            call RspFComment( "Testing RspFAreLagrangianPointsL3L4Stable(...)" )              
            do x1 = 1,10
                write(*,*) RspFConst("Em")**x1
                write(*,*) RspFConst("Em") 
                write(*,*) RspFAreLagrangianPointsL3L4Stable( RspFConst("Em")**x1, RspFConst("Em") )
                write(*,*)       
            end do

            call RspFComment( "Testing RspFMassRatio(...)" )              
            do x1 = 1,10
                write(*,*) a * x1, " ", a* x1**x1
                write(*,*) RspFMassRatio( a* x1, a * x1**x1 )
                write(*,*)       
            end do
        
            call RspFCommentEn( "End RspFTestFmath21" )
            
        end subroutine  


        !> Test for fmath2 subroutines and functions.
        !>
        !> - RspFApproximateRocheLobeEggleton(...)
        !> - RspFHillSphere(...)
        !> - RspFArcsec2Km(...)
        !> - RspFArcsec2Ps(...)
        !> - RspFPs2Km(...)
        !> - RspFOther2Km(...)
        !> - RspFArcsec2Ly(...)
        !> - RspFKm2Other(...)
        !> - RspFAstronomicalAlbedo(...)
        !> - RspFAu2Km(...)
        !> - RspFBarycenter(...)
        !>
        !> Output:
        !> - Test results for fmath2.
        !>       
        subroutine RspFTestFmath22()
            
            implicit none

            real( kind = fmath4_p1 ) :: n, a, b, c
            integer :: x1
            
            n = 100
            a = 1.0
            b = RspFConst("ak")
            c = RspFConst("c")
            
            call RspFLine()
            call RspFCommentEn( "Begin RspFTestFmath22" )
            
            call RspFComment( "Testing RspFApproximateRocheLobeEggleton(...)" ) 
            do x1 = 1,10
                write(*,*) x1, RspFConst("Sm"), x1 * RspFConst("Sm")
                write(*,*) n
                write(*,*) RspFApproximateRocheLobeEggleton( x1 * RspFConst("Sm"), RspFConst("Sm"), RspFConst("Sr") )
                write(*,*)       
            end do

            call RspFComment( "Testing RspFHillSphere(...)" ) 
            do x1 = 1,10
                write(*,*) x1, RspFConst("Sm"), x1 * RspFConst("Sm")
                write(*,*) ( 1 /  (x1 * a ) ), n
                write(*,*) RspFHillSphere( RspFConst("Sm"), x1 * RspFConst("Sm"), ( 1 /  (x1 * a ) ), n  )
                write(*,*)      
            end do

            call RspFComment( "Testing RspFArcsec2Km(...)" ) 
            do x1 = 1,10
                write(*,*) x1
                write(*,*) x1**x1
                write(*,*) RspFArcsec2Km( x1 * a )
                write(*,*)      
            end do

            call RspFComment( "Testing RspFArcsec2Ps(...)" ) 
            do x1 = 1,10
                write(*,*) x1
                write(*,*) x1**x1
                write(*,*) RspFArcsec2Ps( x1 * a )
                write(*,*)      
            end do

            call RspFComment( "Testing RspFPs2Km(...)" ) 
            do x1 = 1,10
                write(*,*) x1
                write(*,*) x1**x1
                write(*,*) RspFPs2Km( x1 * a )
                write(*,*)      
            end do

            call RspFComment( "Testing RspFOther2Km(...)" ) 
            do x1 = 1,10
                write(*,*) x1
                write(*,*) 1 / ( x1 * a )
                write(*,*) RspFOther2Km( x1 * a, 1 / ( x1 * a ) )
                write(*,*)      
            end do

            call RspFComment( "Testing RspFArcsec2Ly(...)" ) 
            do x1 = 1,10
                write(*,*) x1
                write(*,*) RspFArcsec2Ly( x1 * a )
                write(*,*)      
            end do

            call RspFComment( "Testing  RspFKm2Other(...)" ) 
            do x1 = 1,10
                write(*,*) x1 * a
                write(*,*) 1 / ( x1 * a )
                write(*,*) RspFKm2Other( x1 * a, ( 1 / ( x1 * a  ) ) )
                write(*,*)      
            end do

            call RspFComment( "Testing RspFAstronomicalAlbedo(...)" ) 
            do x1 = 1,10
                write(*,*) x1 * a
                write(*,*) RspFConst("Sr")
                write(*,*) RspFAstronomicalAlbedo( x1 * a, RspFConst("Sr") )
                write(*,*)      
            end do

            call RspFComment( "Testing RspFAu2Km(...)" ) 
            do x1 = 1,10
                write(*,*) x1 * a
                write(*,*) RspFAu2Km( x1 * a )
                write(*,*)      
            end do
            
            call RspFComment( "Testing RspFBarycenter(...)" ) 
            do x1 = 1,10
                write(*,*) x1 * a
                write(*,*) x1 * a * 2, ( x1 * a )**2
                write(*,*) RspFBarycenter( x1 * a, x1 * a * 2, ( x1 * a )**2 )
                write(*,*)      
            end do

            call RspFCommentEn( "End RspFTestFmath22" )
            
        end subroutine


        !> Test for fmath2 subroutines and functions.
        !> - RspFRatioAlpha(...)
        !> - RspFRatioBeta(...)
        !> - RspFBlackBodySimpleModel(...)
        !> - RspFDm2Ps(...)
        !> - RspFDopplerFactor(...)
        !> - RspFDrakeEquation1(...)
        !> - RspFDrakeEquation2(...)
        !> - RspFStateEquation(...)
        !> - RspFEscapeVelocity(...)
        !> - RspFStandardGravitationalParameter(...)
        !> - RspFGravitationalBindingEnergyNus(...)
        !> - RspFGravitationalBindingEnergyUs(...)
        !>
        !> Output:
        !> - Test results for fmath2.
        !>       
        subroutine RspFTestFmath23()
            
            implicit none

            real( kind = fmath4_p1 ) :: n, a
            integer :: x1
            
            n = 100
            a = 1.0
            
            call RspFLine()
            call RspFCommentEn( "Begin RspFTestFmath23" )

            call RspFComment( "Testing RspFRatioAlpha(...)" ) 
            do x1 = 1,10
                write(*,*) x1 * a
                write(*,*) x1 * a * 2
                write(*,*) RspFRatioAlpha( x1 * a, x1 * a * 2 )
                write(*,*)      
            end do

            call RspFComment( "Testing RspFRatioBeta(...)" ) 
            do x1 = 1,10
                write(*,*) x1 * a
                write(*,*) x1 * a * 2
                write(*,*) RspFRatioBeta( x1 * a, x1 * a * 2 )
                write(*,*)      
            end do

            call RspFComment( "Testing RspFBlackBodySimpleModel(...)" ) 
            do x1 = 1,10
                write(*,*) x1 * a * 100
                write(*,*) x1 * a * 0.01
                write(*,*) RspFBlackBodySimpleModel( x1 * a * 100, x1 * a * 0.01 )
                write(*,*)      
            end do

            call RspFComment( "Testing RspFDm2Ps(...)" ) 
            do x1 = 1,10
                write(*,*) x1 * a
                write(*,*) RspFDm2Ps( x1 * a )
                write(*,*)      
            end do

            call RspFComment( "Testing RspFDopplerFactor(...)" ) 
            do x1 = 1,10
                write(*,*) x1 * a * 10000
                write(*,*) RspFDopplerFactor( x1 * a * 10000 )
                write(*,*)      
            end do
            
            call RspFComment( "Testing RspFDrakeEquation1(...)" ) 
            do x1 = 1,10
                write(*,*) ( 1 / ( x1 * a ) )
                write(*,*) 0.01, 0.02, 0.03, 0.04, 0.05, 0.06
                write(*,*) RspFDrakeEquation1( 0.01 * a, 0.02 * a, 0.03 * a, 0.04 * a, 0.05 * a, 0.06 * a, &
                     ( 1 / ( x1 * a ) ) )
                write(*,*)      
            end do
            
            call RspFComment( "Testing RspFDrakeEquation2(...)" ) 
            do x1 = 1,10
                write(*,*) ( 1 / ( x1 * a ) )
                write(*,*) 0.01, 0.02, 0.03, 0.04, 0.05, 0.06
                write(*,*) RspFDrakeEquation2( 0.01 * a, 0.02 * a, 0.03 * a, 0.04 * a, 0.05 * a, 0.06 * a, &
                     ( 1 / ( x1 * a ) ), 0.07 * a )
                write(*,*)      
            end do

            call RspFComment( "Testing RspFStateEquation(...)" ) 
            do x1 = 1,10
                write(*,*) x1 * a
                write(*,*) x1 * a * 2
                write(*,*) RspFStateEquation( x1 * a, x1 * a * 2  )
                write(*,*)      
            end do

            call RspFComment( "Testing RspFEscapeVelocity(...)" ) 
            do x1 = 1,10
                write(*,*) x1 * a
                write(*,*) x1 * a * 2
                write(*,*) RspFEscapeVelocity( x1 * a, x1 * a * 2  )
                write(*,*)      
            end do            

            call RspFComment( "Testing RspFStandardGravitationalParameter(...)" ) 
            do x1 = 1,10
                write(*,*) ( x1 * a ) ** 2
                write(*,*) RspFStandardGravitationalParameter( ( x1 * a ) ** 2  )
                write(*,*)      
            end do

            call RspFComment( "Testing RspFGravitationalBindingEnergyNus(...)" ) 
            do x1 = 1,10
                write(*,*) x1 * a, ( x1 * a ) ** 2
                write(*,*) RspFGravitationalBindingEnergyNus( x1 * a, ( x1 * a ) ** 2  )
                write(*,*)      
            end do

            call RspFComment( "Testing  RspFGravitationalBindingEnergyUs(...)" ) 
            do x1 = 1,10
                write(*,*) x1 * a, ( x1 * a ) ** 2
                write(*,*) RspFGravitationalBindingEnergyUs( x1 * a, ( x1 * a ) ** 2  )
                write(*,*)      
            end do
            
            call RspFCommentEn( "End RspFTestFmath23" )
            
        end subroutine


        !> Test for fmath2 subroutines and functions.
        !>
        !> - RspFIsBlackHole(...)
        !> - RspFIsOverChandrasekharLimit(...)
        !> - RspFKg2Em(...)
        !> - RspFKg2Sm(...)
        !> - RspFKineticEnergy(...)
        !> - RspFKm2Au(...)
        !> - RspFKm2Er(...)
        !> - RspFLuminosity1(...)
        !> - RspFLy2Km(...)
        !> - RspFLy2Ps(...)
        !> - RspFMagnitudes2Ly(...)
        !> - RspFMagnitudes2Ps(...)
        !> - RspFMass2SchwarzschildRadius(...)
        !> - RspFMatterDensityParameter(...)
        !>
        !> Output:
        !> - Test results for fmath2.
        !>       
        subroutine RspFTestFmath24()
            
            implicit none

            real( kind = fmath4_p1 ) :: n, a
            integer :: x1
            
            n = 100
            a = 1.0
            
            call RspFLine()
            call RspFCommentEn( "Begin RspFTestFmath24" )

            call RspFComment( "Testing RspFIsBlackHole(...)" ) 
            do x1 = 1,10
                write(*,*) x1 * a * RspFConst("Sm"), a * RspFConst("Sr")
                write(*,*) RspFIsBlackHole( x1 * a * RspFConst("Sm"), a * RspFConst("Sr") )
                write(*,*)      
            end do

            call RspFComment( "Testing RspFIsOverChandrasekharLimit(...)" ) 
            do x1 = 1,10
                write(*,*) x1 * a * RspFConst("Sm")
                write(*,*) RspFIsOverChandrasekharLimit( x1 * a * RspFConst("Sm") )
                write(*,*)      
            end do            

            call RspFComment( "Testing RspFKg2Em(...)" ) 
            do x1 = 1,10
                write(*,*) x1 * a
                write(*,*) RspFKg2Em( x1 * a  )
                write(*,*)      
            end do

            call RspFComment( "Testing RspFKg2Sm(...)" ) 
            do x1 = 1,10
                write(*,*) x1 * a
                write(*,*) RspFKg2Sm( x1 * a  )
                write(*,*)      
            end do

            call RspFComment( "Testing RspFKineticEnergy(...)" ) 
            do x1 = 1,10
                write(*,*) x1 * a, RspFConst("c")/10
                write(*,*) RspFKineticEnergy( x1 * a, RspFConst("c")/10  )
                write(*,*)      
            end do

            call RspFComment( "Testing RspFKm2Au(...)" ) 
            do x1 = 1,10
                write(*,*) x1 * a
                write(*,*) RspFKm2Au( x1 * a )
                write(*,*)      
            end do

            call RspFComment( "Testing RspFKm2Er(...)" ) 
            do x1 = 1,10
                write(*,*) x1 * a
                write(*,*) RspFKm2Er( x1 * a )
                write(*,*)      
            end do

            call RspFComment( "Testing RspFKm2Ps(...)" ) 
            do x1 = 1,10
                write(*,*) x1 * a
                write(*,*) RspFKm2Ps( x1 * a )
                write(*,*)      
            end do

            call RspFComment( "Testing RspFLuminosity1(...)" ) 
            do x1 = 1,10
                write(*,*) x1 * a
                write(*,*) RspFLuminosity1( x1 * a, x1 * a )
                write(*,*)      
            end do

            call RspFComment( "Testing RspFLy2Km(...)" ) 
            do x1 = 1,10
                write(*,*) x1 * a
                write(*,*) RspFLy2Km( x1 * a )
                write(*,*)      
            end do

            call RspFComment( "Testing RspFLy2Ps(...)" ) 
            do x1 = 1,10
                write(*,*) x1 * a
                write(*,*) RspFLy2Ps( x1 * a )
                write(*,*)      
            end do
            
            call RspFComment( "Testing RspFMagnitudes2Ly(...)" ) 
            do x1 = 1,10
                write(*,*) x1 * a, x1 * a * 2
                write(*,*) RspFMagnitudes2Ly( x1 * a, x1 * a * 2 )
                write(*,*)      
            end do

            call RspFComment( "Testing RspFMagnitudes2Ps(...)" ) 
            do x1 = 1,10
                write(*,*) x1 * a, x1 * a * 2
                write(*,*) RspFMagnitudes2Ps( x1 * a, x1 * a * 2 )
                write(*,*)      
            end do

            call RspFComment( "Testing RspFMass2SchwarzschildRadius(...)" ) 
            do x1 = 1,10
                write(*,*) x1 * a
                write(*,*) RspFMass2SchwarzschildRadius( x1 * a )
                write(*,*)      
            end do

            call RspFComment( "Testing RspFMatterDensityParameter(...)" ) 
            do x1 = 1,10
                write(*,*) x1 * a, x1 * a * 2
                write(*,*) RspFMatterDensityParameter( x1 * a, x1 * a * 2 )
                write(*,*)      
            end do            
            
            call RspFCommentEn( "End RspFTestFmath24" )
            
        end subroutine


        !> Test for fmath2 subroutines and functions.
        !>
        !> - RspFMeanMotion(...)
        !> - RspFPs2Ly(...)
        !> - RspFRadius2SchwarzschildRatio(...)
        !> - RspFRedshiftFlrw(...)
        !> - RspFRedshiftRdrm(...)
        !> - RspFRocheLimitFluid(...)
        !> - RspFRocheLimitSolid(...)
        !> - RspFScaleFactor(...)
        !> - RspFSeconds2Years(...)
        !> - RspFSeconds2Years(...)
        !> - RspFSmallAngle2Diameter(...)
        !> - RspFSm2Kg(...)
        !> - RspFStefanLaw(...)
        !>
        !> Output:
        !> - Test results for fmath2.
        !>       
        subroutine RspFTestFmath25()
            
            implicit none

            real( kind = fmath4_p1 ) :: n, a
            integer :: x1
            
            n = 100
            a = 1.0
            
            call RspFLine()
            call RspFCommentEn( "Begin RspFTestFmath25" )

            call RspFComment( "Testing RspFMeanMotion(...)" ) 
            do x1 = 1,10
                write(*,*) a * RspFConst("Sm"), a * RspFConst("Em"), x1 * a * RspFConst("ak")
                write(*,*) RspFMeanMotion( a * RspFConst("Sm"), a * RspFConst("Em"), x1 * a * RspFConst("ak") )
                write(*,*)      
            end do

            call RspFComment( "Testing RspFPs2Ly(...)" ) 
            do x1 = 1,10
                write(*,*) x1 * a
                write(*,*) RspFPs2Ly( x1 * a )
                write(*,*)      
            end do

            call RspFComment( "Testing RspFRadius2SchwarzschildRatio(...)" ) 
            do x1 = 1,10
                write(*,*) a * RspFConst("Sm"), a**x1
                write(*,*) RspFRadius2SchwarzschildRatio( a * RspFConst("Sm"), a**x1 )
                write(*,*)      
            end do

            call RspFComment( "Testing RspFRedshiftFlrw(...)" ) 
            do x1 = 1,10
                write(*,*) a * x1, a * x1 * 0.9
                write(*,*) RspFRedshiftFlrw( a * x1, a * x1 * 0.9 )
                write(*,*)      
            end do

            call RspFComment( "Testing RspFRedshiftRdrm(...)" ) 
            do x1 = 1,10
                write(*,*) a * x1**x1
                write(*,*) RspFRedshiftRdrm( a * x1**x1  )
                write(*,*)      
            end do

            call RspFComment( "Testing RspFRocheLimitFluid(...)" ) 
            do x1 = 1,10
                write(*,*) ( a * x1 * RspFConst("Er") ), ( a / x1 ), ( a / 0.9 * x1 )
                write(*,*) RspFRocheLimitFluid( ( a * x1 * RspFConst("Er") ), ( a / x1 ), ( a / 0.9 * x1 ) )
                write(*,*)      
            end do

            call RspFComment( "Testing RspFRocheLimitSolid(...)" ) 
            do x1 = 1,10
                write(*,*) ( a * x1 * RspFConst("Er") ), ( a / x1 ), ( a / 0.9 * x1 )
                write(*,*) RspFRocheLimitSolid( ( a * x1 * RspFConst("Er") ), ( a / x1 ), ( a / 0.9 * x1 ) )
                write(*,*)      
            end do

            call RspFComment( "Testing RspFScaleFactor(...)" ) 
            do x1 = 1,10
                write(*,*) a * x1**x1
                write(*,*) RspFScaleFactor( a * x1**x1 )
                write(*,*)      
            end do

            call RspFComment( "Testing RspFSeconds2Years(...)" ) 
            do x1 = 1,10
                write(*,*) a * x1
                write(*,*) RspFSeconds2Years( a * x1 )
                write(*,*)      
            end do

            call RspFComment( "Testing RspFSeconds2Years(...)" ) 
            do x1 = 1,10
                write(*,*) a * x1
                write(*,*) RspFSeconds2Years( a * x1 )
                write(*,*)      
            end do

            call RspFComment( "Testing RspFSmallAngle2Diameter(...)" ) 
            do x1 = 1,10
                write(*,*) a * x1, a * x1 * RspFConst("ak")
                write(*,*) RspFSmallAngle2Diameter( a * x1, a * x1 * RspFConst("ak") )
                write(*,*)      
            end do

            call RspFComment( "Testing RspFSm2Kg(...)" ) 
            do x1 = 1,10
                write(*,*) a * x1
                write(*,*) RspFSm2Kg( a * x1 )
                write(*,*)      
            end do

            call RspFComment( "Testing RspFStefanLaw(...)" ) 
            do x1 = 1,10
                write(*,*) a * x1
                write(*,*) RspFStefanLaw( a * x1 )
                write(*,*)      
            end do
            
            call RspFCommentEn( "End RspFTestFmath25" )
            
        end subroutine


        !> Test for fmath2 subroutines and functions.
        !>
        !> - RspFSurfaceGravity1(...)
        !> - RspFSurfaceGravity2(...) 
        !>
        !> Output:
        !> - Test results for fmath2.
        !>       
        subroutine RspFTestFmath26()
            
            implicit none

            real( kind = fmath4_p1 ) :: n, a
            integer :: x1
            
            n = 100
            a = 1.0
            
            call RspFLine()
            call RspFCommentEn( "Begin RspFTestFmath26" )

            call RspFComment( "Testing RspFSurfaceGravity1(...)" ) 
            do x1 = 1,10
                write(*,*) a * x1, a * x1
                write(*,*) RspFSurfaceGravity1( a * x1, a * x1 )
                write(*,*)      
            end do
           
            call RspFComment( "Testing RspFSurfaceGravity2(...)" ) 
            do x1 = 1,10
                write(*,*) a * x1, a * x1
                write(*,*) RspFSurfaceGravity2( a * x1, a * x1 )
                write(*,*)      
            end do
            
            call RspFCommentEn( "End RspFTestFmath26" )
            
        end subroutine


        !> Test for fmath2 subroutines and functions.
        !>
        !> - RspFYorkTime(...)
        !> - RspFMetricAlcubierre1994(...) 
        !>
        !> Output:
        !> - Test results for fmath2.
        !>       
        subroutine RspFTestFmath27()
            
            implicit none

            real( kind = fmath4_p1 ) :: n, a
            integer :: x1
            
            n = 100
            a = 1.0
            
            call RspFLine()
            call RspFCommentEn( "Begin RspFTestFmath27" )

            call RspFComment( "Testing RspFYorkTime(...)" ) 
            do x1 = 1,10
                write(*,*) a * x1, a * x1
                write(*,*) RspFYorkTime( a * x1, a * x1, a * x1, a * x1, a * x1 )
                write(*,*)      
            end do
           
            call RspFComment( "Testing RspFMetricAlcubierre1994(...)" ) 
            do x1 = 1,10
                write(*,*) a * x1, a * x1
                write(*,*) RspFMetricAlcubierre1994( a * x1, a * x1 * 2, a * 0, a * 0, a * x1 * 5, a * x1 * 6 )
                write(*,*)      
            end do
            
            call RspFCommentEn( "End RspFTestFmath27" )
            
        end subroutine
 
end module

