package Physical_Constants is
   Polar_Radius           : constant := 20_856_010.51;  -- feet
   Equatorial_Radius      : constant := 20_926_469.20;  -- feet
   Earth_Diameter         : constant := 2.0 * ((Polar_Radius + Equatorial_Radius) / 2.0);
   Gravity                : constant := 32.1740_4855_6430_4;  -- ft/s^2
   Sea_Level_Air_Density  : constant := 0.002378;  -- slugs/ft^3
   Altitude_Of_Tropopause : constant := 36089.0;   -- feet
   Tropopause_Temperature : constant := -56.5;     -- Celsius
end Physical_Constants;
