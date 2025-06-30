-- orbital_data.ads
package Orbital_Data is

   type Word_32 is mod 2**32;

   type Orbital_Data is record
      RAAN             : Word_32;
      Pitch            : Word_32;
      Roll             : Word_32;
      Radiation        : Word_32;
      Fluid_Turbulence : Word_32;
      Plasma_Level     : Word_32;
      Star_Proximity   : Word_32;
   end record;

   procedure Read_Orbital_Data(Data_Mem : access Word_32; Orbital : out Orbital_Data);

   procedure Decision_Logic(Orbital : Orbital_Data);

end Orbital_Data;
