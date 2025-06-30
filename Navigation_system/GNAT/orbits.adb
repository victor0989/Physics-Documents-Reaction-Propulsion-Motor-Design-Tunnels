with Ada.Text_IO; use Ada.Text_IO;
with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;

procedure Orbits is

   type Vector3D is array(1 .. 3) of Float;

   type Orbit_Parameters is record
      Semi_Major_Axis : Float := 7000.0;  -- en km
      Eccentricity    : Float := 0.01;
      Inclination     : Float := 30.0;    -- grados
      Period          : Float := 5400.0;  -- segundos
   end record;

   My_Orbit : Orbit_Parameters;

   function Deg_To_Rad(Deg : Float) return Float is
   begin
      return Deg * 3.14159265 / 180.0;
   end Deg_To_Rad;

   function Orbital_Position(Time : Float; Params : Orbit_Parameters) return Vector3D is
      Mean_Anomaly : Float := 2.0 * 3.14159265 * Time / Params.Period;
      X, Y : Float;
   begin
      X := Params.Semi_Major_Axis * Cos(Mean_Anomaly);
      Y := Params.Semi_Major_Axis * Sqrt(1.0 - Params.Eccentricity * Params.Eccentricity) * Sin(Mean_Anomaly);
      return (X, Y, 0.0);
   end Orbital_Position;

begin
   Put_Line("Simulación de órbitas:");

   declare
      T : Integer := 0;
      Step : constant Integer := 600;
   begin
      while T <= Integer(My_Orbit.Period) loop
         declare
            Pos : Vector3D := Orbital_Position(Float(T), My_Orbit);
         begin
            Put_Line("Tiempo " & Integer'Image(T) & " s - Posición: (" &
                     Float'Image(Pos(1)) & ", " &
                     Float'Image(Pos(2)) & ", " &
                     Float'Image(Pos(3)) & ")");
         end;
         T := T + Step;
      end loop;
   end;

end Orbits;
