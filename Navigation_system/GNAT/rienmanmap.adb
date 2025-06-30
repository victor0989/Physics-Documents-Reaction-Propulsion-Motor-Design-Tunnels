with Ada.Text_IO;               use Ada.Text_IO;
with Ada.Float_Text_IO;         use Ada.Float_Text_IO;
with Physical_Constants;

procedure RienmanMap is

   subtype Coord is Float;

   type Point is record
      X, Y : Coord;
   end record;

   function Schwarzschild_Radius(Mass : Float) return Float is
      G : constant := 6.67430e-11; -- m^3/kg/s^2
      C : constant := 2.99792458e8; -- m/s
   begin
      return 2.0 * G * Mass / (C * C); -- r_s = 2GM/c²
   end Schwarzschild_Radius;

   BH_Mass : constant := 5.972e24 * 10.0; -- masa 10x Tierra
   R_S     : constant Float := Schwarzschild_Radius(BH_Mass); -- en metros

begin
   Put_Line("Simulación de campo gravitacional de agujero negro:");
   Put("Radio de Schwarzschild: ");
   Put(R_S / 1000.0, Fore => 1, Aft => 2, Exp => 0);  -- km
   New_Line;
end RienmanMap;
