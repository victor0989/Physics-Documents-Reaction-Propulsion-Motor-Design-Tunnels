-- SpacePhysics.adb
with Ada.Numerics;         use Ada.Numerics;
with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;

package body SpacePhysics is

   function Vector_Magnitude(V : Vector3D) return Float is
   begin
      return Sqrt(V.X*V.X + V.Y*V.Y + V.Z*V.Z);
   end Vector_Magnitude;

   function Gravitational_Acceleration(Pos : Vector3D; Mass : Float) return Vector3D is
      G : constant Float := 6.67430E-11;
      R : Float := Vector_Magnitude(Pos);
      Acc : Vector3D;
   begin
      if R = 0.0 then
         return (X => 0.0, Y => 0.0, Z => 0.0);
      else
         Acc.X := -G * Mass / (R*R*R) * Pos.X;
         Acc.Y := -G * Mass / (R*R*R) * Pos.Y;
         Acc.Z := -G * Mass / (R*R*R) * Pos.Z;
         return Acc;
      end if;
   end Gravitational_Acceleration;

end SpacePhysics;
