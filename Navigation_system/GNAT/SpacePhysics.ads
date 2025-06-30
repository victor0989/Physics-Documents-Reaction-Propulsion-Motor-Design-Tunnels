-- SpacePhysics.ads
package SpacePhysics is
   type Vector3D is record
      X, Y, Z : Float;
   end record;

   function Vector_Magnitude(V : Vector3D) return Float;
   function Gravitational_Acceleration(Pos : Vector3D; Mass : Float) return Vector3D;
end SpacePhysics;
