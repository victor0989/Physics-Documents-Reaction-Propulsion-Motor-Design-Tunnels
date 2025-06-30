with Sensors; use Sensors;

package body Navigation is
   Position : Vector3D := (X => 0.0, Y => 0.0, Z => 0.0);
   Velocity : Vector3D := (X => 0.0, Y => 0.0, Z => 0.0);

   function Add_Vectors(A, B : Vector3D) return Vector3D is
   begin
      return (X => A.X + B.X, Y => A.Y + B.Y, Z => A.Z + B.Z);
   end Add_Vectors;

   procedure Update_Position(Delta_Time : Float) is
      Acceleration : Vector3D := Sensors.Read_Accelerometer;
   begin
      Velocity := Add_Vectors(Velocity, (X => Acceleration.X * Delta_Time,
                                         Y => Acceleration.Y * Delta_Time,
                                         Z => Acceleration.Z * Delta_Time));
      Position := Add_Vectors(Position, (X => Velocity.X * Delta_Time,
                                         Y => Velocity.Y * Delta_Time,
                                         Z => Velocity.Z * Delta_Time));
   end Update_Position;

   function Get_Position return Vector3D is
   begin
      return Position;
   end Get_Position;
end Navigation;
