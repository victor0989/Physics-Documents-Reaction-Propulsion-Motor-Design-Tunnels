with Sensors;

package Navigation is
   procedure Update_Position(Delta_Time : Float);
   function Get_Position return Sensors.Vector3D;
end Navigation;
