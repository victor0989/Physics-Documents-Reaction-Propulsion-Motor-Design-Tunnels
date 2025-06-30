with Ada.Text_IO;       use Ada.Text_IO;
with Navigation;        use Navigation;
with Sensors;           use Sensors;

procedure Main is

   task Sensor_Reader;
   task Navigation_Controller;

   task body Sensor_Reader is
   begin
      loop
         declare
            A : Vector3D := Read_Accelerometer;
         begin
            Put_Line("Sensor: A = (" &
                     Float'Image(A.X) & ", " &
                     Float'Image(A.Y) & ", " &
                     Float'Image(A.Z) & ")");
         end;
         delay 0.05;
      end loop;
   end Sensor_Reader;

   task body Navigation_Controller is
   begin
      loop
         Update_Position(0.05);
         declare
            P : Vector3D := Get_Position;
         begin
            Put_Line("Nav: P = (" &
                     Float'Image(P.X) & ", " &
                     Float'Image(P.Y) & ", " &
                     Float'Image(P.Z) & ")");
         end;
         delay 0.05;
      end loop;
   end Navigation_Controller;

begin
   Put_Line("Sistema de navegación multitarea iniciado.");
   delay 1.0;
   Put_Line("Fin del main (las tareas siguen ejecutándose).");
end Main;
