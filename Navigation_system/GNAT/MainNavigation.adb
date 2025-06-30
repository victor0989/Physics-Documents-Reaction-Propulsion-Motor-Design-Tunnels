
with Ada.Text_IO; use Ada.Text_IO;

package body MainNavigation is
   procedure Init_Sensors is
   begin
      Put_Line("Inicializando sensores LIDAR, temperatura y proximidad...");
   end Init_Sensors;

   procedure Check_Environment(Data : in Sensor_Data) is
   begin
      if Data.Distance_LIDAR < 10.0 then
         Put_Line("¡Obstáculo cercano detectado por LIDAR!");
      end if;
      if Data.Temp_Radiation > 150.0 then
         Put_Line("¡Alerta: sobrecalentamiento o radiación elevada!");
      end if;
      if Data.Meteor_Prox < 50.0 then
         Put_Line("¡Peligro: meteorito cercano!");
      end if;
   end Check_Environment;

   procedure Navigate is
      D : Sensor_Data := (
         Distance_LIDAR => 8.5,
         Temp_Radiation => 180.0,
         Meteor_Prox    => 45.0
      );
   begin
      Put_Line("Sistema de navegación iniciado.");
      Check_Environment(D);
      Put_Line("Ajustando trayectoria...");
   end Navigate;
end MainNavigation;


