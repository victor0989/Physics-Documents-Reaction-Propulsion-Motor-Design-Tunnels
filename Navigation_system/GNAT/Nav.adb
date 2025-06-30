with Ada.Text_IO; use Ada.Text_IO;
with MainNavigation; -- si MainNav es un paquete y tiene procedimiento Sensor_Data

procedure Nav is
begin
   Put_Line("Navegaci�n iniciada.");
   loop
      MainNavigation.Init_Sensors  -- suponiendo que exista ese procedimiento
      delay 0.5;
   end loop;
end Nav;
