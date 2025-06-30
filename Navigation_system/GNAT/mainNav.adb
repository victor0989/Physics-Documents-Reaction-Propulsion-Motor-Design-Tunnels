with Ada.Text_IO;                  use Ada.Text_IO;
with Ada.Float_Text_IO;            use Ada.Float_Text_IO;
with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;
with Sensors;                      use Sensors;
with Navigation;                   use Navigation;
with Ethernet_Comm;                use Ethernet_Comm;
with UART_Comm;                    use UART_Comm;

procedure MainNav is

   subtype Seconds is Float;
   Delta_Time   : Seconds := 0.5;
   Current_Time : Float   := 0.0;

   Temp         : Float;
   Meteor_Pos   : Vector3D;
   Danger_Zone  : constant Float := 500.0;

   function Simulate_Meteor_Trajectory(T : Float) return Vector3D is
      X, Y, Z : Float;
   begin
      X := 2000.0 - 5.0 * T;
      Y := 10.0 * Sin(T);
      Z := 5.0 * Cos(T);
      return (X => X, Y => Y, Z => Z);
   end Simulate_Meteor_Trajectory;

begin
   UART_Init;
   Ethernet_Init;
   Put_Line("Sistema de navegación iniciado...");
   UART_Send("NAV SYS ON");
   Ethernet_Send("BOOT: NAVIGATION MODULE ACTIVE");

   loop
      Update_Position(Delta_Time);
      Current_Time := Current_Time + Delta_Time;

      declare
         Pos        : Vector3D := Get_Position;
         Acc        : Vector3D := Read_Accelerometer;
         Local_Temp : Float    := Read_Temperature;
         Local_Meteor_Pos : Vector3D := Simulate_Meteor_Trajectory(Current_Time);

         Dist : Float :=
           Sqrt((Local_Meteor_Pos.X - Pos.X)**2 +
                (Local_Meteor_Pos.Y - Pos.Y)**2 +
                (Local_Meteor_Pos.Z - Pos.Z)**2);
      begin
         Temp       := Local_Temp;
         Meteor_Pos := Local_Meteor_Pos;

         Put_Line("Posición actual: (" &
                  Float'Image(Pos.X) & ", " &
                  Float'Image(Pos.Y) & ", " &
                  Float'Image(Pos.Z) & ")");

         UART_Send("Temp: " & Float'Image(Temp));
         Ethernet_Send("Coord: " & Float'Image(Pos.X) & "," & Float'Image(Pos.Y));

         if Temp > 80.0 then
            Put_Line("Alerta: Temperatura excesiva!");
            UART_Send("TEMP ALERT");
         end if;

         if Dist < Danger_Zone then
            Put_Line("Alerta: Impacto de meteorito inminente!");
            UART_Send("IMPACT WARNING");
            Ethernet_Send("EVASIVE ACTION NEEDED");
         end if;

         delay Duration(Delta_Time);
      end;
   end loop;

end MainNav;

