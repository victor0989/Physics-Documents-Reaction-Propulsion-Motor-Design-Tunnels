with Ada.Text_IO; use Ada.Text_IO;
with GNAT.Serial_Communications; use GNAT.Serial_Communications;

procedure Send_Trajectory is
   Port : Serial_Port;
   Angle : Float := 0.0;
   R, X, Y : Float;
   Buffer : String (1 .. 100);
   Last   : Natural;

begin
   Open(Port, "COM3"); -- O el puerto usado en Linux/Windows
   Set_Speed(Port, B9600);

   for Step in 1 .. 8 loop
      Angle := Float(Step) * 2.0 * 3.14159 / 8.0;
      for I in 1 .. 100 loop
         R := 2.0 + 0.05 * Float(I);
         X := R * Float'Cos(Angle);
         Y := R * Float'Sin(Angle);

         -- Serializa la posición en el UART
         Buffer := "X=" & Float'Image(X) & " Y=" & Float'Image(Y) & ASCII.CR;
         Last := Buffer'Length;
         Write(Port, Buffer(1..Last));
      end loop;
   end loop;

   Close(Port);
end Send_Trajectory;
