with Ada.Text_IO;               use Ada.Text_IO;
with Ada.Float_Text_IO;         use Ada.Float_Text_IO;

procedure Trajectories is

   type Float_Array is array (1 .. 300) of Float;

   Angle : Float := 0.0;
   R     : Float;
   X, Y  : Float;
   Pi    : constant := 3.1415926535;

begin
   Put_Line("Simulación de trayectorias alrededor de un agujero negro:");
   for Step in 1 .. 8 loop
      Angle := Float(Step) * 2.0 * Pi / 8.0;
      Put_Line("Ángulo: " & Float'Image(Angle));

      for I in 1 .. 300 loop
         R := 2.0 + 0.05 * Float(I);
         X := R * Float'Cos(Angle);
         Y := R * Float'Sin(Angle);

         if I mod 100 = 0 then
            Put("Punto[" & Integer'Image(I) & "] = (");
            Put(X, Fore => 1, Aft => 2, Exp => 0);
            Put(", ");
            Put(Y, Fore => 1, Aft => 2, Exp => 0);
            Put_Line(")");
         end if;
      end loop;
      New_Line;
   end loop;
end Trajectories;
