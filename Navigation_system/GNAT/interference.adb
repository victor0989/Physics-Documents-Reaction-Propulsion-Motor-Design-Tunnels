with Ada.Text_IO;         use Ada.Text_IO;
with Ada.Float_Text_IO;   use Ada.Float_Text_IO;

procedure Interference is

   subtype Interference_Drift is Float range -0.000001 .. 0.000001;

   Signal_Shift : array(1 .. 5) of Interference_Drift :=
     (-0.0000002, 0.0000003, -0.0000005, 0.0000008, -0.0000001);

   Total_Drift   : Float := 0.0;
   Average_Drift : Float;
   Drift_Threshold : constant Float := 0.0000005;

begin
   Put_Line("=== Interference Drift Analysis ===");
   for I in Signal_Shift'Range loop
      Put("Drift[" & Integer'Image(I) & "] = ");
      Put(Signal_Shift(I), Fore => 1, Aft => 7, Exp => 0);
      New_Line;
      Total_Drift := Total_Drift + Signal_Shift(I);
   end loop;

   Average_Drift := Total_Drift / Float(Signal_Shift'Length);
   New_Line;
   Put("Average Drift: ");
   Put(Average_Drift, Fore => 1, Aft => 7, Exp => 0);
   New_Line;

   if abs(Average_Drift) > Drift_Threshold then
      Put_Line("Warning: High interference drift detected!");
   else
      Put_Line("Signal interference within nominal range.");
   end if;

end Interference;
