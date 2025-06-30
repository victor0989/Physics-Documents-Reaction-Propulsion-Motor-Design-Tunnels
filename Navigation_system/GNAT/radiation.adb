-- radiation.adb
with Ada.Text_IO;         use Ada.Text_IO;
with Ada.Float_Text_IO;   use Ada.Float_Text_IO;

procedure Radiation is

   subtype Radiation_Type is Float range 0.0 .. 1.0;

   Sample_Readings : array(1 .. 5) of Radiation_Type :=
     (0.65, 0.82, 0.74, 0.9, 0.6);

   Total     : Radiation_Type := 0.0;
   Average   : Radiation_Type;
   Tolerance : constant Radiation_Type := 0.1;

begin
   for I in Sample_Readings'Range loop
      Put("Radiation[" & Integer'Image(I) & "] = ");
      Put(Sample_Readings(I), Fore => 1, Aft => 2, Exp => 0);
      New_Line;
      Total := Total + Sample_Readings(I);
   end loop;

   Average := Total / Float(Sample_Readings'Length);

   New_Line;
   Put("Average Radiation: ");
   Put(Average, Fore => 1, Aft => 3, Exp => 0);
   New_Line;

   if Average > (1.0 - Tolerance) then
      Put_Line("Warning: High Radiation Detected!");
   elsif Average < Tolerance then
      Put_Line("Warning: Low Radiation Signal - Check Sensors.");
   else
      Put_Line("Radiation Levels Normal.");
   end if;

end Radiation;



