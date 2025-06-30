with Ada.Text_IO;         use Ada.Text_IO;
with Ada.Float_Text_IO;   use Ada.Float_Text_IO;

procedure QuantumSensor is

   subtype Photon_Intensity is Float range 0.0 .. 1.0;

   -- Simulamos 6 lecturas de un detector cuántico
   Sensor_Data : array(1 .. 6) of Photon_Intensity :=
     (0.95, 0.93, 0.50, 0.02, 0.90, 0.01);

   Total_Intensity : Float := 0.0;
   Average_Intensity : Float;
   Coherence_Threshold : constant Float := 0.8;
   Noise_Threshold     : constant Float := 0.1;

begin
   Put_Line("=== Quantum Sensor Readings ===");
   for I in Sensor_Data'Range loop
      Put("Q[" & Integer'Image(I) & "] = ");
      Put(Sensor_Data(I), Fore => 1, Aft => 3, Exp => 0);
      New_Line;

      Total_Intensity := Total_Intensity + Sensor_Data(I);
   end loop;

   Average_Intensity := Total_Intensity / Float(Sensor_Data'Length);

   New_Line;
   Put("Average Photon Intensity: ");
   Put(Average_Intensity, Fore => 1, Aft => 3, Exp => 0);
   New_Line;

   -- Análisis cuántico simplificado
   if Average_Intensity < Noise_Threshold then
      Put_Line("Signal lost - Possible decoherence or sensor fault.");
   elsif Average_Intensity > Coherence_Threshold then
      Put_Line("Stable quantum signal.");
   else
      Put_Line("Quantum instability detected - Possible environmental interference.");
   end if;

   -- Análisis de dispersión extrema (lecturas demasiado dispares)
   if Sensor_Data(1) > 0.9 and Sensor_Data(Sensor_Data'Last) < 0.05 then
      Put_Line("Interference anomaly detected: Entanglement collapse or external field?");
   end if;

end QuantumSensor;
