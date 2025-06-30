with Ada.Text_IO;               use Ada.Text_IO;
with Ada.Float_Text_IO;         use Ada.Float_Text_IO;
with Ada.Numerics.Elementary_Functions;

procedure Trajectory_Decision is

   type Float_Array is array (1 .. 300) of Float;

   -- Datos orbitales simulados
   RAAN          : Float := 0.0;    -- Ascensión recta del nodo ascendente (en radianes)
   Inclination   : Float := 0.0;    -- Inclinación orbital (rad)
   Attitude_X    : Float := 0.0;    -- Pitch (ejemplo)
   Attitude_Y    : Float := 0.0;    -- Yaw
   Attitude_Z    : Float := 0.0;    -- Roll

   Step_Size     : constant Float := 2.0 * Ada.Numerics.Pi / 8.0;
   R             : Float;
   X, Y          : Float;

   subtype Degrees is Float range 0.0 .. 360.0;

   -- Umbrales para toma de decisiones
   type Maneuver_Mode is (None, Adjust_Attitude, Activate_Sensor, Spin_Stabilize);

   function RAAN_To_Degrees (R : Float) return Float is
   begin
      return (R * 180.0) / Ada.Numerics.Pi;
   end RAAN_To_Degrees;

   procedure Decide (RAAN : Float; Att_X : Float; Att_Y : Float; Att_Z : Float) is
      Decision : Maneuver_Mode := None;
   begin
      -- Lógica de decisión basada en RAAN y actitud
      if RAAN_To_Degrees(RAAN) > 180.0 and RAAN_To_Degrees(RAAN) < 200.0 then
         if abs Att_X > 5.0 then
            Decision := Adjust_Attitude;
         elsif abs Att_Z > 10.0 then
            Decision := Spin_Stabilize;
         else
            Decision := Activate_Sensor;
         end if;
      else
         Decision := None;
      end if;

      Put_Line("RAAN: " & Float'Image(RAAN_To_Degrees(RAAN)) & "°");

      case Decision is
         when Adjust_Attitude =>
            Put_Line(">> Acción: Ajustar actitud.");
         when Activate_Sensor =>
            Put_Line(">> Acción: Activar sensor de orientación.");
         when Spin_Stabilize =>
            Put_Line(">> Acción: Estabilizar por giro.");
         when None =>
            Put_Line(">> No se requiere acción.");
      end case;
   end Decide;

begin
   Put_Line("=== Simulación extendida de actitud y RAAN ===");

   for Step in 1 .. 8 loop
      RAAN := Float(Step) * Step_Size;
      Inclination := 0.9 + Float(Step) * 0.01;

      -- Simulación simplificada de actitud
      Attitude_X := 3.0 + Float(Step);  -- Pitch
      Attitude_Y := 0.0;                -- Yaw fijo
      Attitude_Z := 2.0 * Float(Step);  -- Roll aumenta

      Put_Line("Paso " & Integer'Image(Step));
      Decide(RAAN, Attitude_X, Attitude_Y, Attitude_Z);

      for I in 1 .. 300 loop
         R := 2.0 + 0.05 * Float(I);
         X := R * Float'Cos(RAAN);
         Y := R * Float'Sin(RAAN);

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
end Trajectory_Decision;
