with Ada.Numerics.Float_Random;

package body Sensors is

   Gen : Ada.Numerics.Float_Random.Generator;

   function Read_Accelerometer return Vector3D is
      V : Vector3D;
   begin
      -- No reseteamos el generador aquí, para que varíe cada llamada
      V.X := Ada.Numerics.Float_Random.Random(Gen) * 0.1 - 0.05;
      V.Y := Ada.Numerics.Float_Random.Random(Gen) * 0.1 - 0.05;
      V.Z := Ada.Numerics.Float_Random.Random(Gen) * 0.1 - 0.05;
      return V;
   end Read_Accelerometer;

   function Read_Temperature return Float is
   begin
      return 65.0 + 10.0 * Ada.Numerics.Float_Random.Random(Gen);
   end Read_Temperature;

begin
   Ada.Numerics.Float_Random.Reset(Gen);  -- Inicializamos el generador solo una vez al iniciar el paquete
end Sensors;




