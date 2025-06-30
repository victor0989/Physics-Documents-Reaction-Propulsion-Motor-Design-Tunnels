with GNAT.Random;        -- para generadores genéricos
with GNAT.Random.Float;  -- para flotantes

procedure Random_Float_Example is
   -- Instancio el generador para rango [0.0 .. 1.0]
   subtype My_Float is Float;
   package RandGen is new GNAT.Random.Float(My_Float, 0.0 .. 1.0);
   Gen : RandGen.Generator;

begin
   Rand.Reset(Gen);      -- inicializar el generador (semilla fija o por defecto)

   -- Generar 10 números aleatorios
   for I in 1 .. 10 loop
      R := Rand.Random(Gen);
      Ada.Text_IO.Put_Line(Float'Image(R));
   end loop;
end Random_Float_Example;
