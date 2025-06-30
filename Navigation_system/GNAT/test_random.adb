with Ada.Text_IO; use Ada.Text_IO;
with GNAT.Random.Float;

procedure Test_Random is
   subtype My_Float is Float;
   package RandGen is new GNAT.Random.Float(My_Float, 0.0 .. 1.0);
   Gen : RandGen.Generator;
begin
   RandGen.Reset(Gen);
   Put_Line("Random value: " & My_Float'Image(RandGen.Random(Gen)));
end Test_Random;
