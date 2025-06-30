with Ada.Text_IO; use Ada.Text_IO;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;

package MainNavigation is
   subtype Float_Measure is Float;

   type Sensor_Data is record
      Distance_LIDAR : Float_Measure;
      Temp_Radiation : Float_Measure;
      Meteor_Prox    : Float_Measure;
   end record;

   procedure Init_Sensors;
   procedure Check_Environment(Data : in Sensor_Data);
   procedure Navigate;
end MainNavigation;
