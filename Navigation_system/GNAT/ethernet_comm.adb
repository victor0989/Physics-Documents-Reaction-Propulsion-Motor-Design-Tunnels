with Ada.Text_IO; use Ada.Text_IO;

package body Ethernet_Comm is
   procedure Ethernet_Init is
   begin
      Put_Line("Ethernet initialized.");
   end Ethernet_Init;

   procedure Ethernet_Send(Message : String) is
   begin
      Put_Line("ETH >> " & Message);
   end Ethernet_Send;
end Ethernet_Comm;
