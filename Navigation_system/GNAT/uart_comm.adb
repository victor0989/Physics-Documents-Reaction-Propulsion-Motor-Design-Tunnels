with Ada.Text_IO; use Ada.Text_IO;

package body UART_Comm is
   procedure UART_Init is
   begin
      Put_Line("UART initialized.");
   end UART_Init;

   procedure UART_Send(Message : String) is
   begin
      Put_Line("UART >> " & Message);
   end UART_Send;
end UART_Comm;
