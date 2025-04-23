with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada_Vision_Unit.Tests; use Ada_Vision_Unit.Tests;

procedure Ada_Vision_Unit_Test_Runner is
begin
   Put_Line("AdaVisionUnit Test Suite");
   Put_Line("=======================");
   Put_Line("");
   
   -- Run all the tests
   Run_All_Tests;
   
   -- For command-line usage in automated testing
   if Argument_Count > 0 and then Argument(1) = "--return-result" then
      -- Return exit status for shell scripts
      -- This would need to be modified to return actual success/fail status
      Set_Exit_Status(0);
   end if;
end Ada_Vision_Unit_Test_Runner;