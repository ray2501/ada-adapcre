-- runner.adb
with Ahven.Text_Runner;
with Ahven.Framework;
with My_Tests;

procedure Runner is
   S : constant Ahven.Framework.Test_Suite_Access :=
     Ahven.Framework.Create_Suite ("All my tests");
begin
   Ahven.Framework.Add_Test (S.all, new My_Tests.Test);
   Ahven.Text_Runner.Run (S);
   Ahven.Framework.Release_Suite (S);
   -- Release_Suite will release all its children also.
end Runner;
