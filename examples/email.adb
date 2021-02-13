--
-- Checks if a string is a valid email address.
--
-- Strings for test are located in file <email.txt>
--
with Ada.Text_IO; use Ada.Text_IO;
with AdaPcre;     use AdaPcre;

procedure Email is

   Email_File : File_Type;

   Email_Pattern : constant String :=
      "^[A-z0-9._%-]+@[A-z0-9.-]+\.[A-z]{2,4}$";

   Email_Str        : String (1 .. 100);
   Last             : Natural;
   Regexp           : Pcre_Type;
   Extra            : Extra_type;
   Retcode          : Integer;
   Msg              : Message;
   Last_Msg, ErrPos : Natural := 0;
   Vector           : Match_Array (0 .. 2);

begin
   Compile (Regexp, Email_Pattern, Msg, Last_Msg, ErrPos);
   if Regexp = Null_Pcre then
      Put_Line
        ("PCRE compilation failed at offset " & Natural'Image (ErrPos));
      Put_Line (Msg (1 .. Last_Msg));
      return;
   end if;
   Study (Extra, Regexp, Msg, Last_Msg);

   Open (Email_File, In_File, "email.txt");
   Set_Input (Email_File);

   while not End_Of_File (Email_File) loop
      Get_Line (Email_Str, Last);
      Put (Email_Str (1 .. Last));
      Match (Retcode, Vector, Regexp, Extra, Email_Str, Last);

      if Retcode = -1 then
         Put_Line (" => Invalid Email. No match !");
      elsif Retcode < -1 then
         Put_Line
           ("Failure in PCRE ! error number => " & Integer'Image (Retcode));
      else
         Put_Line (" => Valid Email");
      end if;
   end loop;

   Close (Email_File);
   Free (Extra);
   Free (Regexp);
end Email;
