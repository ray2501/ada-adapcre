--
-- Basic test : compiling a pattern and finding the first match if any
-- Syntax     : test_2 pattern subject
--
with Ada.Text_IO;      use Ada.Text_IO;
with AdaPcre;          use AdaPcre;
with Ada.Command_Line; use Ada.Command_Line;

procedure Test_2 is

   Regexp           : Pcre_Type;
   Extra            : Extra_type;
   Msg              : Message;
   Last_Msg, ErrPos : Natural := 0;
   Result           : Match_Array (0 .. 30);
   Retcode          : Integer;

begin
   if (Argument_Count = 1 and then Argument (1) = "-h") then
      Put_Line ("A demo for the PCRE library.");
      Put_Line ("Syntax : test_2 pattern subject");
      return;
   elsif (Argument_Count /= 2) then
      Put_Line ("Wrong Argument count :" & Integer'Image (Argument_Count));
      Put_Line ("Syntax : test_2 pattern subject");
      return;
   else
      null;
   end if;

   declare
      Pattern : constant String := Argument (1);
      Subject : constant String := Argument (2);
   begin
      -- Now we are going to compile the regular expression pattern, and handle
      -- errors that are detected.
      Compile
        (Matcher      => Regexp,
         Pattern      => Pattern,
         Error_Msg    => Msg,
         Last_Msg     => Last_Msg,
         Error_Offset => ErrPos);

      -- Compilation failed: print the error message and exit
      if Regexp = Null_Pcre then
         Put_Line
           ("PCRE compilation failed at position " &
            Pattern (Pattern'First + ErrPos .. Pattern'Last));
         Put_Line (Msg (1 .. Last_Msg));
         return;
      end if;

      Study (Extra, Regexp, Msg, Last_Msg);
      -- Study failed: print the error message and exit
      if Extra = Null_Extra and then Last_Msg > 0 then
         Put ("PCRE study failed :");
         Put_Line (Msg (1 .. Last_Msg));
         Free (Regexp);
         return;
      end if;

      Match (Retcode, Result, Regexp, Extra, Subject, Subject'Length);

      if Retcode < 0 then
         if Retcode = PCRE_ERROR_NOMATCH then
            Put_Line ("No match");
         else
            Put_Line ("Matching error :" & Integer'Image (Retcode));
         end if;
      else
         Put_Line ("Return code for pcre_match :" & Natural'Image (Retcode));
         for I in 0 .. Retcode - 1 loop
            Put
              ("Match succeeded at offsets " &
               Integer'Image (Result (2 * I)) &
               " and " &
               Integer'Image (Result (2 * I + 1)));
            Put_Line
              ("<" &
               Subject (Result (2 * I) + 1 .. Result (2 * I + 1)) &
               ">");
         end loop;
         Put_Line ("Remember : position := subject'first + offset;");

      end if;
   end;

   Free (Extra);
   Free (Regexp);
end Test_2;
