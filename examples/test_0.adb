--
-- Basic test : splitting a sentence into words
--
with Ada.Text_IO; use Ada.Text_IO;
with AdaPcre;     use AdaPcre;

procedure Test_0 is

   procedure Search_For_Pattern
     (Compiled_Expression : in AdaPcre.Pcre_Type;
      Search_In           : in String;
      Offset              : in Natural;
      First, Last         : out Positive;
      Found               : out Boolean)
   is
      Result  : Match_Array (0 .. 2);
      Retcode : Integer;
   begin
      Match
        (Retcode,
         Result,
         Compiled_Expression,
         Null_Extra,
         Search_In,
         Search_In'Length,
         Offset);

      if Retcode < 0 then
         Found := False;
      else
         Found := True;
         First := Search_In'First + Result (0);
         Last  := Search_In'First + Result (1) - 1;
      end if;
   end Search_For_Pattern;

   Word_Pattern : constant String := "([A-z]+)";

   Subject          : constant String := ";-)I love PATTERN matching!";
   Current_Offset   : Natural         := 0;
   First, Last      : Positive;
   Found            : Boolean;
   Regexp           : Pcre_Type;
   Msg              : Message;
   Last_Msg, ErrPos : Natural         := 0;

begin
   Compile (Regexp, Word_Pattern, Msg, Last_Msg, ErrPos);

   -- Find all the words in Subject string
   loop
      Search_For_Pattern
        (Regexp,
         Subject,
         Current_Offset,
         First,
         Last,
         Found);
      exit when not Found;
      Put_Line ("<" & Subject (First .. Last) & ">");
      Current_Offset := Last;
   end loop;

   Free (Regexp);
end Test_0;
