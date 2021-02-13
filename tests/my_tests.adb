with Ahven;     use Ahven;
with AdaPcre;   use AdaPcre;

package body My_Tests is
    procedure Initialize (T : in out Test) is
    begin
        Set_Name (T, "My tests");

        Framework.Add_Test_Routine
        (T, PCRE_MATCH'Access, "PCRE_MATCH");

        Framework.Add_Test_Routine
        (T, PCRE_MATCH2'Access, "PCRE_MATCH2");
    end Initialize;

    procedure Search_For_Pattern
        (Compiled_Expression : AdaPcre.Pcre_Type;
        Search_In           : String;
        Offset              : Natural;
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
    
    procedure PCRE_MATCH is

        Word_Pattern : constant String := "([A-z]+)";
     
        Subject          : constant String := ";-)I love PATTERN matching!";
        Current_Offset   : Natural         := 0;
        First, Last      : Positive;
        Found            : Boolean;
        Regexp           : Pcre_Type;
        Msg              : Message;
        Last_Msg, ErrPos : Natural         := 0;
        Count            : Integer := 0;
    begin
        Compile (Regexp, Word_Pattern, Msg, Last_Msg, ErrPos);
        loop
           Search_For_Pattern
             (Regexp,
              Subject,
              Current_Offset,
              First,
              Last,
              Found);
           exit when not Found;
           Count := Count + 1;
           Current_Offset := Last;
        end loop;
 
        Free (Regexp);

        Assert (Condition => Count = 4,
                Message   => "Match 4 patterns");
    end PCRE_MATCH;

   procedure PCRE_MATCH2 is

        Word_Pattern : constant String := "e";
     
        Subject          : constant String := "Eeeee: Weekly Challenge";
        Current_Offset   : Natural         := 0;
        First, Last      : Positive;
        Found            : Boolean;
        Extra            : Extra_type;
        Regexp           : Pcre_Type;
        Msg              : Message;
        Last_Msg, ErrPos : Natural         := 0;
        Count            : Integer := 0;
    begin
        Compile (Regexp, Word_Pattern, Msg, Last_Msg, ErrPos);
        Study (Extra, Regexp, Msg, Last_Msg);
        loop
           Search_For_Pattern
             (Regexp,
              Subject,
              Current_Offset,
              First,
              Last,
              Found);
           exit when not Found;
           Count := Count + 1;
           Current_Offset := Last;
        end loop;
 
        Free (Regexp);

        Assert (Condition => Count = 8,
                Message   => "Match 8 patterns");
    end PCRE_MATCH2;    
    
end My_Tests;
