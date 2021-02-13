--
-- Basic checks of installation of the PCRE Ada binding.
--
-- giving some details of PCRE build options.
-- test of compile/study/match
--
with Text_IO;                   use Text_IO;
with AdaPcre;                   use AdaPcre;
with Ada.Environment_Variables; use Ada.Environment_Variables;

procedure Pcre_Config is

   package V_IO is new Fixed_IO (AdaPcre.Version_Number);
   Retcode         : Integer;
   Conf, Info_Size : Integer;

   Word_Pattern     : constant String := "([a-zA-Z]+)";
   Subject          : constant String := ";-)I love PATTERN matching!";
   Regexp           : Pcre_Type;
   Extra            : Extra_type;
   Msg              : Message;
   Last_Msg, ErrPos : Natural         := 0;
   Result           : Match_Array (0 .. 5);

begin

   -- Checking version retrieval
   Put_Line ("Configuration of PCRE library on your system");
   New_Line;
   Put (Pcre_Version);
   Put ("  => Your version of PCRE is ");
   V_IO.Put (AdaPcre.Version, Fore => 1, Aft => 2);
   New_Line;
   if Version < 8.0 then
      Put_Line ("Old version. Please consider upgrading to a newer version.");
   end if;

   -- checking config options used for library build
   -- UTF-8 support
   Retcode := Config (PCRE_CONFIG_UTF8, Conf'Address);
   if Conf = 1 then
      Put_Line ("PCRE library was build with UTF-8 support");
   elsif Conf = 0 then
      Put_Line ("no UTF-8 support in the PCRE library");
   else
      Put_Line ("Wrong output in pcre_config for CONFIG_UTF8");
   end if;

   -- Configuration of Newline
   Retcode := Config (PCRE_CONFIG_NEWLINE, Conf'Address);
   case Conf is
      when 10 =>
         Put_Line ("Character sequence for newline is LF");
      when 13 =>
         Put_Line ("Character sequence for newline is CR");
      when 3338 =>
         Put_Line ("Character sequence for newline is CRLF");
      when -1 =>
         Put_Line ("Character sequence for newline is ANY");
      when -2 =>
         Put_Line ("Character sequence for newline is ANYCRLF");
      when others =>
         Put_Line
           ("Wrong output in pcre_config for CONFIG_NEWLINE :" &
            Integer'Image (Conf));
   end case;

   -- Locale support
   Put ("Locale support : ");
   if Exists ("LC_CTYPE") then
      Put_Line ("LC_CTYPE is " & Value ("LC_CTYPE"));
   else
      Put_Line ("no LC_CTYPE defined");
   end if;
   New_Line;

   -- Testing a demo case :
   Put_Line ("Testing compile, study, match.");

   Compile (Regexp, Word_Pattern, Msg, Last_Msg, ErrPos);
   if Regexp = Null_Pcre then
      Put_Line
        ("PCRE compilation failed at offset " & Natural'Image (ErrPos));
      Put_Line (Msg (1 .. Last_Msg));
      return;
   end if;

   Study (Extra, Regexp, Msg, Last_Msg);
   if Extra = Null_Extra and then Last_Msg > 0 then
      Put_Line ("PCRE study failed.");
      Put_Line (Msg (1 .. Last_Msg));
      Free (Regexp);
      return;
   end if;
   Put_Line ("Compilation of pattern OK.");

   -- Getting the size of pattern matcher
   Retcode :=
      Fullinfo
        (Regexp,
         Extra,
         What        => PCRE_INFO_SIZE,
         Output_Addr => Info_Size'Address);
   Put_Line ("INFO_SIZE of matcher is" & Integer'Image (Info_Size));

   Match (Retcode, Result, Regexp, Extra, Subject, Subject'Length);

   if Retcode < 0 then
      if Retcode = PCRE_ERROR_NOMATCH then
         Put_Line ("No match");
      else
         Put_Line ("Matching error :" & Integer'Image (Retcode));
      end if;
      Put_Line ("Something is wrong in your PCRE installation");
      Free (Regexp);
      return;
   end if;

   Put_Line ("Match succeeded at offset " & Integer'Image (Result (0)));
   Put_Line ("Return code for pcre_match :" & Natural'Image (Retcode));
   Free (Extra);
   Free (Regexp);
   New_Line;
   Put_Line ("Congratulations : your PCRE installation is working !");

end Pcre_Config;
