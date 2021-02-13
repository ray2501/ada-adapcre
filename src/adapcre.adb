--                                                                    --
--  package                         Copyright (c)  Francois Fabien    --
--     AdaPcre                                                        --
--  Body                                                              --
--                                                                    --
--                                Last revision :   15 Oct 2012       --
--                                                                    --
------------------------------------------------------------------------
--  interface to PCRE
--
--  partial binding : substring extraction is not implemented.
--
------------------------------------------------------------------------
with Interfaces.C.Strings; use Interfaces.C.Strings;
with Interfaces.C;         use Interfaces.C;
with Ada.Unchecked_Conversion;
with Ada.Strings.Fixed;    use Ada.Strings;
with System;               use System;

package body AdaPcre is

   pragma Linker_Options ("-lpcre");

   use Interfaces;

   function To_chars_ptr is new Ada.Unchecked_Conversion (Address, chars_ptr);

   function Pcre_Compile
     (pattern   : chars_ptr; option : Options; errptr : access chars_ptr;
      erroffset : access Integer; tableptr : Table_Type) return Pcre_Type;
   pragma Import (C, Pcre_Compile, "pcre_compile");

   procedure Compile
     (Matcher  : out Pcre_Type; Pattern : String; Error_Msg : out Message;
      Last_Msg : out Natural; Error_Offset : out Natural;
      Option   :     Options := 0; Table : Table_Type := Null_Table)
   is
      Unknown   : constant String := "Unknown error in pcre_compile";
      Error_Ptr : aliased chars_ptr;
      ErrOffset : aliased Integer := 0;
      Pat       : chars_ptr       := New_String (Pattern);
   begin
      Last_Msg     := 0;
      Error_Offset := 0;
      Matcher      :=
        Pcre_Compile (Pat, Option, Error_Ptr'Access, ErrOffset'Access, Table);
      Free (Pat);

      if Matcher = Null_Pcre then
         if Error_Ptr /= Null_Ptr then
            --  copy C error message to an Ada string.
            Last_Msg                  := Natural (Strlen (Error_Ptr));
            Error_Msg (1 .. Last_Msg) := Value (Error_Ptr);
         else
            --  oops ! no error message is available.
            Last_Msg                  := Unknown'Last;
            Error_Msg (1 .. Last_Msg) := Unknown;
         end if;
         if ErrOffset > 0 then
            Error_Offset := ErrOffset;
         else
            Error_Offset := 0;
         end if;
      end if;
   end Compile;

   ---------------------------------------------------------
   --  Imports of GNAT alloc routines that are thread-safe --
   --
   --  so there are no dependency on external library
   --  like ms*.dll on Windows
   ---------------------------------------------------------
   function Gnat_Malloc (Size : C.size_t) return System.Address;
   pragma Import (C, Gnat_Malloc, "__gnat_malloc");

   procedure Gnat_Free (Add : System.Address);
   pragma Import (C, Gnat_Free, "__gnat_free");

   ---------------------------------------------------------
   --  Exporting the Gnat routines to pcre
   ---------------------------------------------------------
   type Access_Free is access procedure (Item : System.Address);
   pragma Convention (C, Access_Free);

   Pcre_Free : Access_Free := Gnat_Free'Access;
   pragma Export (C, Pcre_Free, "pcre_free");

   ---------------------------------------------------------
   --  Free routines
   ---------------------------------------------------------
   procedure Free (T : Table_Type) is
   begin
      Pcre_Free (System.Address (T));
   end Free;

   procedure Free (M : Pcre_Type) is
   begin
      Pcre_Free (System.Address (M));
   end Free;

   ------------------------------------------------------------
   --  If your version is > 8.20 consider using pcre_free_study
   ------------------------------------------------------------
   procedure Free (M : Extra_type) is
   begin
      Pcre_Free (System.Address (M));
   end Free;

   function Pcre_Exec
     (code    : Pcre_Type; extra : Extra_type; subject : chars_ptr;
      length  : Integer; startoffset : Integer; option : Options;
      ovector : System.Address; ovecsize : Integer) return Integer;
   pragma Import (C, Pcre_Exec, "pcre_exec");

   procedure Match
     (Result : out Integer; Match_Vec : out Match_Array; Matcher : Pcre_Type;
      Extra :     Extra_type := Null_Extra; Subject : String; Length : Natural;
      Startoffset :     Natural    := 0; Option : Options := 0)
   is
      Match_Size : constant Natural                     := Match_Vec'Length;
      m          : array (0 .. Match_Size - 1) of C.int := (others => 0);
      pragma Convention (C, m);
      pragma Volatile (m); -- used by the C library

      --  Passing reference of the subject string (without nul terminator)
      --  pcre handle string termination using Length parameter.
      Start : constant chars_ptr :=
        To_chars_ptr (Subject (Subject'First)'Address);
   begin

      Result :=
        Pcre_Exec
          (Matcher, Extra, Start, Length, Startoffset, Option, m (0)'Address,
           Match_Size);
      for I in 0 .. Match_Size - 1 loop
         if m (I) > 0 then
            Match_Vec (I) := Integer (m (I));
         else
            Match_Vec (I) := 0;
         end if;
      end loop;
   end Match;

   type Access_Malloc is access function
     (Size : C.size_t) return System.Address;
   pragma Convention (C, Access_Malloc);

   Pcre_Malloc : Access_Malloc := Gnat_Malloc'Access;
   pragma Export (C, Pcre_Malloc, "pcre_malloc");

   function Pcre_Version return String is
      function Pcre_Version return chars_ptr;
      pragma Import (C, Pcre_Version, "pcre_version");
   begin
      return Value (Pcre_Version);
   end Pcre_Version;

   function Pcre_Study
     (code : Pcre_Type; option : Options; errptr : access chars_ptr)
      return Extra_type;
   pragma Import (C, Pcre_Study, "pcre_study");

   procedure Study
     (Matcher_Extra : out Extra_type; Code : Pcre_Type;
      Error_Msg : out Message; Last_Msg : out Natural; Option : Options := 0)
   is
      Error_Ptr : aliased chars_ptr;
   begin
      Last_Msg := 0;

      Matcher_Extra := Pcre_Study (Code, Option, Error_Ptr'Access);

      if Matcher_Extra = Null_Extra and then Error_Ptr /= Null_Ptr then
         --  an error occured
         Last_Msg                  := Natural (Strlen (Error_Ptr));
         Error_Msg (1 .. Last_Msg) := Value (Error_Ptr);
      end if;
   end Study;

begin
   --  Assignement of Global data Version
   declare
      Version_Str : constant String := Pcre_Version;
      P           : Natural;
   begin
      P       := Fixed.Index (Version_Str (1 .. 5), ".");
      Version := Version_Number'Value (Version_Str (1 .. P + 2));
   end;

end AdaPcre;
