--                                                                    --
--  package                         Copyright (c)  Francois Fabien    --
--     AdaPcre                                                        --
--  Interface                                                         --
--                                                                    --
--                                Last revision :   15 Oct 2012       --
--                                                                    --
--  comments to  <fabien@lorgelog.com>
--
----------------------------------------------------------------------------
--  Permission to use, copy, modify, and distribute this software for any
--  purpose with or without fee is hereby granted, provided that the above
--  copyright notice and this permission notice appear in all copies.
--
--  THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
--  WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
--  MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
--  ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
--  WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
--  ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
--  OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
--
----------------------------------------------------------------------------
--  Ada interface to PCRE
--
--  partial binding : substring extraction is not implemented.
--
--  Gnat specific for memory allocation routines
-----------------------------------------------------------------------
with System;
with Interfaces;

package AdaPcre is

   pragma Elaborate_Body;

   type Version_Number is delta 0.01 range 0.0 .. 100.0;

   --  The value is assigned during elaboration of the body
   Version : Version_Number;

   --  PCRE version and release date (e.g. "7.0 18-Dec-2006").
   function Pcre_Version return String;

   --  Options. Some are compile-time only, some are run-time only,
   --  and some are both, so we keep them all distinct. However, almost
   --  all the bits in the options word are now used.
   --  In the long run, we may have to re-use some of the  compile-time
   --  only bits for runtime options, or vice versa.
   type Options is new Interfaces.Unsigned_32;

   PCRE_CASELESS          : constant Options := 16#0000_0001#;  -- Compile
   PCRE_MULTILINE         : constant Options := 16#0000_0002#;  -- Compile
   PCRE_DOTALL            : constant Options := 16#0000_0004#;  -- Compile
   PCRE_EXTENDED          : constant Options := 16#0000_0008#;  -- Compile
   PCRE_ANCHORED          : constant Options := 16#0000_0010#;  -- Com, Ex, DFA
   PCRE_DOLLAR_ENDONLY    : constant Options := 16#0000_0020#;  -- Compile
   PCRE_EXTRA             : constant Options := 16#0000_0040#;  -- Compile
   PCRE_NOTBOL            : constant Options := 16#0000_0080#;  -- Exec, DFA
   PCRE_NOTEOL            : constant Options := 16#0000_0100#;  -- Exec, DFA
   PCRE_UNGREEDY          : constant Options := 16#0000_0200#;  -- Compile
   PCRE_NOTEMPTY          : constant Options := 16#0000_0400#;  -- Exec, DFA
   PCRE_UTF8              : constant Options := 16#0000_0800#;  -- Compile
   PCRE_NO_AUTO_CAPTURE   : constant Options := 16#0000_1000#;  -- Compile
   PCRE_NO_UTF8_CHECK     : constant Options := 16#0000_2000#;  -- Co, exec,DFA
   PCRE_AUTO_CALLOUT      : constant Options := 16#0000_4000#;  -- Compile
   PCRE_PARTIAL_SOFT      : constant Options := 16#0000_8000#;  -- Exec, DFA
   PCRE_PARTIAL           : constant Options := 16#0000_8000#;  -- Backwards
   PCRE_DFA_SHORTEST      : constant Options := 16#0001_0000#;  -- DFA exec
   PCRE_DFA_RESTART       : constant Options := 16#0002_0000#;  -- DFA exec
   PCRE_FIRSTLINE         : constant Options := 16#0004_0000#;  -- Compile
   PCRE_DUPNAMES          : constant Options := 16#0008_0000#;  -- Compile
   PCRE_NEWLINE_CR        : constant Options := 16#0010_0000#;  -- C, exec,DFA
   PCRE_NEWLINE_LF        : constant Options := 16#0020_0000#;  -- C, exec,DFA
   PCRE_NEWLINE_CRLF      : constant Options := 16#0030_0000#;  -- C, exec,DFA
   PCRE_NEWLINE_ANY       : constant Options := 16#0040_0000#;  -- C, exec,DFA
   PCRE_NEWLINE_ANYCRLF   : constant Options := 16#0050_0000#;  -- C, exec,DFA
   PCRE_BSR_ANYCRLF       : constant Options := 16#0080_0000#;  -- C, exec,DFA
   PCRE_BSR_UNICODE       : constant Options := 16#0100_0000#;  -- C, exec,DFA
   PCRE_JAVASCRIPT_COMPAT : constant Options := 16#0200_0000#;  -- Compile
   PCRE_NO_START_OPTIMIZE : constant Options := 16#0400_0000#;  -- C, exec,DFA
   PCRE_NO_START_OPTIMISE : constant Options := 16#0400_0000#;  -- Synonym
   PCRE_PARTIAL_HARD      : constant Options := 16#0800_0000#;  -- Exec, DFA
   PCRE_NOTEMPTY_ATSTART  : constant Options := 16#1000_0000#;  -- Exec, DFA
   PCRE_UCP               : constant Options := 16#2000_0000#;  -- Compile

   --  Exec-time and get/set-time error codes

   PCRE_ERROR_NOMATCH        : constant Integer := -1;
   PCRE_ERROR_NULL           : constant Integer := -2;
   PCRE_ERROR_BADOPTION      : constant Integer := -3;
   PCRE_ERROR_BADMAGIC       : constant Integer := -4;
   PCRE_ERROR_UNKNOWN_OPCODE : constant Integer := -5;
   PCRE_ERROR_NOMEMORY       : constant Integer := -6;
   PCRE_ERROR_NOSUBSTRING    : constant Integer := -7;
   PCRE_ERROR_MATCHLIMIT     : constant Integer := -8;
   PCRE_ERROR_BADUTF8        : constant Integer := -10;
   PCRE_ERROR_BADUTF8_OFFSET : constant Integer := -11;
   PCRE_ERROR_PARTIAL        : constant Integer := -12;
   PCRE_ERROR_BADPARTIAL     : constant Integer := -13;
   PCRE_ERROR_INTERNAL       : constant Integer := -14;
   PCRE_ERROR_BADCOUNT       : constant Integer := -15;
   PCRE_ERROR_DFA_UITEM      : constant Integer := -16;
   PCRE_ERROR_DFA_UCOND      : constant Integer := -17;
   PCRE_ERROR_DFA_UMLIMIT    : constant Integer := -18;
   PCRE_ERROR_DFA_WSSIZE     : constant Integer := -19;
   PCRE_ERROR_DFA_RECURSE    : constant Integer := -20;
   PCRE_ERROR_RECURSIONLIMIT : constant Integer := -21;
   PCRE_ERROR_BADNEWLINE     : constant Integer := -23;
   PCRE_ERROR_BADOFFSET      : constant Integer := -24;
   PCRE_ERROR_SHORTUTF8      : constant Integer := -25;

   type Pcre_Type is private;
   type Extra_type is private;

   Null_Pcre  : constant Pcre_Type;
   Null_Extra : constant Extra_type;

   type Table_Type is private;
   Null_Table : constant Table_Type;

   function Pcre_Maketables return Table_Type;

   --  output strings for error message; normally size of 80 should be enough
   subtype Message is String (1 .. 80);

   procedure Compile
     (Matcher  : out Pcre_Type; Pattern : String; Error_Msg : out Message;
      Last_Msg : out Natural; Error_Offset : out Natural;
      Option   :     Options := 0; Table : Table_Type := Null_Table);

   procedure Free (T : Table_Type);

   procedure Free (M : Pcre_Type);

   --  if you are using  version 8.20 or later, you can modify the body and
   --  consider interfacing to pcre_free_study
   procedure Free (M : Extra_type);

   --  return info to speed up/ null on error or if no optimization is
   --  possible.
   procedure Study
     (Matcher_Extra : out Extra_type; Code : Pcre_Type;
      Error_Msg : out Message; Last_Msg : out Natural; Option : Options := 0);

   -----------------
   --  Match_Array --
   -----------------
   --  Result of matches : same output as PCRE
   --  size must be a multiple of 3 x (nbr of parentheses + 1)
   --  For top-level, range should be 0 .. 2
   --  For N parentheses, range should be 0 .. 3*(N+1) -1
   --  If the dimension of Match_Array is insufficient, Result of Match is 0.
   --
   type Match_Array is array (Natural range <>) of Natural;

   procedure Match
     (Result : out Integer; Match_Vec : out Match_Array; Matcher : Pcre_Type;
      Extra :     Extra_type := Null_Extra; Subject : String; Length : Natural;
      Startoffset :     Natural    := 0; Option : Options := 0);

   --  Request types for pcre_fullinfo()
   subtype Info is Integer range 0 .. 15;

   PCRE_INFO_OPTIONS        : constant Info := 0;
   PCRE_INFO_SIZE           : constant Info := 1;
   PCRE_INFO_CAPTURECOUNT   : constant Info := 2;
   PCRE_INFO_BACKREFMAX     : constant Info := 3;
   PCRE_INFO_FIRSTBYTE      : constant Info := 4;
   PCRE_INFO_FIRSTTABLE     : constant Info := 5;
   PCRE_INFO_LASTLITERAL    : constant Info := 6;
   PCRE_INFO_NAMEENTRYSIZE  : constant Info := 7;
   PCRE_INFO_NAMECOUNT      : constant Info := 8;
   PCRE_INFO_NAMETABLE      : constant Info := 9;
   PCRE_INFO_STUDYSIZE      : constant Info := 10;
   PCRE_INFO_DEFAULT_TABLES : constant Info := 11;
   PCRE_INFO_OKPARTIAL      : constant Info := 12;
   PCRE_INFO_JCHANGED       : constant Info := 13;
   PCRE_INFO_HASCRORLF      : constant Info := 14;
   PCRE_INFO_MINLENGTH      : constant Info := 15;

   function Fullinfo
     (Code        : Pcre_Type; Extra : Extra_type; What : Info;
      Output_Addr : System.Address) return Integer;

   subtype Configuration is Integer range 0 .. 8;

   PCRE_CONFIG_UTF8                   : constant Configuration := 0;
   PCRE_CONFIG_NEWLINE                : constant Configuration := 1;
   PCRE_CONFIG_LINK_SIZE              : constant Configuration := 2;
   PCRE_CONFIG_POSIX_MALLOC_THRESHOLD : constant Configuration := 3;
   PCRE_CONFIG_MATCH_LIMIT            : constant Configuration := 4;
   PCRE_CONFIG_STACKRECURSE           : constant Configuration := 5;
   PCRE_CONFIG_UNICODE_PROPERTIES     : constant Configuration := 6;
   PCRE_CONFIG_MATCH_LIMIT_RECURSION  : constant Configuration := 7;
   PCRE_CONFIG_BSR                    : constant Configuration := 8;

   function Config
     (What : Configuration; Output_Addr : System.Address) return Integer;

private

   type Pcre_Type is new System.Address;
   type Extra_type is new System.Address;

   Null_Pcre  : constant Pcre_Type  := Pcre_Type (System.Null_Address);
   Null_Extra : constant Extra_type := Extra_type (System.Null_Address);

   type Table_Type is new System.Address;
   Null_Table : constant Table_Type := Table_Type (System.Null_Address);

   pragma Import (C, Pcre_Maketables, "pcre_maketables");
   pragma Import (C, Fullinfo, "pcre_fullinfo");
   pragma Import (C, Config, "pcre_config");

end AdaPcre;
