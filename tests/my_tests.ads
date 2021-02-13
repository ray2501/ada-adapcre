with Ahven.Framework;
package My_Tests is
   type Test is new Ahven.Framework.Test_Case with null record;

   procedure Initialize (T : in out Test);

   procedure PCRE_MATCH;
   procedure PCRE_MATCH2;
end My_Tests;
