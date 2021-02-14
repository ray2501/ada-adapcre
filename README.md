ada-adapcre
=====

[PCRE](https://www.pcre.org/) is a popular C-library that 
implements regular expression pattern matching using the 
same syntax and semantics as Perl 5.
PCRE means Perl Compatible Regular Expressions.

This package is Ada bindings for PCRE library.

Original code base is from 
[interfacing_with_c_-_pcre_library](http://wiki.ada-dk.org/interfacing_with_c_-_pcre_library).
I just rename to adapcre. License is still the same, MIT license.

I put original tests and examples to examples folder.

For learn [Ahven](http://ahven.stronglytyped.org/) Unit Testing 
Library, I add test cases in tests folder.


UNIX BUILD
=====

I only test on openSUSE LEAP 15.2.

Users need install PCRE library development files.

    sudo zypper in pcre-devel

Users need use `make` to build:

    make

And install:

    make install

If users want to build tests/examples:

    make build_examples

If users want to check test with Ahven:

    make test

Default PREFIX is `/usr/local`, and LIBDIR is `lib`.
If users want to change, below is the example:

    make PREFIX=/usr LIBDIR=lib64

and install:

    make PREFIX=/usr LIBDIR=lib64 install


Example
=====

Below is a simple example:

    with Ada.Text_IO;
    with Ada.Integer_Text_IO;
    with AdaPcre;
    use Ada.Text_IO;
    use Ada.Integer_Text_IO;
    use AdaPcre;

    procedure Number is

        procedure Search_For_Pattern
        (Compiled_Expression : AdaPcre.Pcre_Type; Search_In : String;
            Offset : Natural; First, Last : out Positive; Found : out Boolean)
        is
            Result  : Match_Array (0 .. 2);
            Retcode : Integer;
        begin
            Match
            (Retcode, Result, Compiled_Expression, Null_Extra, Search_In,
                Search_In'Length, Offset);

            if Retcode < 0 then
                Found := False;
            else
                Found := True;
                First := Search_In'First + Result (0);
                Last  := Search_In'First + Result (1) - 1;
            end if;
        end Search_For_Pattern;

        Word_Pattern : constant String :=
        "1.*1|2.*2|3.*3|4.*4|5.*5|6.*6|7.*7|8.*8|9.*9|0.*0";
        Regexp           : Pcre_Type;
        Msg              : Message;
        Last_Msg, ErrPos : Natural := 0;
        First, Last      : Positive;
        Found            : Boolean;

        N : Integer;
        M : Integer;
    begin
        Put ("Enter an integer value: ");
        Get (N);
        Compile (Regexp, Word_Pattern, Msg, Last_Msg, ErrPos);

        if N > 0 and N < 10 then
            M := 10**N - 1;
            for Count in 1 .. M loop
                Search_For_Pattern (Regexp, Count'Image, 0, First, Last, Found);
                if not Found then
                    Put_Line (Integer'Image (Count));
                end if;
            end loop;
        else
            Put_Line ("Number requires 10 > N > 0.");
        end if;

        Free (Regexp);
    end Number;

Then write a number.gpr file:

    with "adapcre";

    project Number is
        for Languages use ("Ada");
        for Exec_Dir use ".";
        for Source_Files use ("number.adb");
        for Main use ("number.adb");
        package Builder is
            for Executable ("number") use "number";
        end Builder;
        package Compiler is
        for Default_Switches ("Ada")
            use ("-O2");
        end Compiler;
    end Number;

Then build this project:

    gnatmake -Pnumber

