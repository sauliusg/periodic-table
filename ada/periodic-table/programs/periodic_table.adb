-- ----------------------------------------------------------------------------
-- $Author$
-- $Date$
-- $Revision$
-- $URL$
-- ----------------------------------------------------------------------------

-- Option processing example taken from:
-- https://www.adacore.com/gems/gem-138-gnatcoll.command-line
-- S.G. 2021-12-26

with Text_Io;                use Text_Io;
with Ada.Integer_Text_Io;    use Ada.Integer_Text_Io;
with Ada.Long_Float_Text_IO; use Ada.Long_Float_Text_IO;
with Ada.Command_Line;       use Ada.Command_Line;
with Ada.Strings.Unbounded;  use Ada.Strings.Unbounded;
with Ada.Strings.Fixed;      use Ada.Strings.Fixed;
with GNAT.Command_Line;      use GNAT.Command_Line;

with Float_Format_Option;    use Float_Format_Option;

procedure Periodic_Table is
   
   Program_Id : constant String := "$Id$";
   Program_URL : constant String := "$URL$";
   
   HELP_PRINTED : exception;
   VERSION_PRINTED : exception;
   
   Max_Atoms : Integer := 118; -- The number of chemical elements know to-date.
   
   procedure Print_Help is
      procedure P( S : String ) renames Put_Line;
   begin
      P("Print out atoms in the order of the Mendelejev Periodic table.");
      P("Use the XYZ format [1] for output.");
      New_Line;
      P("[1] https://en.wikipedia.org/wiki/XYZ_file_format");
      New_Line;
      P("USAGE:");
      P("    " & Command_Name & " --options");
      P("    " & Command_Name & " --options > out.xyz");
      New_Line;
      P("OPTIONS:");
      P("    -f, --float-format 2,14,3       Specify format for floating point output");
      P("        For Ada, floating point format consists of three numbers:");
      P("        the integer part length, the fraction part length and the exponent length.");
      P("        Specifying exponent part as 0 outputs no exponent at all (as with C '%f' format).");
      New_Line;
      P("    -H, --human-readable            Use format 8,6,0 for better human readability");
      P("    -M, --machine-readable          Use format 2,14,3 to maintain precision");
      New_Line;
      P("    --help                          Print a short help message and exit;");
      P("    --version                       Print program project version and exit;");
      raise HELP_PRINTED;
   end;
   
   procedure Process_Options is
      Help_Option : String := "-help -hel -he -h h ";
      Float_Format_Option : String := "f: " &
        "-float-format= -float-forma= -float-form= -float-for= " &
        "-float-fo= -float-f= -float= -floa= -flo= -fl= -f= ";
      Human_Readable_Option : String := "H " &
        "-human-readable -human-readabl -human-readab -human-reada " &
        "-human-read -human-rea -human-re -human-r -human -huma " &
        "-hum -hu -h ";
      Machine_Readable_Option : String := "M " &
        "-machine-readable -machine-readabl -machine-readab -machine-reada " &
        "-machine-read -machine-rea -machine-re -machine-r -machine -machin " &
        "-machi -mach -mac -ma -m ";
      Version_Option : String := "-version -versio -versi -vers -ver -ve -v";
   begin
      loop
         case Getopt (Help_Option & Float_Format_Option &
                        Human_Readable_Option & Machine_Readable_Option &
                        Version_Option) is
            when 'f' =>
               Parse_Float_Format (Parameter, Integer_Size, 
                                   Fraction_Size, Exponent_Size);
            when 'H' =>
               Integer_Size := 8;
               Fraction_Size := 6;
               Exponent_Size := 0;
            when 'M' =>
               Integer_Size := 2;
               Fraction_Size := 14;
               Exponent_Size := 3;
            when '-' =>
               if Index("-help", Full_Switch) = 1 then
                  Print_Help;
               elsif Index("-float-format", Full_Switch) = 1 then
                  Parse_Float_Format (Parameter, Integer_Size, 
                                      Fraction_Size, Exponent_Size);
               elsif Index("-human-readable", Full_Switch) = 1 then
                  Integer_Size := 8;
                  Fraction_Size := 6;
                  Exponent_Size := 0;
               elsif Index("-machine-readable", Full_Switch) = 1 then
                  Integer_Size := 2;
                  Fraction_Size := 14;
                  Exponent_Size := 3;
               elsif Index("-version", Full_Switch) = 1 then
                  Put_Line (Command_Name & " " & 
                              Program_Id (2..Program_Id'Last-1));
                  Put_Line (Command_Name & " " & 
                              Program_URL (2..Program_URL'Last-1));
                  raise VERSION_PRINTED;
               end if;
            when others =>
               exit;
         end case;
      end loop;   
   end Process_Options;
   
   procedure Put_Float ( X : Long_Float ) is
   begin
      Put (X, Integer_Size, Fraction_Size, Exponent_Size);
   end;
   
   X, Y, Z : Long_Float;
   
   Delta_X : Long_Float := 4.0; -- In Angstroems
   Delta_Y : Long_Float := 4.0; -- In Angstroems
   
begin
   
   Process_Options;
   
   Put (Max_Atoms, Width => 1);
   New_Line;
   Put ("Mendeleev's Periodic Table of Elements / ");
   Put ("Периодическая таблица Менделеева");
   New_Line;
   
   -- AN -- Atomic Number
   Z := 0.0;
   Y := 0.0;
   for AN in 1..Max_Atoms loop
      
      Put (AN, Width => 1);
      if AN'Image'Last < 3 then
         Put ((3 - AN'Image'Last) * " "); -- Pad with spaces on the right.
      end if;
      X := Long_Float (AN) * Delta_X;
      
      Put (" ");
      Put_Float (X);
      
      Put (" ");
      Put_Float (Y);
      
      Put (" ");
      Put_Float (Z);
      New_Line;
      
   end loop;
   
exception
   when HELP_PRINTED => null;
   when VERSION_PRINTED => null;
   
end Periodic_Table;
