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
   
   -- The number of chemical elements know to-date:
   Max_Atoms : constant Integer := 118;
   
   -- The number of chemical elements to display by default, selected
   --  from what JMol can currently display:
   Display_Atoms : Integer := 109;
   
   Display_Table_Linear : Boolean := False;   
   Rotate : Boolean := False;
   
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
      P("    " & Command_Name & " --options 109");
      P("    " & Command_Name & " --options > out.xyz");
      New_Line;
      P("The optional positional argument (109 in the example) specifies the");
      P("atomic number of the last element to be displayed.");
      New_Line;
      P("OPTIONS:");
      P("    -f, --float-format 2,14,3       Specify format for floating point output");
      P("        For Ada, floating point format consists of three numbers:");
      P("        the integer part length, the fraction part length and the exponent length.");
      P("        Specifying exponent part as 0 outputs no exponent at all (as with C '%f' format).");
      New_Line;
      P("    --linear                        Display all atoms in one line.");
      P("    --folded                        Display atoms in a folded, tabular for (default).");
      P("    --rotate                        Rotate the image 180 around X for Rasmol.");
      P("    --no-rotate                     Do not rotate the image (for JMol, default).");
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
      Version_Option : String := "-version -versio -versi -vers -ver -ve -v ";
      Linear_Option : String := "-linear -linea -line -lin -li -l ";
      Folded_Option : String := "-folded -folde -fold -fol -fo -f ";
      Rotate_Option : String := "-rotate -rotat -rota -rot -ro -r ";
      No_Rotate_Option : String := "-no-rotate -no-rotat -no-rota " &
        "-no-rot -no-ro -no-r -no -n";
   begin
      loop
         case Getopt (Help_Option & Float_Format_Option &
                        Human_Readable_Option & Machine_Readable_Option &
                        Version_Option & Linear_Option & Folded_Option &
                        Rotate_Option & No_Rotate_Option) is
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
               elsif Index("-linear", Full_Switch) = 1 then
                  Display_Table_Linear := True;
               elsif Index("-folded", Full_Switch) = 1 then
                  Display_Table_Linear := False;
               elsif Index("-rotate", Full_Switch) = 1 then
                  Rotate := True;
               elsif Index("-no-rotate", Full_Switch) = 1 then
                  Rotate := False;
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
   
   Delta_X : Long_Float := 8.0; -- In Angstroems
   Delta_Y : Long_Float := 8.0; -- In Angstroems
   Delta_Z : Long_Float := 8.0; -- In Angstroems
   
   procedure Put_Atom_Position ( AN : Integer; X, Y, Z : Long_Float ) is
   begin
      Put (AN, Width => 1);
      if AN'Image'Last < 4 then
         Put ((4 - AN'Image'Last) * " "); -- Pad with spaces on the right.
      end if;
      
      Put (" ");
      Put_Float (X);
      
      Put (" ");
      Put_Float (Y);
      
      Put (" ");
      Put_Float (Z);
   end;
   
   procedure Select_Atom_Position_Linear ( AN : in Integer;
                                           X, Y, Z : out Long_Float ) is
   begin
      X := Long_Float (AN) * Delta_X;
      Y := 0.0;
      Z := 0.0;
   end;
   
   procedure Select_Atom_Position_Folded ( AN : in Integer;
                                           X, Y, Z : out Long_Float ) is
      Period_2_3_Row_Length : constant Integer := 8;
      Period_4_5_Row_Length : constant Integer := 18;
      
      Row_Length : Integer;
      
      Lantanoid_Shift : constant Integer := 1;
      Transition_Shift : constant Integer := 10;
      Main_Shift : constant Integer := 5;
      Lantanoid_Period : constant Integer := 14;
      
      Total_Shift : Integer;
      Row_Start_Offset : Integer;
   begin
      
      Z := 0.0;
      
      if AN = 1 then
         X := Delta_X; Y := -Delta_Y;
      elsif AN = 2 then
         Total_Shift := Lantanoid_Shift + Transition_Shift + Main_Shift + 3;
         X := Long_Float (Total_Shift) * Delta_X; Y := -Delta_Y;
      elsif AN in 3..18 then
         Total_Shift := Lantanoid_Shift + Transition_Shift + 1;
         Row_Length := Period_2_3_Row_Length;
         Row_Start_Offset := (AN - 3) mod Row_Length;
         if Row_Start_Offset in 0..1 then
            X := Long_Float (Row_Start_Offset + 1) * Delta_X;
         else
            X := Long_Float (Row_Start_Offset + Total_Shift) * Delta_X;
         end if;
         Y := Long_Float ((AN - 3) / Row_Length) * Delta_Y;
      elsif AN in 19..54 then
         Total_Shift := Lantanoid_Shift + 1;
         Row_Length := Period_4_5_Row_Length;
         Row_Start_Offset := (AN - 19) mod Row_Length;
         if Row_Start_Offset in 0..1 then
            X := Long_Float (Row_Start_Offset + 1) * Delta_X;
         else
            X := Long_Float (Row_Start_Offset + Total_Shift) * Delta_X;
         end if;
         Y := Long_Float ((AN - 19) / Row_Length + 2) * Delta_Y;
      elsif AN in 55..118 then
         Total_Shift := Lantanoid_Shift + 1;
         Row_Length := Period_4_5_Row_Length + Lantanoid_Period;
         Row_Start_Offset := (AN - 55) mod Row_Length;
         
         if Row_Start_Offset in 0..1 then
            X := Long_Float (Row_Start_Offset + 1) * Delta_X;
         elsif AN in 57..70 or else AN in 89..102 then
            X := 3.0 * Delta_X;
         else
            X := Long_Float (Row_Start_Offset - Lantanoid_Period + Total_Shift)
              * Delta_X;
         end if;
         
         Y := Long_Float ((AN - 55) / Row_Length + 4) * Delta_Y;
         
         if AN in 57..70 then
            Z := Long_Float ((AN - 57) mod Lantanoid_Period) * Delta_Z;
         elsif AN in 89..102 then
            Z := Long_Float ((AN - 89) mod Lantanoid_Period) * Delta_Z;
         end if;
   
      else
         Row_Length := Period_4_5_Row_Length;
         X := Long_Float (((AN - 118) mod Row_Length) + 1) * Delta_X;
         Y := Long_Float ((AN - 118) / Row_Length + 5) * Delta_Y;
      end if;
   end;
   
begin
   
   Process_Options;
   
   declare
      Argument : String := Get_Argument;
   begin
      if Argument /= "" then
         Display_Atoms := Integer'Value (Argument);
      end if;
   end;
   
   Put (Display_Atoms, Width => 1);
   New_Line;
   Put ("Mendeleev's Periodic Table of Elements / ");
   Put ("Периодическая таблица Менделеева");
   New_Line;
   
   -- AN -- Atomic Number
   for AN in 1..Display_Atoms loop
      
      if Display_Table_Linear then
         Select_Atom_Position_Linear (AN, X, Y, Z);
      else
         Select_Atom_Position_Folded (AN, X, Y, Z);
      end if;
      
      if Rotate then
         Put_Atom_Position (AN, X, Y, Z);
      else
         Put_Atom_Position (AN, X, -Y, -Z);
      end if;
      
      New_Line;
      
   end loop;
   
exception
   when HELP_PRINTED => null;
   when VERSION_PRINTED => null;
   
end Periodic_Table;
