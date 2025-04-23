with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada_Vision_Unit.Bounding_Boxes; use Ada_Vision_Unit.Bounding_Boxes;
with Ada_Vision_Unit.Annotations; use Ada_Vision_Unit.Annotations;

package Ada_Vision_Unit.Tests is

   -- Test suite result type
   type Test_Result is record
      Test_Name   : Unbounded_String;
      Success     : Boolean := False;
      Message     : Unbounded_String;
   end record;
   
   -- Container for test results
   package Test_Results_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Test_Result);
   
   -- Main test runner
   procedure Run_All_Tests;
   
   -- Individual test cases for bounding box functions
   function Test_Bounding_Box_Creation return Test_Result;
   function Test_Bounding_Box_Validation return Test_Result;
   function Test_Box_Intersection return Test_Result;
   function Test_Box_Union return Test_Result;
   function Test_Intersection_Over_Union return Test_Result;
   function Test_Coordinate_Conversions return Test_Result;
   
   -- Test cases for annotation parsing
   function Test_Parse_Annotation_Line return Test_Result;
   function Test_Parse_Annotation_File return Test_Result;
   function Test_Dataset_Processing return Test_Result;

   -- New test cases for dataset statistics and quality checking
   function Test_Dataset_Statistics return Test_Result;
   function Test_Class_Distribution return Test_Result;
   function Test_Find_Outliers return Test_Result;
   function Test_Find_Overlapping_Boxes return Test_Result;
   function Test_Dataset_Integrity return Test_Result;
   function Test_Statistics_Empty_Dataset return Test_Result;

   -- Helper functions for test suite
   procedure Print_Test_Results(Results : Test_Results_Vectors.Vector);
   function Create_Mock_Annotation_File(File_Path : String) return Boolean;
   function Create_Mock_Dataset_Directory(Dir_Path : String) return Boolean;
   function Create_Mock_Dataset_With_Overlaps(Dir_Path : String) return Boolean;
   function Create_Mock_Dataset_With_Outliers(Dir_Path : String) return Boolean;
   function Create_Mock_Dataset_With_Classes(Dir_Path : String) return Boolean;

end Ada_Vision_Unit.Tests;