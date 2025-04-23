with Ada.Text_IO; use Ada.Text_IO;
with Ada.Directories; use Ada.Directories;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;

package body Ada_Vision_Unit.Tests is

   -- Helper function to create a test result
   function Make_Test_Result(Name : String; Success : Boolean; 
                            Message : String := "") return Test_Result is
      Result : Test_Result;
   begin
      Result.Test_Name := To_Unbounded_String(Name);
      Result.Success := Success;
      Result.Message := To_Unbounded_String(Message);
      return Result;
   end Make_Test_Result;

   -- Helper function to check if two floats are approximately equal
   function Almost_Equal(A, B : Float; Epsilon : Float := 0.00001) return Boolean is
   begin
      return abs(A - B) < Epsilon;
   end Almost_Equal;

   -- Test bounding box creation
   function Test_Bounding_Box_Creation return Test_Result is
      BB : Bounding_Box;
   begin
      -- Create a simple bounding box
      BB.Class_ID := 0;
      BB.X_Center := 0.5;
      BB.Y_Center := 0.5;
      BB.Width := 0.1;
      BB.Height := 0.2;
      
      -- Verify the values
      if BB.Class_ID = 0 and then
         Almost_Equal(BB.X_Center, 0.5) and then
         Almost_Equal(BB.Y_Center, 0.5) and then
         Almost_Equal(BB.Width, 0.1) and then
         Almost_Equal(BB.Height, 0.2) then
         return Make_Test_Result("Bounding Box Creation", True);
      else
         return Make_Test_Result("Bounding Box Creation", False, 
                               "Failed to create bounding box with correct values");
      end if;
   end Test_Bounding_Box_Creation;
   
   -- Test bounding box validation
   function Test_Bounding_Box_Validation return Test_Result is
      Valid_BB : Bounding_Box := (0, 0.5, 0.5, 0.1, 0.2);
      Invalid_BB1 : Bounding_Box := (0, 1.5, 0.5, 0.1, 0.2);  -- X out of range
      Invalid_BB2 : Bounding_Box := (0, 0.5, 0.5, 0.0, 0.2);  -- Zero width
      Invalid_BB3 : Bounding_Box := (0, 0.5, 0.5, -0.1, 0.2); -- Negative width
   begin
      if Is_Valid(Valid_BB) and then
         not Is_Valid(Invalid_BB1) and then
         not Is_Valid(Invalid_BB2) and then
         not Is_Valid(Invalid_BB3) then
         return Make_Test_Result("Bounding Box Validation", True);
      else
         return Make_Test_Result("Bounding Box Validation", False, 
                               "Validation function failed to correctly validate boxes");
      end if;
   end Test_Bounding_Box_Validation;
   
   -- Test intersection calculation
   function Test_Box_Intersection return Test_Result is
      -- Two overlapping boxes
      BB1 : constant Bounding_Box := (0, 0.5, 0.5, 0.4, 0.4);
      BB2 : constant Bounding_Box := (0, 0.6, 0.6, 0.4, 0.4);
      
      -- Non-overlapping boxes
      BB3 : constant Bounding_Box := (0, 0.2, 0.2, 0.2, 0.2);
      BB4 : constant Bounding_Box := (0, 0.8, 0.8, 0.2, 0.2);
      
      -- Expected values
      -- For BB1 and BB2, the intersection is a 0.2 x 0.2 square = 0.04
      Expected_Intersection : constant Float := 0.04;
      Calculated_Intersection : Float;
   begin
      -- Calculate intersection
      Calculated_Intersection := Calculate_Intersection(BB1, BB2);
      
      if Almost_Equal(Calculated_Intersection, Expected_Intersection) and then
         Almost_Equal(Calculate_Intersection(BB3, BB4), 0.0) then
         return Make_Test_Result("Box Intersection", True);
      else
         return Make_Test_Result("Box Intersection", False, 
                               "Incorrect intersection calculation: " & 
                               "Expected " & Float'Image(Expected_Intersection) & 
                               ", Got " & Float'Image(Calculated_Intersection));
      end if;
   end Test_Box_Intersection;
   
   -- Test union calculation
   function Test_Box_Union return Test_Result is
      -- Two overlapping boxes
      BB1 : constant Bounding_Box := (0, 0.5, 0.5, 0.4, 0.4);
      BB2 : constant Bounding_Box := (0, 0.6, 0.6, 0.4, 0.4);
      
      -- Areas
      Area_BB1 : constant Float := 0.4 * 0.4;  -- 0.16
      Area_BB2 : constant Float := 0.4 * 0.4;  -- 0.16
      Intersection : constant Float := 0.04;
      
      -- Expected values
      Expected_Union : constant Float := Area_BB1 + Area_BB2 - Intersection;  -- 0.28
      Calculated_Union : Float;
   begin
      -- Calculate union
      Calculated_Union := Calculate_Union(BB1, BB2);
      
      if Almost_Equal(Calculated_Union, Expected_Union) then
         return Make_Test_Result("Box Union", True);
      else
         return Make_Test_Result("Box Union", False, 
                               "Incorrect union calculation: " & 
                               "Expected " & Float'Image(Expected_Union) & 
                               ", Got " & Float'Image(Calculated_Union));
      end if;
   end Test_Box_Union;
   
   -- Test IoU calculation
   function Test_Intersection_Over_Union return Test_Result is
      -- Two overlapping boxes
      BB1 : constant Bounding_Box := (0, 0.5, 0.5, 0.4, 0.4);
      BB2 : constant Bounding_Box := (0, 0.6, 0.6, 0.4, 0.4);
      
      -- Non-overlapping boxes
      BB3 : constant Bounding_Box := (0, 0.2, 0.2, 0.2, 0.2);
      BB4 : constant Bounding_Box := (0, 0.8, 0.8, 0.2, 0.2);
      
      -- Same box (IoU should be 1.0)
      BB5 : constant Bounding_Box := (0, 0.3, 0.3, 0.2, 0.2);
      BB6 : constant Bounding_Box := (0, 0.3, 0.3, 0.2, 0.2);
      
      -- Expected values for BB1 and BB2
      -- Intersection = 0.04, Union = 0.28, IoU = 0.04/0.28 = 0.142857...
      Expected_IoU : constant Float := 0.04 / 0.28;
      Calculated_IoU : Float;
   begin
      -- Calculate IoU
      Calculated_IoU := Intersection_Over_Union(BB1, BB2);
      
      if Almost_Equal(Calculated_IoU, Expected_IoU) and then
         Almost_Equal(Intersection_Over_Union(BB3, BB4), 0.0) and then
         Almost_Equal(Intersection_Over_Union(BB5, BB6), 1.0) then
         return Make_Test_Result("Intersection Over Union", True);
      else
         return Make_Test_Result("Intersection Over Union", False, 
                               "Incorrect IoU calculation: " & 
                               "Expected " & Float'Image(Expected_IoU) & 
                               ", Got " & Float'Image(Calculated_IoU));
      end if;
   end Test_Intersection_Over_Union;
   
   -- Test coordinate conversion
   function Test_Coordinate_Conversions return Test_Result is
      -- Normalized bounding box
      Normalized_BB : constant Bounding_Box := (0, 0.5, 0.5, 0.4, 0.4);
      
      -- Image dimensions
      Image_Width : constant Positive := 640;
      Image_Height : constant Positive := 480;
      
      -- Expected pixel values
      Expected_X : constant Float := 0.5 * Float(Image_Width);   -- 320
      Expected_Y : constant Float := 0.5 * Float(Image_Height);  -- 240
      Expected_Width : constant Float := 0.4 * Float(Image_Width);   -- 256
      Expected_Height : constant Float := 0.4 * Float(Image_Height); -- 192
      
      -- Convert to pixel coordinates
      Pixel_BB : Bounding_Box;
      
      -- Convert back to normalized coordinates
      Normalized_BB2 : Bounding_Box;
   begin
      -- Convert to pixel coordinates
      Pixel_BB := To_Pixel_Coordinates(Normalized_BB, Image_Width, Image_Height);
      
      -- Check pixel conversion
      if not (Almost_Equal(Pixel_BB.X_Center, Expected_X) and
             Almost_Equal(Pixel_BB.Y_Center, Expected_Y) and
             Almost_Equal(Pixel_BB.Width, Expected_Width) and
             Almost_Equal(Pixel_BB.Height, Expected_Height)) then
         return Make_Test_Result("Coordinate Conversions", False, 
                               "Incorrect pixel conversion");
      end if;
      
      -- Convert back to normalized coordinates
      Normalized_BB2 := To_Normalized_Coordinates(Pixel_BB, Image_Width, Image_Height);
      
      -- Check normalized conversion (should match original)
      if Almost_Equal(Normalized_BB2.X_Center, Normalized_BB.X_Center) and
         Almost_Equal(Normalized_BB2.Y_Center, Normalized_BB.Y_Center) and
         Almost_Equal(Normalized_BB2.Width, Normalized_BB.Width) and
         Almost_Equal(Normalized_BB2.Height, Normalized_BB.Height) then
         return Make_Test_Result("Coordinate Conversions", True);
      else
         return Make_Test_Result("Coordinate Conversions", False, 
                               "Coordinate conversion roundtrip failed");
      end if;
   end Test_Coordinate_Conversions;
   
   -- Test annotation line parsing
   function Test_Parse_Annotation_Line return Test_Result is
      -- This is a private function in annotations package, so we'll test indirectly through file parsing
      Test_File_Path : constant String := "test_annotation.txt";
      Test_Content : constant String := "0 0.5 0.5 0.4 0.4";
      File : File_Type;
      Annotation : Image_Annotation;
      Success : Boolean := False;
   begin
      -- Create a test file
      begin
         Create(File, Out_File, Test_File_Path);
         Put_Line(File, Test_Content);
         Close(File);
         
         -- Parse the file
         Annotation := Parse_Annotation_File(Test_File_Path);
         
         -- Check if we have one bounding box with correct values
         if not Bounding_Box_Vectors.Is_Empty(Annotation.Boxes) and then
            Bounding_Box_Vectors.Length(Annotation.Boxes) = 1 then
            declare
               BB : constant Bounding_Box := 
                  Bounding_Box_Vectors.Element(Annotation.Boxes, 1);
            begin
               Success := BB.Class_ID = 0 and
                         Almost_Equal(BB.X_Center, 0.5) and
                         Almost_Equal(BB.Y_Center, 0.5) and
                         Almost_Equal(BB.Width, 0.4) and
                         Almost_Equal(BB.Height, 0.4);
            end;
         end if;
         
         -- Clean up
         Delete_File(Test_File_Path);
         
         if Success then
            return Make_Test_Result("Parse Annotation Line", True);
         else
            return Make_Test_Result("Parse Annotation Line", False, 
                                  "Failed to correctly parse annotation line");
         end if;
      exception
         when E : others =>
            if Exists(Test_File_Path) then
               Delete_File(Test_File_Path);
            end if;
            return Make_Test_Result("Parse Annotation Line", False, 
                                  "Exception: " & Exception_Message(E));
      end;
   end Test_Parse_Annotation_Line;
   
   -- Create a mock annotation file for testing
   function Create_Mock_Annotation_File(File_Path : String) return Boolean is
      File : File_Type;
   begin
      Create(File, Out_File, File_Path);
      -- Write multiple bounding boxes
      Put_Line(File, "0 0.5 0.5 0.4 0.4");
      Put_Line(File, "1 0.7 0.7 0.2 0.2");
      Put_Line(File, "2 0.3 0.3 0.1 0.1");
      Close(File);
      return True;
   exception
      when others =>
         if Is_Open(File) then
            Close(File);
         end if;
         return False;
   end Create_Mock_Annotation_File;
   
   -- Test file parsing
   function Test_Parse_Annotation_File return Test_Result is
      Test_File_Path : constant String := "multi_box_test.txt";
      Annotation : Image_Annotation;
      Success : Boolean := False;
   begin
      -- Create a test file with multiple bounding boxes
      if Create_Mock_Annotation_File(Test_File_Path) then
         -- Parse the file
         Annotation := Parse_Annotation_File(Test_File_Path);
         
         -- Check if we have three bounding boxes with correct classes
         if not Bounding_Box_Vectors.Is_Empty(Annotation.Boxes) and then
            Bounding_Box_Vectors.Length(Annotation.Boxes) = 3 then
            
            declare
               BB1 : constant Bounding_Box := 
                  Bounding_Box_Vectors.Element(Annotation.Boxes, 1);
               BB2 : constant Bounding_Box := 
                  Bounding_Box_Vectors.Element(Annotation.Boxes, 2);
               BB3 : constant Bounding_Box := 
                  Bounding_Box_Vectors.Element(Annotation.Boxes, 3);
            begin
               Success := BB1.Class_ID = 0 and BB2.Class_ID = 1 and BB3.Class_ID = 2;
            end;
         end if;
         
         -- Clean up
         Delete_File(Test_File_Path);
         
         if Success then
            return Make_Test_Result("Parse Annotation File", True);
         else
            return Make_Test_Result("Parse Annotation File", False, 
                                  "Failed to correctly parse annotation file");
         end if;
      else
         return Make_Test_Result("Parse Annotation File", False, 
                               "Failed to create test file");
      end if;
   exception
      when E : others =>
         if Exists(Test_File_Path) then
            Delete_File(Test_File_Path);
         end if;
         return Make_Test_Result("Parse Annotation File", False, 
                               "Exception: " & Exception_Message(E));
   end Test_Parse_Annotation_File;
   
   -- Create a mock dataset directory for testing
   function Create_Mock_Dataset_Directory(Dir_Path : String) return Boolean is
   begin
      -- Create directory if it doesn't exist
      if not Exists(Dir_Path) then
         Create_Path(Dir_Path);
      end if;
      
      -- Create mock annotation files
      Create_Mock_Annotation_File(Compose(Dir_Path, "image1.txt"));
      Create_Mock_Annotation_File(Compose(Dir_Path, "image2.txt"));
      Create_Mock_Annotation_File(Compose(Dir_Path, "image3.txt"));
      
      return True;
   exception
      when others =>
         return False;
   end Create_Mock_Dataset_Directory;
   
   -- Test dataset processing
   function Test_Dataset_Processing return Test_Result is
      Test_Dir_Path : constant String := "test_dataset";
      Dataset : Image_Dataset;
      Success : Boolean := False;
   begin
      -- Create a test dataset directory
      if Create_Mock_Dataset_Directory(Test_Dir_Path) then
         -- Parse the directory
         Dataset := Parse_Annotation_Directory(Test_Dir_Path);
         
         -- Check if we have three annotation files
         if not Image_Annotation_Vectors.Is_Empty(Dataset.Annotations) and then
            Image_Annotation_Vectors.Length(Dataset.Annotations) = 3 then
            
            -- Each annotation should have 3 bounding boxes
            declare
               All_Have_Three_Boxes : Boolean := True;
            begin
               for I in 1..3 loop
                  declare
                     Annotation : constant Image_Annotation := 
                        Image_Annotation_Vectors.Element(Dataset.Annotations, I);
                  begin
                     if Bounding_Box_Vectors.Length(Annotation.Boxes) /= 3 then
                        All_Have_Three_Boxes := False;
                     end if;
                  end;
               end loop;
               Success := All_Have_Three_Boxes;
            end;
         end if;
         
         -- Clean up
         Delete_File(Compose(Test_Dir_Path, "image1.txt"));
         Delete_File(Compose(Test_Dir_Path, "image2.txt"));
         Delete_File(Compose(Test_Dir_Path, "image3.txt"));
         Delete_Directory(Test_Dir_Path);
         
         if Success then
            return Make_Test_Result("Dataset Processing", True);
         else
            return Make_Test_Result("Dataset Processing", False, 
                                  "Failed to correctly process dataset");
         end if;
      else
         return Make_Test_Result("Dataset Processing", False, 
                               "Failed to create test dataset");
      end if;
   exception
      when E : others =>
         if Exists(Test_Dir_Path) then
            Delete_File(Compose(Test_Dir_Path, "image1.txt"));
            Delete_File(Compose(Test_Dir_Path, "image2.txt"));
            Delete_File(Compose(Test_Dir_Path, "image3.txt"));
            Delete_Directory(Test_Dir_Path);
         end if;
         return Make_Test_Result("Dataset Processing", False, 
                               "Exception: " & Exception_Message(E));
   end Test_Dataset_Processing;
   
   -- Create a mock dataset with overlapping boxes
   function Create_Mock_Dataset_With_Overlaps(Dir_Path : String) return Boolean is
      File : File_Type;
   begin
      -- Create directory if it doesn't exist
      if not Exists(Dir_Path) then
         Create_Path(Dir_Path);
      end if;
      
      -- Create a file with overlapping boxes (high IoU)
      Create(File, Out_File, Compose(Dir_Path, "overlapping.txt"));
      Put_Line(File, "0 0.5 0.5 0.3 0.3");
      Put_Line(File, "0 0.52 0.52 0.3 0.3");  -- Very similar to first box
      Close(File);
      
      -- Create a file with non-overlapping boxes
      Create(File, Out_File, Compose(Dir_Path, "non_overlapping.txt"));
      Put_Line(File, "0 0.2 0.2 0.1 0.1");
      Put_Line(File, "0 0.8 0.8 0.1 0.1");
      Close(File);
      
      return True;
   exception
      when others =>
         if Is_Open(File) then
            Close(File);
         end if;
         return False;
   end Create_Mock_Dataset_With_Overlaps;
   
   -- Create a mock dataset with outlier boxes
   function Create_Mock_Dataset_With_Outliers(Dir_Path : String) return Boolean is
      File : File_Type;
   begin
      -- Create directory if it doesn't exist
      if not Exists(Dir_Path) then
         Create_Path(Dir_Path);
      end if;
      
      -- Create a file with normal-sized boxes
      Create(File, Out_File, Compose(Dir_Path, "normal.txt"));
      Put_Line(File, "0 0.5 0.5 0.2 0.2");
      Put_Line(File, "0 0.3 0.3 0.2 0.2");
      Put_Line(File, "0 0.7 0.7 0.2 0.2");
      Close(File);
      
      -- Create a file with a very large box (outlier)
      Create(File, Out_File, Compose(Dir_Path, "large_box.txt"));
      Put_Line(File, "0 0.5 0.5 0.9 0.9");  -- Very large box
      Put_Line(File, "0 0.2 0.2 0.1 0.1");  -- Normal box
      Close(File);
      
      -- Create a file with a very small box (outlier)
      Create(File, Out_File, Compose(Dir_Path, "small_box.txt"));
      Put_Line(File, "0 0.5 0.5 0.01 0.01");  -- Very small box
      Put_Line(File, "0 0.7 0.7 0.2 0.2");    -- Normal box
      Close(File);
      
      return True;
   exception
      when others =>
         if Is_Open(File) then
            Close(File);
         end if;
         return False;
   end Create_Mock_Dataset_With_Outliers;
   
   -- Create a mock dataset with different class labels
   function Create_Mock_Dataset_With_Classes(Dir_Path : String) return Boolean is
      File : File_Type;
   begin
      -- Create directory if it doesn't exist
      if not Exists(Dir_Path) then
         Create_Path(Dir_Path);
      end if;
      
      -- Create a file with class 0 objects
      Create(File, Out_File, Compose(Dir_Path, "class0.txt"));
      Put_Line(File, "0 0.5 0.5 0.2 0.2");
      Put_Line(File, "0 0.3 0.3 0.2 0.2");
      Close(File);
      
      -- Create a file with class 1 objects
      Create(File, Out_File, Compose(Dir_Path, "class1.txt"));
      Put_Line(File, "1 0.5 0.5 0.2 0.2");
      Put_Line(File, "1 0.3 0.3 0.2 0.2");
      Close(File);
      
      -- Create a file with class 2 objects
      Create(File, Out_File, Compose(Dir_Path, "class2.txt"));
      Put_Line(File, "2 0.5 0.5 0.2 0.2");
      Close(File);
      
      -- Create a file with multiple classes
      Create(File, Out_File, Compose(Dir_Path, "mixed.txt"));
      Put_Line(File, "0 0.2 0.2 0.1 0.1");
      Put_Line(File, "1 0.5 0.5 0.2 0.2");
      Put_Line(File, "2 0.8 0.8 0.1 0.1");
      Close(File);
      
      -- Create an empty file
      Create(File, Out_File, Compose(Dir_Path, "empty.txt"));
      Close(File);
      
      return True;
   exception
      when others =>
         if Is_Open(File) then
            Close(File);
         end if;
         return False;
   end Create_Mock_Dataset_With_Classes;
   
   -- Test dataset statistics computation
   function Test_Dataset_Statistics return Test_Result is
      Test_Dir_Path : constant String := "test_stats_dataset";
      Dataset : Image_Dataset;
      Stats : Dataset_Statistics;
      Success : Boolean := False;
   begin
      -- Create a test dataset with a variety of boxes
      if Create_Mock_Dataset_With_Classes(Test_Dir_Path) then
         -- Parse the directory
         Dataset := Parse_Annotation_Directory(Test_Dir_Path);
         
         -- Compute statistics
         Stats := Compute_Dataset_Statistics(Dataset);
         
         -- Check basic statistics
         if Stats.Total_Images = 5 and         -- 5 files
            Stats.Total_Objects = 9 and        -- 9 total boxes
            Stats.Empty_Images = 1 and         -- 1 empty file
            Stats.Min_Objects = 0 and          -- Empty file has 0 objects
            Stats.Max_Objects = 3 and          -- mixed.txt has 3 objects
            Almost_Equal(Stats.Avg_Objects, 1.8) then  -- 9/5 = 1.8
            Success := True;
         end if;
         
         -- Clean up
         Delete_File(Compose(Test_Dir_Path, "class0.txt"));
         Delete_File(Compose(Test_Dir_Path, "class1.txt"));
         Delete_File(Compose(Test_Dir_Path, "class2.txt"));
         Delete_File(Compose(Test_Dir_Path, "mixed.txt"));
         Delete_File(Compose(Test_Dir_Path, "empty.txt"));
         Delete_Directory(Test_Dir_Path);
         
         if Success then
            return Make_Test_Result("Dataset Statistics", True);
         else
            return Make_Test_Result("Dataset Statistics", False, 
                                  "Failed to correctly compute dataset statistics");
         end if;
      else
         return Make_Test_Result("Dataset Statistics", False, 
                               "Failed to create test dataset");
      end if;
   exception
      when E : others =>
         if Exists(Test_Dir_Path) then
            Delete_File(Compose(Test_Dir_Path, "class0.txt"));
            Delete_File(Compose(Test_Dir_Path, "class1.txt"));
            Delete_File(Compose(Test_Dir_Path, "class2.txt"));
            Delete_File(Compose(Test_Dir_Path, "mixed.txt"));
            Delete_File(Compose(Test_Dir_Path, "empty.txt"));
            Delete_Directory(Test_Dir_Path);
         end if;
         return Make_Test_Result("Dataset Statistics", False, 
                               "Exception: " & Exception_Message(E));
   end Test_Dataset_Statistics;
   
   -- Test class distribution statistics
   function Test_Class_Distribution return Test_Result is
      Test_Dir_Path : constant String := "test_class_dataset";
      Dataset : Image_Dataset;
      Stats : Dataset_Statistics;
      Success : Boolean := False;
      Class0_Count : Natural := 0;
      Class1_Count : Natural := 0;
      Class2_Count : Natural := 0;
   begin
      -- Create a test dataset with different classes
      if Create_Mock_Dataset_With_Classes(Test_Dir_Path) then
         -- Parse the directory
         Dataset := Parse_Annotation_Directory(Test_Dir_Path);
         
         -- Compute statistics
         Stats := Compute_Dataset_Statistics(Dataset);
         
         -- Check class counts
         for I in 1..Class_Count_Vectors.Length(Stats.Class_Counts) loop
            declare
               Class_Count : constant Class_Count_Record := 
                  Class_Count_Vectors.Element(Stats.Class_Counts, I);
            begin
               case Class_Count.Class_ID is
                  when 0 =>
                     Class0_Count := Class_Count.Object_Count;
                  when 1 =>
                     Class1_Count := Class_Count.Object_Count;
                  when 2 =>
                     Class2_Count := Class_Count.Object_Count;
                  when others =>
                     null;
               end case;
            end;
         end loop;
         
         -- Expected: 3 objects of class 0, 3 of class 1, 3 of class 2
         if Class0_Count = 3 and Class1_Count = 3 and Class2_Count = 3 then
            Success := True;
         end if;
         
         -- Clean up
         Delete_File(Compose(Test_Dir_Path, "class0.txt"));
         Delete_File(Compose(Test_Dir_Path, "class1.txt"));
         Delete_File(Compose(Test_Dir_Path, "class2.txt"));
         Delete_File(Compose(Test_Dir_Path, "mixed.txt"));
         Delete_File(Compose(Test_Dir_Path, "empty.txt"));
         Delete_Directory(Test_Dir_Path);
         
         if Success then
            return Make_Test_Result("Class Distribution", True);
         else
            return Make_Test_Result("Class Distribution", False, 
                                  "Failed to correctly compute class distribution. " &
                                  "Got Class0=" & Natural'Image(Class0_Count) &
                                  ", Class1=" & Natural'Image(Class1_Count) &
                                  ", Class2=" & Natural'Image(Class2_Count));
         end if;
      else
         return Make_Test_Result("Class Distribution", False, 
                               "Failed to create test dataset");
      end if;
   exception
      when E : others =>
         if Exists(Test_Dir_Path) then
            Delete_File(Compose(Test_Dir_Path, "class0.txt"));
            Delete_File(Compose(Test_Dir_Path, "class1.txt"));
            Delete_File(Compose(Test_Dir_Path, "class2.txt"));
            Delete_File(Compose(Test_Dir_Path, "mixed.txt"));
            Delete_File(Compose(Test_Dir_Path, "empty.txt"));
            Delete_Directory(Test_Dir_Path);
         end if;
         return Make_Test_Result("Class Distribution", False, 
                               "Exception: " & Exception_Message(E));
   end Test_Class_Distribution;
   
   -- Test finding outlier boxes
   function Test_Find_Outliers return Test_Result is
      Test_Dir_Path : constant String := "test_outliers_dataset";
      Dataset : Image_Dataset;
      Outliers : Image_Annotation_Vectors.Vector;
      Success : Boolean := False;
   begin
      -- Create a test dataset with outlier boxes
      if Create_Mock_Dataset_With_Outliers(Test_Dir_Path) then
         -- Parse the directory
         Dataset := Parse_Annotation_Directory(Test_Dir_Path);
         
         -- Find outliers
         Outliers := Find_Outliers(Dataset, 2.0);  -- 2 standard deviations
         
         -- Should find 2 outliers (very large and very small boxes)
         if Image_Annotation_Vectors.Length(Outliers) = 2 then
            Success := True;
         end if;
         
         -- Clean up
         Delete_File(Compose(Test_Dir_Path, "normal.txt"));
         Delete_File(Compose(Test_Dir_Path, "large_box.txt"));
         Delete_File(Compose(Test_Dir_Path, "small_box.txt"));
         Delete_Directory(Test_Dir_Path);
         
         if Success then
            return Make_Test_Result("Find Outliers", True);
         else
            return Make_Test_Result("Find Outliers", False, 
                                  "Failed to correctly identify outlier boxes. Found " &
                                  Natural'Image(Natural(Image_Annotation_Vectors.Length(Outliers))) &
                                  " instead of 2");
         end if;
      else
         return Make_Test_Result("Find Outliers", False, 
                               "Failed to create test dataset");
      end if;
   exception
      when E : others =>
         if Exists(Test_Dir_Path) then
            Delete_File(Compose(Test_Dir_Path, "normal.txt"));
            Delete_File(Compose(Test_Dir_Path, "large_box.txt"));
            Delete_File(Compose(Test_Dir_Path, "small_box.txt"));
            Delete_Directory(Test_Dir_Path);
         end if;
         return Make_Test_Result("Find Outliers", False, 
                               "Exception: " & Exception_Message(E));
   end Test_Find_Outliers;
   
   -- Test finding overlapping boxes
   function Test_Find_Overlapping_Boxes return Test_Result is
      Test_Dir_Path : constant String := "test_overlaps_dataset";
      Dataset : Image_Dataset;
      Overlaps : Image_Annotation_Vectors.Vector;
      Success : Boolean := False;
   begin
      -- Create a test dataset with overlapping boxes
      if Create_Mock_Dataset_With_Overlaps(Test_Dir_Path) then
         -- Parse the directory
         Dataset := Parse_Annotation_Directory(Test_Dir_Path);
         
         -- Find overlapping boxes
         Overlaps := Find_Overlapping_Boxes(Dataset, 0.7);  -- IoU threshold of 0.7
         
         -- Should find 1 image with overlaps (the "overlapping.txt" file)
         if Image_Annotation_Vectors.Length(Overlaps) = 1 then
            Success := True;
         end if;
         
         -- Clean up
         Delete_File(Compose(Test_Dir_Path, "overlapping.txt"));
         Delete_File(Compose(Test_Dir_Path, "non_overlapping.txt"));
         Delete_Directory(Test_Dir_Path);
         
         if Success then
            return Make_Test_Result("Find Overlapping Boxes", True);
         else
            return Make_Test_Result("Find Overlapping Boxes", False, 
                                  "Failed to correctly identify overlapping boxes. Found " &
                                  Natural'Image(Natural(Image_Annotation_Vectors.Length(Overlaps))) &
                                  " images with overlaps instead of 1");
         end if;
      else
         return Make_Test_Result("Find Overlapping Boxes", False, 
                               "Failed to create test dataset");
      end if;
   exception
      when E : others =>
         if Exists(Test_Dir_Path) then
            Delete_File(Compose(Test_Dir_Path, "overlapping.txt"));
            Delete_File(Compose(Test_Dir_Path, "non_overlapping.txt"));
            Delete_Directory(Test_Dir_Path);
         end if;
         return Make_Test_Result("Find Overlapping Boxes", False, 
                               "Exception: " & Exception_Message(E));
   end Test_Find_Overlapping_Boxes;
   
   -- Test dataset integrity validation
   function Test_Dataset_Integrity return Test_Result is
      Test_Dir_Path : constant String := "test_integrity_dataset";
      Valid_Dataset, Invalid_Dataset : Image_Dataset;
      Valid_File, Invalid_File : File_Type;
      Valid_Result, Invalid_Result : Boolean;
      Success : Boolean := False;
   begin
      -- Create directory if it doesn't exist
      if not Exists(Test_Dir_Path) then
         Create_Path(Test_Dir_Path);
      end if;
      
      -- Create a valid annotation file
      Create(Valid_File, Out_File, Compose(Test_Dir_Path, "valid.txt"));
      Put_Line(Valid_File, "0 0.5 0.5 0.3 0.3");
      Put_Line(Valid_File, "1 0.7 0.7 0.2 0.2");
      Close(Valid_File);
      
      -- Create an invalid annotation file (coordinates outside [0,1] range)
      Create(Invalid_File, Out_File, Compose(Test_Dir_Path, "invalid.txt"));
      Put_Line(Invalid_File, "0 1.5 0.5 0.3 0.3");  -- x_center > 1.0
      Put_Line(Invalid_File, "1 0.7 0.7 0.2 0.2");
      Close(Invalid_File);
      
      -- Parse directories
      Valid_Dataset := Parse_Annotation_Directory(Test_Dir_Path);
      Invalid_Dataset := Valid_Dataset;  -- Create a copy to modify
      
      -- Replace the valid box with an invalid one
      if not Image_Annotation_Vectors.Is_Empty(Invalid_Dataset.Annotations) and then
         not Bounding_Box_Vectors.Is_Empty(
            Image_Annotation_Vectors.Element(Invalid_Dataset.Annotations, 1).Boxes) then
         
         declare
            Annotation : Image_Annotation := 
               Image_Annotation_Vectors.Element(Invalid_Dataset.Annotations, 1);
            Invalid_Box : Bounding_Box := (0, 1.5, 0.5, 0.3, 0.3);  -- Invalid x_center
         begin
            Bounding_Box_Vectors.Replace_Element(Annotation.Boxes, 1, Invalid_Box);
            Image_Annotation_Vectors.Replace_Element(Invalid_Dataset.Annotations, 1, Annotation);
         end;
      end if;
      
      -- Verify integrity
      Valid_Result := Verify_Dataset_Integrity(Valid_Dataset);
      Invalid_Result := Verify_Dataset_Integrity(Invalid_Dataset);
      
      -- Valid dataset should pass, invalid should fail
      if Valid_Result = True and Invalid_Result = False then
         Success := True;
      end if;
      
      -- Clean up
      Delete_File(Compose(Test_Dir_Path, "valid.txt"));
      Delete_File(Compose(Test_Dir_Path, "invalid.txt"));
      Delete_Directory(Test_Dir_Path);
      
      if Success then
         return Make_Test_Result("Dataset Integrity", True);
      else
         return Make_Test_Result("Dataset Integrity", False, 
                               "Failed to correctly validate dataset integrity. " &
                               "Valid=" & Boolean'Image(Valid_Result) &
                               ", Invalid=" & Boolean'Image(Invalid_Result));
      end if;
   exception
      when E : others =>
         if Exists(Test_Dir_Path) then
            Delete_File(Compose(Test_Dir_Path, "valid.txt"));
            Delete_File(Compose(Test_Dir_Path, "invalid.txt"));
            Delete_Directory(Test_Dir_Path);
         end if;
         return Make_Test_Result("Dataset Integrity", False, 
                               "Exception: " & Exception_Message(E));
   end Test_Dataset_Integrity;
   
   -- Test statistics on empty dataset
   function Test_Statistics_Empty_Dataset return Test_Result is
      Empty_Dataset : Image_Dataset;
      Stats : Dataset_Statistics;
      Success : Boolean := False;
   begin
      -- Compute statistics on empty dataset
      Stats := Compute_Dataset_Statistics(Empty_Dataset);
      
      -- Check that results are reasonable for empty dataset
      if Stats.Total_Images = 0 and
         Stats.Total_Objects = 0 and
         Stats.Min_Objects = 0 and
         Stats.Max_Objects = 0 and
         Stats.Avg_Objects = 0.0 and
         Stats.Min_Box_Width = 0.0 and
         Stats.Max_Box_Width = 0.0 and
         Stats.Avg_Box_Width = 0.0 then
         Success := True;
      end if;
      
      if Success then
         return Make_Test_Result("Statistics Empty Dataset", True);
      else
         return Make_Test_Result("Statistics Empty Dataset", False, 
                               "Failed to handle empty dataset correctly");
      end if;
   exception
      when E : others =>
         return Make_Test_Result("Statistics Empty Dataset", False, 
                               "Exception: " & Exception_Message(E));
   end Test_Statistics_Empty_Dataset;
   
   -- Print test results
   procedure Print_Test_Results(Results : Test_Results_Vectors.Vector) is
      Total_Tests : constant Natural := Natural(Test_Results_Vectors.Length(Results));
      Passed_Tests : Natural := 0;
   begin
      Put_Line("Ada Vision Unit Test Results");
      Put_Line("==========================");
      Put_Line("");
      
      for I in 1..Test_Results_Vectors.Length(Results) loop
         declare
            Result : constant Test_Result := 
               Test_Results_Vectors.Element(Results, I);
            Status : constant String := 
               (if Result.Success then "PASS" else "FAIL");
         begin
            Put_Line(Status & ": " & To_String(Result.Test_Name));
            
            if not Result.Success and then Length(Result.Message) > 0 then
               Put_Line("   " & To_String(Result.Message));
            end if;
            
            if Result.Success then
               Passed_Tests := Passed_Tests + 1;
            end if;
         end;
      end loop;
      
      Put_Line("");
      Put_Line("Summary: " & Natural'Image(Passed_Tests) & 
              " of" & Natural'Image(Total_Tests) & " tests passed");
   end Print_Test_Results;
   
   -- Main test runner
   procedure Run_All_Tests is
      Results : Test_Results_Vectors.Vector;
   begin
      -- Run all tests and collect results
      Test_Results_Vectors.Append(Results, Test_Bounding_Box_Creation);
      Test_Results_Vectors.Append(Results, Test_Bounding_Box_Validation);
      Test_Results_Vectors.Append(Results, Test_Box_Intersection);
      Test_Results_Vectors.Append(Results, Test_Box_Union);
      Test_Results_Vectors.Append(Results, Test_Intersection_Over_Union);
      Test_Results_Vectors.Append(Results, Test_Coordinate_Conversions);
      Test_Results_Vectors.Append(Results, Test_Parse_Annotation_Line);
      Test_Results_Vectors.Append(Results, Test_Parse_Annotation_File);
      Test_Results_Vectors.Append(Results, Test_Dataset_Processing);
      
      -- Add new tests for statistics and data quality
      Test_Results_Vectors.Append(Results, Test_Dataset_Statistics);
      Test_Results_Vectors.Append(Results, Test_Class_Distribution);
      Test_Results_Vectors.Append(Results, Test_Find_Outliers);
      Test_Results_Vectors.Append(Results, Test_Find_Overlapping_Boxes);
      Test_Results_Vectors.Append(Results, Test_Dataset_Integrity);
      Test_Results_Vectors.Append(Results, Test_Statistics_Empty_Dataset);
      
      -- Print results
      Print_Test_Results(Results);
   end Run_All_Tests;

end Ada_Vision_Unit.Tests;