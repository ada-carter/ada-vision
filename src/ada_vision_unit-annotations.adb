with Ada.Text_IO; use Ada.Text_IO;
with Ada.Directories; use Ada.Directories;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;
with GNATCOLL.JSON; use GNATCOLL.JSON;
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Hash_Functions;

package body Ada_Vision_Unit.Annotations is

   -- Helper function to split a string into tokens
   function Split(Source : String; Delimiter : Character) return Bounding_Box_Vectors.Vector is
      Result  : Bounding_Box_Vectors.Vector;
      Start   : Positive := Source'First;
      Current : Positive := Source'First;
      BB      : Bounding_Box;
      Token_Count : Natural := 0;
      
      -- Process one token at a time
      procedure Process_Token(Token : String) is
      begin
         Token_Count := Token_Count + 1;
         
         case Token_Count is
            when 1 =>
               -- First token is class_id
               BB.Class_ID := Integer'Value(Token);
            when 2 =>
               -- Second token is x_center
               BB.X_Center := Float'Value(Token);
            when 3 =>
               -- Third token is y_center
               BB.Y_Center := Float'Value(Token);
            when 4 =>
               -- Fourth token is width
               BB.Width := Float'Value(Token);
            when 5 =>
               -- Fifth token is height
               BB.Height := Float'Value(Token);
               
               -- We have all components of a bounding box, add it to the result
               Bounding_Box_Vectors.Append(Result, BB);
               Token_Count := 0;  -- Reset for the next bounding box
               
            when others =>
               raise Format_Error with "Invalid token count in YOLO annotation";
         end case;
      end Process_Token;
      
   begin
      -- Empty string case
      if Source'Length = 0 then
         return Result;
      end if;
      
      -- Iterate through the string looking for delimiters
      while Current <= Source'Last loop
         if Source(Current) = Delimiter then
            if Start <= Current - 1 then
               Process_Token(Source(Start..Current-1));
            end if;
            Start := Current + 1;
         end if;
         Current := Current + 1;
      end loop;
      
      -- Process final token if any
      if Start <= Source'Last then
         Process_Token(Source(Start..Source'Last));
      end if;
      
      -- Check if we have incomplete bounding box data
      if Token_Count /= 0 then
         raise Format_Error with "Incomplete bounding box data in YOLO annotation";
      end if;
      
      return Result;
   end Split;

   -- Parse a single line of a YOLO annotation file
   function Parse_Line(Line : String) return Bounding_Box is
      Tokens : Bounding_Box_Vectors.Vector;
      BB     : Bounding_Box;
   begin
      -- Skip empty lines
      if Line'Length = 0 or else (Line'Length = 1 and then Line(Line'First) = ' ') then
         BB.Class_ID := -1;  -- Invalid marker
         return BB;
      end if;
      
      -- Split the line into tokens and extract bounding box data
      Tokens := Split(Line, ' ');
      
      -- Ensure we have exactly one bounding box
      if Bounding_Box_Vectors.Length(Tokens) = 1 then
         return Bounding_Box_Vectors.Element(Tokens, 1);
      else
         raise Format_Error with "Invalid bounding box format in line: " & Line;
      end if;
   end Parse_Line;

   -- Parse a single YOLO annotation file
   function Parse_Annotation_File(
      File_Path    : String;
      Image_Path   : String := "";
      Image_Width  : Positive := 1;
      Image_Height : Positive := 1) return Image_Annotation is
      
      File     : File_Type;
      Line     : String(1..1024);
      Last     : Natural;
      Result   : Image_Annotation;
      Box      : Bounding_Box;
   begin
      -- Pre-allocate for typical bounding box count
      Result.Boxes.Reserve(16);
      -- Initialize annotation record
      Result.Image_Path := To_Unbounded_String(Image_Path);
      Result.Image_Width := Image_Width;
      Result.Image_Height := Image_Height;
      
      -- Check if file exists
      if not Exists(File_Path) then
         raise IO_Error with "Annotation file not found: " & File_Path;
      end if;
      
      -- Open and read file line by line
      Open(File, In_File, File_Path);
      
      while not End_Of_File(File) loop
         Get_Line(File, Line, Last);
         
         -- Parse line if not empty
         if Last > 0 then
            Box := Parse_Line(Line(1..Last));
            
            -- Add valid boxes to the annotation
            if Box.Class_ID >= 0 then
               Bounding_Box_Vectors.Append(Result.Boxes, Box);
            end if;
         end if;
      end loop;
      
      Close(File);
      return Result;
      
   exception
      when E : others =>
         if Is_Open(File) then
            Close(File);
         end if;
         raise IO_Error with "Error parsing annotation file: " & File_Path & 
                            " - " & Exception_Message(E);
   end Parse_Annotation_File;
   
   -- Get the corresponding image file path for an annotation file
   function Get_Image_Path(Annotation_File : String; 
                           Image_Dir : String) return String is
      Base_Name : constant String := Base_Name(Annotation_File);
      Ext_Start : constant Natural := Index(Base_Name, ".", Backward);
      Image_Base : String := Base_Name;
      
      -- Common image extensions to check
      type Extension_Array is array (Positive range <>) of Unbounded_String;
      Extensions : constant Extension_Array := 
         (To_Unbounded_String(".jpg"),
          To_Unbounded_String(".jpeg"),
          To_Unbounded_String(".png"),
          To_Unbounded_String(".bmp"));
   begin
      -- Remove extension if present
      if Ext_Start > 0 then
         Image_Base := Base_Name(Base_Name'First..Ext_Start-1);
      end if;
      
      -- Check for image file with different extensions
      for Ext of Extensions loop
         declare
            Img_Path : constant String := 
               (if Image_Dir'Length > 0 then
                  Compose(Image_Dir, Image_Base & To_String(Ext))
                else
                  Image_Base & To_String(Ext));
         begin
            if Exists(Img_Path) then
               return Img_Path;
            end if;
         end;
      end loop;
      
      -- If no image file found, return empty string
      return "";
   end Get_Image_Path;

   -- Parse a directory of YOLO annotation files
   function Parse_Annotation_Directory(
      Dir_Path     : String;
      Image_Dir    : String := "";
      Default_Width  : Positive := 1;
      Default_Height : Positive := 1) return Image_Dataset is
      
      Result    : Image_Dataset;
      Search    : Search_Type;
      Dir_Entry : Directory_Entry_Type;
      File_Path : Unbounded_String;
      Img_Path  : Unbounded_String;
   begin
      -- Pre-allocate storage for expected number of files
      Result.Annotations.Reserve(64);

      -- Check if directory exists
      if not Exists(Dir_Path) then
         raise IO_Error with "Annotation directory not found: " & Dir_Path;
      end if;
      
      -- Start search for text files
      Start_Search(Search, Dir_Path, "*.txt");
      
      while More_Entries(Search) loop
         Get_Next_Entry(Search, Dir_Entry);
         
         -- Get full path of annotation file
         File_Path := To_Unbounded_String(Full_Name(Dir_Entry));
         
         -- Get corresponding image path
         Img_Path := To_Unbounded_String(Get_Image_Path(To_String(File_Path), Image_Dir));
         
         -- Parse annotation file
         declare
            Annotation : Image_Annotation;
         begin
            Annotation := Parse_Annotation_File(
               To_String(File_Path),
               To_String(Img_Path),
               Default_Width,
               Default_Height);
               
            -- Add to dataset
            Image_Annotation_Vectors.Append(Result.Annotations, Annotation);
         exception
            when E : others =>
               Put_Line("Warning: Failed to parse " & To_String(File_Path) & 
                        " - " & Exception_Message(E));
         end;
      end loop;
      
      End_Search(Search);
      return Result;
      
   exception
      when E : others =>
         if Is_Open(Search) then
            End_Search(Search);
         end if;
         raise IO_Error with "Error parsing annotation directory: " & Dir_Path & 
                           " - " & Exception_Message(E);
   end Parse_Annotation_Directory;
   
   -- Verify annotation integrity in a dataset
   function Verify_Dataset_Integrity(Dataset : Image_Dataset) return Boolean is
      Is_Valid_Dataset : Boolean := True;
   begin
      -- Check if dataset is empty
      if Image_Annotation_Vectors.Is_Empty(Dataset.Annotations) then
         return False;
      end if;
      
      -- Check each annotation
      for I in 1..Image_Annotation_Vectors.Length(Dataset.Annotations) loop
         declare
            Annotation : constant Image_Annotation := 
               Image_Annotation_Vectors.Element(Dataset.Annotations, I);
         begin
            -- Check if image file exists
            if Length(Annotation.Image_Path) > 0 and then
               not Exists(To_String(Annotation.Image_Path)) then
               Put_Line("Warning: Image file not found: " & 
                       To_String(Annotation.Image_Path));
               Is_Valid_Dataset := False;
            end if;
            
            -- Check each bounding box
            for J in 1..Bounding_Box_Vectors.Length(Annotation.Boxes) loop
               declare
                  Box : constant Bounding_Box := 
                     Bounding_Box_Vectors.Element(Annotation.Boxes, J);
               begin
                  -- Check if bounding box is valid
                  if not Is_Valid(Box) then
                     Put_Line("Warning: Invalid bounding box in " & 
                              To_String(Annotation.Image_Path));
                     Is_Valid_Dataset := False;
                  end if;
               end;
            end loop;
         end;
      end loop;
      
      return Is_Valid_Dataset;
   end Verify_Dataset_Integrity;
   
   -- Helper function to update class count
   procedure Update_Class_Count(
      Class_Counts : in out Class_Count_Vectors.Vector;
      Class_ID     : Integer) is
      Found : Boolean := False;
   begin
      -- Check if class already exists in counts
      for I in 1..Class_Count_Vectors.Length(Class_Counts) loop
         declare
            Count_Rec : Class_Count_Record := 
               Class_Count_Vectors.Element(Class_Counts, I);
         begin
            if Count_Rec.Class_ID = Class_ID then
               Count_Rec.Object_Count := Count_Rec.Object_Count + 1;
               Class_Count_Vectors.Replace_Element(Class_Counts, I, Count_Rec);
               Found := True;
               exit;
            end if;
         end;
      end loop;
      
      -- If class not found, add a new entry
      if not Found then
         Class_Count_Vectors.Append(Class_Counts, 
                                   (Class_ID => Class_ID, 
                                    Object_Count => 1));
      end if;
   end Update_Class_Count;
   
   -- Compute descriptive statistics for a dataset
   function Compute_Dataset_Statistics(Dataset : Image_Dataset) return Dataset_Statistics is
      Stats : Dataset_Statistics;
      Total_Width_Sum : Float := 0.0;
      Total_Height_Sum : Float := 0.0;
      Total_Area_Sum : Float := 0.0;
      Obj_Count : Natural;
      Box_Area : Float;
   begin
      -- Initialize statistics
      Stats.Total_Images := Natural(Image_Annotation_Vectors.Length(Dataset.Annotations));
      Stats.Class_Counts.Reserve(10);  -- Pre-allocate for expected number of classes
      
      -- Process each image annotation
      for I in 1..Image_Annotation_Vectors.Length(Dataset.Annotations) loop
         declare
            Annotation : constant Image_Annotation := 
               Image_Annotation_Vectors.Element(Dataset.Annotations, I);
         begin
            -- Count objects in this image
            Obj_Count := Natural(Bounding_Box_Vectors.Length(Annotation.Boxes));
            Stats.Total_Objects := Stats.Total_Objects + Obj_Count;
            
            -- Track empty images
            if Obj_Count = 0 then
               Stats.Empty_Images := Stats.Empty_Images + 1;
            end if;
            
            -- Update min/max object counts
            if Obj_Count < Stats.Min_Objects then
               Stats.Min_Objects := Obj_Count;
            end if;
            
            if Obj_Count > Stats.Max_Objects then
               Stats.Max_Objects := Obj_Count;
            end if;
            
            -- Process each bounding box
            for J in 1..Bounding_Box_Vectors.Length(Annotation.Boxes) loop
               declare
                  Box : constant Bounding_Box := 
                     Bounding_Box_Vectors.Element(Annotation.Boxes, J);
               begin
                  -- Update class counts
                  Update_Class_Count(Stats.Class_Counts, Box.Class_ID);
                  
                  -- Update width statistics
                  if Box.Width < Stats.Min_Box_Width then
                     Stats.Min_Box_Width := Box.Width;
                  end if;
                  
                  if Box.Width > Stats.Max_Box_Width then
                     Stats.Max_Box_Width := Box.Width;
                  end if;
                  
                  Total_Width_Sum := Total_Width_Sum + Box.Width;
                  
                  -- Update height statistics
                  if Box.Height < Stats.Min_Box_Height then
                     Stats.Min_Box_Height := Box.Height;
                  end if;
                  
                  if Box.Height > Stats.Max_Box_Height then
                     Stats.Max_Box_Height := Box.Height;
                  end if;
                  
                  Total_Height_Sum := Total_Height_Sum + Box.Height;
                  
                  -- Update area statistics
                  Box_Area := Box.Width * Box.Height;
                  
                  if Box_Area < Stats.Smallest_Box_Area then
                     Stats.Smallest_Box_Area := Box_Area;
                  end if;
                  
                  if Box_Area > Stats.Largest_Box_Area then
                     Stats.Largest_Box_Area := Box_Area;
                  end if;
                  
                  Total_Area_Sum := Total_Area_Sum + Box_Area;
               end;
            end loop;
         end;
      end loop;
      
      -- Compute average values if we have objects
      if Stats.Total_Objects > 0 then
         Stats.Avg_Objects := Float(Stats.Total_Objects) / Float(Stats.Total_Images);
         Stats.Avg_Box_Width := Total_Width_Sum / Float(Stats.Total_Objects);
         Stats.Avg_Box_Height := Total_Height_Sum / Float(Stats.Total_Objects);
         Stats.Avg_Box_Area := Total_Area_Sum / Float(Stats.Total_Objects);
      end if;
      
      -- Handle case when there are no objects
      if Stats.Min_Objects = Natural'Last then
         Stats.Min_Objects := 0;
      end if;
      
      if Stats.Min_Box_Width = Float'Last then
         Stats.Min_Box_Width := 0.0;
      end if;
      
      if Stats.Min_Box_Height = Float'Last then
         Stats.Min_Box_Height := 0.0;
      end if;
      
      if Stats.Smallest_Box_Area = Float'Last then
         Stats.Smallest_Box_Area := 0.0;
      end if;
      
      return Stats;
   end Compute_Dataset_Statistics;
   
   -- Print dataset statistics in human-readable format
   procedure Print_Dataset_Statistics(Stats : Dataset_Statistics) is
   begin
      Put_Line("Dataset Statistics");
      Put_Line("=================");
      Put_Line("Total Images: " & Natural'Image(Stats.Total_Images));
      Put_Line("Total Objects: " & Natural'Image(Stats.Total_Objects));
      Put_Line("Empty Images: " & Natural'Image(Stats.Empty_Images) & 
               " (" & Integer'Image(Integer(Float(Stats.Empty_Images) / 
               Float(Stats.Total_Images) * 100.0)) & "%)");
      Put_Line("");
      
      Put_Line("Objects per Image:");
      Put_Line("  Min: " & Natural'Image(Stats.Min_Objects));
      Put_Line("  Max: " & Natural'Image(Stats.Max_Objects));
      Put_Line("  Avg: " & Float'Image(Stats.Avg_Objects));
      Put_Line("");
      
      Put_Line("Bounding Box Widths (normalized):");
      Put_Line("  Min: " & Float'Image(Stats.Min_Box_Width));
      Put_Line("  Max: " & Float'Image(Stats.Max_Box_Width));
      Put_Line("  Avg: " & Float'Image(Stats.Avg_Box_Width));
      Put_Line("");
      
      Put_Line("Bounding Box Heights (normalized):");
      Put_Line("  Min: " & Float'Image(Stats.Min_Box_Height));
      Put_Line("  Max: " & Float'Image(Stats.Max_Box_Height));
      Put_Line("  Avg: " & Float'Image(Stats.Avg_Box_Height));
      Put_Line("");
      
      Put_Line("Bounding Box Areas (normalized):");
      Put_Line("  Min: " & Float'Image(Stats.Smallest_Box_Area));
      Put_Line("  Max: " & Float'Image(Stats.Largest_Box_Area));
      Put_Line("  Avg: " & Float'Image(Stats.Avg_Box_Area));
      Put_Line("");
      
      Put_Line("Class Distribution:");
      for I in 1..Class_Count_Vectors.Length(Stats.Class_Counts) loop
         declare
            Class_Count : constant Class_Count_Record := 
               Class_Count_Vectors.Element(Stats.Class_Counts, I);
            Percentage : constant Float := 
               Float(Class_Count.Object_Count) / Float(Stats.Total_Objects) * 100.0;
         begin
            Put_Line("  Class" & Integer'Image(Class_Count.Class_ID) & 
                    ":" & Natural'Image(Class_Count.Object_Count) & 
                    " objects (" & Integer'Image(Integer(Percentage)) & "%)");
         end;
      end loop;
   end Print_Dataset_Statistics;
   
   -- Calculate variance of box dimensions
   function Calculate_Dimension_Variance(Dataset : Image_Dataset) return Float is
      Mean_Area : Float := 0.0;
      Variance : Float := 0.0;
      Total_Boxes : Natural := 0;
      Box_Areas : array(1..10000) of Float;  -- Assume a reasonable upper limit
   begin
      -- First pass: collect areas and calculate mean
      for I in 1..Image_Annotation_Vectors.Length(Dataset.Annotations) loop
         declare
            Annotation : constant Image_Annotation := 
               Image_Annotation_Vectors.Element(Dataset.Annotations, I);
         begin
            for J in 1..Bounding_Box_Vectors.Length(Annotation.Boxes) loop
               declare
                  Box : constant Bounding_Box := 
                     Bounding_Box_Vectors.Element(Annotation.Boxes, J);
                  Area : constant Float := Box.Width * Box.Height;
               begin
                  Total_Boxes := Total_Boxes + 1;
                  Box_Areas(Total_Boxes) := Area;
                  Mean_Area := Mean_Area + Area;
               end;
            end loop;
         end;
      end loop;
      
      -- Calculate mean
      if Total_Boxes > 0 then
         Mean_Area := Mean_Area / Float(Total_Boxes);
      else
         return 0.0;
      end if;
      
      -- Second pass: calculate variance
      for I in 1..Total_Boxes loop
         Variance := Variance + (Box_Areas(I) - Mean_Area) ** 2;
      end loop;
      
      if Total_Boxes > 1 then
         Variance := Variance / Float(Total_Boxes - 1);
      else
         Variance := 0.0;
      end if;
      
      return Variance;
   end Calculate_Dimension_Variance;
   
   -- Find outlier boxes in the dataset (unusually large or small)
   function Find_Outliers(
      Dataset      : Image_Dataset;
      Threshold_SD : Float := 2.0) return Image_Annotation_Vectors.Vector is
      
      Result : Image_Annotation_Vectors.Vector;
      Mean_Area : Float := 0.0;
      Total_Boxes : Natural := 0;
      Box_Areas : array(1..10000) of Float;  -- Assume a reasonable upper limit
      All_Boxes : array(1..10000) of Bounding_Box;
      All_Indices : array(1..10000, 1..2) of Positive;  -- Store image and box indices
      Variance : Float;
      Std_Dev : Float;
      Threshold : Float;
   begin
      -- First pass: collect areas and calculate mean
      for I in 1..Image_Annotation_Vectors.Length(Dataset.Annotations) loop
         declare
            Annotation : constant Image_Annotation := 
               Image_Annotation_Vectors.Element(Dataset.Annotations, I);
         begin
            for J in 1..Bounding_Box_Vectors.Length(Annotation.Boxes) loop
               declare
                  Box : constant Bounding_Box := 
                     Bounding_Box_Vectors.Element(Annotation.Boxes, J);
                  Area : constant Float := Box.Width * Box.Height;
               begin
                  Total_Boxes := Total_Boxes + 1;
                  Box_Areas(Total_Boxes) := Area;
                  All_Boxes(Total_Boxes) := Box;
                  All_Indices(Total_Boxes, 1) := I;
                  All_Indices(Total_Boxes, 2) := J;
                  Mean_Area := Mean_Area + Area;
               end;
            end loop;
         end;
      end loop;
      
      -- If no boxes, return empty result
      if Total_Boxes = 0 then
         return Result;
      end if;
      
      -- Calculate mean
      Mean_Area := Mean_Area / Float(Total_Boxes);
      
      -- Second pass: calculate variance
      Variance := 0.0;
      for I in 1..Total_Boxes loop
         Variance := Variance + (Box_Areas(I) - Mean_Area) ** 2;
      end loop;
      
      if Total_Boxes > 1 then
         Variance := Variance / Float(Total_Boxes - 1);
      else
         Variance := 0.0;
      end if;
      
      -- Calculate standard deviation
      Std_Dev := Sqrt(Variance);
      
      -- Set threshold for outliers
      Threshold := Threshold_SD * Std_Dev;
      
      -- Find outliers
      for I in 1..Total_Boxes loop
         if abs(Box_Areas(I) - Mean_Area) > Threshold then
            -- This is an outlier
            declare
               Image_Index : constant Positive := All_Indices(I, 1);
               Orig_Annotation : constant Image_Annotation := 
                  Image_Annotation_Vectors.Element(Dataset.Annotations, Image_Index);
               Outlier_Annotation : Image_Annotation;
            begin
               -- Create a new annotation record with only the outlier box
               Outlier_Annotation.Image_Path := Orig_Annotation.Image_Path;
               Outlier_Annotation.Image_Width := Orig_Annotation.Image_Width;
               Outlier_Annotation.Image_Height := Orig_Annotation.Image_Height;
               
               -- Add the outlier box
               Bounding_Box_Vectors.Append(Outlier_Annotation.Boxes, All_Boxes(I));
               
               -- Add to results
               Image_Annotation_Vectors.Append(Result, Outlier_Annotation);
            end;
         end if;
      end loop;
      
      return Result;
   end Find_Outliers;
   
   -- Find overlapping boxes in the same image that might indicate duplicates
   function Find_Overlapping_Boxes(
      Dataset      : Image_Dataset;
      IoU_Threshold: Float := 0.7) return Image_Annotation_Vectors.Vector is
      
      Result : Image_Annotation_Vectors.Vector;
   begin
      -- Check each image
      for I in 1..Image_Annotation_Vectors.Length(Dataset.Annotations) loop
         declare
            Annotation : constant Image_Annotation := 
               Image_Annotation_Vectors.Element(Dataset.Annotations, I);
            Has_Overlaps : Boolean := False;
            Overlap_Annotation : Image_Annotation;
         begin
            -- Initialize a new annotation record for overlaps in this image
            Overlap_Annotation.Image_Path := Annotation.Image_Path;
            Overlap_Annotation.Image_Width := Annotation.Image_Width;
            Overlap_Annotation.Image_Height := Annotation.Image_Height;
            
            -- Compare each pair of boxes
            for J in 1..Bounding_Box_Vectors.Length(Annotation.Boxes) loop
               declare
                  Box1 : constant Bounding_Box := 
                     Bounding_Box_Vectors.Element(Annotation.Boxes, J);
               begin
                  for K in J+1..Bounding_Box_Vectors.Length(Annotation.Boxes) loop
                     declare
                        Box2 : constant Bounding_Box := 
                           Bounding_Box_Vectors.Element(Annotation.Boxes, K);
                        IoU : constant Float := Intersection_Over_Union(Box1, Box2);
                     begin
                        -- Check if IoU exceeds threshold
                        if IoU > IoU_Threshold then
                           -- Add both boxes to the overlap annotation
                           if not Has_Overlaps then
                              Bounding_Box_Vectors.Append(Overlap_Annotation.Boxes, Box1);
                              Bounding_Box_Vectors.Append(Overlap_Annotation.Boxes, Box2);
                              Has_Overlaps := True;
                           else
                              -- Check if Box2 is already in the list
                              declare
                                 Is_Box2_Present : Boolean := False;
                              begin
                                 for L in 1..Bounding_Box_Vectors.Length(Overlap_Annotation.Boxes) loop
                                    if Bounding_Box_Vectors.Element(Overlap_Annotation.Boxes, L) = Box2 then
                                       Is_Box2_Present := True;
                                       exit;
                                    end if;
                                 end loop;
                                 
                                 if not Is_Box2_Present then
                                    Bounding_Box_Vectors.Append(Overlap_Annotation.Boxes, Box2);
                                 end if;
                              end;
                           end if;
                        end if;
                     end;
                  end loop;
               end;
            end loop;
            
            -- Add to results if we found overlaps
            if Has_Overlaps then
               Image_Annotation_Vectors.Append(Result, Overlap_Annotation);
            end if;
         end;
      end loop;
      
      return Result;
   end Find_Overlapping_Boxes;
   
   -- Format a single bounding box as a YOLO annotation line
   function Format_Bounding_Box(BB : Bounding_Box) return String is
   begin
      return Trim(Integer'Image(BB.Class_ID), Both) & " " &
             Trim(Float'Image(BB.X_Center), Both) & " " &
             Trim(Float'Image(BB.Y_Center), Both) & " " &
             Trim(Float'Image(BB.Width), Both) & " " &
             Trim(Float'Image(BB.Height), Both);
   end Format_Bounding_Box;
   
   -- Save annotation to a YOLO format file
   procedure Save_Annotation(
      Annotation : Image_Annotation;
      File_Path  : String) is
      
      File : File_Type;
   begin
      -- Create directory if it doesn't exist
      declare
         Dir_Path : constant String := Containing_Directory(File_Path);
      begin
         if not Exists(Dir_Path) then
            Create_Path(Dir_Path);
         end if;
      end;
      
      -- Open file for writing
      Create(File, Out_File, File_Path);
      
      -- Write each bounding box as a line
      for I in 1..Bounding_Box_Vectors.Length(Annotation.Boxes) loop
         declare
            Box : constant Bounding_Box := 
               Bounding_Box_Vectors.Element(Annotation.Boxes, I);
            Line : constant String := Format_Bounding_Box(Box);
         begin
            Put_Line(File, Line);
         end;
      end loop;
      
      Close(File);
      
   exception
      when E : others =>
         if Is_Open(File) then
            Close(File);
         end if;
         raise IO_Error with "Error saving annotation file: " & File_Path & 
                           " - " & Exception_Message(E);
   end Save_Annotation;
   
   -- Export a complete dataset
   procedure Export_Dataset(
      Dataset   : Image_Dataset;
      Dir_Path  : String) is
   begin
      -- Create directory if it doesn't exist
      if not Exists(Dir_Path) then
         Create_Path(Dir_Path);
      end if;
      
      -- Save each annotation
      for I in 1..Image_Annotation_Vectors.Length(Dataset.Annotations) loop
         declare
            Annotation : constant Image_Annotation := 
               Image_Annotation_Vectors.Element(Dataset.Annotations, I);
            File_Name : constant String := 
               Simple_Name(To_String(Annotation.Image_Path));
            Name_End : constant Natural := 
               Index(File_Name, ".", Backward);
            Base_Name : constant String := 
               (if Name_End > 0 then
                  File_Name(File_Name'First..Name_End-1)
                else
                  File_Name);
            Out_Path : constant String := 
               Compose(Dir_Path, Base_Name & ".txt");
         begin
            Save_Annotation(Annotation, Out_Path);
         end;
      end loop;
      
   exception
      when E : others =>
         raise IO_Error with "Error exporting dataset: " & Dir_Path & 
                            " - " & Exception_Message(E);
   end Export_Dataset;

   -- Export annotations to CSV (image_path,class_id,x_center,y_center,width,height)
   procedure Export_Annotations_CSV(
      Dataset   : Image_Dataset;
      File_Path : String) is
      File : File_Type;
      Line : String;
   begin
      -- Create output directory if needed
      declare
         Dir : constant String := Containing_Directory(File_Path);
      begin
         if not Exists(Dir) then
            Create_Path(Dir);
         end if;
      end;

      Create(File, Out_File, File_Path);
      -- Write header
      Put_Line(File, "image_path,class_id,x_center,y_center,width,height");
      -- Write each annotation entry
      for I in 1..Image_Annotation_Vectors.Length(Dataset.Annotations) loop
         declare
            Ann : constant Image_Annotation := 
               Image_Annotation_Vectors.Element(Dataset.Annotations, I);
         begin
            for J in 1..Bounding_Box_Vectors.Length(Ann.Boxes) loop
               declare
                  BB    : constant Bounding_Box := 
                     Bounding_Box_Vectors.Element(Ann.Boxes, J);
                  Path  : constant String := To_String(Ann.Image_Path);
               begin
                  Line := Path & "," & Integer'Image(BB.Class_ID) & "," &
                          Float'Image(BB.X_Center) & "," &
                          Float'Image(BB.Y_Center) & "," &
                          Float'Image(BB.Width) & "," &
                          Float'Image(BB.Height);
                  Put_Line(File, Line);
               end;
            end loop;
         end;
      end loop;
      Close(File);
   end Export_Annotations_CSV;

   -- Export dataset statistics to CSV format (field,value)
   procedure Export_Statistics_CSV(
      Stats     : Dataset_Statistics;
      File_Path : String) is
      File : File_Type;
      Line : String;
   begin
      -- Create output directory if needed
      declare
         Dir : constant String := Containing_Directory(File_Path);
      begin
         if not Exists(Dir) then
            Create_Path(Dir);
         end if;
      end;

      Create(File, Out_File, File_Path);
      -- Write header
      Put_Line(File, "field,value");
      -- Basic stats
      Put_Line(File, "Total_Images," & Natural'Image(Stats.Total_Images));
      Put_Line(File, "Total_Objects," & Natural'Image(Stats.Total_Objects));
      Put_Line(File, "Empty_Images," & Natural'Image(Stats.Empty_Images));
      Put_Line(File, "Min_Objects," & Natural'Image(Stats.Min_Objects));
      Put_Line(File, "Max_Objects," & Natural'Image(Stats.Max_Objects));
      Put_Line(File, "Avg_Objects," & Float'Image(Stats.Avg_Objects));
      -- Box dimension stats
      Put_Line(File, "Min_Box_Width," & Float'Image(Stats.Min_Box_Width));
      Put_Line(File, "Max_Box_Width," & Float'Image(Stats.Max_Box_Width));
      Put_Line(File, "Avg_Box_Width," & Float'Image(Stats.Avg_Box_Width));
      Put_Line(File, "Min_Box_Height," & Float'Image(Stats.Min_Box_Height));
      Put_Line(File, "Max_Box_Height," & Float'Image(Stats.Max_Box_Height));
      Put_Line(File, "Avg_Box_Height," & Float'Image(Stats.Avg_Box_Height));
      Put_Line(File, "Min_Box_Area," & Float'Image(Stats.Smallest_Box_Area));
      Put_Line(File, "Max_Box_Area," & Float'Image(Stats.Largest_Box_Area));
      Put_Line(File, "Avg_Box_Area," & Float'Image(Stats.Avg_Box_Area));
      -- Class distribution
      for K in 1..Class_Count_Vectors.Length(Stats.Class_Counts) loop
         declare
            CRec : constant Class_Count_Record := 
               Class_Count_Vectors.Element(Stats.Class_Counts, K);
         begin
            Line := "Class_" & Integer'Image(CRec.Class_ID) & "," & Natural'Image(CRec.Object_Count);
            Put_Line(File, Line);
         end;
      end loop;
      Close(File);
   end Export_Statistics_CSV;

   -- Compare two datasets and compute precision, recall, F1
   function Evaluate_Datasets(
      Ground_Truth  : Image_Dataset;
      Predictions   : Image_Dataset;
      IoU_Threshold : Float := 0.5) return Evaluation_Result is
      TP     : Natural := 0;
      Total_GT       : Natural := 0;
      Total_Pred     : Natural := 0;
      Matched        : Boolean;
      -- Tracking matched predictions per image
      Pred_Matches   : array(1..10000, 1..10000) of Boolean;
      Eval           : Evaluation_Result;
   begin
      -- Count GT and initialize tracking
      for I in 1..Image_Annotation_Vectors.Length(Ground_Truth.Annotations) loop
         declare
            Ann_GT : constant Image_Annotation := 
               Image_Annotation_Vectors.Element(Ground_Truth.Annotations, I);
         begin
            Total_GT := Total_GT + Bounding_Box_Vectors.Length(Ann_GT.Boxes);
         end;
      end loop;
      -- Count predictions
      for I in 1..Image_Annotation_Vectors.Length(Predictions.Annotations) loop
         declare
            Ann_P : constant Image_Annotation := 
               Image_Annotation_Vectors.Element(Predictions.Annotations, I);
         begin
            Total_Pred := Total_Pred + Bounding_Box_Vectors.Length(Ann_P.Boxes);
         end;
      end loop;
      -- For each GT image, find matching pred by path
      for I in 1..Image_Annotation_Vectors.Length(Ground_Truth.Annotations) loop
         declare
            Ann_GT : constant Image_Annotation := 
               Image_Annotation_Vectors.Element(Ground_Truth.Annotations, I);
         begin
            -- find same image in predictions
            for J in 1..Image_Annotation_Vectors.Length(Predictions.Annotations) loop
               declare
                  Ann_P : in out Image_Annotation := 
                     Image_Annotation_Vectors.Element(Predictions.Annotations, J);
               begin
                  if Ann_GT.Image_Path = Ann_P.Image_Path then
                     -- compare each GT box against each pred box
                     for G in 1..Bounding_Box_Vectors.Length(Ann_GT.Boxes) loop
                        declare
                           BB_GT : constant Bounding_Box := 
                              Bounding_Box_Vectors.Element(Ann_GT.Boxes, G);
                        begin
                           for P in 1..Bounding_Box_Vectors.Length(Ann_P.Boxes) loop
                              if not Pred_Matches(J, P) then
                                 declare
                                    BB_P : constant Bounding_Box := 
                                       Bounding_Box_Vectors.Element(Ann_P.Boxes, P);
                                 begin
                                    if Intersection_Over_Union(BB_GT, BB_P) >= IoU_Threshold then
                                       TP := TP + 1;
                                       Pred_Matches(J, P) := True;
                                       exit;
                                    end if;
                                 end;
                              end if;
                           end loop;
                        end;
                     end loop;
                  end if;
               end;
            end loop;
         end;
      end loop;
      -- Compute metrics
      if Total_Pred > 0 then
         Eval.Precision := Float(TP) / Float(Total_Pred);
      else
         Eval.Precision := 0.0;
      end if;
      if Total_GT > 0 then
         Eval.Recall := Float(TP) / Float(Total_GT);
      else
         Eval.Recall := 0.0;
      end if;
      if Eval.Precision + Eval.Recall > 0.0 then
         Eval.F1_Score := 2.0 * Eval.Precision * Eval.Recall / 
                           (Eval.Precision + Eval.Recall);
      else
         Eval.F1_Score := 0.0;
      end if;
      return Eval;
   end Evaluate_Datasets;

   -- Export evaluation result to CSV (precision,recall,f1)
   procedure Export_Evaluation_CSV(
      Eval      : Evaluation_Result;
      File_Path : String) is
      File : File_Type;
   begin
      declare
         Dir : constant String := Containing_Directory(File_Path);
      begin
         if not Exists(Dir) then
            Create_Path(Dir);
         end if;
      end;
      Create(File, Out_File, File_Path);
      Put_Line(File, "precision,recall,f1_score");
      Put_Line(File, Float'Image(Eval.Precision) & "," & 
                       Float'Image(Eval.Recall) & "," & 
                       Float'Image(Eval.F1_Score));
      Close(File);
   end Export_Evaluation_CSV;

   -- Export evaluation result to JSON
   procedure Export_Evaluation_JSON(
      Eval      : Evaluation_Result;
      File_Path : String) is
      File : File_Type;
      Json : String;
   begin
      declare
         Dir : constant String := Containing_Directory(File_Path);
      begin
         if not Exists(Dir) then
            Create_Path(Dir);
         end if;
      end;
      Json := "{" & 
              """precision"": " & Float'Image(Eval.Precision) & ", " & 
              """recall"": " & Float'Image(Eval.Recall) & ", " & 
              """f1_score"": " & Float'Image(Eval.F1_Score) & 
              "}";
      Create(File, Out_File, File_Path);
      Put_Line(File, Json);
      Close(File);
   end Export_Evaluation_JSON;

   -- Parse dataset from COCO JSON annotation file
   function Parse_Coco_Annotation_File(
      Json_File_Path : String;
      Image_Dir      : String := "";
      Default_Width  : Positive := 1;
      Default_Height : Positive := 1) return Image_Dataset is
      package Int_Map is new Ada.Containers.Hashed_Maps(
         Key_Type        => Integer,
         Element_Type    => Positive,
         Hash            => Hash_Functions.Hash,
         Equivalent_Keys => "=");
      Img_Map : Int_Map.Map := Int_Map.Empty_Map;
      Doc     : Json_Value;
      Root    : Json_Object;
      Images  : Json_Array;
      Anns    : Json_Array;
      Result  : Image_Dataset;
      Idx     : Positive;
      Found   : Boolean;
   begin
      -- Read entire JSON document
      Doc := Json_Value'Parse(Json_Text(Json_File_Path));
      Root := Doc.As_Object;
      -- Parse images array
      Images := Root.Get_Array("images");
      Result.Annotations.Reserve(Images.Length);
      for I in Images.Iterate loop
         declare
            Obj       : Json_Object := I.Element.As_Object;
            Img_ID    : Integer     := Obj.Get_Integer("id");
            FName     : String      := Obj.Get_String("file_name");
            W         : Positive    := Positive(Obj.Get_Integer("width"));
            H         : Positive    := Positive(Obj.Get_Integer("height"));
            ANN       : Image_Annotation;
         begin
            ANN.Image_Path   := To_Unbounded_String(
                                  if Image_Dir /= "" then
                                     Compose(Image_Dir, FName)
                                  else
                                     FName);
            ANN.Image_Width  := (if W > 0 then W else Default_Width);
            ANN.Image_Height := (if H > 0 then H else Default_Height);
            ANN.Boxes.Clear;
            Image_Annotation_Vectors.Append(Result.Annotations, ANN);
            Idx := Result.Annotations.Length;
            Int_Map.Insert(Img_Map, Key => Img_ID, New_Item => Idx);
         end;
      end loop;
      -- Parse annotations array
      Anns := Root.Get_Array("annotations");
      for J in Anns.Iterate loop
         declare
            Obj       : Json_Object := J.Element.As_Object;
            Img_ID    : Integer     := Obj.Get_Integer("image_id");
            Cat_ID    : Integer     := Obj.Get_Integer("category_id");
            BBox      : Json_Array  := Obj.Get_Array("bbox");
            X         : Float       := BBox.Element(1).As_Number;
            Y         : Float       := BBox.Element(2).As_Number;
            BW        : Float       := BBox.Element(3).As_Number;
            BH        : Float       := BBox.Element(4).As_Number;
            Index     : Positive;
            ANN       : Image_Annotation;
            BB        : Bounding_Box;
         begin
            -- Find corresponding image annotation
            if Int_Map.Contains(Img_Map, Img_ID) then
               Index := Int_Map.Element(Img_Map, Img_ID);
               ANN   := Image_Annotation_Vectors.Element(Result.Annotations, Index);
               -- Convert pixel bbox to normalized
               BB.Class_ID := Cat_ID;
               BB.X_Center := (X + BW / 2.0) / Float(ANN.Image_Width);
               BB.Y_Center := (Y + BH / 2.0) / Float(ANN.Image_Height);
               BB.Width    := BW / Float(ANN.Image_Width);
               BB.Height   := BH / Float(ANN.Image_Height);
               if Is_Valid(BB) then
                  Bounding_Box_Vectors.Append(ANN.Boxes, BB);
                  Image_Annotation_Vectors.Replace_Element(Result.Annotations,
                     Index, ANN);
               end if;
            end if;
         end;
      end loop;
      return Result;
   end Parse_Coco_Annotation_File;

   -- Export dataset to COCO JSON format
   procedure Export_Coco_Annotation_File(
      Dataset        : Image_Dataset;
      Json_File_Path : String) is
      File        : File_Type;
      Image_Count : Natural := Image_Annotation_Vectors.Length(Dataset.Annotations);
      Ann_ID      : Natural := 0;
   begin
      -- Create output directory if needed
      declare
         Dir : constant String := Containing_Directory(Json_File_Path);
      begin
         if not Exists(Dir) then
            Create_Path(Dir);
         end if;
      end;
      Create(File, Out_File, Json_File_Path);

      -- Begin JSON
      Put_Line(File, "{");
      -- Images array
      Put_Line(File, "  \"images\": [");
      for I in 1..Image_Count loop
         declare
            Ann       : constant Image_Annotation := 
               Image_Annotation_Vectors.Element(Dataset.Annotations, I);
            Idx       : constant Natural := I;
            FileName  : constant String := To_String(Ann.Image_Path);
         begin
            Put(File, "    {");
            Put(File, "\"id\": " & Natural'Image(Idx) & ", ");
            Put(File, "\"width\": " & Integer'Image(Ann.Image_Width) & ", ");
            Put(File, "\"height\": " & Integer'Image(Ann.Image_Height) & ", ");
            Put(File, "\"file_name\": \"" & FileName & "\"}");
            if I < Image_Count then
               Put_Line(File, ",");
            else
               Put_Line(File, "");
            end if;
         end;
      end loop;
      Put_Line(File, "  ],");

      -- Annotations array
      Put_Line(File, "  \"annotations\": [");
      for I in 1..Image_Count loop
         declare
            Ann       : constant Image_Annotation := 
               Image_Annotation_Vectors.Element(Dataset.Annotations, I);
         begin
            for J in 1..Bounding_Box_Vectors.Length(Ann.Boxes) loop
               declare
                  BB        : constant Bounding_Box := 
                     Bounding_Box_Vectors.Element(Ann.Boxes, J);
                  BBox_W    : Float := BB.Width * Float(Ann.Image_Width);
                  BBox_H    : Float := BB.Height * Float(Ann.Image_Height);
                  BBox_X    : Float := (BB.X_Center * Float(Ann.Image_Width)) - BBox_W / 2.0;
                  BBox_Y    : Float := (BB.Y_Center * Float(Ann.Image_Height)) - BBox_H / 2.0;
               begin
                  Ann_ID := Ann_ID + 1;
                  Put(File, "    { ");
                  Put(File, "\"id\": " & Natural'Image(Ann_ID) & ", ");
                  Put(File, "\"image_id\": " & Natural'Image(I) & ", ");
                  Put(File, "\"category_id\": " & Integer'Image(BB.Class_ID) & ", ");
                  Put(File, "\"bbox\": [" & Float'Image(BBox_X) & ", " & 
                       Float'Image(BBox_Y) & ", " & Float'Image(BBox_W) & ", " & 
                       Float'Image(BBox_H) & "] }");
                  -- comma if not last
                  if not (I = Image_Count and then J = Bounding_Box_Vectors.Length(Ann.Boxes)) then
                     Put_Line(File, ",");
                  else
                     Put_Line(File, "");
                  end if;
               end;
            end loop;
         end;
      end loop;
      Put_Line(File, "  ]");
      -- End JSON
      Put_Line(File, "}");
      Close(File);
   end Export_Coco_Annotation_File;

end Ada_Vision_Unit.Annotations;