with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada_Vision_Unit.Bounding_Boxes; use Ada_Vision_Unit.Bounding_Boxes;

package Ada_Vision_Unit.Annotations is
   pragma Optimize (Speed, 3);

   -- Define a container for storing multiple bounding boxes
   package Bounding_Box_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Bounding_Box);

   -- Inline small helper routines
   function Split(Source : String; Delimiter : Character) return Bounding_Box_Vectors.Vector;
   pragma Inline(Split);

   procedure Update_Class_Count(
      Class_Counts : in out Class_Count_Vectors.Vector;
      Class_ID     : Integer);
   pragma Inline(Update_Class_Count);

   function Format_Bounding_Box(BB : Bounding_Box) return String;
   pragma Inline(Format_Bounding_Box);

   -- Image annotation record containing all bounding boxes for an image
   type Image_Annotation is record
      Image_Path   : Unbounded_String;
      Image_Width  : Positive := 1;  -- Default values
      Image_Height : Positive := 1;
      Boxes        : Bounding_Box_Vectors.Vector;
   end record;

   -- Define a container for storing multiple image annotations
   package Image_Annotation_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Image_Annotation);

   -- Image dataset containing annotations for multiple images
   type Image_Dataset is record
      Annotations  : Image_Annotation_Vectors.Vector;
   end record;

   -- Dataset statistics record
   type Class_Count_Record is record
      Class_ID      : Integer;
      Object_Count  : Natural := 0;
   end record;

   package Class_Count_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Class_Count_Record);

   -- Complete dataset statistics 
   type Dataset_Statistics is record
      Total_Images     : Natural := 0;
      Total_Objects    : Natural := 0;
      Empty_Images     : Natural := 0;
      Class_Counts     : Class_Count_Vectors.Vector;
      Min_Objects      : Natural := Natural'Last;
      Max_Objects      : Natural := 0;
      Avg_Objects      : Float := 0.0;
      Min_Box_Width    : Float := Float'Last;
      Max_Box_Width    : Float := 0.0;
      Avg_Box_Width    : Float := 0.0;
      Min_Box_Height   : Float := Float'Last;
      Max_Box_Height   : Float := 0.0;
      Avg_Box_Height   : Float := 0.0;
      Smallest_Box_Area: Float := Float'Last;
      Largest_Box_Area : Float := 0.0;
      Avg_Box_Area     : Float := 0.0;
   end record;

   -- Evaluation result for comparing ground truth vs predictions
   type Evaluation_Result is record
      Precision : Float;
      Recall    : Float;
      F1_Score  : Float;
   end record;

   -- Parse a single YOLO annotation file
   function Parse_Annotation_File(
      File_Path    : String;
      Image_Path   : String := "";
      Image_Width  : Positive := 1;
      Image_Height : Positive := 1) return Image_Annotation;

   -- Parse a directory of YOLO annotation files
   function Parse_Annotation_Directory(
      Dir_Path     : String;
      Image_Dir    : String := "";
      Default_Width  : Positive := 1;
      Default_Height : Positive := 1) return Image_Dataset;

   -- Parse dataset from COCO JSON annotation file
   function Parse_Coco_Annotation_File(
      Json_File_Path : String;
      Image_Dir      : String := "";
      Default_Width  : Positive := 1;
      Default_Height : Positive := 1) return Image_Dataset;
   
   -- Export dataset to COCO JSON format
   procedure Export_Coco_Annotation_File(
      Dataset        : Image_Dataset;
      Json_File_Path : String);

   -- Verify annotation integrity in a dataset
   function Verify_Dataset_Integrity(Dataset : Image_Dataset) return Boolean;

   -- Compute descriptive statistics for a dataset
   function Compute_Dataset_Statistics(Dataset : Image_Dataset) return Dataset_Statistics;

   -- Print dataset statistics in human-readable format
   procedure Print_Dataset_Statistics(Stats : Dataset_Statistics);

   -- Save annotation to a YOLO format file
   procedure Save_Annotation(
      Annotation : Image_Annotation;
      File_Path  : String);

   -- Export a complete dataset
   procedure Export_Dataset(
      Dataset   : Image_Dataset;
      Dir_Path  : String);

   -- Find outlier boxes in the dataset (unusually large or small)
   function Find_Outliers(
      Dataset      : Image_Dataset;
      Threshold_SD : Float := 2.0) return Image_Annotation_Vectors.Vector;

   -- Find overlapping boxes in the same image that might indicate duplicates
   function Find_Overlapping_Boxes(
      Dataset      : Image_Dataset;
      IoU_Threshold: Float := 0.7) return Image_Annotation_Vectors.Vector;

   -- Export annotations to CSV (image_path,class_id,x_center,y_center,width,height)
   procedure Export_Annotations_CSV(
      Dataset   : Image_Dataset;
      File_Path : String);

   -- Export dataset statistics to CSV format (field,value)
   procedure Export_Statistics_CSV(
      Stats     : Dataset_Statistics;
      File_Path : String);

   -- Compare two datasets and compute precision, recall, F1
   function Evaluate_Datasets(
      Ground_Truth  : Image_Dataset;
      Predictions   : Image_Dataset;
      IoU_Threshold : Float := 0.5) return Evaluation_Result;

   -- Export evaluation result to CSV (precision,recall,f1)
   procedure Export_Evaluation_CSV(
      Eval      : Evaluation_Result;
      File_Path : String);

   -- Export evaluation result to JSON
   procedure Export_Evaluation_JSON(
      Eval      : Evaluation_Result;
      File_Path : String);

end Ada_Vision_Unit.Annotations;