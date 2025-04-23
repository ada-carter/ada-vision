package Ada_Vision_Unit.Bounding_Boxes is

   -- Record representing a bounding box in YOLO format (normalized coordinates)
   type Bounding_Box is record
      Class_ID  : Integer;
      X_Center  : Float;
      Y_Center  : Float;
      Width     : Float;
      Height    : Float;
   end record;

   -- Check if a bounding box's coordinates are valid (in range [0.0, 1.0])
   function Is_Valid(BB : Bounding_Box) return Boolean;
   pragma Inline(Is_Valid);

   -- Calculate the Intersection Over Union between two bounding boxes
   function Intersection_Over_Union(BB1, BB2 : Bounding_Box) return Float;
   pragma Inline(Intersection_Over_Union);

   -- Helper functions for IoU calculation
   function Calculate_Intersection(BB1, BB2 : Bounding_Box) return Float;
   pragma Inline(Calculate_Intersection);
   function Calculate_Union(BB1, BB2 : Bounding_Box) return Float;
   pragma Inline(Calculate_Union);

   -- Convert normalized coordinates to pixel coordinates
   function To_Pixel_Coordinates(
      BB            : Bounding_Box;
      Image_Width   : Positive;
      Image_Height  : Positive) return Bounding_Box;
   pragma Inline(To_Pixel_Coordinates);

   -- Convert pixel coordinates to normalized coordinates
   function To_Normalized_Coordinates(
      BB            : Bounding_Box;
      Image_Width   : Positive;
      Image_Height  : Positive) return Bounding_Box;
   pragma Inline(To_Normalized_Coordinates);

end Ada_Vision_Unit.Bounding_Boxes;