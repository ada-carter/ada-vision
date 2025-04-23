with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;

package body Ada_Vision_Unit.Bounding_Boxes is

   function Is_Valid(BB : Bounding_Box) return Boolean is
   begin
      -- Check that all normalized coordinates are in range [0.0, 1.0]
      return BB.X_Center >= 0.0 and BB.X_Center <= 1.0 and
             BB.Y_Center >= 0.0 and BB.Y_Center <= 1.0 and
             BB.Width >= 0.0 and BB.Width <= 1.0 and
             BB.Height >= 0.0 and BB.Height <= 1.0 and
             BB.Width > 0.0 and BB.Height > 0.0;  -- Ensure non-zero dimensions
   end Is_Valid;

   function Calculate_Intersection(BB1, BB2 : Bounding_Box) return Float is
      -- Calculate left, right, top, bottom coordinates for both boxes
      BB1_Left   : constant Float := BB1.X_Center - BB1.Width / 2.0;
      BB1_Right  : constant Float := BB1.X_Center + BB1.Width / 2.0;
      BB1_Top    : constant Float := BB1.Y_Center - BB1.Height / 2.0;
      BB1_Bottom : constant Float := BB1.Y_Center + BB1.Height / 2.0;
      
      BB2_Left   : constant Float := BB2.X_Center - BB2.Width / 2.0;
      BB2_Right  : constant Float := BB2.X_Center + BB2.Width / 2.0;
      BB2_Top    : constant Float := BB2.Y_Center - BB2.Height / 2.0;
      BB2_Bottom : constant Float := BB2.Y_Center + BB2.Height / 2.0;
      
      -- Find intersection rectangle coordinates
      Intersect_Left   : constant Float := Float'Max(BB1_Left, BB2_Left);
      Intersect_Right  : constant Float := Float'Min(BB1_Right, BB2_Right);
      Intersect_Top    : constant Float := Float'Max(BB1_Top, BB2_Top);
      Intersect_Bottom : constant Float := Float'Min(BB1_Bottom, BB2_Bottom);
      
      Intersect_Width  : Float;
      Intersect_Height : Float;
      Intersection_Area : Float;
   begin
      -- Check if there is any intersection
      if Intersect_Right <= Intersect_Left or Intersect_Bottom <= Intersect_Top then
         return 0.0;  -- No intersection
      end if;
      
      -- Calculate intersection dimensions and area
      Intersect_Width := Intersect_Right - Intersect_Left;
      Intersect_Height := Intersect_Bottom - Intersect_Top;
      Intersection_Area := Intersect_Width * Intersect_Height;
      
      return Intersection_Area;
   end Calculate_Intersection;
   
   function Calculate_Union(BB1, BB2 : Bounding_Box) return Float is
      -- Calculate areas of individual boxes
      Area_BB1 : constant Float := BB1.Width * BB1.Height;
      Area_BB2 : constant Float := BB2.Width * BB2.Height;
      
      -- Calculate intersection area
      Intersection_Area : constant Float := Calculate_Intersection(BB1, BB2);
      
      -- Union = Sum of areas - Intersection
      Union_Area : constant Float := Area_BB1 + Area_BB2 - Intersection_Area;
   begin
      return Union_Area;
   end Calculate_Union;
   
   function Intersection_Over_Union(BB1, BB2 : Bounding_Box) return Float is
      Intersection_Area : constant Float := Calculate_Intersection(BB1, BB2);
      Union_Area : constant Float := Calculate_Union(BB1, BB2);
   begin
      if Union_Area = 0.0 then
         return 0.0;  -- Avoid division by zero
      else
         return Intersection_Area / Union_Area;
      end if;
   end Intersection_Over_Union;
   
   function To_Pixel_Coordinates(
      BB            : Bounding_Box;
      Image_Width   : Positive;
      Image_Height  : Positive) return Bounding_Box is
      
      Result : Bounding_Box := BB;
   begin
      -- Convert normalized coordinates to pixel coordinates
      Result.X_Center := BB.X_Center * Float(Image_Width);
      Result.Y_Center := BB.Y_Center * Float(Image_Height);
      Result.Width    := BB.Width * Float(Image_Width);
      Result.Height   := BB.Height * Float(Image_Height);
      
      return Result;
   end To_Pixel_Coordinates;
   
   function To_Normalized_Coordinates(
      BB            : Bounding_Box;
      Image_Width   : Positive;
      Image_Height  : Positive) return Bounding_Box is
      
      Result : Bounding_Box := BB;
   begin
      -- Convert pixel coordinates to normalized coordinates
      Result.X_Center := BB.X_Center / Float(Image_Width);
      Result.Y_Center := BB.Y_Center / Float(Image_Height);
      Result.Width    := BB.Width / Float(Image_Width);
      Result.Height   := BB.Height / Float(Image_Height);
      
      return Result;
   end To_Normalized_Coordinates;

end Ada_Vision_Unit.Bounding_Boxes;