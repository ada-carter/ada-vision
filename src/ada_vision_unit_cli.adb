with Ada.Text_IO;        use Ada.Text_IO;
with Ada.Command_Line;     use Ada.Command_Line;
with Ada.Directories;      use Ada.Directories;
with Ada_Vision_Unit.Annotations; use Ada_Vision_Unit.Annotations;

procedure AdaVisionUnit_CLI is
   Cmd   : String;
   Args  : Natural := Argument_Count;
   Data  : Image_Dataset;
   Stats : Dataset_Statistics;
   Out   : Boolean;
begin
   if Args < 2 then
      Put_Line("Usage: AdaVisionUnit_CLI <command> <dataset> [options]");
      Put_Line("Commands:");
      Put_Line("  stats <dir>");
      Put_Line("  integrity <dir>");
      Put_Line("  export-annotations-csv <dir> <out.csv>");
      Put_Line("  export-stats-csv       <dir> <out.csv>");
      Put_Line("  outliers <dir>");
      Put_Line("  overlaps <dir>");
      Put_Line("  evaluate <gt_dir> <pred_dir> [out.csv] [out.json]");
      return;
   end if;

   Cmd := Argument(1);
   Data := Parse_Annotation_Directory(Argument(2));

   if Cmd = "stats" then
      Stats := Compute_Dataset_Statistics(Data);
      Print_Dataset_Statistics(Stats);

   elsif Cmd = "integrity" then
      Out := Verify_Dataset_Integrity(Data);
      Put_Line("Integrity Check: " & (if Out then "PASS" else "FAIL"));

   elsif Cmd = "export-annotations-csv" and Args = 3 then
      Export_Annotations_CSV(Data, Argument(3));
      Put_Line("Annotations CSV written to " & Argument(3));

   elsif Cmd = "export-stats-csv" and Args = 3 then
      Stats := Compute_Dataset_Statistics(Data);
      Export_Statistics_CSV(Stats, Argument(3));
      Put_Line("Statistics CSV written to " & Argument(3));

   elsif Cmd = "outliers" then
      declare
         Outliers : Image_Annotation_Vectors.Vector := Find_Outliers(Data);
      begin
         Put_Line("Outliers Found: " & Natural'Image(Outliers.Length));
      end;

   elsif Cmd = "overlaps" then
      declare
         Overlaps : Image_Annotation_Vectors.Vector := Find_Overlapping_Boxes(Data);
      begin
         Put_Line("Overlapping sets: " & Natural'Image(Overlaps.Length));
      end;

   elsif Cmd = "evaluate" and then Args >= 3 then
      declare
         GT_Dir      : constant String := Argument(2);
         Pred_Dir    : constant String := Argument(3);
         Eval_Result : Evaluation_Result;
      begin
         Eval_Result := Evaluate_Datasets(
            Parse_Annotation_Directory(GT_Dir),
            Parse_Annotation_Directory(Pred_Dir)
         );
         Put_Line("Evaluation Results:");
         Put_Line("  Precision: " & Float'Image(Eval_Result.Precision));
         Put_Line("  Recall:    " & Float'Image(Eval_Result.Recall));
         Put_Line("  F1 Score:  " & Float'Image(Eval_Result.F1_Score));
         -- Optional CSV export
         if Args >= 4 then
            Export_Evaluation_CSV(Eval_Result, Argument(4));
            Put_Line("Evaluation CSV written to " & Argument(4));
         end if;
         -- Optional JSON export
         if Args >= 5 then
            Export_Evaluation_JSON(Eval_Result, Argument(5));
            Put_Line("Evaluation JSON written to " & Argument(5));
         end if;
      end;

   elsif Cmd = "export-eval-csv" and Args = 4 then
      declare
         GT : Image_Dataset := Parse_Annotation_Directory(Argument(2));
         PR : Image_Dataset := Parse_Annotation_Directory(Argument(3));
         ER : Evaluation_Result := Evaluate_Datasets(GT, PR);
      begin
         Export_Evaluation_CSV(ER, Argument(4));
         Put_Line("Evaluation CSV written to " & Argument(4));
      end;

   elsif Cmd = "export-eval-json" and Args = 4 then
      declare
         GT : Image_Dataset := Parse_Annotation_Directory(Argument(2));
         PR : Image_Dataset := Parse_Annotation_Directory(Argument(3));
         ER : Evaluation_Result := Evaluate_Datasets(GT, PR);
      begin
         Export_Evaluation_JSON(ER, Argument(4));
         Put_Line("Evaluation JSON written to " & Argument(4));
      end;

   else
      Put_Line("Unknown command or invalid arguments");
   end if;

exception
   when E : others =>
      Put_Line("Error: " & Exception_Message(E));
end AdaVisionUnit_CLI;