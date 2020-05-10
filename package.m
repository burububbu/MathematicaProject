(* Content-type: application/vnd.wolfram.mathematica *)

(*** Wolfram Notebook File ***)
(* http://www.wolfram.com/nb *)

(* CreatedBy='Mathematica 12.1' *)

(*CacheID: 234*)
(* Internal cache information:
NotebookFileLineBreakTest
NotebookFileLineBreakTest
NotebookDataPosition[       158,          7]
NotebookDataLength[      3354,         99]
NotebookOptionsPosition[      2963,         84]
NotebookOutlinePosition[      3401,        101]
CellTagsIndexPosition[      3358,         98]
WindowFrame->Normal*)

(* Beginning of Notebook Content *)
Notebook[{
Cell[BoxData[
 RowBox[{
  RowBox[{"(*", " ", 
   RowBox[{":", "Title", ":", " ", "AngleCircBackend"}], "*)"}], 
  "\[IndentingNewLine]", 
  RowBox[{"(*", " ", 
   RowBox[{":", "Context", ":", " ", "AngleCircBackend`"}], "*)"}], 
  "\[IndentingNewLine]", 
  RowBox[{"(*", " ", 
   RowBox[{":", "Author", ":", " ", 
    RowBox[{
     RowBox[{"Don", "'"}], "t", " ", 
     RowBox[{"Panic", "!"}]}]}], "*)"}], "\[IndentingNewLine]", 
  RowBox[{"(*", " ", 
   RowBox[{":", "Summary", ":", " ", 
    RowBox[{
    "package", " ", "for", " ", "angle", " ", "and", " ", "circumference", 
     " ", "exercises"}]}], " ", "*)"}], "\[IndentingNewLine]", 
  RowBox[{"(*", " ", 
   RowBox[{":", "Copyright", ":", " ", 
    RowBox[{
     RowBox[{"Don", "'"}], "t", " ", 
     RowBox[{"Panic", "!"}], " ", "2020"}]}], "*)"}], "\[IndentingNewLine]", 
  RowBox[{"(*", " ", 
   RowBox[{":", 
    RowBox[{"Package", " ", "Version"}], ":", "1"}], "*)"}], 
  "\[IndentingNewLine]", 
  RowBox[{"(*", " ", 
   RowBox[{":", 
    RowBox[{"Mathematica", " ", "Version"}], ":", "12"}], "*)"}], 
  "\[IndentingNewLine]", 
  RowBox[{"(*", " ", 
   RowBox[{":", "History", ":"}], "*)"}], "\[IndentingNewLine]", 
  RowBox[{"(*", " ", 
   RowBox[{":", "Keywords", ":"}], " ", "*)"}], "\[IndentingNewLine]", 
  RowBox[{"(*", " ", 
   RowBox[{":", "Sources", ":", "biblio"}], "*)"}], "\[IndentingNewLine]", 
  RowBox[{"(*", " ", 
   RowBox[{":", "Discussion", ":"}], "*)"}], "\[IndentingNewLine]", 
  "\[IndentingNewLine]", 
  RowBox[{
   RowBox[{
    RowBox[{"PowerSum", "::", "usage"}], "=", 
    "\"\<PowerSum[x, n] returns the sum of the first n powers of x.\>\""}], 
   "\n", 
   RowBox[{"Begin", "[", "\"\<Private`\>\"", "]"}], "\n", 
   RowBox[{
    RowBox[{"PowerSum", "[", 
     RowBox[{"x_", ",", "n_"}], "]"}], ":=", 
    RowBox[{"Module", "[", 
     RowBox[{
      RowBox[{"{", "i", "}"}], ",", 
      RowBox[{"Sum", "[", 
       RowBox[{
        RowBox[{"x", "^", "i"}], ",", 
        RowBox[{"{", 
         RowBox[{"i", ",", "1", ",", "n"}], "}"}]}], "]"}]}], "]"}]}], "\n", 
   RowBox[{"End", "[", "]"}]}]}]], "Input",
 CellChangeTimes->{{3.798110767807403*^9, 3.798110807366911*^9}, 
   3.798111016316534*^9, {3.7981110805954924`*^9, 3.7981112236763935`*^9}, {
   3.7981112547364655`*^9, 3.798111290389886*^9}, {3.798111393178877*^9, 
   3.7981114106924753`*^9}},ExpressionUUID->"4862da6e-8d36-45fd-860c-\
96abea5c9b3f"]
},
WindowSize->{1152., 585.6},
WindowMargins->{{
  Automatic, -5.399999999999864}, {-5.399999999999977, Automatic}},
FrontEndVersion->"12.1 for Microsoft Windows (64-bit) (March 18, 2020)",
StyleDefinitions->"Default.nb",
ExpressionUUID->"71a17b36-851e-447f-9302-980d53cd4e34"
]
(* End of Notebook Content *)

(* Internal cache information *)
(*CellTagsOutline
CellTagsIndex->{}
*)
(*CellTagsIndex
CellTagsIndex->{}
*)
(*NotebookFileOutline
Notebook[{
Cell[558, 20, 2401, 62, 322, "Input",ExpressionUUID->"4862da6e-8d36-45fd-860c-96abea5c9b3f"]
}
]
*)

