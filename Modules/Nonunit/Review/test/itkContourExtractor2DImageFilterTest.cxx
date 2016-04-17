/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#include "itkImageFileReader.h"
#include "itkContourExtractor2DImageFilter.h"

namespace itkContourExtractor2DImageFilterTestNamespace
{
const unsigned int Dimension = 2;
typedef unsigned char                                   PixelType;
typedef itk::Image<PixelType, Dimension>                ImageType;
typedef itk::ImageFileReader<ImageType>                 ReaderType;
typedef itk::ContourExtractor2DImageFilter<ImageType>   ExtractorType;
typedef ExtractorType::VertexType                       VertexType;
typedef std::pair<double, double>                       MyVertexType;
typedef std::vector<MyVertexType>                       MyVertexListType;
typedef std::vector<MyVertexListType>                   MyVertexListList;
const float FLOAT_EPSILON = 0.0001;
}

/* ----------------------------------------------------------------------- */
itkContourExtractor2DImageFilterTestNamespace::MyVertexType _bottom_right[] =
   {itkContourExtractor2DImageFilterTestNamespace::MyVertexType(11.5, 8),
    itkContourExtractor2DImageFilterTestNamespace::MyVertexType(12, 7.5)};
itkContourExtractor2DImageFilterTestNamespace::MyVertexListType
                                bottom_right(_bottom_right, _bottom_right + 2);

itkContourExtractor2DImageFilterTestNamespace::MyVertexType
    _rev_bottom_right[] =
   {itkContourExtractor2DImageFilterTestNamespace::MyVertexType(12, 7.5),
    itkContourExtractor2DImageFilterTestNamespace::MyVertexType(11.5, 8)};
itkContourExtractor2DImageFilterTestNamespace::MyVertexListType
                    rev_bottom_right(_rev_bottom_right, _rev_bottom_right + 2);

itkContourExtractor2DImageFilterTestNamespace::MyVertexType _bottom_left[] =
  {itkContourExtractor2DImageFilterTestNamespace::MyVertexType(0, 7.5),
  itkContourExtractor2DImageFilterTestNamespace::MyVertexType(0.5, 8)};
itkContourExtractor2DImageFilterTestNamespace::MyVertexListType
                    bottom_left(_bottom_left, _bottom_left + 2);

itkContourExtractor2DImageFilterTestNamespace::MyVertexType
   _rev_bottom_left[] =
 {itkContourExtractor2DImageFilterTestNamespace::MyVertexType(0.5, 8),
  itkContourExtractor2DImageFilterTestNamespace::MyVertexType(0, 7.5)};
itkContourExtractor2DImageFilterTestNamespace::MyVertexListType
                   rev_bottom_left(_rev_bottom_left, _rev_bottom_left + 2);

itkContourExtractor2DImageFilterTestNamespace::MyVertexType _row2_col4[] =
 {itkContourExtractor2DImageFilterTestNamespace::MyVertexType(11, 6.5),
  itkContourExtractor2DImageFilterTestNamespace::MyVertexType(10, 6.5),
  itkContourExtractor2DImageFilterTestNamespace::MyVertexType(9.5, 6),
  itkContourExtractor2DImageFilterTestNamespace::MyVertexType(10, 5.5),
  itkContourExtractor2DImageFilterTestNamespace::MyVertexType(10.5, 5),
  itkContourExtractor2DImageFilterTestNamespace::MyVertexType(11, 4.5),
  itkContourExtractor2DImageFilterTestNamespace::MyVertexType(11.5, 5),
  itkContourExtractor2DImageFilterTestNamespace::MyVertexType(11.5, 6),
  itkContourExtractor2DImageFilterTestNamespace::MyVertexType(11, 6.5)};
itkContourExtractor2DImageFilterTestNamespace::MyVertexListType
                            row2_col4(_row2_col4, _row2_col4 + 9);

itkContourExtractor2DImageFilterTestNamespace::MyVertexType _rev_row2_col4[] =
 {itkContourExtractor2DImageFilterTestNamespace::MyVertexType(11, 6.5),
  itkContourExtractor2DImageFilterTestNamespace::MyVertexType(11.5, 6),
  itkContourExtractor2DImageFilterTestNamespace::MyVertexType(11.5, 5),
  itkContourExtractor2DImageFilterTestNamespace::MyVertexType(11, 4.5),
  itkContourExtractor2DImageFilterTestNamespace::MyVertexType(10.5, 5),
  itkContourExtractor2DImageFilterTestNamespace::MyVertexType(10, 5.5),
  itkContourExtractor2DImageFilterTestNamespace::MyVertexType(9.5, 6),
  itkContourExtractor2DImageFilterTestNamespace::MyVertexType(10, 6.5),
 itkContourExtractor2DImageFilterTestNamespace::MyVertexType(11, 6.5)};
itkContourExtractor2DImageFilterTestNamespace::MyVertexListType
                   rev_row2_col4(_rev_row2_col4, _rev_row2_col4 + 9);

itkContourExtractor2DImageFilterTestNamespace::MyVertexType _row2_col3[] =
 {itkContourExtractor2DImageFilterTestNamespace::MyVertexType(8, 6.5),
  itkContourExtractor2DImageFilterTestNamespace::MyVertexType(7, 6.5),
  itkContourExtractor2DImageFilterTestNamespace::MyVertexType(6.5, 6),
  itkContourExtractor2DImageFilterTestNamespace::MyVertexType(6.5, 5),
  itkContourExtractor2DImageFilterTestNamespace::MyVertexType(7, 4.5),
  itkContourExtractor2DImageFilterTestNamespace::MyVertexType(7.5, 5),
  itkContourExtractor2DImageFilterTestNamespace::MyVertexType(8, 5.5),
  itkContourExtractor2DImageFilterTestNamespace::MyVertexType(8.5, 6),
 itkContourExtractor2DImageFilterTestNamespace::MyVertexType(8, 6.5)};
itkContourExtractor2DImageFilterTestNamespace::MyVertexListType
                            row2_col3(_row2_col3, _row2_col3 + 9);

itkContourExtractor2DImageFilterTestNamespace::MyVertexType _rev_row2_col3[] =
 {itkContourExtractor2DImageFilterTestNamespace::MyVertexType(8, 6.5),
  itkContourExtractor2DImageFilterTestNamespace::MyVertexType(8.5, 6),
  itkContourExtractor2DImageFilterTestNamespace::MyVertexType(8, 5.5),
  itkContourExtractor2DImageFilterTestNamespace::MyVertexType(7.5, 5),
  itkContourExtractor2DImageFilterTestNamespace::MyVertexType(7, 4.5),
  itkContourExtractor2DImageFilterTestNamespace::MyVertexType(6.5, 5),
  itkContourExtractor2DImageFilterTestNamespace::MyVertexType(6.5, 6),
  itkContourExtractor2DImageFilterTestNamespace::MyVertexType(7, 6.5),
  itkContourExtractor2DImageFilterTestNamespace::MyVertexType(8, 6.5),};
itkContourExtractor2DImageFilterTestNamespace::MyVertexListType
               rev_row2_col3(_rev_row2_col3, _rev_row2_col3 + 9);

itkContourExtractor2DImageFilterTestNamespace::MyVertexType _row2_col2[] =
 {itkContourExtractor2DImageFilterTestNamespace::MyVertexType(5, 6.5),
  itkContourExtractor2DImageFilterTestNamespace::MyVertexType(4.5, 6),
  itkContourExtractor2DImageFilterTestNamespace::MyVertexType(4, 5.5),
  itkContourExtractor2DImageFilterTestNamespace::MyVertexType(3.5, 5),
  itkContourExtractor2DImageFilterTestNamespace::MyVertexType(4, 4.5),
  itkContourExtractor2DImageFilterTestNamespace::MyVertexType(5, 4.5),
  itkContourExtractor2DImageFilterTestNamespace::MyVertexType(5.5, 5),
  itkContourExtractor2DImageFilterTestNamespace::MyVertexType(5.5, 6),
  itkContourExtractor2DImageFilterTestNamespace::MyVertexType(5, 6.5)};
itkContourExtractor2DImageFilterTestNamespace::MyVertexListType
              row2_col2(_row2_col2, _row2_col2 + 9);

itkContourExtractor2DImageFilterTestNamespace::MyVertexType _rev_row2_col2[] =
 {itkContourExtractor2DImageFilterTestNamespace::MyVertexType(5, 6.5),
  itkContourExtractor2DImageFilterTestNamespace::MyVertexType(5.5, 6),
  itkContourExtractor2DImageFilterTestNamespace::MyVertexType(5.5, 5),
  itkContourExtractor2DImageFilterTestNamespace::MyVertexType(5, 4.5),
  itkContourExtractor2DImageFilterTestNamespace::MyVertexType(4, 4.5),
  itkContourExtractor2DImageFilterTestNamespace::MyVertexType(3.5, 5),
  itkContourExtractor2DImageFilterTestNamespace::MyVertexType(4, 5.5),
  itkContourExtractor2DImageFilterTestNamespace::MyVertexType(4.5, 6),
  itkContourExtractor2DImageFilterTestNamespace::MyVertexType(5, 6.5)};
itkContourExtractor2DImageFilterTestNamespace::MyVertexListType
              rev_row2_col2(_rev_row2_col2, _rev_row2_col2 + 9);

itkContourExtractor2DImageFilterTestNamespace::MyVertexType _row2_col1[] =
 {itkContourExtractor2DImageFilterTestNamespace::MyVertexType(1, 6.5),
  itkContourExtractor2DImageFilterTestNamespace::MyVertexType(0.5, 6),
  itkContourExtractor2DImageFilterTestNamespace::MyVertexType(0.5, 5),
  itkContourExtractor2DImageFilterTestNamespace::MyVertexType(1, 4.5),
  itkContourExtractor2DImageFilterTestNamespace::MyVertexType(2, 4.5),
  itkContourExtractor2DImageFilterTestNamespace::MyVertexType(2.5, 5),
  itkContourExtractor2DImageFilterTestNamespace::MyVertexType(2, 5.5),
  itkContourExtractor2DImageFilterTestNamespace::MyVertexType(1.5, 6),
 itkContourExtractor2DImageFilterTestNamespace::MyVertexType(1, 6.5)};
itkContourExtractor2DImageFilterTestNamespace::MyVertexListType
             row2_col1(_row2_col1, _row2_col1 + 9);

itkContourExtractor2DImageFilterTestNamespace::MyVertexType _rev_row2_col1[] =
 {itkContourExtractor2DImageFilterTestNamespace::MyVertexType(1, 6.5),
  itkContourExtractor2DImageFilterTestNamespace::MyVertexType(1.5, 6),
  itkContourExtractor2DImageFilterTestNamespace::MyVertexType(2, 5.5),
  itkContourExtractor2DImageFilterTestNamespace::MyVertexType(2.5, 5),
  itkContourExtractor2DImageFilterTestNamespace::MyVertexType(2, 4.5),
  itkContourExtractor2DImageFilterTestNamespace::MyVertexType(1, 4.5),
  itkContourExtractor2DImageFilterTestNamespace::MyVertexType(0.5, 5),
  itkContourExtractor2DImageFilterTestNamespace::MyVertexType(0.5, 6),
  itkContourExtractor2DImageFilterTestNamespace::MyVertexType(1, 6.5)};
itkContourExtractor2DImageFilterTestNamespace::MyVertexListType
             rev_row2_col1(_rev_row2_col1, _rev_row2_col1 + 9);

itkContourExtractor2DImageFilterTestNamespace::MyVertexType
     _row1_col4_middle[] =
 {itkContourExtractor2DImageFilterTestNamespace::MyVertexType(10, 3.5),
  itkContourExtractor2DImageFilterTestNamespace::MyVertexType(9.5, 3),
  itkContourExtractor2DImageFilterTestNamespace::MyVertexType(10, 2.5),
  itkContourExtractor2DImageFilterTestNamespace::MyVertexType(10.5, 3),
  itkContourExtractor2DImageFilterTestNamespace::MyVertexType(10, 3.5)};
itkContourExtractor2DImageFilterTestNamespace::MyVertexListType
                   row1_col4_middle(_row1_col4_middle, _row1_col4_middle + 5);

itkContourExtractor2DImageFilterTestNamespace::MyVertexType
   _rev_row1_col4_middle[] =
 {itkContourExtractor2DImageFilterTestNamespace::MyVertexType(10, 3.5),
  itkContourExtractor2DImageFilterTestNamespace::MyVertexType(10.5, 3),
  itkContourExtractor2DImageFilterTestNamespace::MyVertexType(10, 2.5),
  itkContourExtractor2DImageFilterTestNamespace::MyVertexType(9.5, 3),
  itkContourExtractor2DImageFilterTestNamespace::MyVertexType(10, 3.5)};
itkContourExtractor2DImageFilterTestNamespace::MyVertexListType
    rev_row1_col4_middle(_rev_row1_col4_middle, _rev_row1_col4_middle + 5);

itkContourExtractor2DImageFilterTestNamespace::MyVertexType _row1_col2[] =
 {itkContourExtractor2DImageFilterTestNamespace::MyVertexType(5, 3.5),
  itkContourExtractor2DImageFilterTestNamespace::MyVertexType(4, 3.5),
  itkContourExtractor2DImageFilterTestNamespace::MyVertexType(3.5, 3),
  itkContourExtractor2DImageFilterTestNamespace::MyVertexType(4, 2.5),
  itkContourExtractor2DImageFilterTestNamespace::MyVertexType(5, 2.5),
  itkContourExtractor2DImageFilterTestNamespace::MyVertexType(5.5, 3),
  itkContourExtractor2DImageFilterTestNamespace::MyVertexType(5, 3.5)};
itkContourExtractor2DImageFilterTestNamespace::MyVertexListType
              row1_col2(_row1_col2, _row1_col2 + 7);

itkContourExtractor2DImageFilterTestNamespace::MyVertexType _rev_row1_col2[] =
 {itkContourExtractor2DImageFilterTestNamespace::MyVertexType(5, 3.5),
  itkContourExtractor2DImageFilterTestNamespace::MyVertexType(5.5, 3),
  itkContourExtractor2DImageFilterTestNamespace::MyVertexType(5, 2.5),
  itkContourExtractor2DImageFilterTestNamespace::MyVertexType(4, 2.5),
  itkContourExtractor2DImageFilterTestNamespace::MyVertexType(3.5, 3),
  itkContourExtractor2DImageFilterTestNamespace::MyVertexType(4, 3.5),
  itkContourExtractor2DImageFilterTestNamespace::MyVertexType(5, 3.5)};
itkContourExtractor2DImageFilterTestNamespace::MyVertexListType
             rev_row1_col2(_rev_row1_col2, _rev_row1_col2 + 7);

itkContourExtractor2DImageFilterTestNamespace::MyVertexType _row1_col1[] =
 {itkContourExtractor2DImageFilterTestNamespace::MyVertexType(2, 3.5),
  itkContourExtractor2DImageFilterTestNamespace::MyVertexType(1.5, 3),
  itkContourExtractor2DImageFilterTestNamespace::MyVertexType(2, 2.5),
  itkContourExtractor2DImageFilterTestNamespace::MyVertexType(2.5, 3),
  itkContourExtractor2DImageFilterTestNamespace::MyVertexType(2, 3.5)};
itkContourExtractor2DImageFilterTestNamespace::MyVertexListType
                     row1_col1(_row1_col1, _row1_col1 + 5);

itkContourExtractor2DImageFilterTestNamespace::MyVertexType _rev_row1_col1[] =
 {itkContourExtractor2DImageFilterTestNamespace::MyVertexType(2, 3.5),
  itkContourExtractor2DImageFilterTestNamespace::MyVertexType(2.5, 3),
  itkContourExtractor2DImageFilterTestNamespace::MyVertexType(2, 2.5),
  itkContourExtractor2DImageFilterTestNamespace::MyVertexType(1.5, 3),
  itkContourExtractor2DImageFilterTestNamespace::MyVertexType(2, 3.5)};
itkContourExtractor2DImageFilterTestNamespace::MyVertexListType
            rev_row1_col1(_rev_row1_col1, _rev_row1_col1 + 5);

itkContourExtractor2DImageFilterTestNamespace::MyVertexType
  _row1_col4_right[] =
 {itkContourExtractor2DImageFilterTestNamespace::MyVertexType(11, 2.5),
  itkContourExtractor2DImageFilterTestNamespace::MyVertexType(10.5, 2),
  itkContourExtractor2DImageFilterTestNamespace::MyVertexType(11, 1.5),
  itkContourExtractor2DImageFilterTestNamespace::MyVertexType(11.5, 2),
  itkContourExtractor2DImageFilterTestNamespace::MyVertexType(11, 2.5)};
itkContourExtractor2DImageFilterTestNamespace::MyVertexListType
               row1_col4_right(_row1_col4_right, _row1_col4_right + 5);

itkContourExtractor2DImageFilterTestNamespace::MyVertexType
   _rev_row1_col4_right[] =
 {itkContourExtractor2DImageFilterTestNamespace::MyVertexType(11, 2.5),
  itkContourExtractor2DImageFilterTestNamespace::MyVertexType(11.5, 2),
  itkContourExtractor2DImageFilterTestNamespace::MyVertexType(11, 1.5),
  itkContourExtractor2DImageFilterTestNamespace::MyVertexType(10.5, 2),
  itkContourExtractor2DImageFilterTestNamespace::MyVertexType(11, 2.5)};
itkContourExtractor2DImageFilterTestNamespace::MyVertexListType
      rev_row1_col4_right(_rev_row1_col4_right, _rev_row1_col4_right + 5);

itkContourExtractor2DImageFilterTestNamespace::MyVertexType
   _row1_col4_left[] =
 {itkContourExtractor2DImageFilterTestNamespace::MyVertexType(9, 2.5),
  itkContourExtractor2DImageFilterTestNamespace::MyVertexType(8.5, 2),
  itkContourExtractor2DImageFilterTestNamespace::MyVertexType(9, 1.5),
  itkContourExtractor2DImageFilterTestNamespace::MyVertexType(9.5, 2),
  itkContourExtractor2DImageFilterTestNamespace::MyVertexType(9, 2.5)};
itkContourExtractor2DImageFilterTestNamespace::MyVertexListType
     row1_col4_left(_row1_col4_left, _row1_col4_left + 5);

itkContourExtractor2DImageFilterTestNamespace::MyVertexType
   _rev_row1_col4_left[] =
 {itkContourExtractor2DImageFilterTestNamespace::MyVertexType(9, 2.5),
  itkContourExtractor2DImageFilterTestNamespace::MyVertexType(9.5, 2),
  itkContourExtractor2DImageFilterTestNamespace::MyVertexType(9, 1.5),
  itkContourExtractor2DImageFilterTestNamespace::MyVertexType(8.5, 2),
  itkContourExtractor2DImageFilterTestNamespace::MyVertexType(9, 2.5)};
itkContourExtractor2DImageFilterTestNamespace::MyVertexListType
      rev_row1_col4_left(_rev_row1_col4_left, _rev_row1_col4_left + 5);

itkContourExtractor2DImageFilterTestNamespace::MyVertexType _row1_col3[] =
 {itkContourExtractor2DImageFilterTestNamespace::MyVertexType(7, 3.5),
  itkContourExtractor2DImageFilterTestNamespace::MyVertexType(6.5, 3),
  itkContourExtractor2DImageFilterTestNamespace::MyVertexType(6.5, 2),
  itkContourExtractor2DImageFilterTestNamespace::MyVertexType(7, 1.5),
  itkContourExtractor2DImageFilterTestNamespace::MyVertexType(7.5, 2),
  itkContourExtractor2DImageFilterTestNamespace::MyVertexType(7.5, 3),
  itkContourExtractor2DImageFilterTestNamespace::MyVertexType(7, 3.5)};
itkContourExtractor2DImageFilterTestNamespace::MyVertexListType
       row1_col3(_row1_col3, _row1_col3 + 7);

itkContourExtractor2DImageFilterTestNamespace::MyVertexType _rev_row1_col3[] =
 {itkContourExtractor2DImageFilterTestNamespace::MyVertexType(7, 3.5),
  itkContourExtractor2DImageFilterTestNamespace::MyVertexType(7.5, 3),
  itkContourExtractor2DImageFilterTestNamespace::MyVertexType(7.5, 2),
  itkContourExtractor2DImageFilterTestNamespace::MyVertexType(7, 1.5),
  itkContourExtractor2DImageFilterTestNamespace::MyVertexType(6.5, 2),
  itkContourExtractor2DImageFilterTestNamespace::MyVertexType(6.5, 3),
  itkContourExtractor2DImageFilterTestNamespace::MyVertexType(7, 3.5)};
itkContourExtractor2DImageFilterTestNamespace::MyVertexListType
    rev_row1_col3(_rev_row1_col3, _rev_row1_col3 + 7);


itkContourExtractor2DImageFilterTestNamespace::MyVertexType _top_right[] =
 {itkContourExtractor2DImageFilterTestNamespace::MyVertexType(12, 0.5),
  itkContourExtractor2DImageFilterTestNamespace::MyVertexType(11.5, 0)};
itkContourExtractor2DImageFilterTestNamespace::MyVertexListType
    top_right(_top_right, _top_right + 2);

itkContourExtractor2DImageFilterTestNamespace::MyVertexType _rev_top_right[] =
 {itkContourExtractor2DImageFilterTestNamespace::MyVertexType(11.5, 0),
  itkContourExtractor2DImageFilterTestNamespace::MyVertexType(12, 0.5)};
itkContourExtractor2DImageFilterTestNamespace::MyVertexListType
    rev_top_right(_rev_top_right, _rev_top_right + 2);

itkContourExtractor2DImageFilterTestNamespace::MyVertexType _top_left[] =
 {itkContourExtractor2DImageFilterTestNamespace::MyVertexType(1.5, 0),
  itkContourExtractor2DImageFilterTestNamespace::MyVertexType(1, 0.5),
  itkContourExtractor2DImageFilterTestNamespace::MyVertexType(0.5, 1),
  itkContourExtractor2DImageFilterTestNamespace::MyVertexType(0, 1.5)};
itkContourExtractor2DImageFilterTestNamespace::MyVertexListType
   top_left(_top_left, _top_left + 4);

itkContourExtractor2DImageFilterTestNamespace::MyVertexType _rev_top_left[] =
 {itkContourExtractor2DImageFilterTestNamespace::MyVertexType(0, 1.5),
  itkContourExtractor2DImageFilterTestNamespace::MyVertexType(0.5, 1),
  itkContourExtractor2DImageFilterTestNamespace::MyVertexType(1, 0.5),
  itkContourExtractor2DImageFilterTestNamespace::MyVertexType(1.5, 0)};
itkContourExtractor2DImageFilterTestNamespace::MyVertexListType
   rev_top_left(_rev_top_left, _rev_top_left + 4);

itkContourExtractor2DImageFilterTestNamespace::MyVertexType
  _top_left_cropped[] =
 {itkContourExtractor2DImageFilterTestNamespace::MyVertexType(0.5, 1),
  itkContourExtractor2DImageFilterTestNamespace::MyVertexType(0, 1.5)};
itkContourExtractor2DImageFilterTestNamespace::MyVertexListType
   top_left_cropped(_top_left_cropped, _top_left_cropped + 2);

itkContourExtractor2DImageFilterTestNamespace::MyVertexType
   _rev_top_left_cropped[] =
 {itkContourExtractor2DImageFilterTestNamespace::MyVertexType(0, 1.5),
  itkContourExtractor2DImageFilterTestNamespace::MyVertexType(0.5, 1)};
itkContourExtractor2DImageFilterTestNamespace::MyVertexListType
   rev_top_left_cropped(_rev_top_left_cropped, _rev_top_left_cropped + 2);

itkContourExtractor2DImageFilterTestNamespace::MyVertexType _row1_col4_all[] =
  {itkContourExtractor2DImageFilterTestNamespace::MyVertexType(10, 3.5),
   itkContourExtractor2DImageFilterTestNamespace::MyVertexType(9.5, 3),
   itkContourExtractor2DImageFilterTestNamespace::MyVertexType(9, 2.5),
   itkContourExtractor2DImageFilterTestNamespace::MyVertexType(8.5, 2),
   itkContourExtractor2DImageFilterTestNamespace::MyVertexType(9, 1.5),
   itkContourExtractor2DImageFilterTestNamespace::MyVertexType(9.5, 2),
   itkContourExtractor2DImageFilterTestNamespace::MyVertexType(10, 2.5),
   itkContourExtractor2DImageFilterTestNamespace::MyVertexType(10.5, 2),
   itkContourExtractor2DImageFilterTestNamespace::MyVertexType(11, 1.5),
   itkContourExtractor2DImageFilterTestNamespace::MyVertexType(11.5, 2),
   itkContourExtractor2DImageFilterTestNamespace::MyVertexType(11, 2.5),
   itkContourExtractor2DImageFilterTestNamespace::MyVertexType(10.5, 3),
   itkContourExtractor2DImageFilterTestNamespace::MyVertexType(10, 3.5)};
itkContourExtractor2DImageFilterTestNamespace::MyVertexListType
   row1_col4_all(_row1_col4_all, _row1_col4_all + 13);

itkContourExtractor2DImageFilterTestNamespace::MyVertexType
  _rev_row1_col4_all[] =
  {itkContourExtractor2DImageFilterTestNamespace::MyVertexType(10, 3.5),
   itkContourExtractor2DImageFilterTestNamespace::MyVertexType(10.5, 3),
   itkContourExtractor2DImageFilterTestNamespace::MyVertexType(11, 2.5),
   itkContourExtractor2DImageFilterTestNamespace::MyVertexType(11.5, 2),
   itkContourExtractor2DImageFilterTestNamespace::MyVertexType(11, 1.5),
   itkContourExtractor2DImageFilterTestNamespace::MyVertexType(10.5, 2),
   itkContourExtractor2DImageFilterTestNamespace::MyVertexType(10, 2.5),
   itkContourExtractor2DImageFilterTestNamespace::MyVertexType(9.5, 2),
   itkContourExtractor2DImageFilterTestNamespace::MyVertexType(9, 1.5),
   itkContourExtractor2DImageFilterTestNamespace::MyVertexType(8.5, 2),
   itkContourExtractor2DImageFilterTestNamespace::MyVertexType(9, 2.5),
   itkContourExtractor2DImageFilterTestNamespace::MyVertexType(9.5, 3),
   itkContourExtractor2DImageFilterTestNamespace::MyVertexType(10, 3.5)};
itkContourExtractor2DImageFilterTestNamespace::MyVertexListType
   rev_row1_col4_all(_rev_row1_col4_all, _rev_row1_col4_all + 13);

itkContourExtractor2DImageFilterTestNamespace::MyVertexListType _edco[] =
  {top_left, top_right, row1_col3, row1_col4_left,
   row1_col4_right, row1_col1, row1_col2, row1_col4_middle, row2_col1,
   row2_col2, row2_col3, row2_col4, bottom_left, bottom_right};
itkContourExtractor2DImageFilterTestNamespace::MyVertexListList
   expected_disconnected_clockwise_outputs(_edco, _edco + 14);

itkContourExtractor2DImageFilterTestNamespace::MyVertexListType _edcco[] =
  {rev_top_left, rev_top_right, rev_row1_col3,
   rev_row1_col4_left, rev_row1_col4_right, rev_row1_col1, rev_row1_col2,
   rev_row1_col4_middle, rev_row2_col1, rev_row2_col2, rev_row2_col3,
   rev_row2_col4, rev_bottom_left, rev_bottom_right};
itkContourExtractor2DImageFilterTestNamespace::MyVertexListList
   expected_disconnected_counterclockwise_outputs(_edcco, _edcco + 14);

itkContourExtractor2DImageFilterTestNamespace::MyVertexListType _ecco[] =
  {top_left, top_right, row1_col3, row1_col4_all,
   row1_col1, row1_col2, row2_col1, row2_col2, row2_col3, row2_col4,
   bottom_left, bottom_right};
itkContourExtractor2DImageFilterTestNamespace::MyVertexListList
  expected_connected_clockwise_outputs(_ecco, _ecco + 12);

itkContourExtractor2DImageFilterTestNamespace::MyVertexListType _edcro[] =
  {top_left_cropped, row1_col3, row1_col4_left,
   row1_col4_right, row1_col1, row1_col2, row1_col4_middle, row2_col1,
   row2_col2, row2_col3, row2_col4};
itkContourExtractor2DImageFilterTestNamespace::MyVertexListList
   expected_disconnected_clockwise_cropped_outputs(_edcro, _edcro + 11);

/*--------------------------------------------------------------------------*/

bool HasCorrectOutput(
itkContourExtractor2DImageFilterTestNamespace::ExtractorType::Pointer
   extractor,
itkContourExtractor2DImageFilterTestNamespace::MyVertexListList& correct)
{
  if (extractor->GetNumberOfIndexedOutputs() != correct.size())
    {
    return false;
    }

  for(unsigned int i = 0; i < correct.size(); i++)
    {
    itkContourExtractor2DImageFilterTestNamespace::ExtractorType::
                    VertexListConstPointer
        vertices = extractor->GetOutput(i)->GetVertexList();

    itkContourExtractor2DImageFilterTestNamespace::MyVertexListType&
                correctVertices = correct[i];

    if (vertices->Size() != correctVertices.size())
      {
      return false;
      }
    for(unsigned int j = 0; j < correctVertices.size(); j++)
      {
      const itkContourExtractor2DImageFilterTestNamespace::MyVertexType&
          correctVertex = correctVertices[j];

      const itkContourExtractor2DImageFilterTestNamespace::VertexType&
          vertex = vertices->ElementAt(j);
      if (itk::Math::abs(correctVertex.first - vertex[0]) >
            itkContourExtractor2DImageFilterTestNamespace::FLOAT_EPSILON ||
          itk::Math::abs(correctVertex.second - vertex[1]) >
            itkContourExtractor2DImageFilterTestNamespace::FLOAT_EPSILON)
        {
        return false;
        }
      }
    }
  return true;
}

int itkContourExtractor2DImageFilterTest(int argc, char *argv[])
{
  if( argc < 2 )
    {
    std::cerr << "Missing Parameters " << std::endl;
    std::cerr << "Usage: " << argv[0];
    std::cerr << " Input Test Image  " << std::endl;
    return 1;
    }

  itkContourExtractor2DImageFilterTestNamespace::ReaderType::Pointer
    reader = itkContourExtractor2DImageFilterTestNamespace::ReaderType::New();

  reader->SetFileName(argv[1]);
  itkContourExtractor2DImageFilterTestNamespace::ExtractorType::Pointer
   extractor =
      itkContourExtractor2DImageFilterTestNamespace::ExtractorType::New();

  extractor->SetInput(reader->GetOutput());

  // exercise Set/Get ContourValue methods
  extractor->SetContourValue( 255.0 );
  if( extractor->GetContourValue() != 255.0 )
    {
    std::cerr << " Contour Value Set/Get problem" << std::endl;
    return EXIT_FAILURE;
    }

  extractor->SetContourValue(127.5);

  // exercise Set/Get methods of VertexConnectHighPixels
  extractor->VertexConnectHighPixelsOn();
  if( extractor->GetVertexConnectHighPixels() != true )
    {
    std::cerr << "VertexConnectHighPixels Set/Get Problem" << std::endl;
    return EXIT_FAILURE;
    }

  // exercise Set/Get methods of ReverseContourOrientation
  extractor->ReverseContourOrientationOn();
  if( extractor->GetReverseContourOrientation() != true )
    {
    std::cerr << "ReverseContourOrientation Set/Get Problem" << std::endl;
    return EXIT_FAILURE;
    }

  bool testsPassed = true;
  try
    {
    extractor->VertexConnectHighPixelsOff();
    extractor->ReverseContourOrientationOff();
    extractor->Update();
    std::cout << "Test 1... ";
    if (!HasCorrectOutput(extractor, expected_disconnected_clockwise_outputs))
      {
      testsPassed = false;
      std::cout << "failed." << std::endl;
      }
    else std::cout << "passed." << std::endl;

    extractor->VertexConnectHighPixelsOff();
    extractor->ReverseContourOrientationOn();
    extractor->Update();
    std::cout << "Test 2... ";
    if (!HasCorrectOutput(extractor,
            expected_disconnected_counterclockwise_outputs))
      {
      testsPassed = false;
      std::cout << "failed." << std::endl;
      }
    else
      {
      std::cout << "passed." << std::endl;
      }

    extractor->VertexConnectHighPixelsOn();
    extractor->ReverseContourOrientationOff();
    extractor->Update();
    std::cout << "Test 3... ";
    if (!HasCorrectOutput(extractor, expected_connected_clockwise_outputs))
      {
      testsPassed = false;
      std::cout << "failed." << std::endl;
      }
    else
      {
       std::cout << "passed." << std::endl;
      }

    extractor->VertexConnectHighPixelsOff();
    extractor->ReverseContourOrientationOff();
    // Move the region to evaluate in by one on the top and bottom
    itkContourExtractor2DImageFilterTestNamespace::ImageType::RegionType
              region = reader->GetOutput()->GetLargestPossibleRegion();

    itkContourExtractor2DImageFilterTestNamespace::ImageType::IndexType
              index = region.GetIndex();

    itkContourExtractor2DImageFilterTestNamespace::ImageType::SizeType
              size = region.GetSize();

    index[1] += 1;
    size[1] -= 2;

    extractor->SetRequestedRegion(
       itkContourExtractor2DImageFilterTestNamespace::ImageType::RegionType(
                   index, size));

    // exercise Set/Get RequestRegion
    if ( extractor->GetRequestedRegion() !=
     itkContourExtractor2DImageFilterTestNamespace::ImageType::RegionType(
                   index, size) )
      {
      std::cerr << "RequestedRegion Set/Get Problem" << std::endl;
      return EXIT_FAILURE;
      }

    extractor->Update();
    std::cout << "Test 4... ";
    if (!HasCorrectOutput(extractor,
          expected_disconnected_clockwise_cropped_outputs))
      {
      testsPassed = false;
      std::cerr << "failed." << std::endl;
      }
    else
      {
      std::cout << "passed." << std::endl;
      }
    }
  catch( itk::ExceptionObject & err )
    {
    std::cerr << "ExceptionObject caught !" << std::endl;
    std::cerr << err << std::endl;
    return -1;
    }

  if (testsPassed)
    {
    std::cout << "All tests passed." << std::endl;
    return 0;
    }

  return 1;
}
