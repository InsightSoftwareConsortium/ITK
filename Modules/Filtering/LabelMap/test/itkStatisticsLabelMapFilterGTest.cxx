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

#include "itkGTest.h"

#include "itkImage.h"
#include "itkLabelImageToStatisticsLabelMapFilter.h"



// The expected results were verified for these tests cases, unless
// the test is marked with the "resulting value" comment. In which
// case the baseline value was just what was computed by the method.



TEST(StatisticsLabelMap,T1)
{
  static const unsigned int Dimension = 3;

  using PixelType = float;
  using FeatureImageType = itk::Image<PixelType, Dimension>;

  using LabelType = unsigned short;
  using LabelImageType = itk::Image<LabelType, Dimension>;

  using LabelObjectParentType = itk::LabelObject<LabelType, Dimension>;
  using LabelObjectType = itk::StatisticsLabelObject<LabelType, Dimension, LabelObjectParentType>;
  using StatisticsLabelMapType = itk::LabelMap<LabelObjectType>;


  using LabelizerType = itk::LabelImageToLabelMapFilter< LabelImageType, StatisticsLabelMapType >;

  using LabelObjectValuatorType = itk::StatisticsLabelMapFilter< StatisticsLabelMapType,
                                                                 FeatureImageType,
                                                                 itk::InPlaceLabelMapFilter< StatisticsLabelMapType > >;

  auto labelizerFilter = LabelizerType::New();

  auto labelObjectValuatorFilter = LabelObjectValuatorType::New();
}
