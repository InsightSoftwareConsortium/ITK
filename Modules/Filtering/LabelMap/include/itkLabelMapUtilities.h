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
#ifndef itkLabelMapUtilities_h
#define itkLabelMapUtilities_h

/*
 *
 * This code was contributed in the Insight Journal paper:
 * "Label object representation and manipulation with ITK"
 * by Lehmann G.
 * https://hdl.handle.net/1926/584
 * http://www.insight-journal.org/browse/publication/176
 *
 */

#define itkShapeLabelMapFilterDispatchMacro() \
    case LabelObjectType::LABEL: \
      { \
      typedef typename Functor::LabelLabelObjectAccessor< LabelObjectType > AccessorType; \
      AccessorType accessor; \
      this->TemplatedGenerateData(accessor); \
      break; \
      } \
    case LabelObjectType::NUMBER_OF_PIXELS: \
      { \
      typedef typename Functor::NumberOfPixelsLabelObjectAccessor< LabelObjectType > AccessorType; \
      AccessorType accessor; \
      this->TemplatedGenerateData(accessor); \
      break; \
      } \
    case LabelObjectType::PHYSICAL_SIZE: \
      { \
      typedef typename Functor::PhysicalSizeLabelObjectAccessor< LabelObjectType > AccessorType; \
      AccessorType accessor; \
      this->TemplatedGenerateData(accessor); \
      break; \
      } \
    case LabelObjectType::NUMBER_OF_PIXELS_ON_BORDER: \
      { \
      typedef typename Functor::NumberOfPixelsOnBorderLabelObjectAccessor< LabelObjectType > AccessorType; \
      AccessorType accessor; \
      this->TemplatedGenerateData(accessor); \
      break; \
      } \
    case LabelObjectType::PERIMETER_ON_BORDER: \
      { \
      typedef typename Functor::PerimeterOnBorderLabelObjectAccessor< LabelObjectType > AccessorType; \
      AccessorType accessor; \
      this->TemplatedGenerateData(accessor); \
      break; \
      } \
    case LabelObjectType::FERET_DIAMETER: \
      { \
      typedef typename Functor::FeretDiameterLabelObjectAccessor< LabelObjectType > AccessorType; \
      AccessorType accessor; \
      this->TemplatedGenerateData(accessor); \
      break; \
      } \
    case LabelObjectType::ELONGATION: \
      { \
      typedef typename Functor::ElongationLabelObjectAccessor< LabelObjectType > AccessorType; \
      AccessorType accessor; \
      this->TemplatedGenerateData(accessor); \
      break; \
      } \
    case LabelObjectType::PERIMETER: \
      { \
      typedef typename Functor::PerimeterLabelObjectAccessor< LabelObjectType > AccessorType; \
      AccessorType accessor; \
      this->TemplatedGenerateData(accessor); \
      break; \
      } \
    case LabelObjectType::ROUNDNESS: \
      { \
      typedef typename Functor::RoundnessLabelObjectAccessor< LabelObjectType > AccessorType; \
      AccessorType accessor; \
      this->TemplatedGenerateData(accessor); \
      break; \
      } \
    case LabelObjectType::EQUIVALENT_SPHERICAL_RADIUS: \
      { \
      typedef typename Functor::EquivalentSphericalRadiusLabelObjectAccessor< LabelObjectType > AccessorType; \
      AccessorType accessor; \
      this->TemplatedGenerateData(accessor); \
      break; \
      } \
    case LabelObjectType::EQUIVALENT_SPHERICAL_PERIMETER: \
      { \
      typedef typename Functor::EquivalentSphericalPerimeterLabelObjectAccessor< LabelObjectType > AccessorType; \
      AccessorType accessor; \
      this->TemplatedGenerateData(accessor); \
      break; \
      } \
    case LabelObjectType::FLATNESS: \
      { \
      typedef typename Functor::FlatnessLabelObjectAccessor< LabelObjectType > AccessorType; \
      AccessorType accessor; \
      this->TemplatedGenerateData(accessor); \
      break; \
      } \
    case LabelObjectType::PERIMETER_ON_BORDER_RATIO: \
      { \
      typedef typename Functor::PerimeterOnBorderRatioLabelObjectAccessor< LabelObjectType > AccessorType; \
      AccessorType accessor; \
      this->TemplatedGenerateData(accessor); \
      break; \
      }


#define itkStatisticsLabelMapFilterDispatchMacro() \
    case LabelObjectType::MINIMUM: \
      { \
      typedef typename Functor::MinimumLabelObjectAccessor< LabelObjectType > AccessorType; \
      AccessorType accessor; \
      this->TemplatedGenerateData(accessor); \
      break; \
      } \
    case LabelObjectType::MAXIMUM: \
      { \
      typedef typename Functor::MaximumLabelObjectAccessor< LabelObjectType > AccessorType; \
      AccessorType accessor; \
      this->TemplatedGenerateData(accessor); \
      break; \
      } \
    case LabelObjectType::MEAN: \
      { \
      typedef typename Functor::MeanLabelObjectAccessor< LabelObjectType > AccessorType; \
      AccessorType accessor; \
      this->TemplatedGenerateData(accessor); \
      break; \
      } \
    case LabelObjectType::SUM: \
      { \
      typedef typename Functor::SumLabelObjectAccessor< LabelObjectType > AccessorType; \
      AccessorType accessor; \
      this->TemplatedGenerateData(accessor); \
      break; \
      } \
    case LabelObjectType::STANDARD_DEVIATION: \
      { \
      typedef typename Functor::StandardDeviationLabelObjectAccessor< LabelObjectType > AccessorType; \
      AccessorType accessor; \
      this->TemplatedGenerateData(accessor); \
      break; \
      } \
    case LabelObjectType::VARIANCE: \
      { \
      typedef typename Functor::VarianceLabelObjectAccessor< LabelObjectType > AccessorType; \
      AccessorType accessor; \
      this->TemplatedGenerateData(accessor); \
      break; \
      } \
    case LabelObjectType::MEDIAN: \
      { \
      typedef typename Functor::MedianLabelObjectAccessor< LabelObjectType > AccessorType; \
      AccessorType accessor; \
      this->TemplatedGenerateData(accessor); \
      break; \
      } \
    case LabelObjectType::KURTOSIS: \
      { \
      typedef typename Functor::KurtosisLabelObjectAccessor< LabelObjectType > AccessorType; \
      AccessorType accessor; \
      this->TemplatedGenerateData(accessor); \
      break; \
      } \
    case LabelObjectType::SKEWNESS: \
      { \
      typedef typename Functor::SkewnessLabelObjectAccessor< LabelObjectType > AccessorType; \
      AccessorType accessor; \
      this->TemplatedGenerateData(accessor); \
      break; \
      } \
    case LabelObjectType::WEIGHTED_ELONGATION: \
      { \
      typedef typename Functor::WeightedElongationLabelObjectAccessor< LabelObjectType > AccessorType; \
      AccessorType accessor; \
      this->TemplatedGenerateData(accessor); \
      break; \
      } \
    case LabelObjectType::WEIGHTED_FLATNESS: \
      { \
      typedef typename Functor::WeightedFlatnessLabelObjectAccessor< LabelObjectType > AccessorType; \
      AccessorType accessor; \
      this->TemplatedGenerateData(accessor); \
      break; \
      }

#endif
