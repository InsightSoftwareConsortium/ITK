/*=========================================================================
 *
 *  Copyright NumFOCUS
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
 * https://www.insight-journal.org/browse/publication/176
 *
 */

#define itkShapeLabelMapFilterDispatchMacro()                                                                          \
  case LabelObjectType::LABEL:                                                                                         \
  {                                                                                                                    \
    using AccessorType = typename Functor::LabelLabelObjectAccessor<LabelObjectType>;                                  \
    AccessorType accessor;                                                                                             \
    this->TemplatedGenerateData(accessor);                                                                             \
    break;                                                                                                             \
  }                                                                                                                    \
  case LabelObjectType::NUMBER_OF_PIXELS:                                                                              \
  {                                                                                                                    \
    using AccessorType = typename Functor::NumberOfPixelsLabelObjectAccessor<LabelObjectType>;                         \
    AccessorType accessor;                                                                                             \
    this->TemplatedGenerateData(accessor);                                                                             \
    break;                                                                                                             \
  }                                                                                                                    \
  case LabelObjectType::PHYSICAL_SIZE:                                                                                 \
  {                                                                                                                    \
    using AccessorType = typename Functor::PhysicalSizeLabelObjectAccessor<LabelObjectType>;                           \
    AccessorType accessor;                                                                                             \
    this->TemplatedGenerateData(accessor);                                                                             \
    break;                                                                                                             \
  }                                                                                                                    \
  case LabelObjectType::NUMBER_OF_PIXELS_ON_BORDER:                                                                    \
  {                                                                                                                    \
    using AccessorType = typename Functor::NumberOfPixelsOnBorderLabelObjectAccessor<LabelObjectType>;                 \
    AccessorType accessor;                                                                                             \
    this->TemplatedGenerateData(accessor);                                                                             \
    break;                                                                                                             \
  }                                                                                                                    \
  case LabelObjectType::PERIMETER_ON_BORDER:                                                                           \
  {                                                                                                                    \
    using AccessorType = typename Functor::PerimeterOnBorderLabelObjectAccessor<LabelObjectType>;                      \
    AccessorType accessor;                                                                                             \
    this->TemplatedGenerateData(accessor);                                                                             \
    break;                                                                                                             \
  }                                                                                                                    \
  case LabelObjectType::FERET_DIAMETER:                                                                                \
  {                                                                                                                    \
    using AccessorType = typename Functor::FeretDiameterLabelObjectAccessor<LabelObjectType>;                          \
    AccessorType accessor;                                                                                             \
    this->TemplatedGenerateData(accessor);                                                                             \
    break;                                                                                                             \
  }                                                                                                                    \
  case LabelObjectType::ELONGATION:                                                                                    \
  {                                                                                                                    \
    using AccessorType = typename Functor::ElongationLabelObjectAccessor<LabelObjectType>;                             \
    AccessorType accessor;                                                                                             \
    this->TemplatedGenerateData(accessor);                                                                             \
    break;                                                                                                             \
  }                                                                                                                    \
  case LabelObjectType::PERIMETER:                                                                                     \
  {                                                                                                                    \
    using AccessorType = typename Functor::PerimeterLabelObjectAccessor<LabelObjectType>;                              \
    AccessorType accessor;                                                                                             \
    this->TemplatedGenerateData(accessor);                                                                             \
    break;                                                                                                             \
  }                                                                                                                    \
  case LabelObjectType::ROUNDNESS:                                                                                     \
  {                                                                                                                    \
    using AccessorType = typename Functor::RoundnessLabelObjectAccessor<LabelObjectType>;                              \
    AccessorType accessor;                                                                                             \
    this->TemplatedGenerateData(accessor);                                                                             \
    break;                                                                                                             \
  }                                                                                                                    \
  case LabelObjectType::EQUIVALENT_SPHERICAL_RADIUS:                                                                   \
  {                                                                                                                    \
    using AccessorType = typename Functor::EquivalentSphericalRadiusLabelObjectAccessor<LabelObjectType>;              \
    AccessorType accessor;                                                                                             \
    this->TemplatedGenerateData(accessor);                                                                             \
    break;                                                                                                             \
  }                                                                                                                    \
  case LabelObjectType::EQUIVALENT_SPHERICAL_PERIMETER:                                                                \
  {                                                                                                                    \
    using AccessorType = typename Functor::EquivalentSphericalPerimeterLabelObjectAccessor<LabelObjectType>;           \
    AccessorType accessor;                                                                                             \
    this->TemplatedGenerateData(accessor);                                                                             \
    break;                                                                                                             \
  }                                                                                                                    \
  case LabelObjectType::FLATNESS:                                                                                      \
  {                                                                                                                    \
    using AccessorType = typename Functor::FlatnessLabelObjectAccessor<LabelObjectType>;                               \
    AccessorType accessor;                                                                                             \
    this->TemplatedGenerateData(accessor);                                                                             \
    break;                                                                                                             \
  }                                                                                                                    \
  case LabelObjectType::PERIMETER_ON_BORDER_RATIO:                                                                     \
  {                                                                                                                    \
    using AccessorType = typename Functor::PerimeterOnBorderRatioLabelObjectAccessor<LabelObjectType>;                 \
    AccessorType accessor;                                                                                             \
    this->TemplatedGenerateData(accessor);                                                                             \
    break;                                                                                                             \
  }


#define itkStatisticsLabelMapFilterDispatchMacro()                                                                     \
  case LabelObjectType::MINIMUM:                                                                                       \
  {                                                                                                                    \
    using AccessorType = typename Functor::MinimumLabelObjectAccessor<LabelObjectType>;                                \
    AccessorType accessor;                                                                                             \
    this->TemplatedGenerateData(accessor);                                                                             \
    break;                                                                                                             \
  }                                                                                                                    \
  case LabelObjectType::MAXIMUM:                                                                                       \
  {                                                                                                                    \
    using AccessorType = typename Functor::MaximumLabelObjectAccessor<LabelObjectType>;                                \
    AccessorType accessor;                                                                                             \
    this->TemplatedGenerateData(accessor);                                                                             \
    break;                                                                                                             \
  }                                                                                                                    \
  case LabelObjectType::MEAN:                                                                                          \
  {                                                                                                                    \
    using AccessorType = typename Functor::MeanLabelObjectAccessor<LabelObjectType>;                                   \
    AccessorType accessor;                                                                                             \
    this->TemplatedGenerateData(accessor);                                                                             \
    break;                                                                                                             \
  }                                                                                                                    \
  case LabelObjectType::SUM:                                                                                           \
  {                                                                                                                    \
    using AccessorType = typename Functor::SumLabelObjectAccessor<LabelObjectType>;                                    \
    AccessorType accessor;                                                                                             \
    this->TemplatedGenerateData(accessor);                                                                             \
    break;                                                                                                             \
  }                                                                                                                    \
  case LabelObjectType::STANDARD_DEVIATION:                                                                            \
  {                                                                                                                    \
    using AccessorType = typename Functor::StandardDeviationLabelObjectAccessor<LabelObjectType>;                      \
    AccessorType accessor;                                                                                             \
    this->TemplatedGenerateData(accessor);                                                                             \
    break;                                                                                                             \
  }                                                                                                                    \
  case LabelObjectType::VARIANCE:                                                                                      \
  {                                                                                                                    \
    using AccessorType = typename Functor::VarianceLabelObjectAccessor<LabelObjectType>;                               \
    AccessorType accessor;                                                                                             \
    this->TemplatedGenerateData(accessor);                                                                             \
    break;                                                                                                             \
  }                                                                                                                    \
  case LabelObjectType::MEDIAN:                                                                                        \
  {                                                                                                                    \
    using AccessorType = typename Functor::MedianLabelObjectAccessor<LabelObjectType>;                                 \
    AccessorType accessor;                                                                                             \
    this->TemplatedGenerateData(accessor);                                                                             \
    break;                                                                                                             \
  }                                                                                                                    \
  case LabelObjectType::KURTOSIS:                                                                                      \
  {                                                                                                                    \
    using AccessorType = typename Functor::KurtosisLabelObjectAccessor<LabelObjectType>;                               \
    AccessorType accessor;                                                                                             \
    this->TemplatedGenerateData(accessor);                                                                             \
    break;                                                                                                             \
  }                                                                                                                    \
  case LabelObjectType::SKEWNESS:                                                                                      \
  {                                                                                                                    \
    using AccessorType = typename Functor::SkewnessLabelObjectAccessor<LabelObjectType>;                               \
    AccessorType accessor;                                                                                             \
    this->TemplatedGenerateData(accessor);                                                                             \
    break;                                                                                                             \
  }                                                                                                                    \
  case LabelObjectType::WEIGHTED_ELONGATION:                                                                           \
  {                                                                                                                    \
    using AccessorType = typename Functor::WeightedElongationLabelObjectAccessor<LabelObjectType>;                     \
    AccessorType accessor;                                                                                             \
    this->TemplatedGenerateData(accessor);                                                                             \
    break;                                                                                                             \
  }                                                                                                                    \
  case LabelObjectType::WEIGHTED_FLATNESS:                                                                             \
  {                                                                                                                    \
    using AccessorType = typename Functor::WeightedFlatnessLabelObjectAccessor<LabelObjectType>;                       \
    AccessorType accessor;                                                                                             \
    this->TemplatedGenerateData(accessor);                                                                             \
    break;                                                                                                             \
  }

#endif
