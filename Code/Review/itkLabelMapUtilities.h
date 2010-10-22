/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    $RCSfile: itkAttributeUniqueLabelMapFilter.txx,v $
  Language:  C++
  Date:      $Date: 2005/08/23 15:09:03 $
  Version:   $Revision: 1.6 $

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkLabelMapUtilities_h
#define __itkLabelMapUtilities_h

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
    case LabelObjectType::SIZE_REGION_RATIO: \
      { \
      typedef typename Functor::SizeRegionRatioLabelObjectAccessor< LabelObjectType > AccessorType; \
      AccessorType accessor; \
      this->TemplatedGenerateData(accessor); \
      break; \
      } \
    case LabelObjectType::REGION_ELONGATION: \
      { \
      typedef typename Functor::RegionElongationLabelObjectAccessor< LabelObjectType > AccessorType; \
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
