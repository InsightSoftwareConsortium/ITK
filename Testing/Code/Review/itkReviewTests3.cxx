/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkReviewTests3.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
// this file defines the itkBasicFiltersTest for the test driver
// and all it expects is that you have a function called RegisterTests

#include "vnl/vnl_sample.h"
#include "itkTestMain.h"
#include "itkConfigure.h"

void RegisterTests()
{
  vnl_sample_reseed(8775070);

  REGISTER_TEST(itkAggregateLabelMapFilterTest);
  REGISTER_TEST(itkAutoCropLabelMapFilterTest1);
  REGISTER_TEST(itkBinaryImageToLabelMapFilterTest);
  REGISTER_TEST(itkBinaryImageToShapeLabelMapFilterTest1);
  REGISTER_TEST(itkBinaryShapeKeepNObjectsImageFilterTest1);
  REGISTER_TEST(itkBinaryShapeOpeningImageFilterTest1);
  REGISTER_TEST(itkChangeLabelLabelMapFilterTest);
  REGISTER_TEST(itkChangeRegionLabelMapFilterTest1);
  REGISTER_TEST(itkCropLabelMapFilterTest1);
  REGISTER_TEST(itkGaborImageSourceTest);
  REGISTER_TEST(itkGridImageSourceTest);
  REGISTER_TEST(itkLabelImageToShapeLabelMapFilterTest1);
  REGISTER_TEST(itkLabelMapToBinaryImageFilterTest);
  REGISTER_TEST(itkLabelShapeKeepNObjectsImageFilterTest1);
  REGISTER_TEST(itkLabelShapeOpeningImageFilterTest1);
  REGISTER_TEST(itkMergeLabelMapFilterTest1);
  REGISTER_TEST(itkPadLabelMapFilterTest1);
  REGISTER_TEST(itkRegionFromReferenceLabelMapFilterTest1);
  REGISTER_TEST(itkRelabelLabelMapFilterTest1);
  REGISTER_TEST(itkShapeKeepNObjectsLabelMapFilterTest1);
  REGISTER_TEST(itkShapeLabelObjectAccessorsTest1);
  REGISTER_TEST(itkShapeOpeningLabelMapFilterTest1);
  REGISTER_TEST(itkShapeRelabelImageFilterTest1);
  REGISTER_TEST(itkShapeRelabelLabelMapFilterTest1);
  REGISTER_TEST(itkShapeUniqueLabelMapFilterTest1);
  REGISTER_TEST(itkShiftScaleLabelMapFilterTest1);
  REGISTER_TEST(itkTriangleMeshToBinaryImageFilterTest3);
  REGISTER_TEST(itkDiscreteGaussianDerivativeImageFilterTest);
  REGISTER_TEST(itkAttributeLabelObjectAccessorsTest1);
  REGISTER_TEST(itkAttributeOpeningLabelMapFilterTest1);
  REGISTER_TEST(itkAttributeKeepNObjectsLabelMapFilterTest1);
  REGISTER_TEST(itkAttributeRelabelLabelMapFilterTest1);
  REGISTER_TEST(itkLabelSelectionLabelMapFilterTest);
  REGISTER_TEST(itkLabelMapMaskImageFilterTest);
  REGISTER_TEST(itkObjectByObjectLabelMapFilterTest);
  REGISTER_TEST(itkAttributeUniqueLabelMapFilterTest1);
  REGISTER_TEST(itkLabelUniqueLabelMapFilterTest1);
  REGISTER_TEST(itkLabelMapToAttributeImageFilterTest1);
  REGISTER_TEST(itkLabelMapToRGBImageFilterTest1);
  REGISTER_TEST(itkLabelMapOverlayImageFilterTest1);
  REGISTER_TEST(itkLabelMapContourOverlayImageFilterTest1);
  REGISTER_TEST(itkBinaryFillholeImageFilterTest1);
  REGISTER_TEST(itkBinaryGrindPeakImageFilterTest1);
  REGISTER_TEST(itkShapePositionLabelMapFilterTest1);
  REGISTER_TEST(itkStatisticsPositionLabelMapFilterTest1);
  REGISTER_TEST(itkAttributePositionLabelMapFilterTest1);
  REGISTER_TEST(itkBinaryReconstructionLabelMapFilterTest);
  REGISTER_TEST(itkBinaryReconstructionByDilationImageFilterTest);
  REGISTER_TEST(itkBinaryReconstructionByErosionImageFilterTest);
  REGISTER_TEST(itkBinaryClosingByReconstructionImageFilterTest);
  REGISTER_TEST(itkBinaryOpeningByReconstructionImageFilterTest);
  }
