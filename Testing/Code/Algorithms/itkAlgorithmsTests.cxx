/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkAlgorithmsTests.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif

#include "itkTestMain.h" 

// WARNING: Do not add any more tests to this program. Add new tests
// to itkAlgorithmsTests2.cxx. This file has become too large for the
// Borland linker.

void RegisterTests()
{
REGISTER_TEST(itkAntiAliasBinaryImageFilterTest );
REGISTER_TEST(itkAutomaticTopologyMeshSourceTest );
REGISTER_TEST(itkAnisotropicFourthOrderLevelSetImageFilterTest );
REGISTER_TEST(itkAntiAliasBinaryImageFilterTest  );
REGISTER_TEST(itkBinaryMinMaxCurvatureFlowImageFilterTest  );  
REGISTER_TEST(itkBinaryMask3DMeshSourceTest  );  
REGISTER_TEST(itkBioCellTest  );  
REGISTER_TEST(itkBioCellularAggregateTest  );  
REGISTER_TEST(itkBioGeneTest  );  
REGISTER_TEST(itkBioGenomeTest  );  
REGISTER_TEST(itkBioGeneNetworkTest  );  
REGISTER_TEST(itkCannySegmentationLevelSetImageFilterTest );
REGISTER_TEST(itkCorrelationCoefficientHistogramImageToImageMetricTest );
REGISTER_TEST(itkCurvatureFlowTest );
REGISTER_TEST(itkCurvesLevelSetImageFilterTest );
REGISTER_TEST(itkDemonsRegistrationFilterTest );
REGISTER_TEST(itkExtensionVelocitiesImageFilterTest );
REGISTER_TEST(itkExtractMeshConnectedRegionsTest );
REGISTER_TEST(itkFastChamferDistanceImageFilterTest );
REGISTER_TEST(itkFastMarchingTest );
REGISTER_TEST(itkFastMarchingExtensionImageFilterTest );
REGISTER_TEST(itkFEMRegistrationFilterTest );
REGISTER_TEST(itkGeodesicActiveContourLevelSetImageFilterTest );
REGISTER_TEST(itkGeodesicActiveContourShapePriorLevelSetImageFilterTest );
REGISTER_TEST(itkGeodesicActiveContourShapePriorLevelSetImageFilterTest_2 );
REGISTER_TEST(itkGradientVectorFlowImageFilterTest );
REGISTER_TEST(itkHistogramMatchingImageFilterTest );
REGISTER_TEST(itkHistogramImageToImageMetricTest );
REGISTER_TEST(itkImageMomentsTest );
REGISTER_TEST(itkImageRegistrationMethodTest     );
REGISTER_TEST(itkImageRegistrationMethodTest_1 );
REGISTER_TEST(itkImageRegistrationMethodTest_2 );
REGISTER_TEST(itkImageRegistrationMethodTest_3 );
REGISTER_TEST(itkImageRegistrationMethodTest_4 );
REGISTER_TEST(itkImageRegistrationMethodTest_5 );
REGISTER_TEST(itkImageRegistrationMethodTest_6 );
REGISTER_TEST(itkImageRegistrationMethodTest_7 );
REGISTER_TEST(itkImageRegistrationMethodTest_8 );
REGISTER_TEST(itkImageRegistrationMethodTest_9 );
REGISTER_TEST(itkImageRegistrationMethodTest_10);
REGISTER_TEST(itkImageRegistrationMethodTest_11);
REGISTER_TEST(itkImageRegistrationMethodTest_12);
REGISTER_TEST(itkImageRegistrationMethodTest_13);
REGISTER_TEST(itkImageRegistrationMethodTest_14);
REGISTER_TEST(itkImageRegistrationMethodTest_15);
REGISTER_TEST(itkImageRegistrationMethodTest_16);
REGISTER_TEST(itkInterpolateTest );
REGISTER_TEST(itkIsotropicFourthOrderLevelSetImageFilterTest );
REGISTER_TEST(itkIsoContourDistanceImageFilterTest );
REGISTER_TEST(itkKalmanLinearEstimatorTest );
REGISTER_TEST(itkKmeansModelEstimatorTest );
REGISTER_TEST(itkLaplacianSegmentationLevelSetImageFilterTest );
REGISTER_TEST(itkLevelSetNeighborhoodExtractorTest );
REGISTER_TEST(itkLevelSetVelocityNeighborhoodExtractorTest );
}

// WARNING: Do not add any more tests to this program. Add new tests
// to itkAlgorithmsTests2.cxx. This file has become too large for the
// Borland linker.
