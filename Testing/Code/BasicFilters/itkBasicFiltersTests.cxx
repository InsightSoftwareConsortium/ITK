/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBasicFiltersTests.cxx
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

// this file defines the itkBasicFiltersTest for the test driver
// and all it expects is that you have a function called RegisterTests
#include <iostream>
#include "itkTestMain.h" 


void RegisterTests()
{
  REGISTER_TEST(itkAbsImageFilterAndAdaptorTest );
  REGISTER_TEST(itkAbsoluteValueDifferenceImageFilterTest );
  REGISTER_TEST(itkAcosImageFilterAndAdaptorTest );
  REGISTER_TEST(itkAdaptImageFilterTest );
  REGISTER_TEST(itkAdaptImageFilterTest2 );
  REGISTER_TEST(itkAdaptiveHistogramEqualizationImageFilterTest);
  REGISTER_TEST(itkAddImageFilterTest );
  REGISTER_TEST(itkAndImageFilterTest );
  REGISTER_TEST(itkAsinImageFilterAndAdaptorTest );
  REGISTER_TEST(itkAtanImageFilterAndAdaptorTest );
  REGISTER_TEST(itkBSplineDecompositionImageFilterTest );
  REGISTER_TEST(itkBSplineInterpolateImageFunctionTest );
  REGISTER_TEST(itkBSplineResampleImageFilterTest );
  REGISTER_TEST(itkBSplineResampleImageFunctionTest );
  REGISTER_TEST(itkBasicArchitectureTest );
  REGISTER_TEST(itkBilateralImageFilterTest );
  REGISTER_TEST(itkBilateralImageFilterTest2 );
  REGISTER_TEST(itkBilateralImageFilterTest3 );
  REGISTER_TEST(itkBinaryDilateImageFilterTest );
  REGISTER_TEST(itkBinaryDilateImageFilterTest2 );
  REGISTER_TEST(itkBinaryErodeImageFilterTest );
  REGISTER_TEST(itkBinaryMagnitudeImageFilterTest );
  REGISTER_TEST(itkBinaryMaskToNarrowBandPointSetFilterTest);
  REGISTER_TEST(itkBinaryMedianImageFilterTest );
  REGISTER_TEST(itkBinaryThresholdImageFilterTest );
  REGISTER_TEST(itkBinaryThresholdImageFilterTest2 );
  REGISTER_TEST(itkBloxBoundaryPointImageTest );
  REGISTER_TEST(itkBloxBoundaryPointImageToBloxBoundaryProfileImageFilterTest );
  REGISTER_TEST(itkBloxBoundaryProfileImageToBloxCoreAtomImageFilterTest );
  REGISTER_TEST(itkBloxCoreAtomTest );
  REGISTER_TEST(itkCannyEdgeDetectionImageFilterTest );
  REGISTER_TEST(itkChainCodeToFourierSeriesPathFilterTest );
  REGISTER_TEST(itkChangeInformationImageFilterTest );
  REGISTER_TEST(itkChangeLabelImageFilterTest );
  REGISTER_TEST(itkCompose2DCovariantVectorImageFilterTest );
  REGISTER_TEST(itkCompose2DVectorImageFilterTest );
  REGISTER_TEST(itkCompose3DCovariantVectorImageFilterTest );
  REGISTER_TEST(itkCompose3DVectorImageFilterTest );
  REGISTER_TEST(itkComposeRGBImageFilterTest );
  REGISTER_TEST(itkConfidenceConnectedImageFilterTest  );
  REGISTER_TEST(itkConnectedComponentImageFilterTest );
  REGISTER_TEST(itkConnectedThresholdImageFilterTest  );
  REGISTER_TEST(itkConstantPadImageTest );
  REGISTER_TEST(itkCosImageFilterAndAdaptorTest );
  REGISTER_TEST(itkCropImageFilterTest );
  REGISTER_TEST(itkCurvatureAnisotropicDiffusionImageFilterTest );
  REGISTER_TEST(itkCyclicReferences );
  REGISTER_TEST(itkDanielssonDistanceMapImageFilterTest );
  REGISTER_TEST(itkDeformationFieldSourceTest );
  REGISTER_TEST(itkDerivativeImageFilterTest );
  REGISTER_TEST(itkDifferenceOfGaussiansGradientTest );
  REGISTER_TEST(itkDiffusionTensor3DReconstructionImageFilterTest);
  REGISTER_TEST(itkDiscreteGaussianImageFilterTest );
  REGISTER_TEST(itkDivideImageFilterTest );
  REGISTER_TEST(itkDoubleThresholdImageFilterTest );
  REGISTER_TEST(itkEdgePotentialImageFilterTest );
  REGISTER_TEST(itkEigenAnalysis2DImageFilterTest );
  REGISTER_TEST(itkExpImageFilterAndAdaptorTest );
  REGISTER_TEST(itkExpNegativeImageFilterAndAdaptorTest );
  REGISTER_TEST(itkExpandImageFilterTest );
  REGISTER_TEST(itkExtractImageTest );
  REGISTER_TEST(itkExtractOrthogonalSwath2DImageFilterTest );
  REGISTER_TEST(itkFilterDispatchTest );
  REGISTER_TEST(itkFlipImageFilterTest );
  REGISTER_TEST(itkFloodFillIteratorTest );
  REGISTER_TEST(itkGaussianImageSourceTest );
  REGISTER_TEST(itkGradientAnisotropicDiffusionImageFilterTest );
  REGISTER_TEST(itkGradientAnisotropicDiffusionImageFilterTest2 );
  REGISTER_TEST(itkGradientImageFilterTest );
  REGISTER_TEST(itkGradientMagnitudeImageFilterTest );
  REGISTER_TEST(itkGradientMagnitudeImageFilterTest );
  REGISTER_TEST(itkGradientMagnitudeRecursiveGaussianFilterTest );
  REGISTER_TEST(itkGradientMagnitudeRecursiveGaussianFilterTest );
  REGISTER_TEST(itkGradientRecursiveGaussianFilterTest );
  REGISTER_TEST(itkGradientToMagnitudeImageFilterTest );
  REGISTER_TEST(itkGradientToMagnitudeImageFilterTest );
  REGISTER_TEST(itkGrayscaleConnectedClosingImageFilterTest );
  REGISTER_TEST(itkGrayscaleConnectedOpeningImageFilterTest );
  REGISTER_TEST(itkGrayscaleFillholeImageFilterTest );
  REGISTER_TEST(itkGrayscaleFunctionDilateImageFilterTest );
  REGISTER_TEST(itkGrayscaleFunctionErodeImageFilterTest );
  REGISTER_TEST(itkHConvexConcaveImageFilterTest );
  REGISTER_TEST(itkHMaximaMinimaImageFilterTest );
  REGISTER_TEST(itkHardConnectedComponentImageFilterTest );
  REGISTER_TEST(itkHausdorffDistanceImageFilterTest );
  REGISTER_TEST(itkHoughTransform2DCirclesImageTest );
  REGISTER_TEST(itkHoughTransform2DLinesImageTest );
  REGISTER_TEST(itkImageAdaptorNthElementTest );
  REGISTER_TEST(itkImageAdaptorPipeLineTest );
  REGISTER_TEST(itkImageToMeshFilterTest );
  REGISTER_TEST(itkImageToParametricSpaceFilterTest );
  REGISTER_TEST(itkImplicitManifoldNormalVectorFilterTest );
  REGISTER_TEST(itkImportImageTest );
  REGISTER_TEST(itkIntensityWindowingImageFilterTest );
  REGISTER_TEST(itkInteriorExteriorMeshFilterTest  );
  REGISTER_TEST(itkInterpolateImageFilterTest );
  REGISTER_TEST(itkInterpolateImagePointsFilterTest );
  REGISTER_TEST(itkInverseDeformationFieldImageFilterTest );
  REGISTER_TEST(itkIsolatedConnectedImageFilterTest  );
  REGISTER_TEST(itkIterativeInverseDeformationFieldImageFilterTest );
  REGISTER_TEST(itkJoinImageFilterTest );
  REGISTER_TEST(itkLaplacianImageFilterTest );
  REGISTER_TEST(itkLaplacianRecursiveGaussianImageFilterTest );
  REGISTER_TEST(itkLog10ImageFilterAndAdaptorTest );
  REGISTER_TEST(itkLogImageFilterAndAdaptorTest );
  REGISTER_TEST(itkMaskImageFilterTest );
  REGISTER_TEST(itkMaskNegatedImageFilterTest );
  REGISTER_TEST(itkMathematicalMorphologyImageFilterTest );
  REGISTER_TEST(itkMaximumImageFilterTest );
  REGISTER_TEST(itkMeanImageFilterTest );
  REGISTER_TEST(itkMedianImageFilterTest );
  REGISTER_TEST(itkMinimumImageFilterTest );
  REGISTER_TEST(itkMinimumMaximumImageCalculatorTest );
  REGISTER_TEST(itkMinimumMaximumImageFilterTest );
  REGISTER_TEST(itkMirrorPadImageTest );
  REGISTER_TEST(itkMultiplyImageFilterTest );
  REGISTER_TEST(itkNarrowBandImageFilterBaseTest );
  REGISTER_TEST(itkNarrowBandTest );
  REGISTER_TEST(itkNaryAddImageFilterTest );
  REGISTER_TEST(itkNaryMaximumImageFilterTest );
  REGISTER_TEST(itkNeighborhoodConnectedImageFilterTest  );
  REGISTER_TEST(itkNeighborhoodOperatorImageFilterTest );
  REGISTER_TEST(itkNoiseImageFilterTest );
  REGISTER_TEST(itkNonThreadedShrinkImageTest );
  REGISTER_TEST(itkNormalizeImageFilterTest  );
  REGISTER_TEST(itkObjectMorphologyImageFilterTest );
  REGISTER_TEST(itkOrImageFilterTest );
  REGISTER_TEST(itkOrientImageFilterTest);
  REGISTER_TEST(itkOrientImageFilterTest2);
  REGISTER_TEST(itkParallelSparseFieldLevelSetImageFilterTest );
  REGISTER_TEST(itkPasteImageFilterTest );
  REGISTER_TEST(itkPathToChainCodePathFilterTest );
  REGISTER_TEST(itkPathToImageFilterTest );
  REGISTER_TEST(itkPermuteAxesImageFilterTest );
  REGISTER_TEST(itkPromoteDimensionImageTest);
  REGISTER_TEST(itkRGBToLuminanceImageFilterAndAdaptorTest );
  REGISTER_TEST(itkRGBToVectorAdaptImageFilterTest );
  REGISTER_TEST(itkRecursiveGaussianImageFiltersTest );
  REGISTER_TEST(itkRecursiveGaussianImageFiltersOnTensorsTest );
  REGISTER_TEST(itkReflectImageFilterTest );
  REGISTER_TEST(itkReflectiveImageRegionIteratorTest  );
  REGISTER_TEST(itkRegionOfInterestImageFilterTest  );
  REGISTER_TEST(itkRelabelComponentImageFilterTest );
  REGISTER_TEST(itkRemoveBoundaryObjectsTest );
  REGISTER_TEST(itkRemoveBoundaryObjectsTest2 );
  REGISTER_TEST(itkResampleImageTest );
  REGISTER_TEST(itkResamplePhasedArray3DSpecialCoordinatesImageTest );
  REGISTER_TEST(itkRescaleIntensityImageFilterTest );
}

