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
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif

#include <iostream>

#include "itkGrayscaleFunctionDilateImageFilter.hxx"
#include "itkMorphologyHistogram.h"
#include "itkBlackTopHatImageFilter.hxx"
#include "itkGrayscaleDilateImageFilter.h"
#include "itkBinaryMorphologyImageFilter.hxx"
#include "itkMovingHistogramDilateImageFilter.h"
#include "itkBasicErodeImageFilter.hxx"
#include "itkReconstructionImageFilter.hxx"
#include "itkHMaximaImageFilter.hxx"
#include "itkMorphologyImageFilter.hxx"
#include "itkWhiteTopHatImageFilter.hxx"
#include "itkBinaryDilateImageFilter.hxx"
#include "itkErodeObjectMorphologyImageFilter.hxx"
#include "itkClosingByReconstructionImageFilter.hxx"
#include "itkGrayscaleGrindPeakImageFilter.hxx"
#include "itkGrayscaleGeodesicDilateImageFilter.hxx"
#include "itkAnchorErodeDilateLine.hxx"
#include "itkVanHerkGilWermanDilateImageFilter.h"
#include "itkDoubleThresholdImageFilter.hxx"
#include "itkGrayscaleConnectedOpeningImageFilter.hxx"
#include "itkReconstructionByDilationImageFilter.h"
#include "itkMorphologicalGradientImageFilter.hxx"
#include "itkAnchorErodeDilateImageFilter.hxx"
#include "itkGrayscaleFunctionErodeImageFilter.hxx"
#include "itkHConcaveImageFilter.hxx"
#include "itkGrayscaleFillholeImageFilter.hxx"
#include "itkMovingHistogramErodeImageFilter.h"
#include "itkAnchorOpenImageFilter.h"
#include "itkAnchorOpenCloseImageFilter.hxx"
#include "itkBinaryCrossStructuringElement.hxx"
#include "itkAnchorErodeImageFilter.h"
#include "itkMovingHistogramImageFilterBase.hxx"
#include "itkReconstructionByErosionImageFilter.h"
#include "itkMovingHistogramImageFilter.hxx"
#include "itkVanHerkGilWermanUtilities.h"
#include "itkGrayscaleErodeImageFilter.hxx"
#include "itkHMinimaImageFilter.h"
#include "itkBinaryThinningImageFilter.hxx"
#include "itkDilateObjectMorphologyImageFilter.h"
#include "itkObjectMorphologyImageFilter.hxx"
#include "itkMovingHistogramMorphologicalGradientImageFilter.h"
#include "itkGrayscaleMorphologicalClosingImageFilter.hxx"
#include "itkVanHerkGilWermanUtilities.hxx"
#include "itkFastIncrementalBinaryDilateImageFilter.h"
#include "itkAnchorUtilities.hxx"
#include "itkDilateObjectMorphologyImageFilter.hxx"
#include "itkMovingHistogramMorphologyImageFilter.h"
#include "itkAnchorOpenCloseLine.hxx"
#include "itkHConvexImageFilter.hxx"
#include "itkHMinimaImageFilter.hxx"
#include "itkWhiteTopHatImageFilter.h"
#include "itkKernelImageFilter.h"
#include "itkVanHerkGilWermanErodeImageFilter.h"
#include "itkBinaryErodeImageFilter.h"
#include "itkGrayscaleMorphologicalOpeningImageFilter.hxx"
#include "itkKernelImageFilter.hxx"
#include "itkOpeningByReconstructionImageFilter.hxx"
#include "itkBasicDilateImageFilter.hxx"
#include "itkGrayscaleGeodesicErodeImageFilter.hxx"
#include "itkSharedMorphologyUtilities.hxx"
#include "itkVanHerkGilWermanErodeDilateImageFilter.hxx"
#include "itkAnchorCloseImageFilter.h"
#include "itkFlatStructuringElement.hxx"
#include "itkSharedMorphologyUtilities.h"
#include "itkAnchorErodeDilateImageFilter.h"
#include "itkBinaryBallStructuringElement.hxx"
#include "itkMovingHistogramMorphologyImageFilter.hxx"
#include "itkGrayscaleConnectedClosingImageFilter.hxx"
#include "itkGrayscaleDilateImageFilter.hxx"
#include "itkBinaryPruningImageFilter.hxx"
#include "itkBinaryErodeImageFilter.hxx"
#include "itkAnchorDilateImageFilter.h"



int itkMathematicalMorphologyHeaderTest ( int , char * [] )
{

  return EXIT_SUCCESS;
}
