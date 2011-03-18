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

#include "itkGrayscaleFunctionDilateImageFilter.txx"
#include "itkMorphologyHistogram.h"
#include "itkBlackTopHatImageFilter.txx"
#include "itkGrayscaleDilateImageFilter.h"
#include "itkBinaryMorphologyImageFilter.txx"
#include "itkMovingHistogramDilateImageFilter.h"
#include "itkBasicErodeImageFilter.txx"
#include "itkReconstructionImageFilter.txx"
#include "itkHMaximaImageFilter.txx"
#include "itkMorphologyImageFilter.txx"
#include "itkWhiteTopHatImageFilter.txx"
#include "itkBinaryDilateImageFilter.txx"
#include "itkErodeObjectMorphologyImageFilter.txx"
#include "itkClosingByReconstructionImageFilter.txx"
#include "itkGrayscaleGrindPeakImageFilter.txx"
#include "itkGrayscaleGeodesicDilateImageFilter.txx"
#include "itkAnchorErodeDilateLine.txx"
#include "itkVanHerkGilWermanDilateImageFilter.h"
#include "itkDoubleThresholdImageFilter.txx"
#include "itkGrayscaleConnectedOpeningImageFilter.txx"
#include "itkReconstructionByDilationImageFilter.h"
#include "itkMorphologicalGradientImageFilter.txx"
#include "itkAnchorErodeDilateImageFilter.txx"
#include "itkGrayscaleFunctionErodeImageFilter.txx"
#include "itkHConcaveImageFilter.txx"
#include "itkGrayscaleFillholeImageFilter.txx"
#include "itkMovingHistogramErodeImageFilter.h"
#include "itkAnchorOpenImageFilter.h"
#include "itkAnchorOpenCloseImageFilter.txx"
#include "itkBinaryCrossStructuringElement.txx"
#include "itkAnchorErodeImageFilter.h"
#include "itkMovingHistogramImageFilterBase.txx"
#include "itkReconstructionByErosionImageFilter.h"
#include "itkMovingHistogramImageFilter.txx"
#include "itkVanHerkGilWermanUtilities.h"
#include "itkGrayscaleErodeImageFilter.txx"
#include "itkHMinimaImageFilter.h"
#include "itkBinaryThinningImageFilter.txx"
#include "itkDilateObjectMorphologyImageFilter.h"
#include "itkObjectMorphologyImageFilter.txx"
#include "itkMovingHistogramMorphologicalGradientImageFilter.h"
#include "itkGrayscaleMorphologicalClosingImageFilter.txx"
#include "itkVanHerkGilWermanUtilities.txx"
#include "itkFastIncrementalBinaryDilateImageFilter.h"
#include "itkAnchorUtilities.txx"
#include "itkDilateObjectMorphologyImageFilter.txx"
#include "itkMovingHistogramMorphologyImageFilter.h"
#include "itkAnchorOpenCloseLine.txx"
#include "itkHConvexImageFilter.txx"
#include "itkHMinimaImageFilter.txx"
#include "itkWhiteTopHatImageFilter.h"
#include "itkKernelImageFilter.h"
#include "itkVanHerkGilWermanErodeImageFilter.h"
#include "itkBinaryErodeImageFilter.h"
#include "itkGrayscaleMorphologicalOpeningImageFilter.txx"
#include "itkKernelImageFilter.txx"
#include "itkOpeningByReconstructionImageFilter.txx"
#include "itkBasicDilateImageFilter.txx"
#include "itkGrayscaleGeodesicErodeImageFilter.txx"
#include "itkSharedMorphologyUtilities.txx"
#include "itkVanHerkGilWermanErodeDilateImageFilter.txx"
#include "itkAnchorCloseImageFilter.h"
#include "itkFlatStructuringElement.txx"
#include "itkSharedMorphologyUtilities.h"
#include "itkAnchorErodeDilateImageFilter.h"
#include "itkBinaryBallStructuringElement.txx"
#include "itkMovingHistogramMorphologyImageFilter.txx"
#include "itkGrayscaleConnectedClosingImageFilter.txx"
#include "itkGrayscaleDilateImageFilter.txx"
#include "itkBinaryPruningImageFilter.txx"
#include "itkBinaryErodeImageFilter.txx"
#include "itkAnchorDilateImageFilter.h"



int itkMathematicalMorphologyHeaderTest ( int , char * [] )
{

  return EXIT_SUCCESS;
}
