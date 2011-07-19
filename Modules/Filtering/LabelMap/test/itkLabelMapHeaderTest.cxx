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

#include "itkAggregateLabelMapFilter.hxx"
#include "itkAttributeKeepNObjectsLabelMapFilter.hxx"
#include "itkAttributeOpeningLabelMapFilter.hxx"
#include "itkAttributePositionLabelMapFilter.hxx"
#include "itkAttributeRelabelLabelMapFilter.hxx"
#include "itkAttributeSelectionLabelMapFilter.hxx"
#include "itkAttributeUniqueLabelMapFilter.hxx"
#include "itkAutoCropLabelMapFilter.hxx"
#include "itkBinaryClosingByReconstructionImageFilter.hxx"
#include "itkBinaryMorphologicalClosingImageFilter.hxx"
#include "itkBinaryMorphologicalOpeningImageFilter.hxx"
#include "itkBinaryFillholeImageFilter.hxx"
#include "itkBinaryGrindPeakImageFilter.hxx"
#include "itkBinaryImageToLabelMapFilter.hxx"
#include "itkBinaryImageToShapeLabelMapFilter.hxx"
#include "itkBinaryImageToStatisticsLabelMapFilter.hxx"
#include "itkBinaryOpeningByReconstructionImageFilter.hxx"
#include "itkBinaryReconstructionByDilationImageFilter.hxx"
#include "itkBinaryReconstructionByErosionImageFilter.hxx"
#include "itkBinaryReconstructionLabelMapFilter.hxx"
#include "itkBinaryShapeKeepNObjectsImageFilter.hxx"
#include "itkBinaryShapeOpeningImageFilter.hxx"
#include "itkBinaryStatisticsKeepNObjectsImageFilter.hxx"
#include "itkBinaryStatisticsOpeningImageFilter.hxx"
#include "itkChangeLabelLabelMapFilter.hxx"
#include "itkChangeRegionLabelMapFilter.hxx"
#include "itkConvertLabelMapFilter.hxx"
#include "itkCropLabelMapFilter.hxx"
#include "itkInPlaceLabelMapFilter.hxx"
#include "itkLabelImageToLabelMapFilter.hxx"
#include "itkLabelImageToShapeLabelMapFilter.hxx"
#include "itkLabelImageToStatisticsLabelMapFilter.hxx"
#include "itkLabelMapFilter.hxx"
#include "itkLabelMap.hxx"
#include "itkLabelMapMaskImageFilter.hxx"
#include "itkLabelMapToAttributeImageFilter.hxx"
#include "itkLabelMapToBinaryImageFilter.hxx"
#include "itkLabelMapToLabelImageFilter.hxx"
#include "itkLabelObject.hxx"
#include "itkLabelObjectLine.hxx"
#include "itkLabelShapeKeepNObjectsImageFilter.hxx"
#include "itkLabelShapeOpeningImageFilter.hxx"
#include "itkLabelStatisticsKeepNObjectsImageFilter.hxx"
#include "itkLabelStatisticsOpeningImageFilter.hxx"
#include "itkMergeLabelMapFilter.hxx"
#include "itkObjectByObjectLabelMapFilter.hxx"
#include "itkPadLabelMapFilter.hxx"
#include "itkRegionFromReferenceLabelMapFilter.hxx"
#include "itkShapeKeepNObjectsLabelMapFilter.hxx"
#include "itkShapeLabelMapFilter.hxx"
#include "itkShapeOpeningLabelMapFilter.hxx"
#include "itkShapePositionLabelMapFilter.hxx"
#include "itkShapeRelabelImageFilter.hxx"
#include "itkShapeRelabelLabelMapFilter.hxx"
#include "itkShapeUniqueLabelMapFilter.hxx"
#include "itkShiftScaleLabelMapFilter.hxx"
#include "itkStatisticsKeepNObjectsLabelMapFilter.hxx"
#include "itkStatisticsLabelMapFilter.hxx"
#include "itkStatisticsOpeningLabelMapFilter.hxx"
#include "itkStatisticsPositionLabelMapFilter.hxx"
#include "itkStatisticsRelabelImageFilter.hxx"
#include "itkStatisticsRelabelLabelMapFilter.hxx"
#include "itkStatisticsUniqueLabelMapFilter.hxx"
#include "itkGeometryUtilities.h"

int itkLabelMapHeaderTest ( int , char * [] )
{
  return EXIT_SUCCESS;
}
