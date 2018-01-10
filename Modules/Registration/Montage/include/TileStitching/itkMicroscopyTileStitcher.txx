/*=========================================================================
 *
 *  Copyright Kitware Inc.
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

#include "itkPasteImageFilter.h"
#include "itkPhaseCorrelationImageRegistrationMethod.h"
#include "itkPhaseCorrelationOperator.h"
#include "itkMaxPhaseCorrelationOptimizer.h"
#include "itkRegisterThresholdedImageFilter.h"

#include "vtkMicroscopyTileStitcherConfig.h"

namespace itk {

//----------------------------------------------------------------------------
template <class TPixelType, unsigned int VImageDimension>
MicroscopyTileStitcher<TPixelType, VImageDimension>::MicroscopyTileStitcher()
{
  this->m_PreprocessFlag = false;
}

//----------------------------------------------------------------------------
template <class TPixelType, unsigned int VImageDimension>
MicroscopyTileStitcher<TPixelType, VImageDimension>::~MicroscopyTileStitcher()
{
}

//----------------------------------------------------------------------------
template <class TPixelType, unsigned int VImageDimension>
const typename MicroscopyTileStitcher<TPixelType, VImageDimension>::StitchingParameterType &
MicroscopyTileStitcher<TPixelType, VImageDimension>::GetStitchingParameters() const
{
  return this->m_StitchingParameters;
}

//----------------------------------------------------------------------------
template <class TPixelType, unsigned int VImageDimension>
int MicroscopyTileStitcher<TPixelType, VImageDimension>
::ComputeOverlapImageRegion(ImageRegionType & fixedOverlapRegion,
                            ImageRegionType & movingOverlapRegion)
{
  ImagePointType origin1 = this->m_FixedImage->GetOrigin();
  ImagePointType origin2 = this->m_MovingImage->GetOrigin();
  ImageSpacingType spacing1 = this->m_FixedImage->GetSpacing();
  ImageSpacingType spacing2 = this->m_MovingImage->GetSpacing();
  ImageRegionType region1 = this->m_FixedImage->GetLargestPossibleRegion();
  ImageRegionType region2 = this->m_MovingImage->GetLargestPossibleRegion();
  ImageSizeType size1 = region1.GetSize();
  ImageSizeType size2 = region2.GetSize();
  ImagePointType corner1, corner2, min1, max1, min2, max2, origin, corner;

  const unsigned int dimension = this->m_FixedImage->ImageDimension;
  for (unsigned int i = 0; i < dimension; i++)
    {
    corner1[i] = origin1[i] + spacing1[i] * (size1[i] - 1);
    corner2[i] = origin2[i] + spacing2[i] * (size2[i] - 1);
    min1[i] = corner1[i] > origin1[i] ? origin1[i] : corner1[i];
    max1[i] = corner1[i] < origin1[i] ? origin1[i] : corner1[i];
    min2[i] = corner2[i] > origin2[i] ? origin2[i] : corner2[i];
    max2[i] = corner2[i] < origin2[i] ? origin2[i] : corner2[i];
    origin[i] = min1[i] < min2[i] ? min2[i] : min1[i];
    corner[i] = max1[i] > max2[i] ? max2[i] : max1[i];
    if (origin[i] > corner[i])
      {
      return 0;
      }
    }

  ImageIndexType start1, end1;
  ImageSizeType overlapSize1;
  this->m_FixedImage->TransformPhysicalPointToIndex(origin, start1);
  this->m_FixedImage->TransformPhysicalPointToIndex(corner, end1);

  for (unsigned int i = 0; i < dimension; i++)
    {
    overlapSize1[i] = abs(end1[i] - start1[i]);
    }
  fixedOverlapRegion.SetIndex(start1);
  fixedOverlapRegion.SetSize(overlapSize1);

  ImageIndexType start2, end2;
  ImageSizeType overlapSize2;
  this->m_MovingImage->TransformPhysicalPointToIndex(origin, start2);
  this->m_MovingImage->TransformPhysicalPointToIndex(corner, end2);
  for (unsigned int i = 0; i < dimension; i++)
    {
    overlapSize2[i] = abs(end2[i] - start2[i]);
    }
  movingOverlapRegion.SetIndex(start2);
  movingOverlapRegion.SetSize(overlapSize2);

  return 1;
}

//----------------------------------------------------------------------------
template <class TPixelType, unsigned int VImageDimension>
int MicroscopyTileStitcher<TPixelType, VImageDimension>::
RegisterThresholdedImages(ImagePointerType fixedImage,
                                       ImagePointerType movingImage)
{
  typedef itk::RegisterThresholdedImageFilter<TPixelType,VImageDimension>
                                                              FilterType;
  typedef typename FilterType::Pointer                        FilterPointerType;
  typedef typename FilterType::ParametersType                 FilterParametersType;
  FilterPointerType filter = FilterType::New();
  filter->SetFixedImage(fixedImage);
  filter->SetMovingImage(movingImage);
  filter->SetPreprocessFlag(this->GetPreprocessFlag());
  try
    {
    filter->Update();
    }
  catch( itk::ExceptionObject & excep )
    {
    std::cout << "Exception caught during registration. " << excep << std::endl;
    return 0;
    }

  FilterParametersType parameters = filter->GetOutputParameters();
  for (size_t i = 0; i < parameters.Size(); i++)
    {
    this->m_StitchingParameters.push_back(parameters[i]);
    }
  return 1;
}

//----------------------------------------------------------------------------
template <class TPixelType, unsigned int VImageDimension>
int MicroscopyTileStitcher<TPixelType, VImageDimension>::
RegisterWithPhaseCorrelationFilter(ImagePointerType fixedImage,
                                   ImagePointerType movingImage)
{
  typedef itk::PhaseCorrelationImageRegistrationMethod<ImageType,ImageType>
                                                               RegistrationType;
  typedef typename RegistrationType::Pointer                   RegistrationPointerType;
  typedef typename RegistrationType::ParametersType            RegistrationParametersType;
  typedef itk::PhaseCorrelationOperator<RegistrationType>      OperatorType;
  typedef typename OperatorType::Pointer                       OperatorPointerType;
  typedef itk::MaxPhaseCorrelationOptimizer<RegistrationType>  OptimizerType;
  typedef typename OptimizerType::Pointer                      OptimizerPointerType;

  //
  // setup registration filter
  //
  RegistrationPointerType pcmRegistration  = RegistrationType::New();
  OperatorPointerType     pcmOperator      = OperatorType::New();
  OptimizerPointerType    pcmOptimizer     = OptimizerType::New();

  pcmRegistration->SetOperator(pcmOperator);
  pcmRegistration->SetOptimizer(pcmOptimizer);

  try
    {
    pcmRegistration->SetFixedImage(fixedImage);
    pcmRegistration->SetMovingImage(movingImage);
    pcmRegistration->Update();
    }
  catch( itk::ExceptionObject & excep )
    {
    std::cout << "Exception caught during phase correlation registratoin."
               << excep << std::endl;
    return 0;
    }

  RegistrationParametersType parameters = pcmRegistration->GetTransformParameters();
  for (size_t i = 0; i < parameters.Size(); i++)
    {
    this->m_StitchingParameters.push_back(parameters[i]);
    }
  return 1;
}

//----------------------------------------------------------------------------
template <class TPixelType, unsigned int VImageDimension>
void MicroscopyTileStitcher<TPixelType, VImageDimension>::Update()
{
  this->m_StitchingParameters.clear();

  // compute overlapping regions
  ImageRegionType overlapRegion1;
  ImageRegionType overlapRegion2;
  if (!this->ComputeOverlapImageRegion(overlapRegion1, overlapRegion2))
    {
    itkExceptionMacro("Error computing overlapping region.");
    }

  const unsigned int dimension = this->m_FixedImage->ImageDimension;
  ImageIndexType zeroIndex;
  for (unsigned int i = 0; i < dimension; i++)
    {
    zeroIndex[i] = 0;
    }

  ImageSizeType size1 = this->m_FixedImage->GetLargestPossibleRegion().GetSize();
  ImageSizeType size2 = this->m_MovingImage->GetLargestPossibleRegion().GetSize();
  ImageSpacingType imageSpacing1 = this->m_FixedImage->GetSpacing();
  ImageSpacingType imageSpacing2 = this->m_MovingImage->GetSpacing();

  ImageSizeType overlapSize1 = overlapRegion1.GetSize();
  ImageSizeType overlapSize2 = overlapRegion2.GetSize();
  ImageIndexType overlapIndex1 = overlapRegion1.GetIndex();
  ImageIndexType overlapIndex2 = overlapRegion2.GetIndex();

  ImageRegionType newImageRegion1;
  newImageRegion1.SetIndex(zeroIndex);
  newImageRegion1.SetSize(overlapSize1);

  ImageRegionType newImageRegion2;
  newImageRegion2.SetIndex(zeroIndex);
  newImageRegion2.SetSize(overlapSize2);

  ImagePointerType newImage1 = ImageType::New();
  newImage1->SetRegions(newImageRegion1);
  newImage1->SetSpacing(imageSpacing1);
  newImage1->Allocate();

  ImagePointerType newImage2 = ImageType::New();
  newImage2->SetRegions(newImageRegion2);
  newImage2->SetSpacing(imageSpacing2);
  newImage2->Allocate();

  typedef itk::PasteImageFilter<ImageType, ImageType, ImageType> PasterFilterType;
  typedef typename PasterFilterType::Pointer                     PasterFilterPointerType;

  // generate copies of overlapping regions
  PasterFilterPointerType pasteFilter1 = PasterFilterType::New();
  pasteFilter1->SetInput(newImage1);
  pasteFilter1->SetInPlace(false);
  pasteFilter1->SetDestinationIndex(zeroIndex);
  pasteFilter1->SetSourceImage(this->m_FixedImage);
  pasteFilter1->SetSourceRegion(overlapRegion1);
  pasteFilter1->Update();

  PasterFilterPointerType pasteFilter2 = PasterFilterType::New();
  pasteFilter2->SetInput(newImage2);
  pasteFilter2->SetInPlace(false);
  pasteFilter2->SetDestinationIndex(zeroIndex);
  pasteFilter2->SetSourceImage(this->m_MovingImage);
  pasteFilter2->SetSourceRegion(overlapRegion2);
  pasteFilter2->Update();

  // register overlapping regions
  int ret = 0;
  switch (vtkMicroscopyTileStitcherConfig::GetInstance()->GetRegistrationMethod())
    {
    case vtkMicroscopyTileStitcherConfig::PHASE_CORRELATION:
      ret = this->RegisterWithPhaseCorrelationFilter(pasteFilter1->GetOutput(),
                                                     pasteFilter2->GetOutput());
      break;
    case vtkMicroscopyTileStitcherConfig::THRESHOLD_COMPONENT:
      ret = this->RegisterThresholdedImages(pasteFilter1->GetOutput(),
                                            pasteFilter2->GetOutput());
      break;
    default:
      std::cout << "Registration method undefined for stitcher." << std::endl;
      this->m_StitchingParameters.clear();
    }
  if (!ret)
    {
    itkExceptionMacro("Error registering the estimated overlapping regions.");
    }
  for (size_t i = 0; i < this->m_StitchingParameters.size(); i++)
    {
    std::cout << this->m_StitchingParameters[i] << " ";
    }
  std::cout << std::endl;
}

}
