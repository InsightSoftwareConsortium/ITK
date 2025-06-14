/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef itkMultiResolutionPDEDeformableRegistration_hxx
#define itkMultiResolutionPDEDeformableRegistration_hxx

#include "itkRecursiveGaussianImageFilter.h"
#include "itkRecursiveMultiResolutionPyramidImageFilter.h"
#include "itkImageRegionIterator.h"
#include "itkMath.h"

namespace itk
{

template <typename TFixedImage,
          typename TMovingImage,
          typename TDisplacementField,
          typename TRealType,
          typename TFloatImageType,
          typename TRegistrationType,
          typename TDefaultRegistrationType>
MultiResolutionPDEDeformableRegistration<TFixedImage,
                                         TMovingImage,
                                         TDisplacementField,
                                         TRealType,
                                         TFloatImageType,
                                         TRegistrationType,
                                         TDefaultRegistrationType>::MultiResolutionPDEDeformableRegistration()
{
  this->SetNumberOfRequiredInputs(2);
  // Primary input is optional in this filter
  this->RemoveRequiredInputName("Primary");

  auto registrator = DefaultRegistrationType::New();
  m_RegistrationFilter = registrator.GetPointer();

  m_MovingImagePyramid = MovingImagePyramidType::New();
  m_FixedImagePyramid = FixedImagePyramidType::New();
  m_FieldExpander = FieldExpanderType::New();
  m_InitialDisplacementField = nullptr;

  m_NumberOfLevels = 3;
  m_NumberOfIterations.SetSize(m_NumberOfLevels);
  m_FixedImagePyramid->SetNumberOfLevels(m_NumberOfLevels);
  m_MovingImagePyramid->SetNumberOfLevels(m_NumberOfLevels);

  for (unsigned int ilevel = 0; ilevel < m_NumberOfLevels; ++ilevel)
  {
    m_NumberOfIterations[ilevel] = 10;
  }
  m_CurrentLevel = 0;

  m_StopRegistrationFlag = false;
}

template <typename TFixedImage,
          typename TMovingImage,
          typename TDisplacementField,
          typename TRealType,
          typename TFloatImageType,
          typename TRegistrationType,
          typename TDefaultRegistrationType>
void
MultiResolutionPDEDeformableRegistration<TFixedImage,
                                         TMovingImage,
                                         TDisplacementField,
                                         TRealType,
                                         TFloatImageType,
                                         TRegistrationType,
                                         TDefaultRegistrationType>::SetMovingImage(const MovingImageType * ptr)
{
  this->ProcessObject::SetNthInput(2, const_cast<MovingImageType *>(ptr));
}

template <typename TFixedImage,
          typename TMovingImage,
          typename TDisplacementField,
          typename TRealType,
          typename TFloatImageType,
          typename TRegistrationType,
          typename TDefaultRegistrationType>
const typename MultiResolutionPDEDeformableRegistration<TFixedImage,
                                                        TMovingImage,
                                                        TDisplacementField,
                                                        TRealType,
                                                        TFloatImageType,
                                                        TRegistrationType,
                                                        TDefaultRegistrationType>::MovingImageType *
MultiResolutionPDEDeformableRegistration<TFixedImage,
                                         TMovingImage,
                                         TDisplacementField,
                                         TRealType,
                                         TFloatImageType,
                                         TRegistrationType,
                                         TDefaultRegistrationType>::GetMovingImage() const
{
  return dynamic_cast<const MovingImageType *>(this->ProcessObject::GetInput(2));
}

template <typename TFixedImage,
          typename TMovingImage,
          typename TDisplacementField,
          typename TRealType,
          typename TFloatImageType,
          typename TRegistrationType,
          typename TDefaultRegistrationType>
void
MultiResolutionPDEDeformableRegistration<TFixedImage,
                                         TMovingImage,
                                         TDisplacementField,
                                         TRealType,
                                         TFloatImageType,
                                         TRegistrationType,
                                         TDefaultRegistrationType>::SetFixedImage(const FixedImageType * ptr)
{
  this->ProcessObject::SetNthInput(1, const_cast<FixedImageType *>(ptr));
}

template <typename TFixedImage,
          typename TMovingImage,
          typename TDisplacementField,
          typename TRealType,
          typename TFloatImageType,
          typename TRegistrationType,
          typename TDefaultRegistrationType>
const typename MultiResolutionPDEDeformableRegistration<TFixedImage,
                                                        TMovingImage,
                                                        TDisplacementField,
                                                        TRealType,
                                                        TFloatImageType,
                                                        TRegistrationType,
                                                        TDefaultRegistrationType>::FixedImageType *
MultiResolutionPDEDeformableRegistration<TFixedImage,
                                         TMovingImage,
                                         TDisplacementField,
                                         TRealType,
                                         TFloatImageType,
                                         TRegistrationType,
                                         TDefaultRegistrationType>::GetFixedImage() const
{
  return dynamic_cast<const FixedImageType *>(this->ProcessObject::GetInput(1));
}

template <typename TFixedImage,
          typename TMovingImage,
          typename TDisplacementField,
          typename TRealType,
          typename TFloatImageType,
          typename TRegistrationType,
          typename TDefaultRegistrationType>
std::vector<SmartPointer<DataObject>>::size_type
MultiResolutionPDEDeformableRegistration<TFixedImage,
                                         TMovingImage,
                                         TDisplacementField,
                                         TRealType,
                                         TFloatImageType,
                                         TRegistrationType,
                                         TDefaultRegistrationType>::GetNumberOfValidRequiredInputs() const
{
  typename std::vector<SmartPointer<DataObject>>::size_type num = 0;

  if (this->GetFixedImage())
  {
    ++num;
  }

  if (this->GetMovingImage())
  {
    ++num;
  }

  return num;
}

template <typename TFixedImage,
          typename TMovingImage,
          typename TDisplacementField,
          typename TRealType,
          typename TFloatImageType,
          typename TRegistrationType,
          typename TDefaultRegistrationType>
void
MultiResolutionPDEDeformableRegistration<TFixedImage,
                                         TMovingImage,
                                         TDisplacementField,
                                         TRealType,
                                         TFloatImageType,
                                         TRegistrationType,
                                         TDefaultRegistrationType>::SetNumberOfIterations(NumberOfIterationsType
                                                                                            numberOfIterations)
{
  // In case numberOfIterations.GetSize() differs from this->m_numberOfLevels, first set the
  // latter to equal the former.
  this->SetNumberOfLevels(numberOfIterations.GetSize());
  // Now do the requested set
  if (this->m_NumberOfIterations != numberOfIterations)
  {
    this->m_NumberOfIterations = std::move(numberOfIterations);
    this->Modified();
  }
}

template <typename TFixedImage,
          typename TMovingImage,
          typename TDisplacementField,
          typename TRealType,
          typename TFloatImageType,
          typename TRegistrationType,
          typename TDefaultRegistrationType>
void
MultiResolutionPDEDeformableRegistration<TFixedImage,
                                         TMovingImage,
                                         TDisplacementField,
                                         TRealType,
                                         TFloatImageType,
                                         TRegistrationType,
                                         TDefaultRegistrationType>::SetNumberOfLevels(unsigned int num)
{
  if (m_NumberOfLevels != num)
  {
    this->Modified();
    m_NumberOfLevels = num;
    m_NumberOfIterations.SetSize(m_NumberOfLevels);
  }

  if (m_MovingImagePyramid && m_MovingImagePyramid->GetNumberOfLevels() != num)
  {
    m_MovingImagePyramid->SetNumberOfLevels(m_NumberOfLevels);
  }
  if (m_FixedImagePyramid && m_FixedImagePyramid->GetNumberOfLevels() != num)
  {
    m_FixedImagePyramid->SetNumberOfLevels(m_NumberOfLevels);
  }
}

template <typename TFixedImage,
          typename TMovingImage,
          typename TDisplacementField,
          typename TRealType,
          typename TFloatImageType,
          typename TRegistrationType,
          typename TDefaultRegistrationType>
void
MultiResolutionPDEDeformableRegistration<TFixedImage,
                                         TMovingImage,
                                         TDisplacementField,
                                         TRealType,
                                         TFloatImageType,
                                         TRegistrationType,
                                         TDefaultRegistrationType>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  itkPrintSelfObjectMacro(RegistrationFilter);
  itkPrintSelfObjectMacro(MovingImagePyramid);
  itkPrintSelfObjectMacro(FixedImagePyramid);
  itkPrintSelfObjectMacro(FieldExpander);
  itkPrintSelfObjectMacro(InitialDisplacementField);

  os << indent << "NumberOfLevels: " << m_NumberOfLevels << std::endl;
  os << indent << "CurrentLevel: " << m_CurrentLevel << std::endl;

  os << indent << "NumberOfIterations: [";
  unsigned int ilevel = 0;
  for (; ilevel < m_NumberOfLevels - 1; ++ilevel)
  {
    os << m_NumberOfIterations[ilevel] << ", ";
  }
  os << m_NumberOfIterations[ilevel] << ']' << std::endl;

  itkPrintSelfBooleanMacro(StopRegistrationFlag);
}

template <typename TFixedImage,
          typename TMovingImage,
          typename TDisplacementField,
          typename TRealType,
          typename TFloatImageType,
          typename TRegistrationType,
          typename TDefaultRegistrationType>
void
MultiResolutionPDEDeformableRegistration<TFixedImage,
                                         TMovingImage,
                                         TDisplacementField,
                                         TRealType,
                                         TFloatImageType,
                                         TRegistrationType,
                                         TDefaultRegistrationType>::GenerateData()
{
  // Check for nullptr images and pointers
  const MovingImageConstPointer movingImage = this->GetMovingImage();
  const FixedImageConstPointer  fixedImage = this->GetFixedImage();

  if (!movingImage || !fixedImage)
  {
    itkExceptionMacro("Fixed and/or moving image not set");
  }

  if (!m_MovingImagePyramid || !m_FixedImagePyramid)
  {
    itkExceptionMacro("Fixed and/or moving pyramid not set");
  }

  if (!m_RegistrationFilter)
  {
    itkExceptionMacro("Registration filter not set");
  }

  if (this->m_InitialDisplacementField && this->GetInput(0))
  {
    itkExceptionMacro("Only one initial deformation can be given. "
                      << "SetInitialDisplacementField should not be used in "
                      << "cunjunction with SetArbitraryInitialDisplacementField "
                      << "or SetInput.");
  }

  // as per suggestion in this bug report:
  // https://public.kitware.com/Bug/view.php?id=3590
  // this should allow input images to be released, since
  // they are no longer needed after generating the image pyramid
  this->RestoreInputReleaseDataFlags();

  // Create the image pyramids.
  m_MovingImagePyramid->SetInput(movingImage);
  m_MovingImagePyramid->UpdateLargestPossibleRegion();

  m_FixedImagePyramid->SetInput(fixedImage);
  m_FixedImagePyramid->UpdateLargestPossibleRegion();

  // Initializations
  m_CurrentLevel = 0;
  m_StopRegistrationFlag = false;

  unsigned int movingLevel =
    std::min(static_cast<int>(m_CurrentLevel), static_cast<int>(m_MovingImagePyramid->GetNumberOfLevels()));

  unsigned int fixedLevel =
    std::min(static_cast<int>(m_CurrentLevel), static_cast<int>(m_FixedImagePyramid->GetNumberOfLevels()));

  DisplacementFieldPointer tempField = nullptr;

  const DisplacementFieldPointer inputPtr = const_cast<DisplacementFieldType *>(this->GetInput(0));

  if (this->m_InitialDisplacementField)
  {
    tempField = this->m_InitialDisplacementField;
  }
  else if (inputPtr)
  {
    // Arbitrary initial deformation field is set.
    // smooth it and resample

    // First smooth it
    tempField = inputPtr;

    using GaussianFilterType = RecursiveGaussianImageFilter<DisplacementFieldType, DisplacementFieldType>;
    auto smoother = GaussianFilterType::New();

    for (unsigned int dim = 0; dim < DisplacementFieldType::ImageDimension; ++dim)
    {
      // sigma accounts for the subsampling of the pyramid
      double sigma = 0.5 * static_cast<float>(m_FixedImagePyramid->GetSchedule()[fixedLevel][dim]);

      // but also for a possible discrepancy in the spacing
      sigma *= fixedImage->GetSpacing()[dim] / inputPtr->GetSpacing()[dim];

      smoother->SetInput(tempField);
      smoother->SetSigma(sigma);
      smoother->SetDirection(dim);

      smoother->Update();

      tempField = smoother->GetOutput();
      tempField->DisconnectPipeline();
    }

    // Now resample
    m_FieldExpander->SetInput(tempField);

    const typename FloatImageType::Pointer fi = m_FixedImagePyramid->GetOutput(fixedLevel);
    m_FieldExpander->SetSize(fi->GetLargestPossibleRegion().GetSize());
    m_FieldExpander->SetOutputStartIndex(fi->GetLargestPossibleRegion().GetIndex());
    m_FieldExpander->SetOutputOrigin(fi->GetOrigin());
    m_FieldExpander->SetOutputSpacing(fi->GetSpacing());
    m_FieldExpander->SetOutputDirection(fi->GetDirection());

    m_FieldExpander->UpdateLargestPossibleRegion();
    m_FieldExpander->SetInput(nullptr);
    tempField = m_FieldExpander->GetOutput();
    tempField->DisconnectPipeline();
  }

  bool lastShrinkFactorsAllOnes = false;

  while (!this->Halt())
  {
    if (tempField.IsNull())
    {
      m_RegistrationFilter->SetInitialDisplacementField(nullptr);
    }
    else
    {
      // Resample the field to be the same size as the fixed image
      // at the current level
      m_FieldExpander->SetInput(tempField);

      const typename FloatImageType::Pointer fi = m_FixedImagePyramid->GetOutput(fixedLevel);
      m_FieldExpander->SetSize(fi->GetLargestPossibleRegion().GetSize());
      m_FieldExpander->SetOutputStartIndex(fi->GetLargestPossibleRegion().GetIndex());
      m_FieldExpander->SetOutputOrigin(fi->GetOrigin());
      m_FieldExpander->SetOutputSpacing(fi->GetSpacing());
      m_FieldExpander->SetOutputDirection(fi->GetDirection());

      m_FieldExpander->UpdateLargestPossibleRegion();
      m_FieldExpander->SetInput(nullptr);
      tempField = m_FieldExpander->GetOutput();
      tempField->DisconnectPipeline();

      m_RegistrationFilter->SetInitialDisplacementField(tempField);
    }

    // setup registration filter and pyramids
    m_RegistrationFilter->SetMovingImage(m_MovingImagePyramid->GetOutput(movingLevel));
    m_RegistrationFilter->SetFixedImage(m_FixedImagePyramid->GetOutput(fixedLevel));

    m_RegistrationFilter->SetNumberOfIterations(m_NumberOfIterations[m_CurrentLevel]);

    // cache shrink factors for computing the next expand factors.
    lastShrinkFactorsAllOnes = true;
    for (unsigned int idim = 0; idim < ImageDimension; ++idim)
    {
      if (m_FixedImagePyramid->GetSchedule()[fixedLevel][idim] > 1)
      {
        lastShrinkFactorsAllOnes = false;
        break;
      }
    }

    // compute new deformation field
    m_RegistrationFilter->UpdateLargestPossibleRegion();
    tempField = m_RegistrationFilter->GetOutput();
    tempField->DisconnectPipeline();

    // Increment level counter.
    ++m_CurrentLevel;
    movingLevel =
      std::min(static_cast<int>(m_CurrentLevel), static_cast<int>(m_MovingImagePyramid->GetNumberOfLevels()));
    fixedLevel = std::min(static_cast<int>(m_CurrentLevel), static_cast<int>(m_FixedImagePyramid->GetNumberOfLevels()));

    // Invoke an iteration event.
    this->InvokeEvent(MultiResolutionIterationEvent());

    // We can release data from pyramid which are no longer required.
    if (movingLevel > 0)
    {
      m_MovingImagePyramid->GetOutput(movingLevel - 1)->ReleaseData();
    }
    if (fixedLevel > 0)
    {
      m_FixedImagePyramid->GetOutput(fixedLevel - 1)->ReleaseData();
    }
  } // while not Halt()

  if (!lastShrinkFactorsAllOnes)
  {
    // Some of the last shrink factors are not one
    // graft the output of the expander filter to
    // to output of this filter

    // resample the field to the same size as the fixed image
    m_FieldExpander->SetInput(tempField);
    m_FieldExpander->SetSize(fixedImage->GetLargestPossibleRegion().GetSize());
    m_FieldExpander->SetOutputStartIndex(fixedImage->GetLargestPossibleRegion().GetIndex());
    m_FieldExpander->SetOutputOrigin(fixedImage->GetOrigin());
    m_FieldExpander->SetOutputSpacing(fixedImage->GetSpacing());
    m_FieldExpander->SetOutputDirection(fixedImage->GetDirection());

    m_FieldExpander->UpdateLargestPossibleRegion();
    this->GraftOutput(m_FieldExpander->GetOutput());
  }
  else
  {
    // all the last shrink factors are all ones
    // graft the output of registration filter to
    // to output of this filter
    this->GraftOutput(tempField);
  }

  // Release memory
  m_FieldExpander->SetInput(nullptr);
  m_FieldExpander->GetOutput()->ReleaseData();
  m_RegistrationFilter->SetInput(nullptr);
  m_RegistrationFilter->GetOutput()->ReleaseData();
}

template <typename TFixedImage,
          typename TMovingImage,
          typename TDisplacementField,
          typename TRealType,
          typename TFloatImageType,
          typename TRegistrationType,
          typename TDefaultRegistrationType>
void
MultiResolutionPDEDeformableRegistration<TFixedImage,
                                         TMovingImage,
                                         TDisplacementField,
                                         TRealType,
                                         TFloatImageType,
                                         TRegistrationType,
                                         TDefaultRegistrationType>::StopRegistration()
{
  m_RegistrationFilter->StopRegistration();
  m_StopRegistrationFlag = true;
}

template <typename TFixedImage,
          typename TMovingImage,
          typename TDisplacementField,
          typename TRealType,
          typename TFloatImageType,
          typename TRegistrationType,
          typename TDefaultRegistrationType>
bool
MultiResolutionPDEDeformableRegistration<TFixedImage,
                                         TMovingImage,
                                         TDisplacementField,
                                         TRealType,
                                         TFloatImageType,
                                         TRegistrationType,
                                         TDefaultRegistrationType>::Halt()
{
  // Halt the registration after the user-specified number of levels
  if (m_NumberOfLevels != 0)
  {
    this->UpdateProgress(static_cast<float>(m_CurrentLevel) / static_cast<float>(m_NumberOfLevels));
  }

  if (m_CurrentLevel >= m_NumberOfLevels)
  {
    return true;
  }
  if (m_StopRegistrationFlag)
  {
    return true;
  }

  return false;
}

template <typename TFixedImage,
          typename TMovingImage,
          typename TDisplacementField,
          typename TRealType,
          typename TFloatImageType,
          typename TRegistrationType,
          typename TDefaultRegistrationType>
void
MultiResolutionPDEDeformableRegistration<TFixedImage,
                                         TMovingImage,
                                         TDisplacementField,
                                         TRealType,
                                         TFloatImageType,
                                         TRegistrationType,
                                         TDefaultRegistrationType>::GenerateOutputInformation()
{
  typename DataObject::Pointer output;

  if (this->GetInput(0))
  {
    // Initial deformation field is set.
    // Copy information from initial field.
    this->Superclass::GenerateOutputInformation();
  }
  else if (this->GetFixedImage())
  {
    // Initial deformation field is not set.
    // Copy information from the fixed image.
    for (unsigned int idx = 0; idx < this->GetNumberOfIndexedOutputs(); ++idx)
    {
      output = this->GetOutput(idx);
      if (output)
      {
        output->CopyInformation(this->GetFixedImage());
      }
    }
  }
}

template <typename TFixedImage,
          typename TMovingImage,
          typename TDisplacementField,
          typename TRealType,
          typename TFloatImageType,
          typename TRegistrationType,
          typename TDefaultRegistrationType>
void
MultiResolutionPDEDeformableRegistration<TFixedImage,
                                         TMovingImage,
                                         TDisplacementField,
                                         TRealType,
                                         TFloatImageType,
                                         TRegistrationType,
                                         TDefaultRegistrationType>::GenerateInputRequestedRegion()
{
  // call the superclass's implementation
  Superclass::GenerateInputRequestedRegion();

  // request the largest possible region for the moving image
  const MovingImagePointer movingPtr = const_cast<MovingImageType *>(this->GetMovingImage());
  if (movingPtr)
  {
    movingPtr->SetRequestedRegionToLargestPossibleRegion();
  }

  // just propagate up the output requested region for
  // the fixed image and initial deformation field.
  const DisplacementFieldPointer inputPtr = const_cast<DisplacementFieldType *>(this->GetInput());
  const DisplacementFieldPointer outputPtr = this->GetOutput();
  const FixedImagePointer        fixedPtr = const_cast<FixedImageType *>(this->GetFixedImage());

  if (inputPtr)
  {
    inputPtr->SetRequestedRegion(outputPtr->GetRequestedRegion());
  }

  if (fixedPtr)
  {
    fixedPtr->SetRequestedRegion(outputPtr->GetRequestedRegion());
  }
}

template <typename TFixedImage,
          typename TMovingImage,
          typename TDisplacementField,
          typename TRealType,
          typename TFloatImageType,
          typename TRegistrationType,
          typename TDefaultRegistrationType>
void
MultiResolutionPDEDeformableRegistration<TFixedImage,
                                         TMovingImage,
                                         TDisplacementField,
                                         TRealType,
                                         TFloatImageType,
                                         TRegistrationType,
                                         TDefaultRegistrationType>::EnlargeOutputRequestedRegion(DataObject * ptr)
{
  // call the superclass's implementation
  Superclass::EnlargeOutputRequestedRegion(ptr);

  // set the output requested region to largest possible.
  auto * outputPtr = dynamic_cast<DisplacementFieldType *>(ptr);

  if (outputPtr)
  {
    outputPtr->SetRequestedRegionToLargestPossibleRegion();
  }
}
} // end namespace itk

#endif
