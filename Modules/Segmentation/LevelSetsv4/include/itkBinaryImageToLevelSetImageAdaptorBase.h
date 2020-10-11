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

#ifndef itkBinaryImageToLevelSetImageAdaptorBase_h
#define itkBinaryImageToLevelSetImageAdaptorBase_h

#include "itkImage.h"
#include "itkObject.h"

namespace itk
{
/**
 *\class BinaryImageToLevelSetImageAdaptorBase
 *  \ingroup ITKLevelSetsv4
 */
template <typename TInputImage, typename TLevelSet>
class BinaryImageToLevelSetImageAdaptorBase : public Object
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(BinaryImageToLevelSetImageAdaptorBase);

  using Self = BinaryImageToLevelSetImageAdaptorBase;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;
  using Superclass = Object;

  /** Run-time type information */
  itkTypeMacro(BinaryImageToLevelSetImageAdaptorBase, Object);

  using InputImageType = TInputImage;
  using InputImagePixelType = typename InputImageType::PixelType;
  using InputImageIndexType = typename InputImageType::IndexType;
  using InputImagePointer = typename InputImageType::Pointer;
  using InputImageRegionType = typename InputImageType::RegionType;
  using InputPixelRealType = typename NumericTraits<InputImagePixelType>::RealType;

  static constexpr unsigned int ImageDimension = InputImageType::ImageDimension;

  using LevelSetType = TLevelSet;
  using LevelSetPointer = typename LevelSetType::Pointer;

  /**
   * Input is a binary image m_InputImage
   * Output is a WhitakerSparseLevelSetImagePointer  */
  virtual void
  Initialize() = 0;

  /** Get the sparse levet set function */
  itkGetModifiableObjectMacro(LevelSet, LevelSetType);

  /** Set/Get the input image*/
  itkSetObjectMacro(InputImage, InputImageType);
  itkGetModifiableObjectMacro(InputImage, InputImageType);

protected:
  /** Constructor */
  BinaryImageToLevelSetImageAdaptorBase() { this->m_LevelSet = LevelSetType::New(); }

  /** Destructor */
  ~BinaryImageToLevelSetImageAdaptorBase() override = default;

  InputImagePointer m_InputImage;
  LevelSetPointer   m_LevelSet;
};
} // namespace itk

#endif // itkBinaryImageToLevelSetImageAdaptorBase_h
