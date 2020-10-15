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
#ifndef itkVectorImageToImageAdaptor_h
#define itkVectorImageToImageAdaptor_h

#include "itkImageAdaptor.h"
#include "itkVectorImage.h"

namespace itk
{
namespace Accessor
{
/**
 *\class VectorImageToImagePixelAccessor
 * \brief Extract components from a VectorImage.
 *
 * This accessor is used to extract components from a VectorImage. It is used
 * from VectorImageComponentExtractAdaptor. The component to extract is set
 * using SetExtractComponentIdx.
 *
 * \note
 * This work is part of the National Alliance for Medical Image Computing
 * (NAMIC), funded by the National Institutes of Health through the NIH Roadmap
 * for Medical Research, Grant U54 EB005149.
 * \deprecated Please use the more generic NthElementImageAdaptor.
 *
 * \sa NthElementImageAdaptor
 *
 * \ingroup ImageAdaptors
 * \ingroup ITKImageAdaptors
 */
template <typename TType>
class VectorImageToImagePixelAccessor : private DefaultVectorPixelAccessor<TType>
{
public:
  using VectorLengthType = unsigned int;

  /** External type alias. It defines the external aspect
   * that this class will exhibit. */
  using ExternalType = TType;

  /** Internal type alias used by the ImageAdaptor for the buffer pointer */
  using InternalType = TType;

  using ActualPixelType = VariableLengthVector<TType>;

  inline void
  Set(ActualPixelType output, const ExternalType & input) const
  {
    output[m_ComponentIdx] = input;
  }

  inline void
  Set(InternalType & output, const ExternalType & input, const unsigned long offset) const
  {
    return Set(Superclass::Get(output, offset), input);
  }

  inline ExternalType
  Get(const ActualPixelType & input) const
  {
    ExternalType output;

    output = input[m_ComponentIdx];
    return output;
  }

  inline ExternalType
  Get(const InternalType & input, const SizeValueType offset) const
  {
    return Get(Superclass::Get(input, offset));
  }

  void
  SetExtractComponentIdx(VectorLengthType idx)
  {
    m_ComponentIdx = idx;
  }

  VectorLengthType
  GetExtractComponentIdx() const
  {
    return m_ComponentIdx;
  }

  /** Set the length of each vector in the VectorImage */
  void
  SetVectorLength(VectorLengthType l)
  {
    Superclass::SetVectorLength(l);
  }

  /** Get Vector lengths */
  VectorLengthType
  GetVectorLength() const
  {
    return Superclass::GetVectorLength();
  }

  VectorImageToImagePixelAccessor(unsigned int length = 1) { Superclass::SetVectorLength(length); }

protected:
  using Superclass = DefaultVectorPixelAccessor<TType>;

private:
  VectorLengthType m_ComponentIdx{ 0 };
};
} // end namespace Accessor

/**
 *\class VectorImageToImageAdaptor
 * \brief Presents a VectorImage and extracts a component from it into an image.
 *
 * The class is expected to be templated over a pixel type and dimension. These
 * are the pixel types and dimension of the VectorImage.
 *
 * The component to extract is set with SetExtractComponentIdx() method.
 *
 * \note
 * This work is part of the National Alliance for Medical Image Computing
 * (NAMIC), funded by the National Institutes of Health through the NIH Roadmap
 * for Medical Research, Grant U54 EB005149.
 *
 * \ingroup ImageAdaptors
 *
 * \ingroup ITKImageAdaptors
 *
 * \sphinx
 * \sphinxexample{Core/ImageAdaptors/ViewComponentVectorImageAsScaleImage,View Component Vector Image As Scalar Image}
 * \endsphinx
 */
template <typename TPixelType, unsigned int Dimension>
class VectorImageToImageAdaptor
  : public ImageAdaptor<VectorImage<TPixelType, Dimension>, Accessor::VectorImageToImagePixelAccessor<TPixelType>>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(VectorImageToImageAdaptor);

  /** Standard class type aliases. */
  using Self = VectorImageToImageAdaptor;
  using VectorImageType = VectorImage<TPixelType, Dimension>;
  using Superclass = ImageAdaptor<VectorImageType, Accessor::VectorImageToImagePixelAccessor<TPixelType>>;

  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(VectorImageToImageAdaptor, ImageAdaptor);

  /** PixelContainer type alias support Used to construct a container for
   * the pixel data. */
  using PixelContainer = typename Superclass::PixelContainer;
  using PixelContainerPointer = typename Superclass::PixelContainerPointer;
  using PixelContainerConstPointer = typename Superclass::PixelContainerConstPointer;
  using IOPixelType = typename Superclass::IOPixelType;

  /** Typedef for the length of vectors in the VectorImage. */
  using VectorLengthType = typename VectorImageType::VectorLengthType;

  // Set/GetMethods to set the component to be extracted.
  void
  SetExtractComponentIndex(VectorLengthType componentIdx)
  {
    this->GetPixelAccessor().SetExtractComponentIdx(componentIdx);
  }

  // Set/GetMethods to set the component to be extracted.
  VectorLengthType
  GetExtractComponentIndex() const
  {
    return this->GetPixelAccessor().GetExtractComponentIdx();
  }

protected:
  VectorImageToImageAdaptor() = default;
  ~VectorImageToImageAdaptor() override = default;
};
} // end namespace itk

#endif
