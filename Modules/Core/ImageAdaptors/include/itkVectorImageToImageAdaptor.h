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
#ifndef itkVectorImageToImageAdaptor_h
#define itkVectorImageToImageAdaptor_h

#include "itkImageAdaptor.h"
#include "itkVectorImage.h"

namespace itk
{
namespace Accessor
{
/** \class VectorImageToImagePixelAccessor
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
template< typename TType >
class VectorImageToImagePixelAccessor
  : private DefaultVectorPixelAccessor< TType >
{
public:

  typedef unsigned int VectorLengthType;

  /** External typedef. It defines the external aspect
   * that this class will exhibit. */
  typedef  TType ExternalType;

  /** Internal typedef used by the ImageAdaptor for the buffer pointer */
  typedef TType InternalType;

  typedef VariableLengthVector< TType > ActualPixelType;

  inline void Set(ActualPixelType output, const ExternalType & input) const
  {
    output[m_ComponentIdx] = input;
  }

  inline void Set(InternalType &output, const ExternalType & input,
                  const unsigned long offset) const
  {
    return Set( Superclass::Get( output, offset ), input );
  }

  inline ExternalType Get(const ActualPixelType & input) const
  {
    ExternalType output;

    output = input[m_ComponentIdx];
    return output;
  }

  inline ExternalType Get(const InternalType &input, const SizeValueType offset) const
  {
    return Get( Superclass::Get(input, offset) );
  }

  void SetExtractComponentIdx(VectorLengthType idx)
  {
    m_ComponentIdx = idx;
  }

  VectorLengthType GetExtractComponentIdx() const
  {
    return m_ComponentIdx;
  }

  /** Set the length of each vector in the VectorImage */
  void SetVectorLength(VectorLengthType l)
  {
    Superclass::SetVectorLength( l );
  }

  /** Get Vector lengths */
  VectorLengthType GetVectorLength() const { return Superclass::GetVectorLength(); }

  VectorImageToImagePixelAccessor( unsigned int length = 1)
    :m_ComponentIdx(0)
    {
    Superclass::SetVectorLength( length );
    }

protected:
  typedef DefaultVectorPixelAccessor< TType > Superclass;

private:
  VectorLengthType m_ComponentIdx;
};
} // end namespace Accessor

/** \class VectorImageToImageAdaptor
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
 * \wiki
 * \wikiexample{VectorImages/VectorImageToImageAdaptor,View a component of a vector image as if it were a scalar image}
 * \endwiki
 */
template< typename TPixelType, unsigned int Dimension >
class VectorImageToImageAdaptor:public
  ImageAdaptor< VectorImage< TPixelType, Dimension >,
                Accessor::VectorImageToImagePixelAccessor< TPixelType > >
{
public:
  /** Standard class typedefs. */
  typedef VectorImageToImageAdaptor            Self;
  typedef VectorImage< TPixelType, Dimension > VectorImageType;
  typedef ImageAdaptor< VectorImageType,
                        Accessor::VectorImageToImagePixelAccessor< TPixelType >  > Superclass;

  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(VectorImageToImageAdaptor, ImageAdaptor);

  /** PixelContainer typedef support. Used to construct a container for
   * the pixel data. */
  typedef typename Superclass::PixelContainer             PixelContainer;
  typedef typename Superclass::PixelContainerPointer      PixelContainerPointer;
  typedef typename Superclass::PixelContainerConstPointer PixelContainerConstPointer;
  typedef typename Superclass::IOPixelType                IOPixelType;

  /** Typedef for the length of vectors in the VectorImage. */
  typedef typename VectorImageType::VectorLengthType VectorLengthType;

  // Set/GetMethods to set the component to be extracted.
  void SetExtractComponentIndex(VectorLengthType componentIdx)
  {
    this->GetPixelAccessor().SetExtractComponentIdx(componentIdx);
  }

  // Set/GetMethods to set the component to be extracted.
  VectorLengthType GetExtractComponentIndex() const
  {
    return this->GetPixelAccessor().GetExtractComponentIdx();
  }

protected:
  VectorImageToImageAdaptor() {}
  virtual ~VectorImageToImageAdaptor() ITK_OVERRIDE {}

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(VectorImageToImageAdaptor);
};
} // end namespace itk

#endif
