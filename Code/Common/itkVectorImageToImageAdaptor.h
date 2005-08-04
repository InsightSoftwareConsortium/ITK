/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVectorImageToImageAdaptor.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkVectorImageToImageAdaptor_h
#define __itkVectorImageToImageAdaptor_h

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
 * \thanks
 * This work is part of the National Alliance for Medical Image Computing 
 * (NAMIC), funded by the National Institutes of Health through the NIH Roadmap
 * for Medical Research, Grant U54 EB005149.
 *
 * \ingroup ImageAdaptors
 */
template <class TType >
class ITK_EXPORT VectorImageToImagePixelAccessor  
{
public:

  typedef unsigned long VectorLengthType;
  
  /** External typedef. It defines the external aspect
   * that this class will exhibit. */
  typedef  TType                 ExternalType;

  /** Internal typedef. It defines the internal real
   * representation of data. */
  typedef Array< TType >         InternalType;

  /** The pixel type that TInternalContainerType holds */
  typedef TType                  PixelType;

  inline void Set(InternalType & output, const ExternalType & input) const
    { 
    output[m_ComponentIdx] = input;
    }

  inline ExternalType Get( const InternalType & input ) const
    {
    ExternalType output;
    output = input[m_ComponentIdx];
    return output;
    }

  void SetExtractComponentIdx( VectorLengthType idx )
    {
    m_ComponentIdx = idx;
    }
  VectorLengthType GetExtractComponentIdx() const
    {
    return m_ComponentIdx;
    }
  
  VectorImageToImagePixelAccessor() : m_ComponentIdx( 0 ) {};
  virtual ~VectorImageToImagePixelAccessor() {};
  
private:
  VectorLengthType m_ComponentIdx;
};

} // end namespace Accessor



/** \class VectorToRGBImageAdaptor
 * \brief Presents a VectorImage and extracts a component from it into an image.
 *
 * The class is expected to be templated over a pixel type and dimension. These
 * are the pixel types and dimension of the VectorImage.
 * 
 * The component to extract is set with SetExtractComponentIdx() method
 * RGBPixel type.
 *
 * \thanks
 * This work is part of the National Alliance for Medical Image Computing 
 * (NAMIC), funded by the National Institutes of Health through the NIH Roadmap
 * for Medical Research, Grant U54 EB005149.
 *
 * \ingroup ImageAdaptors
 *
 */
template<class TPixelType, unsigned int Dimension>
class ITK_EXPORT VectorImageToImageAdaptor : public
      ImageAdaptor< VectorImage< TPixelType, Dimension >,
        Accessor::VectorImageToImagePixelAccessor< TPixelType > >
{
public:
  /** Standard class typedefs. */
  typedef VectorImageToImageAdaptor    Self;
  typedef VectorImage< TPixelType, Dimension > VectorImageType;
  typedef ImageAdaptor< VectorImageType,
      Accessor::VectorImageToImagePixelAccessor< TPixelType >  > Superclass;
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
  
  /** Method for creation through the object factory. */
  itkNewMacro(Self);  

  /** Run-time type information (and related methods). */
  itkTypeMacro( VectorImageToImagePixelAccessor, ImageAdaptor );

  /** PixelContainer typedef support. Used to construct a container for
   * the pixel data. */
  typedef typename Superclass::PixelContainer        PixelContainer;
  typedef typename Superclass::PixelContainerPointer PixelContainerPointer;
  typedef typename Superclass::PixelContainerConstPointer PixelContainerConstPointer;

  // Set/GetMethods to set the component to be extracted.
  void SetExtractComponentIdx( unsigned long componentIdx )
    {
    this->GetPixelAccessor().SetExtractComponentIdx( componentIdx );
    }

  // Set/GetMethods to set the component to be extracted.
  unsigned long GetExtractComponentIdx() const
    {
    this->GetPixelAccessor().GetExtractComponentIdx( componentIdx );
    }
  
 protected:
  VectorImageToImageAdaptor() {}
  virtual ~VectorImageToImageAdaptor() {}
  
 private:
  VectorImageToImageAdaptor(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

};

  
} // end namespace itk
  
#endif
