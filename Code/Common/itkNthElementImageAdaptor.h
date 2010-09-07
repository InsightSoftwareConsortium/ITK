/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkNthElementImageAdaptor.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkNthElementImageAdaptor_h
#define __itkNthElementImageAdaptor_h

#include <itkImageAdaptor.h>
#include <itkNthElementPixelAccessor.h>

namespace itk
{
/** \class NthElementImageAdaptor
 * \brief Presents an image as being composed of the N-th element of its pixels
 *
 * It assumes that the pixels are of container type and have in their API
 * an operator[]( unsigned int ) defined.
 *
 * Additional casting is performed according to the input and output image
 * types following C++ default casting rules.
 *
 * \ingroup ImageAdaptors
 */

// Create a helper class to help the SunPro CC compiler
// parse the templates for the NthElementImageAdaptor.
// This is used to define the Super class.  for NthElementImageAdaptor
template< class TImage, class TOutputPixelType >
class ITK_EXPORT NthElementImageAdaptorHelper
{
public:
  typedef  NthElementPixelAccessor<
    TOutputPixelType,
    typename TImage::PixelType > PixelAccessor;

  typedef  ImageAdaptor< TImage, PixelAccessor > Super;
};

template< class TImage, class TOutputPixelType >
class ITK_EXPORT NthElementImageAdaptor:
  public NthElementImageAdaptorHelper< TImage, TOutputPixelType >::Super
{
public:
  /** Standard class typedefs. */
  typedef NthElementImageAdaptor                                                   Self;
  typedef typename NthElementImageAdaptorHelper< TImage, TOutputPixelType >::Super Superclass;
  typedef SmartPointer< Self >                                                     Pointer;
  typedef SmartPointer< const Self >                                               ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(NthElementImageAdaptor, ImageAdaptor);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Select the element number to be accessed */
  void SelectNthElement(unsigned int nth)
  {
    this->GetPixelAccessor().SetElementNumber(nth);
    this->Modified();
  }

protected:
  NthElementImageAdaptor() {}
  virtual ~NthElementImageAdaptor() {}
private:
  NthElementImageAdaptor(const Self &); //purposely not implemented
  void operator=(const Self &);         //purposely not implemented
};
} // end namespace itk

#endif
