/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkOrientedImage.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkOrientedImage_h
#define __itkOrientedImage_h

#include "itkImage.h"

namespace itk
{

/** \class OrientedImage
 *  \brief Templated n-dimensional oriented image class.
 *
 * \note
 * This work is part of the National Alliance for Medical Image Computing 
 * (NAMIC), funded by the National Institutes of Health through the NIH Roadmap
 * for Medical Research, Grant U54 EB005149.
 *
 * \ingroup ImageObjects */
template <class TPixel, unsigned int VImageDimension>
class ITK_EXPORT OrientedImage : public Image<TPixel,VImageDimension>
{
public:
  /** Standard class typedefs */
  typedef OrientedImage                   Self;
  typedef Image<TPixel,VImageDimension>   Superclass;
  typedef SmartPointer<Self>              Pointer;
  typedef SmartPointer<const Self>        ConstPointer;
  typedef WeakPointer<const Self>         ConstWeakPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(OrientedImage, Image);

  /** Index typedef support. An index is used to access pixel values. */
  typedef typename Superclass::IndexType  IndexType;

  /** Direction typedef support. The direction cosines of the image. */
  typedef typename Superclass::DirectionType  DirectionType;

  /** Spacing typedef support.  Spacing holds the size of a pixel.  The
   * spacing is the geometric distance between image samples. */
  typedef typename Superclass::SpacingType SpacingType;

  typedef typename Superclass::AccessorType        AccessorType;
  typedef typename Superclass::AccessorFunctorType AccessorFunctorType;
  typedef typename Superclass::IOPixelType         IOPixelType;

  /** Typedef for the functor used to access a neighborhood of pixel
   * pointers. */
  typedef NeighborhoodAccessorFunctor< Self >      NeighborhoodAccessorFunctorType;

  /** Return the NeighborhoodAccessor functor. This method is called by the 
   * neighborhood iterators. */
  NeighborhoodAccessorFunctorType GetNeighborhoodAccessor() 
    { return NeighborhoodAccessorFunctorType(); }
  
  /** Return the NeighborhoodAccessor functor. This method is called by the 
   * neighborhood iterators. */
  const NeighborhoodAccessorFunctorType GetNeighborhoodAccessor() const
    { return NeighborhoodAccessorFunctorType(); }
  


protected:
  OrientedImage();
  virtual ~OrientedImage() {};

  /** Compute helper matrices used to transform Index coordinates to
   * PhysicalPoint coordinates and back. This method is virtual and will be
   * overloaded in derived classes in order to provide backward compatibility
   * behavior in classes that did not used to take image orientation into
   * account.  */ 
  virtual void ComputeIndexToPhysicalPointMatrices();

private:
  OrientedImage(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

};
} // end namespace itk

// Define instantiation macro for this template.
#define ITK_TEMPLATE_OrientedImage(_, EXPORT, x, y) namespace itk { \
  _(2(class EXPORT OrientedImage< ITK_TEMPLATE_2 x >)) \
  namespace Templates { typedef OrientedImage< ITK_TEMPLATE_2 x > OrientedImage##y; } \
  }

#if ITK_TEMPLATE_EXPLICIT
# include "Templates/itkOrientedImage+-.h"
#endif

#if ITK_TEMPLATE_TXX
# include "itkOrientedImage.txx"
#endif

#endif
