/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMedianImageFunction.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkMedianImageFunction_h
#define __itkMedianImageFunction_h

#include "itkImageFunction.h"
#include "itkNumericTraits.h"

namespace itk
{

/**
 * \class MedianImageFunction
 * \brief Calculate the median value in the neighborhood of a pixel
 *
 * Calculate the median pixel value over the standard 8, 26, etc. connected
 * neighborhood.  This calculation uses a ZeroFluxNeumannBoundaryCondition.
 *
 * If called with a ContinuousIndex or Point, the calculation is performed
 * at the nearest neighbor.
 *
 * This class is templated over the input image type and the 
 * coordinate representation type (e.g. float or double ).
 *
 * \ingroup ImageFunctions
 */
template <class TInputImage, class TCoordRep = float >
class ITK_EXPORT MedianImageFunction :
  public ImageFunction< TInputImage, ITK_TYPENAME TInputImage::PixelType,
    TCoordRep >
{
public:
  /** Standard class typedefs. */
  typedef MedianImageFunction Self;
  typedef ImageFunction<TInputImage, ITK_TYPENAME TInputImage::PixelType,
    TCoordRep > Superclass;
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
  
  /** Run-time type information (and related methods). */
  itkTypeMacro(MedianImageFunction, ImageFunction);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** InputImageType typedef support. */
  typedef TInputImage InputImageType;
  typedef typename Superclass::InputPixelType InputPixelType;

  /** OutputType typedef support. */
  typedef typename Superclass::OutputType OutputType;

  /** Index typedef support. */
  typedef typename Superclass::IndexType IndexType;
  
  /** ContinuousIndex typedef support. */
  typedef typename Superclass::ContinuousIndexType ContinuousIndexType;

  /** Point typedef support. */
  typedef typename Superclass::PointType PointType;

  /** Dimension of the underlying image. */
  itkStaticConstMacro(ImageDimension, unsigned int,
                      InputImageType::ImageDimension);

  /** Evalulate the function at specified index */
  virtual OutputType EvaluateAtIndex( const IndexType& index ) const;
  
  /** Evaluate the function at non-integer positions */
  virtual OutputType Evaluate( const PointType& point ) const
    { 
      IndexType index;
      this->ConvertPointToNearestIndex( point, index );
      return this->EvaluateAtIndex( index ); 
    }
  virtual OutputType EvaluateAtContinuousIndex( 
    const ContinuousIndexType& cindex ) const
    { 
      IndexType index;
      this->ConvertContinuousIndexToNearestIndex( cindex, index );
      return this->EvaluateAtIndex( index ) ; 
    }
  
protected:
  MedianImageFunction();
  ~MedianImageFunction(){};
  void PrintSelf(std::ostream& os, Indent indent) const;

private:
  MedianImageFunction( const Self& ); //purposely not implemented
  void operator=( const Self& ); //purposely not implemented

};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkMedianImageFunction.txx"
#endif

#endif

