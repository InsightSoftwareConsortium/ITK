/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkNeighborhoodBinaryThresholdImageFunction.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkNeighborhoodBinaryThresholdImageFunction_h
#define _itkNeighborhoodBinaryThresholdImageFunction_h

#include "itkBinaryThresholdImageFunction.h"

namespace itk
{

/**
 * \class NeighborhoodBinaryThresholdImageFunction
 * \brief Determine whether all the pixels in the specified neighborhood meet a threshold criteria
 *
 * Determine whether all the pixels in the specified neighborhood meet
 * a threshold criteria.
 *
 * If called with a ContinuousIndex or Point, the calculation is performed
 * at the nearest neighbor.
 *
 * This class is templated over the input image type.
 *
 * \ingroup ImageFunctions
 */
template <class TInputImage >
class ITK_EXPORT NeighborhoodBinaryThresholdImageFunction :
  public BinaryThresholdImageFunction< TInputImage >
{
public:
  /** Standard class typedefs. */
  typedef NeighborhoodBinaryThresholdImageFunction Self;
  typedef BinaryThresholdImageFunction<TInputImage> Superclass;
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
  
  /** Run-time type information (and related methods). */
  itkTypeMacro(NeighborhoodBinaryThresholdImageFunction, BinaryThresholdImageFunction);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** InputImageType typedef support. */
  typedef TInputImage InputImageType;

  /** OutputType typdef support. */
  typedef typename Superclass::OutputType OutputType;

  /** Index typedef support. */
  typedef typename Superclass::IndexType IndexType;
  
  /** ContinuousIndex typedef support. */
  typedef typename Superclass::ContinuousIndexType ContinuousIndexType;

  /** Point typedef support. */
  typedef typename Superclass::PointType PointType;

  /** Dimension of the underlying image. */
  enum { ImageDimension = InputImageType::ImageDimension };

  /** SizeType of the input image */
  typedef typename InputImageType::SizeType InputSizeType;

  /** Set the radius of the neighborhood used to compute the median. */
  itkSetMacro(Radius, InputSizeType);

  /** Get the radius of the neighborhood used to compute the median */
  itkGetConstReferenceMacro(Radius, InputSizeType);

  /** Evalulate the function at specified index */
  virtual bool EvaluateAtIndex( const IndexType& index ) const;
  
  /** Evaluate the function at non-integer positions */
  virtual bool Evaluate( const PointType& point ) const
    { 
      IndexType index;
      this->ConvertPointToNearestIndex( point, index );
      return this->EvaluateAtIndex( index ); 
    }
  virtual bool EvaluateAtContinuousIndex( 
    const ContinuousIndexType& cindex ) const
    { 
      IndexType index;
      this->ConvertContinuousIndexToNearestIndex( cindex, index );
      return this->EvaluateAtIndex( index ) ; 
    }
  
protected:
  NeighborhoodBinaryThresholdImageFunction();
  ~NeighborhoodBinaryThresholdImageFunction(){};
  void PrintSelf(std::ostream& os, Indent indent) const;

private:
  NeighborhoodBinaryThresholdImageFunction( const Self& ); //purposely not implemented
  void operator=( const Self& ); //purposely not implemented

  InputSizeType m_Radius;
};

} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkNeighborhoodBinaryThresholdImageFunction.txx"
#endif

#endif

