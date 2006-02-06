/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkNearestNeighborExtrapolateImageFunction.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkNearestNeighborExtrapolateImageFunction_h
#define __itkNearestNeighborExtrapolateImageFunction_h

#include "itkExtrapolateImageFunction.h"

namespace itk
{

/** \class NearestNeighborExtrapolateImageFunction
 * \brief Nearest neighbor extrapolation of a scalar image.
 *
 * NearestNeighborExtrapolateImageFunction extrapolate image intensity at
 * a specified point, continuous index or index by copying the intensity
 * of the nearest neighbor within the image buffer.
 *
 * This class is templated
 * over the input image type and the coordinate representation type 
 * (e.g. float or double).
 *
 * \ingroup ImageFunctions
 */
template <class TInputImage, class TCoordRep = float>
class ITK_EXPORT NearestNeighborExtrapolateImageFunction : 
  public ExtrapolateImageFunction<TInputImage,TCoordRep> 
{
public:
  /** Standard class typedefs. */
  typedef NearestNeighborExtrapolateImageFunction Self;
  typedef ExtrapolateImageFunction<TInputImage,TCoordRep> Superclass;
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
  
  /** Run-time type information (and related methods). */
  itkTypeMacro(NearestNeighborExtrapolateImageFunction, 
    InterpolateImageFunction);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);  

  /** OutputType typedef support. */
  typedef typename Superclass::OutputType OutputType;

  /** InputImageType typedef support. */
  typedef typename Superclass::InputImageType InputImageType;

  /** Dimension underlying input image. */
  itkStaticConstMacro(ImageDimension, unsigned int,Superclass::ImageDimension);

  /** Index typedef support. */
  typedef typename Superclass::IndexType IndexType;

  /** ContinuousIndex typedef support. */
  typedef typename Superclass::ContinuousIndexType ContinuousIndexType;

  /** Evaluate the function at a ContinuousIndex position
   *
   * Returns the extrapolated image intensity at a 
   * specified position by returning the intensity of the
   * nearest neighbor within the image buffer.
   *
   */
  virtual OutputType EvaluateAtContinuousIndex( 
    const ContinuousIndexType & index ) const
  {
    typedef typename IndexType::IndexValueType ValueType;
    IndexType nindex;
    for ( unsigned int j = 0; j < ImageDimension; j++ )
      {
      if ( index[j] < this->GetStartContinuousIndex()[j] ) 
        { 
        nindex[j] = this->GetStartIndex()[j]; 
        }
      else if ( index[j] > this->GetEndContinuousIndex()[j] ) 
        { 
        nindex[j] = this->GetEndIndex()[j];
        }
      else
        {
        nindex[j] = static_cast<ValueType>( vnl_math_rnd( index[j] ) );
        }
      }
    return static_cast<OutputType>( this->GetInputImage()->GetPixel( nindex ) );
  }


  /** Evaluate the function at a ContinuousIndex position
   *
   * Returns the extrapolated image intensity at a 
   * specified position by returning the intensity of the
   * nearest neighbor within the image buffer.
   *
   */
  virtual OutputType EvaluateAtIndex( 
    const IndexType & index ) const
  {
    IndexType nindex;
    for ( unsigned int j = 0; j < ImageDimension; j++ )
      {
      if ( index[j] < this->GetStartIndex()[j] ) 
        { 
        nindex[j] = this->GetStartIndex()[j]; 
        }
      else if ( index[j] > this->GetEndIndex()[j] ) 
        { 
        nindex[j] = this->GetEndIndex()[j];
        }
      else
        {
        nindex[j] = index[j];
        }
      }
    return static_cast<OutputType>( this->GetInputImage()->GetPixel( nindex ) );
  }


protected:
  NearestNeighborExtrapolateImageFunction(){};
  ~NearestNeighborExtrapolateImageFunction(){};
  void PrintSelf(std::ostream& os, Indent indent) const
   { Superclass::PrintSelf( os, indent ); }

private:
  NearestNeighborExtrapolateImageFunction(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

};

} // end namespace itk

#endif
