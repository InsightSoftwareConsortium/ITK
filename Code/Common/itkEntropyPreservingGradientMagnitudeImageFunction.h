/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkEntropyPreservingGradientMagnitudeImageFunction.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkEntropyPreservingGradientMagnitudeImageFunction_h
#define _itkEntropyPreservingGradientMagnitudeImageFunction_h

#include "itkImageFunction.h"

namespace itk
{

/** \class EntropyPreservingGradientMagnitudeImageFunction
 * \brief Calculate an entropy satisfying gradient magnitude.
 *
 * EntropyPreservingGradientMagnitudeImageFunction calculates an entropy 
 * satisfying image gradient magnitude. This is class is templated over the
 * input image type and the coordinate representation type 
 * (e.g. float or double).
 *
 * In level set methods, the propagating front can form corners
 * as it evolves. At these singularities, the front is no longer
 * differentiable and a weak solution must be constructed to
 * continue the solution.
 *
 * Viscosity or entropy solution can be formed by using upwind
 * finite differencing. Depending on the flow direction, either a forward
 * or backward differencing scheme is used. This is to ensure that only
 * grid points upstream in the flow is used to update the current point.
 *
 * In this function, the flow direction can be specified by the sign
 * of the speed value set via the SetSpeed() method.
 *
 * Reference:
 * "Level Set Methods and Fast Marching Methods", J.A. Sethian,
 * Cambridge Press, Chapter 6, Second edition, 1999.
 *
 * Possible improvements:
 * - the use of Neighborhood operators may improve efficiency.
 *
 * \ingroup ImageFunctions
 */
template < class TInputImage, class TCoordRep = float >
class ITK_EXPORT EntropyPreservingGradientMagnitudeImageFunction :
  public ImageFunction< TInputImage, double, TCoordRep >
{
public:
  /** Standard class typedefs. */
  typedef EntropyPreservingGradientMagnitudeImageFunction Self;
  typedef ImageFunction<TInputImage, double, TCoordRep> Superclass;
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
  
  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(EntropyPreservingGradientMagnitudeImageFunction, ImageFunction);

  /** InputImageType typedef support. */
  typedef TInputImage InputImageType;
  typedef typename InputImageType::ConstPointer InputImageConstPointer;

  /** Dimension of the underlying image. */
  itkStaticConstMacro(ImageDimension, unsigned int,
                      InputImageType::ImageDimension);

  /** Index typedef support. */
  typedef typename Superclass::IndexType IndexType;

  /** ContinuousIndex typedef support. */
  typedef typename Superclass::ContinuousIndexType ContinuousIndexType;

  /** Point typedef support. */
  typedef typename Superclass::PointType PointType;

  /** Set the input image. */
  virtual void SetInputImage( const InputImageType * ptr );

  /** Set the speed parameter. */
  void SetSpeed( double value )
    { m_Speed = value; }

  /** Get the speed parameter. */
  double GetSpeed() const
    { return m_Speed; }

  /** Evalulate the function at specified index */
  virtual double EvaluateAtIndex( const IndexType& index ) const;

  /** Evaluate the function at non-integer positions. */
  virtual double Evaluate( const PointType& point ) const
    { 
      IndexType index;
      this->ConvertPointToNearestIndex( point, index );
      return this->EvaluateAtIndex( index ); 
    }

  /** Evaluate the function at continuous positions. */
  virtual double EvaluateAtContinuousIndex( 
    const ContinuousIndexType& cindex ) const
    { 
      IndexType index;
      this->ConvertContinuousIndexToNearestIndex( cindex, index );
      return this->EvaluateAtIndex( index ) ; 
    }

  /** Get the magnitude from last evaluation */
  double GetMagnitude() const
    { return m_Magnitude; }

protected:
  EntropyPreservingGradientMagnitudeImageFunction(){};
  ~EntropyPreservingGradientMagnitudeImageFunction(){};
  void PrintSelf(std::ostream& os, Indent indent) const;

private:
  EntropyPreservingGradientMagnitudeImageFunction ( const Self& ); //purposely not implemented
  void operator=( const Self& ); //purposely not implemented

  signed long             m_ImageSize[ImageDimension];
  bool                    m_ImageSizeOK;

  double                  m_Speed;
  mutable double          m_Magnitude;

  mutable IndexType       m_NeighIndex;
  mutable double          m_CenterValue;
  mutable double          m_DiffValue;

};

} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkEntropyPreservingGradientMagnitudeImageFunction.txx"
#endif

#endif
