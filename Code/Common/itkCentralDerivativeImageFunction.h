/*==========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkCentralDerivativeImageFunction.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

==========================================================================*/
#ifndef _itkCentralDerivativeImageFunction_h
#define _itkCentralDerivativeImageFunction_h

#include "itkImageFunction.h"

namespace itk
{

/**
 * \class CentralDerivativeImageFunction
 * \brief Calculate the derivative by central differencing
 *
 * This class is templated over the input image type.
 *
 * Possible improvements:
 * - the use of Neighborhood operators may improve efficiency.
 */
template <class TInputImage >
class ITK_EXPORT CentralDerivativeImageFunction :
  public ImageFunction< TInputImage, double >
{
public:
  /**
   * Standard "Self" typedef
   */
  typedef CentralDerivativeImageFunction Self;

  /**
   * Standard "Superclass" typedef
   */
  typedef ImageFunction<TInputImage, double> Superclass;

  /**
   * Smart pointer typedef support.
   */
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);

  /**
   * InputImageType typedef support.
   */
  typedef TInputImage InputImageType;

  /**
   * Dimension of the underlying image.
   */
  enum { ImageDimension = InputImageType::ImageDimension };

  /**
   * Index typedef support.
   */
  typedef Index<ImageDimension> IndexType;

  /**
   * Set the input image.
   */
  virtual void SetInputImage( InputImageType * ptr );

  /**
   * Evalulate the function at specified index
   */
  virtual double Evaluate( const IndexType& index )
  {
    return ( this->Evaluate( index, 0 ) );
  }
  virtual double Evaluate( const IndexType& index, unsigned int dim = 0 );

  /**
   * Evaluate the function at a non-integer position
   */
  virtual double Evaluate( double coord[] )
  {
    return ( this->Evaluate( coord, 0 ) );
  }

  /**
   * Evaluate the function at a non-integer position
   */
  virtual double Evaluate( double coord[], unsigned int dim = 0 )
    {
      IndexType index;
      for( int j = 0; j < ImageDimension; j++ )
        {
          index[j] = vnl_math_rnd( coord[j] );
        }
      return ( this->Evaluate( index, dim ) );
    };


  /**
   * Get the derivative from last evaluation
   */
  virtual double GetDerivative() const
  { return m_Derivative; }

protected:
  CentralDerivativeImageFunction(){};
  CentralDerivativeImageFunction( const Self& ){};

  ~CentralDerivativeImageFunction(){};

  void operator=( const Self& ){};
  void PrintSelf(std::ostream& os, Indent indent);

  double                  m_Derivative;

private:
  Size<ImageDimension>    m_ImageSize;

};

} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkCentralDerivativeImageFunction.txx"
#endif

#endif

