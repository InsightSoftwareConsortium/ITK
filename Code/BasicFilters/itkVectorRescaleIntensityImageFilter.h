/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVectorRescaleIntensityImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkVectorRescaleIntensityImageFilter_h
#define __itkVectorRescaleIntensityImageFilter_h

#include "itkUnaryFunctorImageFilter.h"

namespace itk
{

// This functor class applies a scaling transformation A.x 
// to input values.
namespace Functor {  
 
template< typename TInput, typename  TOutput>
class VectorMagnitudeLinearTransform
{
public:
  typedef typename NumericTraits< typename TInput::ValueType >::RealType RealType;
  VectorMagnitudeLinearTransform() {}
  ~VectorMagnitudeLinearTransform() {}
  void SetFactor( RealType a ) { m_Factor = a; }
  itkStaticConstMacro(VectorDimension,unsigned int,TInput::Dimension);
  bool operator!=( const VectorMagnitudeLinearTransform & other ) const
  {
    if( m_Factor != other.m_Factor )
      {
      return true;
      }
    return false;
   }
  bool operator==( const VectorMagnitudeLinearTransform & other ) const
  {
    return !(*this != other);
  }
  inline TOutput operator()( const TInput & x )
  {
    TOutput  result;
    for(unsigned int i=0; i<VectorDimension; i++)
      {
      const RealType scaledComponent = static_cast<RealType>( x[i] ) * m_Factor;
      result[i]= static_cast< typename TOutput::ValueType >( scaledComponent );
      }
    return result;
  }
private:
  RealType m_Factor;
}; 

}  // end namespace functor


/** \class VectorRescaleIntensityImageFilter
 * \brief Applies a linear transformation to the magnitude of pixel vectors in a
 * vector Image. 
 *
 * VectorRescaleIntensityImageFilter applies pixel-wise a linear transformation
 * to the intensity values of input image pixels. The linear transformation is
 * defined by the user in terms of the maximum magnitude value of the vectors
 * in the pixels that the output image should have.
 * 
 * All computations are performed in the precison of the input pixel's 
 * RealType. Before assigning the computed value to the output pixel. 
 *
 * \sa RescaleIntensityImageFilter
 * 
 * \ingroup IntensityImageFilters  Multithreaded
 *
 */
template <typename  TInputImage, typename  TOutputImage=TInputImage>
class ITK_EXPORT VectorRescaleIntensityImageFilter :
    public
UnaryFunctorImageFilter<TInputImage,TOutputImage, 
                        Functor::VectorMagnitudeLinearTransform< 
  typename TInputImage::PixelType, 
  typename TOutputImage::PixelType>   >
{
public:
  /** Standard class typedefs. */
  typedef VectorRescaleIntensityImageFilter  Self;
  typedef UnaryFunctorImageFilter<TInputImage,TOutputImage, 
                                  Functor::VectorMagnitudeLinearTransform< 
    typename TInputImage::PixelType, 
    typename TOutputImage::PixelType> >  Superclass;
  typedef SmartPointer<Self>   Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  typedef typename TOutputImage::PixelType OutputPixelType;
  typedef typename TInputImage::PixelType  InputPixelType;
  typedef typename InputPixelType::ValueType      InputValueType;
  typedef typename OutputPixelType::ValueType     OutputValueType;
  typedef typename NumericTraits<InputValueType>::RealType InputRealType;
  typedef typename NumericTraits<OutputValueType>::RealType OutputRealType;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);
  
  itkSetMacro( OutputMaximumMagnitude, OutputRealType );
  itkGetConstReferenceMacro( OutputMaximumMagnitude, OutputRealType );

  /** Get the Scale and Shift used for the linear transformation
      of magnitude values. 
   \warning These Values are only valid after the filter has been updated */
  itkGetConstReferenceMacro( Scale, InputRealType );
  itkGetConstReferenceMacro( Shift, InputRealType );

  /** Get the Maximum value of the input image magnitudes.
   \warning These Values are only valid after the filter has been updated */
  itkGetConstReferenceMacro( InputMaximumMagnitude, InputRealType );

  /** Process to execute before entering the multithreaded section */
  void BeforeThreadedGenerateData(void);

  /** Print internal ivars */
  void PrintSelf(std::ostream& os, Indent indent) const;

protected:
  VectorRescaleIntensityImageFilter();
  virtual ~VectorRescaleIntensityImageFilter() {};

private:
  VectorRescaleIntensityImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  InputRealType m_Scale;
  InputRealType m_Shift;

  InputRealType        m_InputMaximumMagnitude;
  OutputRealType       m_OutputMaximumMagnitude;

};


  
} // end namespace itk
  
#ifndef ITK_MANUAL_INSTANTIATION
#include "itkVectorRescaleIntensityImageFilter.txx"
#endif
  
#endif
