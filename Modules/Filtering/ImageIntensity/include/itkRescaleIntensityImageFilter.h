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
#ifndef itkRescaleIntensityImageFilter_h
#define itkRescaleIntensityImageFilter_h

#include "itkUnaryFunctorImageFilter.h"
#include "itkMath.h"

namespace itk
{
// This functor class applies a linear transformation A.x + B
// to input values.
namespace Functor
{
template< typename TInput, typename  TOutput >
class ITK_TEMPLATE_EXPORT IntensityLinearTransform
{
public:
  typedef typename NumericTraits< TInput >::RealType RealType;
  IntensityLinearTransform()
  {
    m_Factor = 1.0;
    m_Offset = 0.0;
    m_Minimum = NumericTraits< TOutput >::NonpositiveMin();
    m_Maximum = NumericTraits< TOutput >::max();
#if defined (__GNUC__) && (__GNUC__ == 5) && (__GNUC_MINOR__ == 2) && defined(NDEBUG) && defined(__i386__)
    m_EpsilonCompensation = static_cast<RealType>(std::numeric_limits<TOutput>::epsilon());
    if (m_EpsilonCompensation == 0)
      {
      m_EpsilonCompensation = std::numeric_limits<RealType>::epsilon();
      }
#endif
  }

  ~IntensityLinearTransform() {}
  void SetFactor(RealType a) { m_Factor = a; }
  void SetOffset(RealType b) { m_Offset = b; }
  void SetMinimum(TOutput min) { m_Minimum = min; }
  void SetMaximum(TOutput max) { m_Maximum = max; }
  bool operator!=(const IntensityLinearTransform & other) const
  {
    if ( Math::NotExactlyEquals(m_Factor, other.m_Factor)
         || Math::NotExactlyEquals(m_Offset, other.m_Offset)
         || Math::NotExactlyEquals(m_Maximum, other.m_Maximum)
         || Math::NotExactlyEquals(m_Minimum, other.m_Minimum) )
      {
      return true;
      }
    return false;
  }

  bool operator==(const IntensityLinearTransform & other) const
  {
    return !( *this != other );
  }

  inline TOutput operator()(const TInput & x) const
  {
#if defined (__GNUC__) && (__GNUC__ == 5) && (__GNUC_MINOR__ == 2) && defined(NDEBUG) && defined(__i386__)
    RealType value  = static_cast< RealType >( x ) * m_Factor + m_Offset + m_EpsilonCompensation;
    TOutput  result = static_cast< TOutput >( value ) - static_cast< TOutput >( m_EpsilonCompensation );
#else
    RealType value  = static_cast< RealType >( x ) * m_Factor + m_Offset;
    TOutput  result = static_cast< TOutput >( value );
#endif
    result = ( result > m_Maximum ) ? m_Maximum : result;
    result = ( result < m_Minimum ) ? m_Minimum : result;
    return result;
  }

private:
  RealType m_Factor;
  RealType m_Offset;
  TOutput  m_Maximum;
  TOutput  m_Minimum;
#if defined (__GNUC__) && (__GNUC__ == 5) && (__GNUC_MINOR__ == 2) && defined(NDEBUG) && defined(__i386__)
  RealType m_EpsilonCompensation;
#endif
};
}  // end namespace functor

/** \class RescaleIntensityImageFilter
 * \brief Applies a linear transformation to the intensity levels of the
 * input Image.
 *
 * RescaleIntensityImageFilter applies pixel-wise a linear transformation
 * to the intensity values of input image pixels. The linear transformation
 * is defined by the user in terms of the minimum and maximum values that
 * the output image should have.
 *
 * The following equation gives the mapping of the intensity values
 *
 * \par
 * \f[
 *  outputPixel = ( inputPixel - inputMin) \cdot
 *  \frac{(outputMax - outputMin )}{(inputMax - inputMin)} + outputMin
 * \f]
 *
 * All computations are performed in the precision of the input pixel's
 * RealType. Before assigning the computed value to the output pixel.
 *
 * NOTE: In this filter the minimum and maximum values of the input image are
 * computed internally using the MinimumMaximumImageCalculator. Users are not
 * supposed to set those values in this filter. If you need a filter where you
 * can set the minimum and maximum values of the input, please use the
 * IntensityWindowingImageFilter. If you want a filter that can use a
 * user-defined linear transformation for the intensity, then please use the
 * ShiftScaleImageFilter.
 *
 * \sa IntensityWindowingImageFilter
 *
 * \ingroup IntensityImageFilters  MultiThreaded
 *
 * \ingroup ITKImageIntensity
 *
 * \wiki
 * \wikiexample{ImageProcessing/RescaleIntensityImageFilter,Rescale the intensity values of an image to a specified range}
 * \endwiki
 */
template< typename  TInputImage, typename  TOutputImage = TInputImage >
class ITK_TEMPLATE_EXPORT RescaleIntensityImageFilter:
  public
  UnaryFunctorImageFilter< TInputImage, TOutputImage,
                           Functor::IntensityLinearTransform<
                             typename TInputImage::PixelType,
                             typename TOutputImage::PixelType >   >
{
public:
  /** Standard class typedefs. */
  typedef RescaleIntensityImageFilter Self;
  typedef UnaryFunctorImageFilter<
    TInputImage, TOutputImage,
    Functor::IntensityLinearTransform<
      typename TInputImage::PixelType,
      typename TOutputImage::PixelType > >  Superclass;

  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  typedef typename TOutputImage::PixelType                   OutputPixelType;
  typedef typename TInputImage::PixelType                    InputPixelType;
  typedef typename NumericTraits< InputPixelType >::RealType RealType;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(RescaleIntensityImageFilter,
               UnaryFunctorImageFilter);

  itkSetMacro(OutputMinimum, OutputPixelType);
  itkSetMacro(OutputMaximum, OutputPixelType);
  itkGetConstReferenceMacro(OutputMinimum, OutputPixelType);
  itkGetConstReferenceMacro(OutputMaximum, OutputPixelType);

  /** Get the Scale and Shift used for the linear transformation
      of gray level values.
   \warning These Values are only valid after the filter has been updated */
  itkGetConstReferenceMacro(Scale, RealType);
  itkGetConstReferenceMacro(Shift, RealType);

  /** Get the Minimum and Maximum values of the input image.
   \warning These Values are only valid after the filter has been updated */
  itkGetConstReferenceMacro(InputMinimum, InputPixelType);
  itkGetConstReferenceMacro(InputMaximum, InputPixelType);

  /** Process to execute before entering the multithreaded section */
  void BeforeThreadedGenerateData() ITK_OVERRIDE;

  /** Print internal ivars */
  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro( InputHasNumericTraitsCheck,
                   ( Concept::HasNumericTraits< InputPixelType > ) );
  itkConceptMacro( OutputHasNumericTraitsCheck,
                   ( Concept::HasNumericTraits< OutputPixelType > ) );
  itkConceptMacro( RealTypeMultiplyOperatorCheck,
                   ( Concept::MultiplyOperator< RealType > ) );
  itkConceptMacro( RealTypeAdditiveOperatorsCheck,
                   ( Concept::AdditiveOperators< RealType > ) );
  // End concept checking
#endif

protected:
  RescaleIntensityImageFilter();
  virtual ~RescaleIntensityImageFilter() ITK_OVERRIDE {}

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(RescaleIntensityImageFilter);

  RealType m_Scale;
  RealType m_Shift;

  InputPixelType m_InputMinimum;
  InputPixelType m_InputMaximum;

  OutputPixelType m_OutputMinimum;
  OutputPixelType m_OutputMaximum;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkRescaleIntensityImageFilter.hxx"
#endif

#endif
