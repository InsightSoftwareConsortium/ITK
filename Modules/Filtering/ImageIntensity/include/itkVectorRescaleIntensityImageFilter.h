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
#ifndef itkVectorRescaleIntensityImageFilter_h
#define itkVectorRescaleIntensityImageFilter_h

#include "itkMath.h"
#include "itkUnaryFunctorImageFilter.h"

namespace itk
{
// This functor class applies a scaling transformation A.x
// to input values.
namespace Functor
{
template< typename TInput, typename  TOutput >
class ITK_TEMPLATE_EXPORT VectorMagnitudeLinearTransform
{
public:
  typedef typename NumericTraits< typename TInput::ValueType >::RealType RealType;
  VectorMagnitudeLinearTransform() : m_Factor(0.0) {}
  ~VectorMagnitudeLinearTransform() {}
  void SetFactor(RealType a) { m_Factor = a; }
  itkStaticConstMacro(VectorDimension, unsigned int, TInput::Dimension);
  bool operator!=(const VectorMagnitudeLinearTransform & other) const
  {
    if ( Math::NotExactlyEquals(m_Factor, other.m_Factor) )
      {
      return true;
      }
    return false;
  }

  bool operator==(const VectorMagnitudeLinearTransform & other) const
  {
    return !( *this != other );
  }

  inline TOutput operator()(const TInput & x) const
  {
    TOutput result;

    for ( unsigned int i = 0; i < VectorDimension; i++ )
      {
      const RealType scaledComponent = static_cast< RealType >( x[i] ) * m_Factor;
      result[i] = static_cast< typename TOutput::ValueType >( scaledComponent );
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
 * All computations are performed in the precision of the input pixel's
 * RealType. Before assigning the computed value to the output pixel.
 *
 * \sa RescaleIntensityImageFilter
 *
 * \ingroup IntensityImageFilters  MultiThreaded
 *
 * \ingroup ITKImageIntensity
 *
 * \wiki
 * \wikiexample{Images/VectorRescaleIntensityImageFilter,Apply a transformation to the magnitude of vector valued image pixels}
 * \endwiki
 */
template< typename TInputImage, typename TOutputImage = TInputImage >
class ITK_TEMPLATE_EXPORT VectorRescaleIntensityImageFilter:
  public
  UnaryFunctorImageFilter< TInputImage, TOutputImage,
                           Functor::VectorMagnitudeLinearTransform<
                             typename TInputImage::PixelType,
                             typename TOutputImage::PixelType > >
{
public:
  /** Standard class typedefs. */
  typedef VectorRescaleIntensityImageFilter Self;
  typedef UnaryFunctorImageFilter<
    TInputImage, TOutputImage,
    Functor::VectorMagnitudeLinearTransform<
      typename TInputImage::PixelType,
      typename TOutputImage::PixelType > >    Superclass;

  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  typedef typename TOutputImage::PixelType                    OutputPixelType;
  typedef typename TInputImage::PixelType                     InputPixelType;
  typedef typename InputPixelType::ValueType                  InputValueType;
  typedef typename OutputPixelType::ValueType                 OutputValueType;
  typedef typename NumericTraits< InputValueType >::RealType  InputRealType;
  typedef typename NumericTraits< OutputValueType >::RealType OutputRealType;

  typedef typename Superclass::InputImageType    InputImageType;
  typedef typename Superclass::InputImagePointer InputImagePointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(VectorRescaleIntensityImageFilter, UnaryFunctorImageFilter);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  itkSetMacro(OutputMaximumMagnitude, OutputRealType);
  itkGetConstReferenceMacro(OutputMaximumMagnitude, OutputRealType);

  /** Get the Scale and Shift used for the linear transformation
      of magnitude values.
   \warning These values are only valid after the filter has been updated. */
  itkGetConstReferenceMacro(Scale, InputRealType);
  itkGetConstReferenceMacro(Shift, InputRealType);

  /** Get the Maximum value of the input image magnitudes.
   \warning These values are only valid after the filter has been updated. */
  itkGetConstReferenceMacro(InputMaximumMagnitude, InputRealType);

  /** Process to execute before entering the multithreaded section. */
  void BeforeThreadedGenerateData() ITK_OVERRIDE;

  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro( InputHasNumericTraitsCheck,
                   ( Concept::HasNumericTraits< InputValueType > ) );
  itkConceptMacro( OutputHasNumericTraitsCheck,
                   ( Concept::HasNumericTraits< OutputValueType > ) );
  // End concept checking
#endif

protected:
  VectorRescaleIntensityImageFilter();
  virtual ~VectorRescaleIntensityImageFilter() ITK_OVERRIDE {}

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(VectorRescaleIntensityImageFilter);

  InputRealType m_Scale;
  InputRealType m_Shift;

  InputRealType  m_InputMaximumMagnitude;
  OutputRealType m_OutputMaximumMagnitude;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkVectorRescaleIntensityImageFilter.hxx"
#endif

#endif
