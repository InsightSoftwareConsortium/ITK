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
#ifndef itkIntensityWindowingImageFilter_h
#define itkIntensityWindowingImageFilter_h

#include "itkUnaryFunctorImageFilter.h"

namespace itk
{
// This functor class applies a linear transformation A.x + B inside a specified
// range. Values below the range are mapped to a constant. Values over the range
// are mapped to another constant.
namespace Functor
{
template< typename TInput, typename  TOutput >
class ITK_TEMPLATE_EXPORT IntensityWindowingTransform
{
public:
  typedef typename NumericTraits< TInput >::RealType RealType;
  IntensityWindowingTransform() :
    m_Factor(0.0),
    m_Offset(0.0),
    m_OutputMaximum(0),
    m_OutputMinimum(0),
    m_WindowMaximum(0),
    m_WindowMinimum(0) {}
  ~IntensityWindowingTransform() {}
  bool operator!=(const IntensityWindowingTransform & other) const
  {
    if (    Math::NotExactlyEquals( m_Factor     , other.m_Factor )
         || Math::NotExactlyEquals( m_Offset     , other.m_Offset )
         || Math::NotExactlyEquals( m_OutputMaximum, other.m_OutputMaximum )
         || Math::NotExactlyEquals( m_OutputMinimum, other.m_OutputMinimum )
         || Math::NotExactlyEquals( m_WindowMaximum, other.m_WindowMaximum )
         || Math::NotExactlyEquals( m_WindowMinimum, other.m_WindowMinimum ) )
      {
      return true;
      }
    return false;
  }

  bool operator==(const IntensityWindowingTransform & other) const
  {
    return !( *this != other );
  }

  void SetFactor(RealType a) { m_Factor = a; }
  void SetOffset(RealType b) { m_Offset = b; }
  void SetOutputMinimum(TOutput min) { m_OutputMinimum = min; }
  void SetOutputMaximum(TOutput max) { m_OutputMaximum = max; }
  void SetWindowMinimum(TInput min) { m_WindowMinimum = min; }
  void SetWindowMaximum(TInput max) { m_WindowMaximum = max; }
  inline TOutput operator()(const TInput & x) const
  {
    if ( x < m_WindowMinimum )
      {
      return m_OutputMinimum;
      }
    if ( x > m_WindowMaximum )
      {
      return m_OutputMaximum;
      }
    const RealType value  = static_cast< RealType >( x ) * m_Factor + m_Offset;
    const TOutput  result = static_cast< TOutput >( value );
    return result;
  }

private:
  RealType m_Factor;
  RealType m_Offset;
  TOutput  m_OutputMaximum;
  TOutput  m_OutputMinimum;
  TInput   m_WindowMaximum;
  TInput   m_WindowMinimum;
};
}  // end namespace functor

/** \class IntensityWindowingImageFilter
 * \brief Applies a linear transformation to the intensity levels of the
 * input Image that are inside a user-defined interval. Values below this
 * interval are mapped to a constant. Values over the interval are mapped
 * to another constant.
 *
 * IntensityWindowingImageFilter applies pixel-wise a linear transformation
 * to the intensity values of input image pixels. The linear transformation
 * is defined by the user in terms of the minimum and maximum values that
 * the output image should have and the lower and upper limits of the intensity
 * window of the input image. This operation is very common in visualization,
 * and can also be applied as a convenient preprocessing operation for image
 * segmentation.
 *
 * All computations are performed in the precision of the input pixel's
 * RealType. Before assigning the computed value to the output pixel.
 *
 * \ingroup IntensityImageFilters  MultiThreaded
 *
 * \ingroup ITKImageIntensity
 *
 * \wiki
 * \wikiexample{ImageProcessing/IntensityWindowingImageFilter,IntensityWindowingImageFilter}
 * \endwiki
 *
 * \sa RescaleIntensityImageFilter
 */
template< typename  TInputImage, typename  TOutputImage = TInputImage >
class ITK_TEMPLATE_EXPORT IntensityWindowingImageFilter:
  public
  UnaryFunctorImageFilter< TInputImage, TOutputImage,
                           Functor::IntensityWindowingTransform<
                             typename TInputImage::PixelType,
                             typename TOutputImage::PixelType >   >
{
public:
  /** Standard class typedefs. */
  typedef IntensityWindowingImageFilter Self;
  typedef UnaryFunctorImageFilter<
    TInputImage, TOutputImage,
    Functor::IntensityWindowingTransform<
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
  itkTypeMacro(IntensityWindowingImageFilter,
               UnaryFunctorImageFilter);

  /** Set/Get the values of the maximum and minimum
   *  intensities of the output image. */
  itkSetMacro(OutputMinimum, OutputPixelType);
  itkSetMacro(OutputMaximum, OutputPixelType);
  itkGetConstReferenceMacro(OutputMinimum, OutputPixelType);
  itkGetConstReferenceMacro(OutputMaximum, OutputPixelType);

  /** Set/Get the values of the maximum and minimum
   *  intensities of the input intensity window. */
  itkSetMacro(WindowMinimum, InputPixelType);
  itkSetMacro(WindowMaximum, InputPixelType);
  itkGetConstReferenceMacro(WindowMinimum, InputPixelType);
  itkGetConstReferenceMacro(WindowMaximum, InputPixelType);

  /** Set/Get the window width and level. This is an alternative API
   * to using the SetWindowMinimum()/SetWindowMaximum(). The window
   * minimum and maximum are set as [level-window/2,
   * level+window/2]. */
  void SetWindowLevel(const InputPixelType & window,
                      const InputPixelType & level);

  InputPixelType GetWindow() const;

  InputPixelType GetLevel() const;

  /** Get the Scale and Shift used for the linear transformation
      of gray level values.
   \warning These Values are only valid after the filter has been updated. */
  itkGetConstReferenceMacro(Scale, RealType);
  itkGetConstReferenceMacro(Shift, RealType);

  /** Process to execute before entering the multithreaded section. */
  void BeforeThreadedGenerateData(void) ITK_OVERRIDE;

  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro( InputHasNumericTraitsCheck,
                   ( Concept::HasNumericTraits< InputPixelType > ) );
  // End concept checking
#endif

protected:
  IntensityWindowingImageFilter();
  virtual ~IntensityWindowingImageFilter() ITK_OVERRIDE {}

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(IntensityWindowingImageFilter);

  RealType m_Scale;
  RealType m_Shift;

  InputPixelType m_WindowMinimum;
  InputPixelType m_WindowMaximum;

  OutputPixelType m_OutputMinimum;
  OutputPixelType m_OutputMaximum;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkIntensityWindowingImageFilter.hxx"
#endif

#endif
