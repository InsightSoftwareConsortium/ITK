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
#ifndef itkLaplacianRecursiveGaussianImageFilter_h
#define itkLaplacianRecursiveGaussianImageFilter_h

#include "itkRecursiveGaussianImageFilter.h"
#include "itkImage.h"
#include "itkCommand.h"

namespace itk
{
/** \class LaplacianRecursiveGaussianImageFilter
 * \brief Computes the Laplacian of Gaussian (LoG) of an image.
 *
 * Computes the Laplacian of Gaussian (LoG) of an image by convolution
 * with the second derivative of a Gaussian.
 * This filter is implemented using the recursive gaussian filters.
 *
 * \ingroup GradientFilters
 * \ingroup MultiThreaded
 * \ingroup ITKImageFeature
 *
 * \wiki
 * \wikiexample{EdgesAndGradients/LaplacianRecursiveGaussianImageFilter,Compute the Laplacian of Gaussian (LoG) of an image}
 * \endwiki
 */
template< typename TInputImage,
          typename TOutputImage = TInputImage >
class ITK_TEMPLATE_EXPORT LaplacianRecursiveGaussianImageFilter:
  public ImageToImageFilter< TInputImage, TOutputImage >
{
public:
  /** Standard class typedefs. */
  typedef LaplacianRecursiveGaussianImageFilter           Self;
  typedef ImageToImageFilter< TInputImage, TOutputImage > Superclass;
  typedef SmartPointer< Self >                            Pointer;
  typedef SmartPointer< const Self >                      ConstPointer;

  /** Pixel Type of the input image */
  typedef TInputImage                        InputImageType;
  typedef typename InputImageType::PixelType PixelType;

  /** Image dimension. */
  itkStaticConstMacro(ImageDimension, unsigned int,
                      TInputImage::ImageDimension);

  itkStaticConstMacro(NumberOfSmoothingFilters ,unsigned int, ImageDimension - 1);

  typedef typename NumericTraits< PixelType >::RealType RealType;

  /** Define the image type for internal computations.
      RealType is usually 'double' in NumericTraits.
      Here we prefer float in order to save memory.  */
  typedef float InternalRealType;
  typedef Image< InternalRealType,
                 itkGetStaticConstMacro(ImageDimension) >   RealImageType;

  /**  Smoothing filter type */
  typedef RecursiveGaussianImageFilter<
    RealImageType,
    RealImageType
    >    GaussianFilterType;

  /**  Derivative filter type, it will be the first in the pipeline  */
  typedef RecursiveGaussianImageFilter<
    InputImageType,
    RealImageType
    >    DerivativeFilterType;

  /**  Pointer to a gaussian filter.  */
  typedef typename GaussianFilterType::Pointer GaussianFilterPointer;

  /**  Pointer to a derivative filter.  */
  typedef typename DerivativeFilterType::Pointer DerivativeFilterPointer;

  /**  Pointer to the Output Image */
  typedef typename TOutputImage::Pointer OutputImagePointer;

  /** Type of the output Image */
  typedef TOutputImage                                 OutputImageType;
  typedef typename          OutputImageType::PixelType OutputPixelType;


  /**  Command for observing progress of internal pipeline filters */
  typedef          MemberCommand< Self > CommandType;
  typedef typename CommandType::Pointer  CommandPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(LaplacianRecursiveGaussianImageFilter,
               ImageToImageFilter);

  /** Set Sigma value. Sigma is measured in the units of image spacing. */
  void SetSigma(RealType sigma);
  RealType GetSigma() const;

  /** Define which normalization factor will be used for the Gaussian
   *  \sa  RecursiveGaussianImageFilter::SetNormalizeAcrossScale
   */
  void SetNormalizeAcrossScale(bool normalizeInScaleSpace);\
  itkGetConstMacro(NormalizeAcrossScale, bool);

protected:
  LaplacianRecursiveGaussianImageFilter();
  virtual ~LaplacianRecursiveGaussianImageFilter() ITK_OVERRIDE {}
  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  /** Generate Data */
  void GenerateData(void) ITK_OVERRIDE;

  // Override since the filter produces the entire dataset
  void EnlargeOutputRequestedRegion(DataObject *output) ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(LaplacianRecursiveGaussianImageFilter);

  // special binary functor to perform A+B*ConstValue
  //
  // Where A is the cumulativeImage, B is the last filter, and
  // ConstValue is the spacing scalling
  class AddMultConstFunctor
  {
  public:
    typedef AddMultConstFunctor Self;

    AddMultConstFunctor( void ) : m_Value( NumericTraits<PixelType>::OneValue() ) {}

    bool operator!=( const Self &other ) const { return !(*this==other); }
    bool operator==( const Self &other ) const { return m_Value == other.m_Value; }

    inline InternalRealType operator()( const InternalRealType &a, const InternalRealType &b ) const
    {
      return static_cast<InternalRealType>( a + b*m_Value );
    }

    RealType m_Value;
  };

  GaussianFilterPointer   m_SmoothingFilters[NumberOfSmoothingFilters];
  DerivativeFilterPointer m_DerivativeFilter;

  /** Normalize the image across scale space */
  bool m_NormalizeAcrossScale;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkLaplacianRecursiveGaussianImageFilter.hxx"
#endif

#endif
