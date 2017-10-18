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
#ifndef itkUnsharpMaskImageFilter_h
#define itkUnsharpMaskImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkSmoothingRecursiveGaussianImageFilter.h"

namespace itk
{
/**
 * \class UnsharpMaskImageFilter
 * \brief Edge enhancement filter.
 *
 * This filter subtracts a smoothed version of the image from the image
 * to achieve the edge enhancing effect.
 * https://en.wikipedia.org/w/index.php?title=Unsharp_masking&oldid=750486803#Photographic_unsharp_masking
 *
 * It has configurable amount, radius (sigma) and threshold,
 * and whether to clamp the resulting values to the range of output type.
 *
 * Formula: sharpened=original+[abs(original-blurred)-threshold]*amount
 *
 * If clamping is turned off (it is on by default),
 * casting to output pixel format is done using C++ defaults,
 * meaning that values are not clamped but rather wrap around
 * e.g. 260 -> 4 (unsigned char).
 *
 * \sa ImageToImageFilter
 * \sa SmoothingRecursiveGaussianImageFilter
 * \sa RescaleIntensityImageFilter
 *
 * \ingroup ImageFeatureExtraction
 *
 * \ingroup ITKImageFeature
 *
 */

template< typename TInputImage, typename TOutputImage = TInputImage, typename TInternalPrecision = float >
class ITK_TEMPLATE_EXPORT UnsharpMaskImageFilter:
  public ImageToImageFilter< TInputImage, TOutputImage >
{
public:
  /**
   * Standard "Self" & Superclass typedef.
   */
  typedef UnsharpMaskImageFilter                          Self;
  typedef ImageToImageFilter< TInputImage, TOutputImage > Superclass;

  /**
   * Extract some information from the image types.
   */
  typedef typename TOutputImage::PixelType         OutputPixelType;
  typedef typename TOutputImage::InternalPixelType OutputInternalPixelType;
  typedef typename TInputImage::PixelType          InputPixelType;
  typedef typename TInputImage::InternalPixelType  InputInternalPixelType;
  typedef typename TOutputImage::RegionType        OutputImageRegionType;
  typedef typename TInputImage::RegionType         InputImageRegionType;
  itkStaticConstMacro(ImageDimension, unsigned int,
                      TOutputImage::ImageDimension);
  itkStaticConstMacro(InputImageDimension, unsigned int,
                      TInputImage::ImageDimension);

  /**
   * Image typedef support
   */
  typedef TInputImage                      InputImageType;
  typedef TOutputImage                     OutputImageType;
  typedef typename InputImageType::Pointer InputImagePointer;

  typedef TInternalPrecision               InternalPrecisionType;

  /**
   * Smart pointer typedef support
   */
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /**
   * Run-time type information (and related methods)
   */
  itkTypeMacro(UnsharpMaskImageFilter, ImageToImageFilter);

  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);

#ifdef ITK_USE_CONCEPT_CHECKING
  itkConceptMacro( SameDimensionCheck,
    ( Concept::SameDimension< InputImageDimension, ImageDimension > ) );
  itkConceptMacro( InputHasNumericTraitsCheck,
    ( Concept::HasNumericTraits< OutputPixelType > ) );
  itkConceptMacro( InternalTypeIsFloatingPoint,
    ( Concept::IsFloatingPoint< TInternalPrecision > ) );
#endif

  typedef SmoothingRecursiveGaussianImageFilter<
    TInputImage, Image<TInternalPrecision, TOutputImage::ImageDimension> > GaussianType;

  typedef typename GaussianType::SigmaArrayType SigmaArrayType;

  /** Set/Get Sigma values measured in the units of image spacing. Default: 1.0. */
  itkSetMacro(Sigmas, SigmaArrayType);
  itkGetConstMacro(Sigmas, SigmaArrayType);

  /** Convenience method for setting all dimensional parameters
  * to the same values. */
  void SetSigma(const typename SigmaArrayType::ValueType sigma)
  {
    SigmaArrayType sigmas;
    sigmas.Fill(sigma);
    this->SetSigmas(sigmas); //checks whether it is actually modified
  }

  /** Set/Get amount of enhancement. Usual range: 0.1 to 2.0. Default: 0.5. */
  itkSetMacro(Amount, TInternalPrecision);
  itkGetConstMacro(Amount, TInternalPrecision);


  /** Set/Get threshold for enhancement. Default: 0. */
  itkSetMacro(Threshold, TInternalPrecision);
  itkGetConstMacro(Threshold, TInternalPrecision);

  /** Set/Get whether to clamp values to supported
  * range of output type. Default: On. */
  itkSetMacro(Clamp, bool);
  itkGetConstMacro(Clamp, bool);
  itkBooleanMacro(Clamp);

protected:
  UnsharpMaskImageFilter();
  virtual ~UnsharpMaskImageFilter() ITK_OVERRIDE {}

  /**
  * UnsharpMaskImageFilter needs a larger input requested region than
  * the output requested region (larger by the size of the
  * Gaussian kernel).  As such, UnsharpMaskImageFilter needs to
  * provide an implementation for GenerateInputRequestedRegion() in
  * order to inform the pipeline execution model.
  * \sa ImageToImageFilter::GenerateInputRequestedRegion() */
  virtual void GenerateInputRequestedRegion() ITK_OVERRIDE;

  virtual void VerifyPreconditions() ITK_OVERRIDE;
  virtual void GenerateData() ITK_OVERRIDE;

  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(UnsharpMaskImageFilter);

  /** The edge amplification amount */
  TInternalPrecision m_Amount;
  TInternalPrecision m_Threshold;
  SigmaArrayType     m_Sigmas;
  bool               m_Clamp;

  template<typename InPixelType, typename FunctorRealType = TInternalPrecision, typename OutPixelType = InPixelType>
  class UnsharpMaskingFunctor
  {
  private:
    FunctorRealType m_Amount;
    FunctorRealType m_Threshold;
    bool m_Clamp;

  public:
    UnsharpMaskingFunctor()
      : m_Amount(0.5),
      m_Threshold(0.0),
      m_Clamp(false)
    {
    }

    UnsharpMaskingFunctor(FunctorRealType amount, FunctorRealType threshold, bool clamp)
      : m_Amount(amount),
      m_Threshold(threshold),
      m_Clamp(clamp)
    {
      assert(m_Threshold >= 0.0);
    }

    bool operator==(const UnsharpMaskingFunctor & other)
    {
      return (m_Amount == other.m_Amount)
             && (m_Threshold == other.m_Threshold)
             && (m_Clamp == other.m_Clamp);
    }

    bool operator!= (const UnsharpMaskingFunctor & other)
    {
      return !(*this == other);
    }

    inline OutPixelType operator()(const InPixelType & v, const FunctorRealType & s) const
    {
      FunctorRealType diff = v - s;
      FunctorRealType result;
      if (diff > m_Threshold)
        {
        result = v + (diff - m_Threshold)*m_Amount;
        }
      else if (-diff > m_Threshold)
        {
        result = v + (diff + m_Threshold)*m_Amount;
        }
      else
        {
        result = v;
        }

      if (m_Clamp)
        {
        if (result < itk::NumericTraits< OutPixelType >::NonpositiveMin())
          {
          return itk::NumericTraits< OutPixelType >::NonpositiveMin();
          }
        else if (result > itk::NumericTraits< OutPixelType >::max())
          {
          return itk::NumericTraits< OutPixelType >::max();
          }
        }

      return static_cast<OutPixelType>(result);
    }
  }; //end UnsharpMaskingFunctor
}; //end UnsharpMaskImageFilter
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkUnsharpMaskImageFilter.hxx"
#endif

#endif
