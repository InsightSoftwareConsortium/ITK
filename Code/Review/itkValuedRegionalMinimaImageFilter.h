#ifndef __itkValuedRegionalMinimaImageFilter_h
#define __itkValuedRegionalMinimaImageFilter_h

#include "itkValuedRegionalExtremaImageFilter.h"
#include "itkNumericTraits.h"

namespace itk {
/** \class ValuedRegionalMinimaImageFilter
 * \brief Transforms the image so that any pixel that is not a
 * regional minima is set to the maximum value for the pixel
 * type. Pixels that are regional minima retain their value.
 *
 * Regional minima are flat zones surrounded by pixels of higher
 * value. A completely flat image will be marked as a regional minima
 * by this filter.

 * \author Richard Beare. Department of Medicine, Monash University,
 * Melbourne, Australia.
 *
 * \sa ValuedRegionalMaximaImageFilter, ValuedRegionalExtremaImageFilter, HMinimaImageFilter
 * \ingroup MathematicalMorphologyImageFilters
 */


template <class TInputImage, class TOutputImage>
class ITK_EXPORT ValuedRegionalMinimaImageFilter :
    public
    ValuedRegionalExtremaImageFilter<TInputImage, TOutputImage,
             std::less<typename TInputImage::PixelType>,
             std::less<typename TOutputImage::PixelType>
    >
{
public:
  typedef ValuedRegionalMinimaImageFilter Self;
  typedef ValuedRegionalExtremaImageFilter<TInputImage, TOutputImage,
             std::less<typename TInputImage::PixelType>,
             std::less<typename TOutputImage::PixelType>  > Superclass;

  typedef SmartPointer<Self>   Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);


protected:
  ValuedRegionalMinimaImageFilter() 
  {
    SetMarkerValue(NumericTraits<typename TOutputImage::PixelType>::max());
  }
  virtual ~ValuedRegionalMinimaImageFilter() {}

private:
  ValuedRegionalMinimaImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented



}; // end ValuedRegionalMinimaImageFilter

} //end namespace itk
#endif
