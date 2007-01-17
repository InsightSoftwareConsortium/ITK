#ifndef __itkValuedRegionalMaximaImageFilter_h
#define __itkValuedRegionalMaximaImageFilter_h

#include "itkValuedRegionalExtremaImageFilter.h"
#include "itkNumericTraits.h"

namespace itk {
/** \class ValuedRegionalMaximaImageFilter
 * \brief Transforms the image so that any pixel that is not a
 * regional maxima is set to the minimum value for the pixel
 * type. Pixels that are regional maxima retain their value.
 *
 * Regional maxima are flat zones surrounded by pixels of lower
 * value. A completely flat image will be marked as a regional maxima
 * by this filter.

 * \author Richard Beare. Department of Medicine, Monash University,
 * Melbourne, Australia.
 *
 * \sa ValuedRegionalMinimaImageFilter, ValuedRegionalExtremaImageFilter, HMinimaImageFilter
 * \ingroup MathematicalMorphologyImageFilters
 */


template <class TInputImage, class TOutputImage>
class ITK_EXPORT ValuedRegionalMaximaImageFilter :
    public
    ValuedRegionalExtremaImageFilter<TInputImage, TOutputImage,
             std::greater<typename TInputImage::PixelType>,
             std::greater<typename TOutputImage::PixelType>  >
{
public:
  typedef ValuedRegionalMaximaImageFilter Self;
  typedef ValuedRegionalExtremaImageFilter<TInputImage, TOutputImage,
             std::greater<typename TInputImage::PixelType>,
             std::greater<typename TOutputImage::PixelType> > Superclass;

  typedef SmartPointer<Self>   Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);


protected:
  ValuedRegionalMaximaImageFilter() 
  {
    SetMarkerValue(NumericTraits<typename TOutputImage::PixelType>::NonpositiveMin());
  }
  virtual ~ValuedRegionalMaximaImageFilter() {}

private:
  ValuedRegionalMaximaImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented



}; // end ValuedRegionalMaximaImageFilter

} //end namespace itk
#endif
