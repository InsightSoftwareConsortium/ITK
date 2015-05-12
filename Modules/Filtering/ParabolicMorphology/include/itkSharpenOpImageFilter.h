
#ifndef __itkSharpenOpImageFilter_h
#define __itkSharpenOpImageFilter_h

#include "itkTernaryFunctorImageFilter.h"

namespace itk
{

/** \class SharpenOpImageFilter
 * \brief Implements the sharpening operation. The inputs are the
 * dilated, eroded and original images.
 *
 * This class is parametrized over the types of the three
 * input images and the type of the output image.
 * Numeric conversions (castings) are done by the C++ defaults.
 *
 * In reality the input and output types of this filter are expected
 * to be the same.
 *
 * \author Richard Beare, Department of Medicine, Monash University,
 * Australia.  <Richard.Beare@monash.edu>
 *
 */
namespace Function
{

template <class TInput1, class TInput2, class TInput3, class TOutput>
class SharpM
{
public:
  SharpM() {}
  ~SharpM() {}
  bool
  operator!=(const SharpM &) const
  {
    return false;
  }
  bool
  operator==(const SharpM & other) const
  {
    return !(*this != other);
  }
  inline TOutput
  operator()(const TInput1 & A, const TInput2 & B, const TInput3 & C)
  {
    // the sharpening operator. A is the dilation, B the original, C
    // the erosion
    TInput2 diff1 = A - B;
    TInput2 diff2 = B - C;

    if (diff1 < diff2)
      return (TOutput)A;
    if (diff2 < diff1)
      return (TOutput)C;
    return ((TOutput)B);
  }
};
} // namespace Function

template <class TInputImage1, class TInputImage2, class TInputImage3, class TOutputImage>
class ITK_EXPORT SharpenOpImageFilter
  : public TernaryFunctorImageFilter<TInputImage1,
                                     TInputImage2,
                                     TInputImage3,
                                     TOutputImage,
                                     Function::SharpM<typename TInputImage1::PixelType,
                                                      typename TInputImage2::PixelType,
                                                      typename TInputImage3::PixelType,
                                                      typename TOutputImage::PixelType>>
{
public:
  /** Standard class typedefs. */
  typedef SharpenOpImageFilter Self;
  typedef TernaryFunctorImageFilter<TInputImage1,
                                    TInputImage2,
                                    TInputImage3,
                                    TOutputImage,
                                    Function::SharpM<typename TInputImage1::PixelType,
                                                     typename TInputImage2::PixelType,
                                                     typename TInputImage3::PixelType,
                                                     typename TOutputImage::PixelType>>
                                   Superclass;
  typedef SmartPointer<Self>       Pointer;
  typedef SmartPointer<const Self> ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(SharpenOpImageFilter, TernaryFunctorImageFilter);

protected:
  SharpenOpImageFilter() {}
  virtual ~SharpenOpImageFilter() {}

private:
  SharpenOpImageFilter(const Self &); // purposely not implemented
  void
  operator=(const Self &); // purposely not implemented
};

} // end namespace itk


#endif
