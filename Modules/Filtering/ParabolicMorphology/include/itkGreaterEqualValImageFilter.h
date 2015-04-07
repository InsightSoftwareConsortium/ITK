#ifndef __itkGreaterEqualValImageFilter_h
#define __itkGreaterEqualValImageFilter_h
#include <itkUnaryFunctorImageFilter.h>
namespace itk
{

/** \class GreaterEqualValImageFilter
 * \brief Computes the absolute difference between an image and a
 * constant. Can be done with ShiftScale and AbsIamgeFilters.
 */


namespace Function
{

template <class TInput, class TOutput>
class GEConst
{
public:
  GEConst() { m_Val = (TInput)0.0; }
  void
  SetVal(const TInput i)
  {
    m_Val = i;
  }

  ~GEConst() {}
  bool
  operator!=(const GEConst &) const
  {
    return false;
  }
  bool
  operator==(const GEConst & other) const
  {
    return !(*this != other);
  }
  inline TOutput
  operator()(const TInput & A)
  {
    return static_cast<TOutput>(A >= m_Val);
  }

private:
  TInput m_Val;
};
} // namespace Function

template <class TInputImage, class TOutputImage>
class ITK_EXPORT GreaterEqualValImageFilter
  : public itk::UnaryFunctorImageFilter<
      TInputImage,
      TOutputImage,
      Function::GEConst<typename TInputImage::PixelType, typename TOutputImage::PixelType>>
{
public:
  /** Standard class typedefs. */
  typedef GreaterEqualValImageFilter Self;
  typedef UnaryFunctorImageFilter<TInputImage,
                                  TOutputImage,
                                  Function::GEConst<typename TInputImage::PixelType, typename TOutputImage::PixelType>>
                                   Superclass;
  typedef SmartPointer<Self>       Pointer;
  typedef SmartPointer<const Self> ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  void
  SetVal(typename TInputImage::PixelType val)
  {
    this->GetFunctor().SetVal(val);
    this->Modified();
  }


#ifdef ITK_USE_CONCEPT_CHECKING
  /** Begin concept checking */
  itkConceptMacro(InputConvertibleToDoubleCheck, (Concept::Convertible<typename TInputImage::PixelType, double>));
  itkConceptMacro(DoubleConvertibleToOutputCheck, (Concept::Convertible<double, typename TOutputImage::PixelType>));
  /** End concept checking */
#endif

protected:
  GreaterEqualValImageFilter() {}
  virtual ~GreaterEqualValImageFilter() {}

private:
  GreaterEqualValImageFilter(const Self &); // purposely not implemented
  void
  operator=(const Self &); // purposely not implemented
};

} // end namespace itk


#endif
