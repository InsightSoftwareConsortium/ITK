#ifndef __itkMaximumProjectionImageFilter_h
#define __itkMaximumProjectionImageFilter_h

#include "itkProjectionImageFilter.h"
#include "itkNumericTraits.h"

namespace itk {
/** \class MaximumProjectionImageFilter
 * \brief Maximum projection
 *
 * \author Gaëtan Lehmann. Biologie du Développement et de la Reproduction, INRA de Jouy-en-Josas, France.
 *
 * \sa ProjectionImageFilter
 */


namespace Function {
template <class TInputPixel>
class MaximumAccumulator
{
public:
  MaximumAccumulator( unsigned long size ) {}
  ~MaximumAccumulator(){}

  inline void Init()
    {
    m_Maximum = NumericTraits< TInputPixel >::NonpositiveMin();
    }

  inline void operator()( const TInputPixel &input )
    {
    m_Maximum = vnl_math_max( m_Maximum, input );
    }

  inline TInputPixel GetValue()
    {
    return m_Maximum;
    }

  TInputPixel m_Maximum;
};
} // end namespace Function


template <class TInputImage, class TOutputImage>
class ITK_EXPORT MaximumProjectionImageFilter :
    public
    ProjectionImageFilter<TInputImage, TOutputImage, Function::MaximumAccumulator< typename TInputImage::PixelType > >
{
public:
  typedef MaximumProjectionImageFilter Self;
  typedef ProjectionImageFilter<TInputImage, TOutputImage, Function::MaximumAccumulator< typename TInputImage::PixelType > > Superclass;

  typedef SmartPointer<Self>   Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Runtime information support. */
  itkTypeMacro(MaximumProjectionImageFilter, ProjectionImageFilter);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);


protected:
  MaximumProjectionImageFilter() {}
  virtual ~MaximumProjectionImageFilter() {}

private:
  MaximumProjectionImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented



}; // end MaximumProjectionImageFilter

} //end namespace itk
#endif
