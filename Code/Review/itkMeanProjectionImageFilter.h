#ifndef __itkMeanProjectionImageFilter_h
#define __itkMeanProjectionImageFilter_h

#include "itkProjectionImageFilter.h"
#include "itkNumericTraits.h"

namespace itk {
/** \class MeanProjectionImageFilter
 * \brief Mean projection
 *
 * \author Gaëtan Lehmann. Biologie du Développement et de la Reproduction, INRA de Jouy-en-Josas, France.
 *
 * \sa ProjectionImageFilter
 */


namespace Function {
template <class TInputPixel, class TAccumulate>
class MeanAccumulator
{
public:
  typedef typename NumericTraits<TInputPixel>::RealType RealType;

  MeanAccumulator( unsigned long size )
    {
    m_Size = size;
    }
  ~MeanAccumulator(){}

  inline void Init()
    {
    m_Sum = NumericTraits< TAccumulate >::Zero;
    }

  inline void operator()( const TInputPixel &input )
    {
    m_Sum = m_Sum + input;
    }

  inline RealType GetValue()
    {
    return ((RealType) m_Sum) / m_Size;
    }

  TAccumulate m_Sum;
  unsigned long m_Size;
};
} // end namespace Function


template <class TInputImage, class TOutputImage, class TAccumulate=typename NumericTraits< typename TOutputImage::PixelType >::AccumulateType >
class ITK_EXPORT MeanProjectionImageFilter :
    public
    ProjectionImageFilter<TInputImage, TOutputImage,
      Function::MeanAccumulator< typename TInputImage::PixelType, TAccumulate > >
{
public:
  typedef MeanProjectionImageFilter Self;
  typedef ProjectionImageFilter<TInputImage, TOutputImage, 
    Function::MeanAccumulator< typename TInputImage::PixelType, TAccumulate > > Superclass;

  typedef SmartPointer<Self>   Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Runtime information support. */
  itkTypeMacro(MeanProjectionImageFilter, ProjectionImageFilter);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);


protected:
  MeanProjectionImageFilter() {}
  virtual ~MeanProjectionImageFilter() {}

private:
  MeanProjectionImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented



}; // end MeanProjectionImageFilter

} //end namespace itk
#endif
