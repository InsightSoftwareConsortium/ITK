#ifndef __itkSigmaProjectionImageFilter_h
#define __itkSigmaProjectionImageFilter_h

#include "itkProjectionImageFilter.h"
#include "itkNumericTraits.h"

namespace itk {
/** \class SigmaProjectionImageFilter
 * \brief Mean projection
 *
 * \author Gaëtan Lehmann. Biologie du Développement et de la Reproduction, INRA de Jouy-en-Josas, France.
 *
 * \sa ProjectionImageFilter
 */


namespace Function {
template <class TInputPixel, class TAccumulate>
class SigmaAccumulator
{
public:
  typedef typename NumericTraits<TInputPixel>::RealType RealType;

  SigmaAccumulator( unsigned long size )
    {
    m_Size = size;
    m_Values.reserve( size );
    }
  ~SigmaAccumulator(){}

  inline void Init()
    {
    m_Sum = NumericTraits< TAccumulate >::Zero;
    m_Values.clear();
    }

  inline void operator()( const TInputPixel &input )
    {
    m_Sum = m_Sum + input;
    m_Values.push_back( input );
    }

  inline RealType GetValue()
    {
    // to avoid division by zero
    if( m_Size <= 1 )
      return NumericTraits<RealType>::Zero;

    typename NumericTraits<TInputPixel>::RealType mean = ((RealType) m_Sum) / m_Size;
    typename std::vector<TInputPixel>::iterator it;
    RealType squaredSum = NumericTraits<RealType>::Zero;
    for( it=m_Values.begin(); it!=m_Values.end(); it++ )
      {
      squaredSum += vnl_math_sqr(*it - mean);
      }
    return vcl_sqrt( squaredSum / ( m_Size - 1) );
    }

  TAccumulate m_Sum;
  unsigned long m_Size;
  std::vector<TInputPixel> m_Values;
};
} // end namespace Function


template <class TInputImage, class TOutputImage, class TAccumulate= ITK_TYPENAME NumericTraits< ITK_TYPENAME TOutputImage::PixelType >::AccumulateType >
class ITK_EXPORT SigmaProjectionImageFilter :
    public
    ProjectionImageFilter<TInputImage, TOutputImage,
      Function::SigmaAccumulator< ITK_TYPENAME TInputImage::PixelType, TAccumulate > >
{
public:
  typedef SigmaProjectionImageFilter Self;
  typedef ProjectionImageFilter<TInputImage, TOutputImage, 
    Function::SigmaAccumulator< typename TInputImage::PixelType, TAccumulate > > Superclass;

  typedef SmartPointer<Self>   Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Runtime information support. */
  itkTypeMacro(SigmaProjectionImageFilter, ProjectionImageFilter);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);


protected:
  SigmaProjectionImageFilter() {}
  virtual ~SigmaProjectionImageFilter() {}

private:
  SigmaProjectionImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented



}; // end SigmaProjectionImageFilter

} //end namespace itk

#endif
