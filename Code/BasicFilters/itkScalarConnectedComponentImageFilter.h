/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkScalarConnectedComponentImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkScalarConnectedComponentImageFilter_h
#define __itkScalarConnectedComponentImageFilter_h

#include "itkNumericTraits.h"
#include "itkConnectedComponentFunctorImageFilter.h"

namespace itk
{

/** \class ScalarConnectedComponentImageFilter
 *
 *  \brief A connected components filter that labels the
 *         objects in an arbitrary image.  Two pixels are
 *         similar if they are within threshold of each other.
 *         Uses ConnectedComponentFunctorImageFilter.
 */

namespace Functor {  
  
template<class TInput>
class SimilarPixelsFunctor
{
public:
  SimilarPixelsFunctor()
    {threshold = itk::NumericTraits<TInput>::Zero;};

  ~SimilarPixelsFunctor() {};

  void SetDistanceThreshold(const TInput &thresh) {threshold = thresh;};
  TInput GetDistanceThreshold() {return (threshold);};
  
  bool operator()(const TInput &a, const TInput &b) {
    return (typename itk::NumericTraits<TInput>::AbsType(a-b) <= threshold);
  };

protected:
  TInput threshold;
                            
};

} // end namespace Functor 

template <class TInputImage, class TOutputImage, class TMaskImage=TInputImage>
class ITK_EXPORT ScalarConnectedComponentImageFilter :
    public ConnectedComponentFunctorImageFilter<TInputImage,TOutputImage,
             Functor::SimilarPixelsFunctor<typename TInputImage::ValueType>,
                                           TMaskImage> 
{
public:
  /** Standard class typedefs. */
  typedef ScalarConnectedComponentImageFilter Self;
  typedef ConnectedComponentFunctorImageFilter<TInputImage,TOutputImage, 
             Functor::SimilarPixelsFunctor<typename TInputImage::ValueType>,
             TMaskImage> Superclass;
  typedef SmartPointer<Self>   Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(ScalarConnectedComponentImageFilter,ConnectedComponentFunctorImageFilter);

  typedef typename TInputImage::PixelType  InputPixelType;
  
  virtual void SetDistanceThreshold(const InputPixelType& thresh)
    {this->GetFunctor().SetDistanceThreshold(thresh);}

  virtual InputPixelType GetDistanceThreshold()
    {return (this->GetFunctor().GetDistanceThreshold());}
  
protected:
  ScalarConnectedComponentImageFilter() {};
  virtual ~ScalarConnectedComponentImageFilter() {};

private:
  ScalarConnectedComponentImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

};

} // end namespace itk

#endif
