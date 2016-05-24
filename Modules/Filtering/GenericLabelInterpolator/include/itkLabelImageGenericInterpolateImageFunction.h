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
#ifndef itkLabelImageGenericInterpolateImageFunction_h
#define itkLabelImageGenericInterpolateImageFunction_h

#include <itkInterpolateImageFunction.h>
#include "itkLabelSelectionImageAdaptor.h"
#include <vector>
#include <set>

namespace itk
{

/** \class LabelImageGenericInterpolateImageFunction
 * \brief Interpolation function for multi-label images that implicitly interpolates each
 * unique value in the image corresponding to each label set element and returns the
 * corresponding label set element with the largest weight.
 *
 * This filter is an alternative to nearest neighbor interpolation for multi-label
 * images. It can use almost any underlying interpolator.
 * * \ingroup ITKImageFunction
 * * \ingroup GenericLabelInterpolator
 */

template <typename TInputImage,template<class, typename> class TInterpolator, typename TCoordRep=double >
class ITK_EXPORT LabelImageGenericInterpolateImageFunction :
  public InterpolateImageFunction<TInputImage, TCoordRep>
{
public:
  /** Standard class typedefs. */
  typedef LabelImageGenericInterpolateImageFunction                Self;
  typedef InterpolateImageFunction<TInputImage, TCoordRep>         Superclass;
  typedef SmartPointer<Self>                                       Pointer;
  typedef SmartPointer<const Self>                                 ConstPointer;
  typedef typename TInputImage::PixelType                          InputPixelType;

  /** Run-time type information (and related methods). */
  itkTypeMacro( LabelImageGenericInterpolateImageFunction, InterpolateImageFunction );

  /** Method for creation through the object factory. */
  itkNewMacro( Self );

  /** ImageDimension constant */
  itkStaticConstMacro( ImageDimension, unsigned int, TInputImage::ImageDimension );

  /** OutputType typedef support. */
  typedef typename Superclass::OutputType OutputType;

  /** InputImageType typedef support. */
  typedef typename Superclass::InputImageType InputImageType;

  /** RealType typedef support. */
  typedef typename Superclass::RealType RealType;

  /** Index typedef support. */
  typedef typename Superclass::IndexType IndexType;

  /** ContinuousIndex typedef support. */
  typedef typename Superclass::ContinuousIndexType ContinuousIndexType;

  typedef LabelSelectionImageAdaptor<TInputImage,double> LabelSelectionAdaptorType;

  // The interpolator used for individual binary masks corresponding to each label
  typedef TInterpolator<LabelSelectionAdaptorType,TCoordRep> InternalInterpolatorType;

  /**
   * Evaluate at the given index
   */
  virtual OutputType EvaluateAtContinuousIndex(
    const ContinuousIndexType & cindex ) const
    {
    return this->EvaluateAtContinuousIndex( cindex, NULL );
    }

  virtual void SetInputImage( const TInputImage *image );

protected:
  LabelImageGenericInterpolateImageFunction();
  ~LabelImageGenericInterpolateImageFunction(){};

  std::vector<typename InternalInterpolatorType::Pointer>   m_InternalInterpolators;
  std::vector<typename LabelSelectionAdaptorType::Pointer>  m_LabelSelectionAdaptors;
  typedef std::set<typename TInputImage::PixelType>         LabelSetType;
  LabelSetType m_Labels;

private:
  LabelImageGenericInterpolateImageFunction( const Self& ); //purposely not implemented
  void operator=( const Self& ); //purposely not implemented

  /**
   * Evaluate function value at the given index
   */
  virtual OutputType EvaluateAtContinuousIndex(
    const ContinuousIndexType &, OutputType * ) const;
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkLabelImageGenericInterpolateImageFunction.hxx"
#endif

#endif
