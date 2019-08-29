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
#ifndef itkConnectedComponentFunctorImageFilter_h
#define itkConnectedComponentFunctorImageFilter_h

#include "itkConnectedComponentImageFilter.h"

namespace itk
{
/**
 * \class ConnectedComponentFunctorImageFilter
 * \brief A generic connected components filter that labels the
 * objects in an artibitrary image.
 *
 * ConnectedComponentFunctorImageFilter labels the objects in an arbitrary
 * image. Each distinct object is assigned a unique label. The filter makes
 * three passes through the image.  The first pass initializes the
 * output.  The second pass labels each foreground pixel such that all
 * the pixels associated with an object either have the same label or
 * have had their labels entered into a equivalency table.  The third
 * pass through the image flattens the equivalency table such that all
 * pixels for an object have the same label.
 *
 * The functor specifies the criteria to join neighboring pixels.  For
 * example a simple intensity threshold difference might be used for
 * scalar imagery.
 *
 * The final object labels are in no particular order (and some object
 * labels may not be used on the final objects).  You can reorder the
 * labels such that object labels are consecutive and sorted based on
 * object size by passing the output of this filter to a
 * RelabelComponentImageFilter.
 *
 * \sa ImageToImageFilter
 * \ingroup ITKConnectedComponents
 */

template <typename TInputImage, typename TOutputImage, typename TFunctor, typename TMaskImage = TInputImage>
class ITK_TEMPLATE_EXPORT ConnectedComponentFunctorImageFilter
  : public ConnectedComponentImageFilter<TInputImage, TOutputImage, TMaskImage>
{
public:
  /**
   * Standard "Self" & Superclass typedef.
   */
  using Self = ConnectedComponentFunctorImageFilter;
  using Superclass = ConnectedComponentImageFilter<TInputImage, TOutputImage, TMaskImage>;

  /**
   * Types from the Superclass
   */
  using InputImagePointer = typename Superclass::InputImagePointer;

  /**
   * Extract some information from the image types.  Dimensionality
   * of the two images is assumed to be the same.
   */
  using OutputPixelType = typename TOutputImage::PixelType;
  using OutputInternalPixelType = typename TOutputImage::InternalPixelType;
  using InputPixelType = typename TInputImage::PixelType;
  using InputInternalPixelType = typename TInputImage::InternalPixelType;
  using MaskPixelType = typename TMaskImage::PixelType;
  static constexpr unsigned int ImageDimension = TOutputImage::ImageDimension;
  static constexpr unsigned int InputImageDimension = TInputImage::ImageDimension;

  /**
   * Image type alias support
   */
  using FunctorType = TFunctor;
  using InputImageType = TInputImage;
  using MaskImageType = TMaskImage;
  using OutputImageType = TOutputImage;

  using IndexType = typename TInputImage::IndexType;
  using SizeType = typename TInputImage::SizeType;
  using RegionType = typename TOutputImage::RegionType;
  using ListType = std::list<IndexType>;

  using MaskImagePointer = typename MaskImageType::Pointer;

  /**
   * Smart pointer type alias support
   */
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /**
   * Run-time type information (and related methods)
   */
  itkTypeMacro(ConnectedComponentFunctorImageFilter, ImageToImageFilter);

  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);

  /** Get the functor object.  The functor is returned by reference.
   * (Functors do not have to derive from itk::LightObject, so they do
   * not necessarily have a reference count. So we cannot return a
   * SmartPointer.) */
  FunctorType &
  GetFunctor()
  {
    return m_Functor;
  }
  const FunctorType &
  GetFunctor() const
  {
    return m_Functor;
  }

  /** Set the functor object.  This replaces the current Functor with a
   * copy of the specified Functor. This allows the user to specify a
   * functor that has ivars set differently than the default functor.
   * This method requires an operator!=() be defined on the functor
   * (or the compiler's default implementation of operator!=() being
   * appropriate). */
  void
  SetFunctor(const FunctorType & functor)
  {
    m_Functor = functor;
    this->Modified();
  }

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro(SameDimensionCheck, (Concept::SameDimension<InputImageDimension, ImageDimension>));
  itkConceptMacro(InputEqualityComparableCheck, (Concept::EqualityComparable<InputPixelType>));
  itkConceptMacro(OutputEqualityComparableCheck, (Concept::EqualityComparable<OutputPixelType>));
  itkConceptMacro(OutputConvertibleToUnsignedIntCheck, (Concept::Convertible<OutputPixelType, unsigned int>));
  itkConceptMacro(OutputConvertibleToUnsignedLongCheck, (Concept::Convertible<OutputPixelType, unsigned long>));
  itkConceptMacro(OutputConvertibleToLongCheck, (Concept::Convertible<OutputPixelType, long>));
  itkConceptMacro(UnsignedLongConvertibleToOutputCheck, (Concept::Convertible<unsigned long, OutputPixelType>));
  itkConceptMacro(OutputIncrementDecrementOperatorsCheck, (Concept::IncrementDecrementOperators<OutputPixelType>));
  // End concept checking
#endif

protected:
  ConnectedComponentFunctorImageFilter() = default;
  ~ConnectedComponentFunctorImageFilter() override = default;
  ConnectedComponentFunctorImageFilter(const Self &) {}

  FunctorType m_Functor;

  /**
   * Standard pipeline method.
   */
  void
  GenerateData() override;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkConnectedComponentFunctorImageFilter.hxx"
#endif

#endif
