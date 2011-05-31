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
#ifndef __itkTernaryFunctorImageFilter_h
#define __itkTernaryFunctorImageFilter_h

#include "itkInPlaceImageFilter.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkLinearInterpolateImageFunction.h"
#include "itkVectorLinearInterpolateImageFunction.h"

namespace itk
{
/** \class TernaryFunctorImageFilter
 * \brief Implements pixel-wise generic operation of three images.
 *
 * This class is parameterized over the types of the three input images
 * and the type of the output image.  It is also parameterized by the
 * operation to be applied, using a Functor style.
 *
 * \sa BinaryFunctorImageFilter UnaryFunctorImageFilter
 *
 * \ingroup IntensityImageFilters Multithreaded
 * \ingroup ITK-ImageFilterBase
 */
template< class TInputImage1, class TInputImage2,
          class TInputImage3, class TOutputImage, class TFunction    >
class ITK_EXPORT TernaryFunctorImageFilter:
  public InPlaceImageFilter< TInputImage1, TOutputImage >
{
public:
  /** Standard class typedefs. */
  typedef TernaryFunctorImageFilter                        Self;
  typedef InPlaceImageFilter< TInputImage1, TOutputImage > Superclass;
  typedef SmartPointer< Self >                             Pointer;
  typedef SmartPointer< const Self >                       ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(TernaryFunctorImageFilter, InPlaceImageFilter);

  /** Some typedefs. */
  typedef TFunction                              FunctorType;
  typedef TInputImage1                           Input1ImageType;
  typedef typename Input1ImageType::ConstPointer Input1ImagePointer;
  typedef typename Input1ImageType::RegionType   Input1ImageRegionType;
  typedef typename Input1ImageType::PixelType    Input1ImagePixelType;
  typedef TInputImage2                           Input2ImageType;
  typedef typename Input2ImageType::ConstPointer Input2ImagePointer;
  typedef typename Input2ImageType::RegionType   Input2ImageRegionType;
  typedef typename Input2ImageType::PixelType    Input2ImagePixelType;
  typedef TInputImage3                           Input3ImageType;
  typedef typename Input3ImageType::ConstPointer Input3ImagePointer;
  typedef typename Input3ImageType::RegionType   Input3ImageRegionType;
  typedef typename Input3ImageType::PixelType    Input3ImagePixelType;
  typedef TOutputImage                           OutputImageType;
  typedef typename OutputImageType::Pointer      OutputImagePointer;
  typedef typename OutputImageType::RegionType   OutputImageRegionType;
  typedef typename OutputImageType::PixelType    OutputImagePixelType;

  /** the generic type for interpolating the image in Physical Space
   *  Mode
   */
  typedef ImageFunction<Input2ImageType,
                        ITK_TYPENAME NumericTraits<Input2ImagePixelType>::RealType,
                        double>                  Interpolator2Type;
  typedef typename Interpolator2Type::OutputType Interpolator2OutputPixelType;
  typedef ImageFunction<Input3ImageType,
                        ITK_TYPENAME NumericTraits<Input3ImagePixelType>::RealType,
                        double>                  Interpolator3Type;
  typedef typename Interpolator3Type::OutputType Interpolator3OutputPixelType;

  /** Connect one of the operands for pixel-wise addition. */
  void SetInput1(const TInputImage1 *image1);

  /** Connect one of the operands for pixel-wise addition. */
  void SetInput2(const TInputImage2 *image2);

  /** Connect one of the operands for pixel-wise addition. */
  void SetInput3(const TInputImage3 *image3);

  /** Get the functor object.  The functor is returned by reference.
   * (Functors do not have to derive from itk::LightObject, so they do
   * not necessarily have a reference count. So we cannot return a
   * SmartPointer). */
  FunctorType & GetFunctor(void) { return m_Functor; }

  /** Get the functor object.  The functor is returned by reference.
   * (Functors do not have to derive from itk::LightObject, so they do
   * not necessarily have a reference count. So we cannot return a
   * SmartPointer.) */
  const FunctorType & GetFunctor() const
  {
    return m_Functor;
  }

  /** Set the functor object.  This replaces the current Functor with a
   * copy of the specified Functor. This allows the user to specify a
   * functor that has ivars set differently than the default functor.
   * This method requires an operator==() be defined on the functor
   * (or the compiler's default implementation of operator==() being
   * appropriate). */
  void SetFunctor(const FunctorType & functor)
  {
    if ( !( functor == m_Functor ) )
      {
      m_Functor = functor;
      this->Modified();
      }
  }

  /** Set/Get UsePhysicalSpace
   *  The default behavior of this filter is to assume that
   *  the dimensions, physical orientation, and spacing are
   *  the same.
   *  If UsePhysicalSpace is true, an InterpolateImageFunction is
   *  used to use the interpolated pixel in Image2 & Image3 that
   *  is at the physical location of the pixel in Image1 for
   *  computations.
   *  This allows, for example, a binary operation on two images
   *  that occupy the same physical volume but have different size
   *  & spacing.
   */
  itkSetMacro(UsePhysicalSpace,bool)
  itkGetMacro(UsePhysicalSpace,bool)

  /** Set the Interpolator for Physical Space Mode
   *  The default interpolator is the LinearInterpolateImageFunction,
   *  or for Vector Images, VectorLinearInterpolateImageFunction.
   */
  itkSetObjectMacro(Interpolator2, Interpolator2Type);
  itkSetObjectMacro(Interpolator3, Interpolator3Type);
  itkGetObjectMacro(Interpolator2, Interpolator2Type);
  itkGetObjectMacro(Interpolator3, Interpolator3Type);

  /** Set the default pixel value.  In Physical Space Mode, it is
   *  possible for the two images to have disjoint physical volumes,
   *  and in that case, the default pixel value will be used for
   *  pixels outside Image2's image volume.
   */
  itkSetMacro(DefaultValue2, Interpolator2OutputPixelType);
  itkGetMacro(DefaultValue2, Interpolator2OutputPixelType);
  itkSetMacro(DefaultValue3, Interpolator3OutputPixelType);
  itkGetMacro(DefaultValue3, Interpolator3OutputPixelType);

  /** Image dimensions */
  itkStaticConstMacro(Input1ImageDimension, unsigned int,
                      TInputImage1::ImageDimension);
  itkStaticConstMacro(Input2ImageDimension, unsigned int,
                      TInputImage2::ImageDimension);
  itkStaticConstMacro(Input3ImageDimension, unsigned int,
                      TInputImage3::ImageDimension);
  itkStaticConstMacro(OutputImageDimension, unsigned int,
                      TOutputImage::ImageDimension);

#ifdef ITK_USE_CONCEPT_CHECKING
  /** Begin concept checking */
  itkConceptMacro( SameDimensionCheck1,
                   ( Concept::SameDimension< Input1ImageDimension, Input2ImageDimension > ) );
  itkConceptMacro( SameDimensionCheck2,
                   ( Concept::SameDimension< Input1ImageDimension, Input3ImageDimension > ) );
  itkConceptMacro( SameDimensionCheck3,
                   ( Concept::SameDimension< Input1ImageDimension, OutputImageDimension > ) );
  /** End concept checking */
#endif
protected:
  TernaryFunctorImageFilter();
  virtual ~TernaryFunctorImageFilter() {}

  /** Validate the presence of all three inputs. If one or more inputs
   * are missing, throw an exception. */
  void BeforeThreadedGenerateData();

  /** TernaryFunctorImageFilter can be implemented as a multithreaded filter.
   * Therefore, this implementation provides a ThreadedGenerateData() routine
   * which is called for each processing thread. The output image data is
   * allocated automatically by the superclass prior to calling
   * ThreadedGenerateData().  ThreadedGenerateData can only write to the
   * portion of the output image specified by the parameter
   * "outputRegionForThread"
   *
   * \sa ImageToImageFilter::ThreadedGenerateData(),
   *     ImageToImageFilter::GenerateData()  */
  void ThreadedGenerateData(const OutputImageRegionType & outputRegionForThread,
                            int threadId);

private:
  TernaryFunctorImageFilter(const Self &); //purposely not implemented
  void operator=(const Self &);            //purposely not implemented

   /** Instantiate default interpolator, based on pixel type
   */
  template <typename TScalar>
    typename Interpolator2Type::Pointer
    NewDefaultInterpolator2(TScalar *)
  {
    return LinearInterpolateImageFunction<TInputImage2>::New().GetPointer();
  }

  template <typename TScalar,unsigned VVecLength>
    typename Interpolator2Type::Pointer
    NewDefaultInterpolator2(Vector<TScalar,VVecLength> *)
  {
    return VectorLinearInterpolateImageFunction<TInputImage2>::New().GetPointer();
  }

  template <typename TScalar>
    typename Interpolator3Type::Pointer
    NewDefaultInterpolator3(TScalar *)
  {
    return LinearInterpolateImageFunction<TInputImage3>::New().GetPointer();
  }

  template <typename TScalar,unsigned VVecLength>
    typename Interpolator3Type::Pointer
    NewDefaultInterpolator3(Vector<TScalar,VVecLength> *)
  {
    return VectorLinearInterpolateImageFunction<TInputImage3>::New().GetPointer();
  }

  FunctorType m_Functor;

  /** whether or not to use physical space for calculations */
  bool                                m_UsePhysicalSpace;
  /** true if size, spacing, origin, and orientation match */
  bool                                m_PhysicalSpacesMatch;
  /** interpolators to use by default if none is set. */
  typename Interpolator2Type::Pointer m_Interpolator2;
  typename Interpolator3Type::Pointer m_Interpolator3;
  /** default pixel value to use if interpolator is given a
   *  point outside the image volume.
   */
  Interpolator2OutputPixelType        m_DefaultValue2;
  Interpolator3OutputPixelType        m_DefaultValue3;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkTernaryFunctorImageFilter.txx"
#endif

#endif
