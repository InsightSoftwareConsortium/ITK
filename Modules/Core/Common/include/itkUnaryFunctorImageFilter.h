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
#ifndef itkUnaryFunctorImageFilter_h
#define itkUnaryFunctorImageFilter_h

#include "itkMath.h"
#include "itkInPlaceImageFilter.h"
#include "itkImageRegionIteratorWithIndex.h"

namespace itk
{
/** \class UnaryFunctorImageFilter
 * \brief Implements pixel-wise generic operation on one image.
 *
 * This class is parameterized over the type of the input image and
 * the type of the output image.  It is also parameterized by the
 * operation to be applied, using a Functor style.
 *
 * UnaryFunctorImageFilter allows the output dimension of the filter
 * to be larger than the input dimension. Thus subclasses of the
 * UnaryFunctorImageFilter (like the CastImageFilter) can be used
 * to promote a 2D image to a 3D image, etc.
 *
 * \sa BinaryFunctorImageFilter TernaryFunctorImageFilter
 *
 * \ingroup   IntensityImageFilters     MultiThreaded
 * \ingroup ITKCommon
 *
 * \wiki
 * \wikiexample{ImageProcessing/UnaryFunctorImageFilter,Apply a custom operation to each pixel in an image}
 * \endwiki
 */
template< typename TInputImage, typename TOutputImage, typename TFunction >
class ITK_TEMPLATE_EXPORT UnaryFunctorImageFilter:public InPlaceImageFilter< TInputImage, TOutputImage >
{
public:
  /** Standard class typedefs. */
  typedef UnaryFunctorImageFilter                         Self;
  typedef InPlaceImageFilter< TInputImage, TOutputImage > Superclass;
  typedef SmartPointer< Self >                            Pointer;
  typedef SmartPointer< const Self >                      ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(UnaryFunctorImageFilter, InPlaceImageFilter);

  /** Some typedefs. */
  typedef TFunction FunctorType;

  typedef TInputImage                              InputImageType;
  typedef typename    InputImageType::ConstPointer InputImagePointer;
  typedef typename    InputImageType::RegionType   InputImageRegionType;
  typedef typename    InputImageType::PixelType    InputImagePixelType;

  typedef TOutputImage                             OutputImageType;
  typedef typename     OutputImageType::Pointer    OutputImagePointer;
  typedef typename     OutputImageType::RegionType OutputImageRegionType;
  typedef typename     OutputImageType::PixelType  OutputImagePixelType;

  /** Get the functor object.  The functor is returned by reference.
   * (Functors do not have to derive from itk::LightObject, so they do
   * not necessarily have a reference count. So we cannot return a
   * SmartPointer.) */
  FunctorType &       GetFunctor() { return m_Functor; }
  const FunctorType & GetFunctor() const { return m_Functor; }

  /** Set the functor object.  This replaces the current Functor with a
   * copy of the specified Functor. This allows the user to specify a
   * functor that has ivars set differently than the default functor.
   * This method requires an operator!=() be defined on the functor
   * (or the compiler's default implementation of operator!=() being
   * appropriate). */
  void SetFunctor(const FunctorType & functor)
  {
    if ( m_Functor != functor )
      {
      m_Functor = functor;
      this->Modified();
      }
  }

protected:
  UnaryFunctorImageFilter();
  virtual ~UnaryFunctorImageFilter() ITK_OVERRIDE {}

  /** UnaryFunctorImageFilter can produce an image which is a different
   * resolution than its input image.  As such, UnaryFunctorImageFilter
   * needs to provide an implementation for
   * GenerateOutputInformation() in order to inform the pipeline
   * execution model.  The original documentation of this method is
   * below.
   *
   * \sa ProcessObject::GenerateOutputInformaton()  */
  virtual void GenerateOutputInformation() ITK_OVERRIDE;

  /** UnaryFunctorImageFilter can be implemented as a multithreaded filter.
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
                            ThreadIdType threadId) ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(UnaryFunctorImageFilter);

  FunctorType m_Functor;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkUnaryFunctorImageFilter.hxx"
#endif

#endif
