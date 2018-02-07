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

#ifndef itkEigenToMeasureImageFilter_h
#define itkEigenToMeasureImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkSpatialObject.h"
#include "itkSimpleDataObjectDecorator.h"

namespace itk {
/** \class EigenToMeasureImageFilter
 * \brief Abstract class for computing a measure from local structure.
 *
 * TODO
 * 
 * This is an abstract class that estimates parameters for local-structure enhancement
 * from eigen-images. Other classes should inherit from this class so they can be used
 * in the MultiScaleHessianEnhancementImageFilter framework.
 * 
 * To estimate the parameters for local-structure enhancement, every pixel in the input
 * image must be seen. To implement this functionality in a memory efficinet way, this
 * class inherits from itk::StreamingImageFilter. This algorithm can be spead up by
 * restricting the output image region.
 * 
 * \sa MultiScaleHessianEnhancementImageFilter
 * \sa UnaryFunctorImageFilter
 * 
 * \author: Bryce Besler
 * \ingroup BoneEnhancement
 */
template< typename TInputImage, typename TOutputImage, typename TInputSpatialObject, typename TFunction>
class ITK_TEMPLATE_EXPORT EigenToMeasureImageFilter:
public ImageToImageFilter< TInputImage, TOutputImage >
{
public:
  /** Standard Self typedef */
  typedef EigenToMeasureImageFilter                       Self;
  typedef ImageToImageFilter< TInputImage, TOutputImage > Superclass;
  typedef SmartPointer< Self >                            Pointer;
  typedef SmartPointer< const Self >                      ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(EigenToMeasureImageFilter, ImageToImageFilter);

  /** Input Image typedefs. */
  typedef TInputImage                           InputImageType;
  typedef typename InputImageType::Pointer      InputImagePointer;
  typedef typename InputImageType::ConstPointer InputImageConstPointer;
  typedef typename InputImageType::RegionType   InputImageRegionType;
  typedef typename InputImageType::PixelType    InputImagePixelType;
  itkStaticConstMacro(ImageDimension, unsigned int,  TInputImage::ImageDimension);

  /** Output image typedefs. */
  typedef TOutputImage                          OutputImageType;
  typedef typename OutputImageType::Pointer     OutputImagePointer;
  typedef typename OutputImageType::RegionType  OutputImageRegionType;
  typedef typename OutputImageType::PixelType   OutputImagePixelType;

  /** Input SpatialObject typedefs. */
  typedef TInputSpatialObject                       SpatialObjectType;
  typedef typename SpatialObjectType::ConstPointer  SpatialObjectConstPointer;

  /** Functor typedefs. */
  typedef TFunction                                   FunctorType;
  typedef typename FunctorType::ParameterType         ParameterType;
  typedef typename ParameterType::ValueType           ParameterValueType;
  typedef SimpleDataObjectDecorator< ParameterType >  ParameterDecoratedType;
  itkStaticConstMacro(NumberOfParameters, unsigned int, ParameterType::Length);

  /** Get the functor object.  The functor is returned by reference.
   * (Functors do not have to derive from itk::LightObject, so they do
   * not necessarily have a reference count. So we cannot return a
   * SmartPointer.) */
  FunctorType &       GetFunctor() { return m_Functor; }
  const FunctorType & GetFunctor() const { return m_Functor; }

  /** Set the functor object.  This replaces the current Functor with a
   * copy of the specified Functor. This allows the user to specify a
   * functor that has ivars set differently than the default functor.
   * This method requires the following to be defined:
   *    Initialize()
   *    ProcessPixel()
   *    GetComputedParameters()
   */
  void SetFunctor(const FunctorType & functor)
  {
    if ( m_Functor != functor )
      {
      m_Functor = functor;
      this->Modified();
      }
  }

  /** Process object */
  itkSetGetDecoratedInputMacro(Parameters, ParameterType);

  /** Methods to set/get the mask image */
  itkSetInputMacro(MaskingSpatialObject, SpatialObjectType);
  itkGetInputMacro(MaskingSpatialObject, SpatialObjectType);

  /** Template the EigenValueOrderType. Methods that inherit from this class can override this function
   * to produce a different eigenvalue ordering. Ideally, the enum EigenValueOrderType should come from
   * itkSymmetricEigenAnalysisImageFilter.h or itkSymmetricEigenAnalysis.h. That turns out to be non-trivial
   * because the enumeration is hidden within the templated class. Therefore, you would need the hessian type
   * and eigenvalue type to do such an operation. We do not necessarily have the hessian type information.
   */
  typedef enum {
    OrderByValue = 1,
    OrderByMagnitude,
    DoNotOrder
  } EigenValueOrderType;
  virtual EigenValueOrderType GetEigenValueOrder() const = 0;

protected:
  EigenToMeasureImageFilter();
  virtual ~EigenToMeasureImageFilter() ITK_OVERRIDE;

  /** Need to access the input parameters at execution time */
  void BeforeThreadedGenerateData() ITK_OVERRIDE;
  void ThreadedGenerateData(const OutputImageRegionType & outputRegionForThread, ThreadIdType threadId);

  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;
private:
  ITK_DISALLOW_COPY_AND_ASSIGN(EigenToMeasureImageFilter);

  /* Private data members. */
  ParameterType m_Parameters;
  FunctorType   m_Functor;
}; // end class
} /* end namespace */

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkEigenToMeasureImageFilter.hxx"
#endif

#endif /* itkEigenToMeasureImageFilter_h */
