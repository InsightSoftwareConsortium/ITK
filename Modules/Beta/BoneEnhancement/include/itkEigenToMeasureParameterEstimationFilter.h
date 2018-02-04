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

#ifndef itkEigenToMeasureParameterEstimationFilter_h
#define itkEigenToMeasureParameterEstimationFilter_h

#include "itkStreamingImageFilter.h"
#include "itkSpatialObject.h"
#include "itkSimpleDataObjectDecorator.h"

namespace itk {
/** \class EigenToMeasureParameterEstimationFilter
 * \brief Abstract class for estimating local-structure parameters from eigenvalues.
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
 * \sa StreamingImageFilter
 * 
 * \author: Bryce Besler
 * \ingroup BoneEnhancement
 */
template< typename TInputImage, typename TInputSpatialObject, typename TFunction>
class ITK_TEMPLATE_EXPORT EigenToMeasureParameterEstimationFilter:
public StreamingImageFilter< TInputImage, TInputImage >
{
public:
  /** Standard Self typedef */
  typedef EigenToMeasureParameterEstimationFilter           Self;
  typedef StreamingImageFilter< TInputImage, TInputImage >  Superclass;
  typedef SmartPointer< Self >                              Pointer;
  typedef SmartPointer< const Self >                        ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(EigenToMeasureParameterEstimationFilter, StreamingImageFilter);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Input Image typedefs. */
  typedef TInputImage                           InputImageType;
  typedef typename InputImageType::Pointer      InputImagePointer;
  typedef typename InputImageType::ConstPointer InputImageConstPointer;
  typedef typename InputImageType::RegionType   InputImageRegionType;
  typedef typename InputImageType::PixelType    InputImagePixelType;
  itkStaticConstMacro(ImageDimension, unsigned int,  TInputImage::ImageDimension);

  /** Output image typedefs. */
  typedef InputImageType        OutputImageType;
  typedef InputImageRegionType  OutputImageRegionType;

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

  /** Decorators for parameters so they can be passed as a process object */
  ParameterDecoratedType * GetParametersOutput();
  const ParameterDecoratedType * GetParametersOutput() const;

  /** Standard getters for the parameters */
  ParameterType GetParameters() const
  {
    return this->GetParametersOutput()->Get();
  }

  /** Methods to set/get the mask image */
  itkSetInputMacro(MaskingSpatialObject, SpatialObjectType);
  itkGetInputMacro(MaskingSpatialObject, SpatialObjectType);

  /** Override UpdateOutputData() from StreamingImageFilter to divide
   * upstream updates into pieces. This filter does not have a GenerateData()
   * or ThreadedGenerateData() method.  Instead, all the work is done
   * in UpdateOutputData() since it must update a little, execute a little,
   * update some more, execute some more, etc. */
  void UpdateOutputData(DataObject *output) ITK_OVERRIDE;

  /** Static function used as a "callback" by the MultiThreader.  The threading
   * library will call this routine for each thread, which will delegate the
   * control to ThreadedGenerateData(). */
  static ITK_THREAD_RETURN_TYPE ThreaderCallback(void *arg) ITK_OVERRIDE;

  /** Repeatedly call the functor for each pixel we index. */
  void ThreadedGenerateData(const InputImageRegionType& region, unsigned int streamNumber);

  struct ThreadStruct {
    Pointer Filter;
  };
protected:
  EigenToMeasureParameterEstimationFilter();
  virtual ~EigenToMeasureParameterEstimationFilter() ITK_OVERRIDE;

  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;
private:
  ITK_DISALLOW_COPY_AND_ASSIGN(EigenToMeasureParameterEstimationFilter);

  /* Private data members. */
  FunctorType   m_Functor;
  unsigned int  m_CurrentSplit;
}; //end class
} // end namespace

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkEigenToMeasureParameterEstimationFilter.hxx"
#endif

#endif // itkEigenToMeasureParameterEstimationFilter_h
