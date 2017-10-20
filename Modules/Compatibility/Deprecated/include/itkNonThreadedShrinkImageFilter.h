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
#ifndef itkNonThreadedShrinkImageFilter_h
#define itkNonThreadedShrinkImageFilter_h
#if !defined( ITK_LEGACY_REMOVE )

#include "itkShrinkImageFilter.h"

namespace itk
{
/** \class NonThreadedShrinkImageFilter
 * \brief Reduce the size of an image by an integer factor.
 *
 * NonThreadedShrinkImageFilter reduces the size of an image by an
 * integer factor. The algorithm implemented is a simple
 * subsample. Since this filter produces an image which is a different
 * resolution and with different pixel spacing than its input image,
 * it needs to override several of the methods defined in
 * ProcessObject in order to properly manage the pipeline execution
 * model.  In particular, this filter overrides
 * ProcessObject::GenerateInputRequestedRegion() and
 * ProcessObject::GenerateOutputInformation().
 *
 * NOTE: This filter runs the ShrinkImageFilter with the number of
 * threads set to 1. To avoid confusion, developers shopuld replace
 * this class with ShrinkImageFilter. If you need to limit the number
 * of threads, instantiate the afilter and apply the
 * SetNumberOfThreads(1) methods.
 * \deprecated
 * \ingroup ITKDeprecated
 * \ingroup GeometricTransform
 */
template< typename TInputImage, typename TOutputImage >
class NonThreadedShrinkImageFilter:
  public ShrinkImageFilter< TInputImage, TOutputImage >
{
public:
  /** Standard class typedefs. */
  typedef NonThreadedShrinkImageFilter                    Self;
  typedef ImageToImageFilter< TInputImage, TOutputImage > Superclass;
  typedef SmartPointer< Self >                            Pointer;
  typedef SmartPointer< const Self >                      ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(NonThreadedShrinkImageFilter, ShrinkImageFilter);

  virtual void SetNumberOfThreads(ThreadIdType) ITK_OVERRIDE
  {
    Superclass::SetNumberOfThreads(1);
  }

protected:
  NonThreadedShrinkImageFilter()
  {
    Superclass::SetNumberOfThreads(1);
  }

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(NonThreadedShrinkImageFilter);
};
} // end namespace itk

#endif //#if !defined( ITK_LEGACY_REMOVE )
#endif
