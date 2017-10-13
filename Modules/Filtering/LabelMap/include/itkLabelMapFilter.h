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
/*=========================================================================
 *
 *  Portions of this file are subject to the VTK Toolkit Version 3 copyright.
 *
 *  Copyright (c) Ken Martin, Will Schroeder, Bill Lorensen
 *
 *  For complete copyright, license and disclaimer of warranty information
 *  please refer to the NOTICE file at the top of the ITK source tree.
 *
 *=========================================================================*/
#ifndef itkLabelMapFilter_h
#define itkLabelMapFilter_h

#include "itkImageToImageFilter.h"
#include "itkFastMutexLock.h"

namespace itk
{
/** \class LabelMapFilter
 * \brief Base class for filters that take an image as input and overwrite that image as the output
 *
 * LabelMapFilter is the base class for all process objects whose
 * are using a LabelMapFilter as input. It manage several threads,
 * and run a method ThreadedGenerateData() for each object in the LabelMapFilter.
 * With that class, the developer doesn't need to take care of iterating over all the objects in
 * the image, or to manage by hand the threads.
 *
 * \author Gaetan Lehmann. Biologie du Developpement et de la Reproduction, INRA de Jouy-en-Josas, France.
 *
 * This implementation was taken from the Insight Journal paper:
 * https://hdl.handle.net/1926/584  or
 * http://www.insight-journal.org/browse/publication/176
 *
 * \sa LabelMapToBinaryImageFilter, LabelMapToLabelImageFilter
 * \ingroup ImageEnhancement  MathematicalMorphologyImageFilters
 * \ingroup LabeledImageFilters
 * \ingroup ITKLabelMap
 */
template< typename TInputImage, typename TOutputImage >
class ITK_TEMPLATE_EXPORT LabelMapFilter:
  public
  ImageToImageFilter< TInputImage, TOutputImage >
{
public:
  /** Standard class typedefs. */
  typedef LabelMapFilter                                  Self;
  typedef ImageToImageFilter< TInputImage, TOutputImage > Superclass;
  typedef SmartPointer< Self >                            Pointer;
  typedef SmartPointer< const Self >                      ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(LabelMapFilter, ImageToImageFilter);

  /** Standard New method. */
  itkNewMacro(Self);

  /** Some convenient typedefs. */
  typedef typename Superclass::InputImageType         InputImageType;
  typedef typename Superclass::InputImagePointer      InputImagePointer;
  typedef typename Superclass::InputImageConstPointer InputImageConstPointer;
  typedef typename Superclass::InputImageRegionType   InputImageRegionType;
  typedef typename Superclass::InputImagePixelType    InputImagePixelType;
  typedef typename InputImageType::LabelObjectType    LabelObjectType;

  typedef TOutputImage                           OutputImageType;
  typedef typename OutputImageType::Pointer      OutputImagePointer;
  typedef typename OutputImageType::ConstPointer OutputImageConstPointer;
  typedef typename OutputImageType::RegionType   OutputImageRegionType;
  typedef typename OutputImageType::PixelType    OutputImagePixelType;

  /** ImageDimension constants */
  itkStaticConstMacro(InputImageDimension, unsigned int, TInputImage::ImageDimension);
  itkStaticConstMacro(OutputImageDimension, unsigned int, TOutputImage::ImageDimension);

  /** LabelMapFilter requires the entire input to be
   * available. Thus, it needs to provide an implementation of
   * GenerateInputRequestedRegion(). */
  void GenerateInputRequestedRegion() ITK_OVERRIDE;

  /** LabelMapFilter will produce the entire output. */
  void EnlargeOutputRequestedRegion( DataObject * itkNotUsed(output) ) ITK_OVERRIDE;

protected:
  LabelMapFilter();
  ~LabelMapFilter() ITK_OVERRIDE;

  virtual void BeforeThreadedGenerateData() ITK_OVERRIDE;

  virtual void AfterThreadedGenerateData() ITK_OVERRIDE;

  virtual void ThreadedGenerateData(const OutputImageRegionType & outputRegionForThread, ThreadIdType threadId) ITK_OVERRIDE;

  virtual void ThreadedProcessLabelObject(LabelObjectType *labelObject);

  /**
   * Return the label collection image to use. This method may be overloaded
   * if the label collection image to use is not the input image.
   */
  virtual InputImageType * GetLabelMap()
  {
    return static_cast< InputImageType * >( const_cast< DataObject * >( this->ProcessObject::GetInput(0) ) );
  }

  typename FastMutexLock::Pointer m_LabelObjectContainerLock;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(LabelMapFilter);

  typename InputImageType::Iterator m_LabelObjectIterator;
  float                             m_InverseNumberOfLabelObjects;
  SizeValueType                     m_NumberOfLabelObjectsProcessed;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkLabelMapFilter.hxx"
#endif

#endif
