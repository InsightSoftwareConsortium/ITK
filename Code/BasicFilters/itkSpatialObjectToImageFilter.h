/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSpatialObjectToImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkSpatialObjectToImageFilter_h
#define __itkSpatialObjectToImageFilter_h

#include "itkImageSource.h"
#include "itkConceptChecking.h"

namespace itk
{
  
/** \class SpatialObjectToImageFilter
 * \brief Base class for filters that take a SpatialObject 
 *        as input and produce an image as output.
 *  By default, if the user does not specify the size of the output image,
 *  the maximum size of the object's bounding box is used. 
 *  The spacing of the image is given by the spacing of the input 
 *  Spatial object.
 */
template <class TInputSpatialObject, class TOutputImage>
class ITK_EXPORT SpatialObjectToImageFilter : public ImageSource<TOutputImage>
{
public:
  /** Standard class typedefs. */
  typedef SpatialObjectToImageFilter  Self;
  typedef ImageSource<TOutputImage>  Superclass;
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
  typedef typename TOutputImage::SizeType   SizeType;
  
  /** Method for creation through the object factory. */
  itkNewMacro(Self);
  
  /** Run-time type information (and related methods). */
  itkTypeMacro(SpatialObjectToImageFilter,ImageSource);

  /** Superclass typedefs. */
  typedef typename Superclass::OutputImageRegionType OutputImageRegionType;

  /** Some convenient typedefs. */
  typedef TInputSpatialObject InputSpatialObjectType;
  typedef typename InputSpatialObjectType::Pointer        InputSpatialObjectPointer;
  typedef typename InputSpatialObjectType::ConstPointer   InputSpatialObjectConstPointer;
  typedef typename TInputSpatialObject::ChildrenListType   ChildrenListType;

  /** ImageDimension constants */
  itkStaticConstMacro(ObjectDimension, unsigned int,
                      InputSpatialObjectType::ObjectDimension);
  itkStaticConstMacro(OutputImageDimension, unsigned int,
                      TOutputImage::ImageDimension);

  /** Set/Get the image input of this process object.  */
  virtual void SetInput( const InputSpatialObjectType *object);
  virtual void SetInput( unsigned int, const InputSpatialObjectType * object);
  const InputSpatialObjectType * GetInput(void);
  const InputSpatialObjectType * GetInput(unsigned int idx);

  virtual void GenerateOutputInformation() {}; // do nothing
  virtual void GenerateData() ; // do nothing

  itkSetMacro(ChildrenDepth, unsigned int);
  itkGetMacro(ChildrenDepth, unsigned int);

  /** Set/Get Size */
  itkSetMacro(Size,SizeType);
  itkGetMacro(Size,SizeType);

protected:
  SpatialObjectToImageFilter();
  ~SpatialObjectToImageFilter();

  SizeType          m_Size;
  unsigned int m_ChildrenDepth;
  virtual void PrintSelf(std::ostream& os, Indent indent) const;

private:
  SpatialObjectToImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented


};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
  #include "itkSpatialObjectToImageFilter.txx"
#endif

#endif
