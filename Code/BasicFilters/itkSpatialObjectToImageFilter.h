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
  typedef TOutputImage  OutputImageType;
  typedef typename OutputImageType::Pointer OutputImagePointer;
  typedef typename OutputImageType::ValueType  ValueType;
  
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

  /** Spacing (size of a pixel) of the output image. The
   * spacing is the geometric distance between image samples.
   * It is stored internally as double, but may be set from
   * float. \sa GetSpacing() */
  virtual void SetSpacing( const double spacing[OutputImageDimension] );
  virtual void SetSpacing( const float spacing[OutputImageDimension] );
  virtual const double* GetSpacing() const;

  /** Set/Get the value for pixels inside the spatial object. 
  * By default, this filter will return an image
  * that contains values from the spatial object specified as input. 
  * If this "inside" value is changed to a non-null value,
  * the output produced by this filter will be a mask with inside/outside values 
  * specified by the user. */
  itkSetMacro(InsideValue, ValueType);
  itkGetMacro(InsideValue, ValueType);

  /** Set/Get the value for pixels outside the spatial object.
  * By default, this filter will return an image
  * that contains values from the spatial object specified as input. 
  * If this "outside" value is changed to a non-null value,
  * the output produced by this filter will be a mask with inside/outside values
  * specified by the user. */
  itkSetMacro(OutsideValue, ValueType);
  itkGetMacro(OutsideValue, ValueType);

  /** The origin of the output image. The origin is the geometric
   * coordinates of the index (0,0,...,0).  It is stored internally
   * as double but may be set from float.
   * \sa GetOrigin() */
  virtual void SetOrigin( const double origin[OutputImageDimension] );
  virtual void SetOrigin( const float origin[OutputImageDimension] );
  virtual const double * GetOrigin() const;

  /** The spatial object being transformed can be part of a hierarchy.
   * How deep in the hierarchy should we descend in generating the
   * image?  A ChildrenDepth of 0 means to only include the object
   * itself. */
  itkSetMacro(ChildrenDepth, unsigned int);
  itkGetMacro(ChildrenDepth, unsigned int);

  /** Set/Get Size */
  itkSetMacro(Size,SizeType);
  itkGetMacro(Size,SizeType);

protected:
  SpatialObjectToImageFilter();
  ~SpatialObjectToImageFilter();

  virtual void GenerateOutputInformation(){}; // do nothing
  virtual void GenerateData();


  SizeType     m_Size;
  double       m_Spacing[OutputImageDimension];
  double       m_Origin[OutputImageDimension];
  unsigned int m_ChildrenDepth;
  ValueType    m_InsideValue;
  ValueType    m_OutsideValue;

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
