/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVTKImageExport.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkVTKImageExport_h
#define __itkVTKImageExport_h

#include "itkVTKImageExportBase.h"

namespace itk
{

/** \class VTKImageExport
 * \brief Connect the end of an ITK image pipeline to a VTK pipeline.
 *
 * VTKImageExport can be used at the end of an ITK image pipeline to
 * connect with a VTK pipeline that begins with vtkImageImport.
 * Callbacks provided by VTKImageExport are registered with
 * vtkImageImport to connect the pipeline execution together.  Once
 * connected, update requests coming through the VTK pipeline are
 * automatically propagated to the ITK pipeline.
 *
 * While VTKImageExportBase provides the pipeline functionality
 * independent of image type, instances must be created through
 * VTKImageExport.  This class provides the implementations for
 * callbacks that depend on the image type.
 *
 * Note that not all image types will work correctly.  VTK will only
 * support images of 1, 2, or 3 dimensions.  Scalar value types can be
 * one of: float, double, char, unsigned char, short, unsigned short,
 * int, unsigned int, long, unsigned long.
 *
 * Currently VTKImageExport does not support pixel types with multiple
 * components (like RGBPixel).
 *
 * \ingroup IOFilters
 * \sa VTKImageExportBase
 */
template <class TInputImage>
class ITK_EXPORT VTKImageExport: public VTKImageExportBase
{
public:
  /** Standard class typedefs. */
  typedef VTKImageExport Self;
  typedef ProcessObject Superclass;
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(VTKImageExport,VTKImageExportBase);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** The type of the input image. */
  typedef TInputImage InputImageType;
  
  /** Set the input image of this image exporter. */
  void SetInput(const InputImageType*);
  
protected:
  VTKImageExport();
  ~VTKImageExport() {}
  void PrintSelf(std::ostream& os, Indent indent) const;  

  typedef typename InputImageType::Pointer InputImagePointer;
  typedef typename InputImageType::RegionType InputRegionType;
  typedef typename InputRegionType::SizeType InputSizeType;
  typedef typename InputRegionType::IndexType InputIndexType;
  itkStaticConstMacro(InputImageDimension, unsigned int,
                      InputImageType::ImageDimension );
  
  InputImageType * GetInput(void);
  
  int* WholeExtentCallback();
  float* SpacingCallback();
  float* OriginCallback();
  const char* ScalarTypeCallback();
  int NumberOfComponentsCallback();
  void PropagateUpdateExtentCallback(int*);
  int* DataExtentCallback();
  void* BufferPointerCallback();
  
private:
  VTKImageExport(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  std::string m_ScalarTypeName;
  int m_WholeExtent[6];
  int m_DataExtent[6];
  float m_DataSpacing[3];
  float m_DataOrigin[3];
};

} // end namespace itk
  
#ifndef ITK_MANUAL_INSTANTIATION
#include "itkVTKImageExport.txx"
#endif

#endif
