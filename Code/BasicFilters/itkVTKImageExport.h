/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVTKImageExport.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
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
template <class TInputImage, typename TVTKRealType=float>
class ITK_EXPORT VTKImageExport: public VTKImageExportBase
{
public:
  /** Standard class typedefs. */
  typedef VTKImageExport Self;
  typedef VTKImageExportBase Superclass;
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(VTKImageExport,VTKImageExportBase);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Typedef for VTK interface.  VTK 4.2 uses floats for positions,
   * VTK 4.4 uses doubles. */
  typedef TVTKRealType vtkRealType;
  typedef vtkRealType VTKSpacingType;
  typedef vtkRealType VTKOriginType;

  /** The type of the input image. */
  typedef TInputImage InputImageType;
  
  /** The function pointer type expected for a callback. These
   * callbacks depend on the "RealType" used by VTK, so they cannot be
   * defined in the superclass. */
  typedef TVTKRealType * (*SpacingCallbackType)(void*);
  typedef TVTKRealType * (*OriginCallbackType)(void*);
  
  /** Get a pointer to function to set as a callback in vtkImageImport. */
  SpacingCallbackType               GetSpacingCallback() const;
  OriginCallbackType                GetOriginCallback() const;

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
  virtual VTKSpacingType * SpacingCallback();
  virtual VTKOriginType  * OriginCallback();
  const char* ScalarTypeCallback();
  int NumberOfComponentsCallback();
  void PropagateUpdateExtentCallback(int*);
  int* DataExtentCallback();
  void* BufferPointerCallback();
  
private:
  VTKImageExport(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  /** Actual function sent to VTK as a callback.  Casts the user data
   * to a VTKImageExportBase pointer and invokes the corresponding
   * virtual method in that instance. */
  static VTKSpacingType * SpacingCallbackFunction(void*);
  static VTKOriginType  * OriginCallbackFunction(void*);

  std::string m_ScalarTypeName;
  int m_WholeExtent[6];
  int m_DataExtent[6];
  VTKSpacingType m_DataSpacing[3];
  VTKOriginType  m_DataOrigin[3];
};

} // end namespace itk
  
#ifndef ITK_MANUAL_INSTANTIATION
#include "itkVTKImageExport.txx"
#endif

#endif
