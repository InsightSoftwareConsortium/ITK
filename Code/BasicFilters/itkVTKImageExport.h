/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVTKImageExport.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

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
  ///! Standard "Self" typedef.
  typedef VTKImageExport Self;

  ///! Standard "Superclass" typedef.
  typedef ProcessObject Superclass;

  ///! Smart pointer typedef support.
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  ///! Run-time type information (and related methods).
  itkTypeMacro(VTKImageExport,VTKImageExportBase);

  ///! Method for creation through the object factory.
  itkNewMacro(Self);

  ///! The type of the input image.
  typedef TInputImage InputImageType;
  
  ///! Set the input image of this image exporter.
  void SetInput(InputImageType*);
  
protected:
  VTKImageExport();
  ~VTKImageExport() {}
  void PrintSelf(std::ostream& os, Indent indent) const;  

  typedef typename InputImageType::Pointer InputImagePointer;
  typedef typename InputImageType::RegionType InputRegionType;
  typedef typename InputRegionType::SizeType InputSizeType;
  typedef typename InputRegionType::IndexType InputIndexType;
  enum { InputImageDimension = InputImageType::ImageDimension };
  
  InputImagePointer GetInput();
  
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
