/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVTKImageImport.h
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
#ifndef __itkVTKImageImport_h
#define __itkVTKImageImport_h

#include "itkImageSource.h"
#include "itkImage.h"
#include "itkImportImageContainer.h"

#define itkSetMacro2(name,type) \
  virtual void Set##name (type _arg) \
  { \
    itkDebugMacro("setting " #name " to " << _arg); \
    if (this->m_##name != _arg) \
      { \
      this->m_##name = _arg; \
      this->Modified(); \
      } \
  } 

namespace itk
{

/** \brief Connect the end of an VTK pipeline to an ITK image pipeline.
 *
 * VTKImageImport can be used at the beginning of an ITK image pipeline to
 * connect with a VTK pipeline that ends with vtkImageExport.  Callbacks
 * provided by VTKImageImport are registered with vtkImageExport to connect
 * the pipeline execution together.  Once connected, update requests coming
 * through the ITK pipeline are automatically propagated to the VTK pipeline.
 *
 * Note that the VTK images are assumed to be of 1, 2, or 3 dimensions.
 * Scalar value types can be one of: float, double, char, unsigned char,
 * short, unsigned short, int, unsigned int, long, unsigned long. The
 * images must have pixel types with one component.
 *
 * \ingroup IOFilters
 * \sa VTKImageImport 
 */
template <typename TOutputImage>
class ITK_EXPORT VTKImageImport: public ImageSource<TOutputImage>
{
public:
  /** Standard class typedefs. */
  typedef VTKImageImport Self;
  typedef ImageSource<TOutputImage> Superclass;
  typedef SmartPointer<Self>  Pointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(VTKImageImport, ImageSource);

  /** Convenient typedefs from the output image. */
  typedef TOutputImage OutputImageType;
  typedef typename OutputImageType::Pointer OutputImagePointer;
  typedef typename OutputImageType::PixelType OutputPixelType;
  typedef typename OutputImageType::SizeType OutputSizeType;
  typedef typename OutputImageType::IndexType OutputIndexType;
  typedef typename OutputImageType::RegionType OutputRegionType;

  /** The output image dimension. */
  enum { OutputImageDimension = OutputImageType::ImageDimension };

  /** These are function pointer types for the pipeline connection
   * callbacks. */
  typedef void (*UpdateInformationCallbackType)(void*);
  typedef int (*PipelineModifiedCallbackType)(void*);
  typedef int* (*WholeExtentCallbackType)(void*);
  typedef float* (*SpacingCallbackType)(void*);
  typedef float* (*OriginCallbackType)(void*);
  typedef const char* (*ScalarTypeCallbackType)(void*); 
  typedef int (*NumberOfComponentsCallbackType)(void*);
  typedef void (*PropagateUpdateExtentCallbackType)(void*, int*);
  typedef void (*UpdateDataCallbackType)(void*);
  typedef int* (*DataExtentCallbackType)(void*);
  typedef void* (*BufferPointerCallbackType)(void*);
  
  /** What to do when receiving UpdateInformation(). */
  itkSetMacro(UpdateInformationCallback, UpdateInformationCallbackType);
  itkGetMacro(UpdateInformationCallback, UpdateInformationCallbackType);
  
  /** What to do when receiving PipelineModified(). */
  itkSetMacro(PipelineModifiedCallback, PipelineModifiedCallbackType);
  itkGetMacro(PipelineModifiedCallback, PipelineModifiedCallbackType);
  
  /** What to do when receiving SetWholeExtent(). */
  itkSetMacro(WholeExtentCallback, WholeExtentCallbackType);
  itkGetMacro(WholeExtentCallback, WholeExtentCallbackType);
  
  /** What to do when receiving SetSpacing(). */
  itkSetMacro(SpacingCallback, SpacingCallbackType);
  itkGetMacro(SpacingCallback, SpacingCallbackType);
  
  /** What to do when receiving SetOrigin(). */
  itkSetMacro(OriginCallback, OriginCallbackType);
  itkGetMacro(OriginCallback, OriginCallbackType);
  
  /** What to do when receiving UpdateInformation(). */
  itkSetMacro(ScalarTypeCallback, ScalarTypeCallbackType);
  itkGetMacro(ScalarTypeCallback, ScalarTypeCallbackType);
  
  /** What to do when receiving SetNumberOfComponents(). */
  itkSetMacro(NumberOfComponentsCallback, NumberOfComponentsCallbackType);
  itkGetMacro(NumberOfComponentsCallback, NumberOfComponentsCallbackType);
  
  /** What to do when receiving PropagateUpdateExtent(). */
  itkSetMacro(PropagateUpdateExtentCallback,PropagateUpdateExtentCallbackType);
  itkGetMacro(PropagateUpdateExtentCallback,PropagateUpdateExtentCallbackType);
  
  /** What to do when receiving UpdateData(). */
  itkSetMacro(UpdateDataCallback, UpdateDataCallbackType);
  itkGetMacro(UpdateDataCallback, UpdateDataCallbackType);

  /** What to do when receiving DataExtent(). */
  itkSetMacro(DataExtentCallback, DataExtentCallbackType);
  itkGetMacro(DataExtentCallback, DataExtentCallbackType);
  
  /** What to do when receiving BufferPointer(). */
  itkSetMacro(BufferPointerCallback, BufferPointerCallbackType);
  itkGetMacro(BufferPointerCallback, BufferPointerCallbackType);

  /** Specify callback data. */
  itkSetMacro2(CallbackUserData, void*);
  itkGetMacro(CallbackUserData, void*);
  
protected:
  VTKImageImport();
  ~VTKImageImport() {}
  void PrintSelf(std::ostream& os, Indent indent) const;

  virtual void PropagateRequestedRegion(DataObject*);  
  virtual void UpdateOutputInformation();
  virtual void GenerateData();
  virtual void GenerateOutputInformation();
  
private:
  VTKImageImport(const Self&); //purposely not implemented
  void operator= (const Self&); //purposely not implemented

  void* m_CallbackUserData;
  UpdateInformationCallbackType     m_UpdateInformationCallback;
  PipelineModifiedCallbackType      m_PipelineModifiedCallback;
  WholeExtentCallbackType           m_WholeExtentCallback;
  SpacingCallbackType               m_SpacingCallback;
  OriginCallbackType                m_OriginCallback;
  ScalarTypeCallbackType            m_ScalarTypeCallback;
  NumberOfComponentsCallbackType    m_NumberOfComponentsCallback;  
  PropagateUpdateExtentCallbackType m_PropagateUpdateExtentCallback;  
  UpdateDataCallbackType            m_UpdateDataCallback;
  DataExtentCallbackType            m_DataExtentCallback;
  BufferPointerCallbackType         m_BufferPointerCallback;
  
  std::string m_ScalarTypeName;
};

} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkVTKImageImport.txx"
#endif

#endif // __itkVTKImageImport_h
