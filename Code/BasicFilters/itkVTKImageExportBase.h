/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVTKImageExportBase.h
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
#ifndef __itkVTKImageExportBase_h
#define __itkVTKImageExportBase_h

#include "itkProcessObject.h"

namespace itk
{

/** \class VTKImageExportBase
 * \brief Superclass for VTKImageExport instantiations.
 *
 * VTKImageExportBase provides the functions that serve as callbacks
 * given to vtkImageImport to connect the end of an ITK pipeline to
 * the beginning of a VTK pipeline.
 * 
 * \ingroup IOFilters
 * \sa VTKImageExport
 */
class ITK_EXPORT VTKImageExportBase: public ProcessObject
{
public:
  /** Standard class typedefs. */
  typedef VTKImageExportBase Self;
  typedef ProcessObject Superclass;
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(VTKImageExportBase,ProcessObject);

  /** Returns the user data to set for the vtkImageImport callbacks. */
  void* GetCallbackUserData();

  /** The function pointer type expected for a callback. */
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
  
  /** Get a pointer to function to set as a callback in vtkImageImport. */
  UpdateInformationCallbackType     GetUpdateInformationCallback() const;
  PipelineModifiedCallbackType      GetPipelineModifiedCallback() const;
  WholeExtentCallbackType           GetWholeExtentCallback() const;
  SpacingCallbackType               GetSpacingCallback() const;
  OriginCallbackType                GetOriginCallback() const;
  ScalarTypeCallbackType            GetScalarTypeCallback() const;
  NumberOfComponentsCallbackType    GetNumberOfComponentsCallback() const;
  PropagateUpdateExtentCallbackType GetPropagateUpdateExtentCallback() const;
  UpdateDataCallbackType            GetUpdateDataCallback() const;
  DataExtentCallbackType            GetDataExtentCallback() const;
  BufferPointerCallbackType         GetBufferPointerCallback() const;
  
protected:
  VTKImageExportBase();
  ~VTKImageExportBase() {}
  void PrintSelf(std::ostream& os, Indent indent) const;  

  typedef DataObject::Pointer DataObjectPointer;
  
  virtual void UpdateInformationCallback();
  virtual int PipelineModifiedCallback();
  virtual void UpdateDataCallback();  
  
  /** These callbacks are image-type specific, and are implemented
   * in VTKImageExport. */
  virtual int* WholeExtentCallback()=0;
  virtual float* SpacingCallback()=0;
  virtual float* OriginCallback()=0;
  virtual const char* ScalarTypeCallback()=0;
  virtual int NumberOfComponentsCallback()=0;
  virtual void PropagateUpdateExtentCallback(int*)=0;
  virtual int* DataExtentCallback()=0;
  virtual void* BufferPointerCallback()=0;

private:
  VTKImageExportBase(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  /** Actual function sent to VTK as a callback.  Casts the user data
   * to a VTKImageExportBase pointer and invokes the corresponding
   * virtual method in that instance. */
  static void UpdateInformationCallbackFunction(void*);
  static int PipelineModifiedCallbackFunction(void*);
  static int* WholeExtentCallbackFunction(void*);
  static float* SpacingCallbackFunction(void*);
  static float* OriginCallbackFunction(void*);
  static const char* ScalarTypeCallbackFunction(void*); 
  static int NumberOfComponentsCallbackFunction(void*);
  static void PropagateUpdateExtentCallbackFunction(void*, int*);
  static void UpdateDataCallbackFunction(void*);
  static int* DataExtentCallbackFunction(void*);
  static void* BufferPointerCallbackFunction(void*);

private:
  /** PipelineMTime from the last call to PipelineModifiedCallback. */
  unsigned long m_LastPipelineMTime;
};

} // end namespace itk
  
#endif
