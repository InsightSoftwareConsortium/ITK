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
#ifndef itkMatlabTransformIO_h
#define itkMatlabTransformIO_h

#include "ITKIOTransformMatlabExport.h"

#include "itkTransformIOBase.h"

namespace itk
{
/** \class MatlabTransformIOTemplate
 *  \brief Create instances of MatlabTransformIOTemplate objects.
 * \ingroup ITKIOTransformMatlab
 */
template<typename ParametersValueType>
class ITK_TEMPLATE_EXPORT MatlabTransformIOTemplate:public TransformIOBaseTemplate<ParametersValueType>
{
public:
  typedef MatlabTransformIOTemplate                       Self;
  typedef TransformIOBaseTemplate<ParametersValueType>    Superclass;
  typedef SmartPointer< Self >                            Pointer;
  typedef typename Superclass::TransformType              TransformType;
  typedef typename Superclass::TransformPointer           TransformPointer;
  typedef typename Superclass::TransformListType          TransformListType;

  typedef typename TransformIOBaseTemplate
                      <ParametersValueType>::ConstTransformListType
                                                          ConstTransformListType;

  /** Run-time type information (and related methods). */
  itkTypeMacro(MatlabTransformIOTemplate, Superclass);
  itkNewMacro(Self);

  /** Determine the file type. Returns true if this ImageIO can read the
   * file specified. */
  virtual bool CanReadFile(const char *) ITK_OVERRIDE;

  /** Determine the file type. Returns true if this ImageIO can read the
   * file specified. */
  virtual bool CanWriteFile(const char *) ITK_OVERRIDE;

  /** Reads the data from disk into the memory buffer provided. */
  virtual void Read() ITK_OVERRIDE;

  /** Writes the data to disk from the memory buffer provided. Make sure
   * that the IORegions has been set properly. The buffer is cast to a
   * pointer to the beginning of the image data. */
  virtual void Write() ITK_OVERRIDE;

protected:
  MatlabTransformIOTemplate();
  virtual ~MatlabTransformIOTemplate() ITK_OVERRIDE;
};

/** This helps to meet backward compatibility */
typedef MatlabTransformIOTemplate<double> MatlabTransformIO;

}

// Note: Explicit instantiation is done in itkMatlabTransformIO.cxx

#endif // itkMatlabTransformIO_h

/** Explicit instantiations */
#ifndef ITK_TEMPLATE_EXPLICIT_MatlabTransformIO
// Explicit instantiation is required to ensure correct dynamic_cast
// behavior across shared libraries.
//
// IMPORTANT: Since within the same compilation unit,
//            ITK_TEMPLATE_EXPLICIT_<classname> defined and undefined states
//            need to be considered. This code *MUST* be *OUTSIDE* the header
//            guards.
//
#  if defined( ITKIOTransformMatlab_EXPORTS )
//   We are building this library
#    define ITKIOTransformMatlab_EXPORT_EXPLICIT ITK_FORWARD_EXPORT
#  else
//   We are using this library
#    define ITKIOTransformMatlab_EXPORT_EXPLICIT ITKIOTransformMatlab_EXPORT
#  endif
namespace itk
{
#ifdef ITK_HAS_GCC_PRAGMA_DIAG_PUSHPOP
  ITK_GCC_PRAGMA_DIAG_PUSH()
#endif
ITK_GCC_PRAGMA_DIAG(ignored "-Wattributes")
extern template class ITKIOTransformMatlab_EXPORT_EXPLICIT MatlabTransformIOTemplate< double >;
extern template class ITKIOTransformMatlab_EXPORT_EXPLICIT MatlabTransformIOTemplate< float >;
#ifdef ITK_HAS_GCC_PRAGMA_DIAG_PUSHPOP
  ITK_GCC_PRAGMA_DIAG_POP()
#else
  ITK_GCC_PRAGMA_DIAG(warning "-Wattributes")
#endif
} // end namespace itk
#  undef ITKIOTransformMatlab_EXPORT_EXPLICIT
#endif
