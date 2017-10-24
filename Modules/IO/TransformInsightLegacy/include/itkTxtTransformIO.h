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
#ifndef itkTxtTransformIO_h
#define itkTxtTransformIO_h

#include "ITKIOTransformInsightLegacyExport.h"

#include "itkTransformIOBase.h"

namespace itk
{
/** \class TxtTransformIOTemplate
   * \brief Create instances of TxtTransformIOTemplate objects.
   * \ingroup ITKIOTransformInsightLegacy
   */
template<typename TParametersValueType>
class ITK_TEMPLATE_EXPORT TxtTransformIOTemplate:public TransformIOBaseTemplate<TParametersValueType>
{
public:
  typedef TxtTransformIOTemplate                        Self;
  typedef TransformIOBaseTemplate<TParametersValueType> Superclass;
  typedef SmartPointer<Self>                            Pointer;
  typedef typename Superclass::TransformType            TransformType;
  typedef typename Superclass::TransformPointer         TransformPointer;
  typedef typename Superclass::TransformListType        TransformListType;
  typedef typename TransformIOBaseTemplate<
                      TParametersValueType>::ConstTransformListType
                                                        ConstTransformListType;

  typedef typename TransformType::ParametersType           ParametersType;
  typedef typename TransformType::ParametersValueType      ParametersValueType;
  typedef typename TransformType::FixedParametersType      FixedParametersType;
  typedef typename TransformType::FixedParametersValueType FixedParametersValueType;

  /** Run-time type information (and related methods). */
  itkTypeMacro(TxtTransformIOTemplate, Superclass);
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

  /* Helper function for Read method, used for CompositeTransform reading. */
  void ReadComponentFile( std::string Value );

protected:
  TxtTransformIOTemplate();
  virtual ~TxtTransformIOTemplate() ITK_OVERRIDE;

private:
  /** trim spaces and newlines from start and end of a string */
  std::string trim(std::string const & source, char const *delims = " \t\r\n");
};

/** This helps to meet backward compatibility */
typedef TxtTransformIOTemplate<double> TxtTransformIO;

}

// Note: Explicit instantiation is done in itkTxtTransformIO.cxx

#endif // itkTxtTransformIO_h

/** Explicit instantiations */
#ifndef ITK_TEMPLATE_EXPLICIT_TxtTransformIO
// Explicit instantiation is required to ensure correct dynamic_cast
// behavior across shared libraries.
//
// IMPORTANT: Since within the same compilation unit,
//            ITK_TEMPLATE_EXPLICIT_<classname> defined and undefined states
//            need to be considered. This code *MUST* be *OUTSIDE* the header
//            guards.
//
#  if defined( ITKIOTransformInsightLegacy_EXPORTS )
//   We are building this library
#    define ITKIOTransformInsightLegacy_EXPORT_EXPLICIT ITK_FORWARD_EXPORT
#  else
//   We are using this library
#    define ITKIOTransformInsightLegacy_EXPORT_EXPLICIT ITKIOTransformInsightLegacy_EXPORT
#  endif
namespace itk
{

#ifdef ITK_HAS_GCC_PRAGMA_DIAG_PUSHPOP
  ITK_GCC_PRAGMA_DIAG_PUSH()
#endif
ITK_GCC_PRAGMA_DIAG(ignored "-Wattributes")

  extern template class ITKIOTransformInsightLegacy_EXPORT_EXPLICIT TxtTransformIOTemplate< double >;
extern template class ITKIOTransformInsightLegacy_EXPORT_EXPLICIT TxtTransformIOTemplate< float >;

#ifdef ITK_HAS_GCC_PRAGMA_DIAG_PUSHPOP
  ITK_GCC_PRAGMA_DIAG_POP()
#else
  ITK_GCC_PRAGMA_DIAG(warning "-Wattributes")
#endif

} // end namespace itk
#  undef ITKIOTransformInsightLegacy_EXPORT_EXPLICIT
#endif
