/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef itkMINCTransformIO_h
#define itkMINCTransformIO_h

#include "ITKIOTransformMINCExport.h"

#include "itkTransformIOBase.h"

#include <string>
#include <vector>
#include <itk_minc2.h>
#include "itkMatrixOffsetTransformBase.h"

namespace itk
{

/** \class MINCTransformIOTemplate
 *
 * \brief Read and write transforms in MINC format (.xfm).
 *        Takes into account PositiveCoordinateOrientation RAS to PositiveCoordinateOrientation LPS conversion
 *        flag to convert from internal MINC to ITK conventions and back, if enabled.
 *
 * \author Vladimir S. FONOV
 *         Brain Imaging Center, Montreal Neurological Institute, McGill University, Montreal Canada 2012
 *
 * \ingroup ITKIOTransformMINC
 */
template <typename TParametersValueType>
class ITK_TEMPLATE_EXPORT MINCTransformIOTemplate : public TransformIOBaseTemplate<TParametersValueType>
{
public:
  using Self = MINCTransformIOTemplate;
  using Superclass = TransformIOBaseTemplate<TParametersValueType>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  using typename Superclass::TransformType;
  using typename Superclass::TransformPointer;
  using typename Superclass::TransformListType;
  using typename Superclass::ConstTransformListType;
  using ParametersType = typename TransformType::ParametersType;

  using MatrixOffsetTransformBaseType = MatrixOffsetTransformBase<TParametersValueType, 3, 3>;

  using MatrixType = typename MatrixOffsetTransformBaseType::MatrixType;
  using OffsetType = typename MatrixOffsetTransformBaseType::OffsetType;

  /** \see LightObject::GetNameOfClass() */
  itkOverrideGetNameOfClassMacro(MINCTransformIOTemplate);
  itkNewMacro(Self);

  /** Determine the file type. Returns true if this ImageIO can read the
   * file specified. */
  bool
  CanReadFile(const char * fileName) override;

  /** Determine the file type. Returns true if this ImageIO can write the
   * file specified. */
  bool
  CanWriteFile(const char * fileName) override;

  /** Reads the data from disk into the memory buffer provided. */
  void
  Read() override;

  void
  Write() override;

  /** Set to automatically convert from RAS PositiveCoordinateOrientation to LPS PositiveCoordinateOrientation*/
  itkSetMacro(RAStoLPS, bool);
  itkGetConstMacro(RAStoLPS, bool);
  itkBooleanMacro(RAStoLPS);

protected:
  MINCTransformIOTemplate();
  ~MINCTransformIOTemplate() override;

  VIO_General_transform m_XFM{};
  bool                  m_XFM_initialized{};

private:
  void
  _cleanup();
  void
  WriteOneTransform(const int                            transformIndex,
                    const TransformType *                transform,
                    std::vector<VIO_General_transform> & _xfm,
                    const char *                         xfm_file_base,
                    int &                                serial);

  void
       ReadOneTransform(VIO_General_transform * xfm);
  bool m_RAStoLPS;
};

/** This helps to meet backward compatibility */
using MINCTransformIO = MINCTransformIOTemplate<double>;

} // end namespace itk

// Note: Explicit instantiation is done in itkMINCTransformIO.cxx

#endif // itkMINCTransformIO_h

/** Explicit instantiations */
#ifndef ITK_TEMPLATE_EXPLICIT_MINCTransformIO
// Explicit instantiation is required to ensure correct dynamic_cast
// behavior across shared libraries.
//
// IMPORTANT: Since within the same compilation unit,
//            ITK_TEMPLATE_EXPLICIT_<classname> defined and undefined states
//            need to be considered. This code *MUST* be *OUTSIDE* the header
//            guards.
//
#if defined(ITKIOTransformMINC_EXPORTS)
//   We are building this library
#  define ITKIOTransformMINC_EXPORT_EXPLICIT ITK_FORWARD_EXPORT
#else
//   We are using this library
#  define ITKIOTransformMINC_EXPORT_EXPLICIT ITKIOTransformMINC_EXPORT
#endif
namespace itk
{

ITK_GCC_PRAGMA_DIAG_PUSH()
ITK_GCC_PRAGMA_DIAG(ignored "-Wattributes")

extern template class ITKIOTransformMINC_EXPORT_EXPLICIT MINCTransformIOTemplate<double>;
extern template class ITKIOTransformMINC_EXPORT_EXPLICIT MINCTransformIOTemplate<float>;

ITK_GCC_PRAGMA_DIAG_POP()

} // end namespace itk
#undef ITKIOTransformMINC_EXPORT_EXPLICIT
#endif
