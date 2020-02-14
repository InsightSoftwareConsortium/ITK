/*=========================================================================
 *
 *  Copyright NumFOCUS
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
#ifndef itkOrthogonallyCorrected2DParametricPath_h
#define itkOrthogonallyCorrected2DParametricPath_h

#include "itkParametricPath.h"
#include "itkVectorContainer.h"
#include "itkIndex.h"

namespace itk
{
/**
 *\class OrthogonallyCorrected2DParametricPath
 * \brief  Represent an orthogonally corrected 2D parametric path
 *
 * Description
 *
 * \sa EllipseParametricPath
 * \sa PolyLineParametricPath
 * \sa ParametricPath
 * \sa Path
 * \sa ContinuousIndex
 * \sa Index
 * \sa Offset
 * \sa Vector
 *
 * \ingroup PathObjects
 * \ingroup ITKPath
 */
class ITK_TEMPLATE_EXPORT OrthogonallyCorrected2DParametricPath : public ParametricPath<2>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(OrthogonallyCorrected2DParametricPath);

  /** Standard class type aliases. */
  using Self = OrthogonallyCorrected2DParametricPath;
  using Superclass = ParametricPath<2>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Run-time type information (and related methods). */
  itkTypeMacro(OrthogonallyCorrected2DParametricPath, ParametricPath);

  /** Input type */
  using InputType = Superclass::InputType;

  /** Output type */
  using OutputType = Superclass::OutputType;

  /** Basic data-structure types used */
  using ContinuousIndexType = ContinuousIndex<double, 2>;
  using IndexType = Index<2>;
  using OffsetType = Offset<2>;
  using VectorType = Superclass::VectorType;
  using OriginalPathType = ParametricPath<2>;
  using OriginalPathConstPointer = OriginalPathType::ConstPointer;
  using OrthogonalCorrectionTableType = VectorContainer<unsigned, double>;
  using OrthogonalCorrectionTablePointer = OrthogonalCorrectionTableType::Pointer;

  using OrthogonalCorrectionTableSizeType = OrthogonalCorrectionTableType::ElementIdentifier;

  /** Return the location of the parametric path at the specified location. */
  OutputType
  Evaluate(const InputType & input) const override;

  /** Set pointer to the original path.  The path MUST be continuous in its
   * first derivative to prevent discontinuities in the corrected path.  The
   * path should also be closed, since the first correction is applied to both
   * the beginning and the end of the original path. */
  // The usual itkSetObjectMacro can not be used here because
  // m_DefaultInputStepSize must also be copied over.
  void
  SetOriginalPath(const OriginalPathType * originalPath);

  /** Set table of evenly-spaced orthogonal offsets for the original path. */
  itkSetObjectMacro(OrthogonalCorrectionTable, OrthogonalCorrectionTableType)

    /** New() method for dynamic construction */
    itkNewMacro(Self);

  /** Needed for Pipelining */
  void
  Initialize() override
  {
    this->m_OriginalPath = nullptr;
    this->m_OrthogonalCorrectionTable = nullptr;
  }

  /** These are determined by the original path */
  InputType
  StartOfInput() const override
  {
    return m_OriginalPath->StartOfInput();
  }

  InputType
  EndOfInput() const override
  {
    return m_OriginalPath->EndOfInput();
  }

protected:
  OrthogonallyCorrected2DParametricPath();
  ~OrthogonallyCorrected2DParametricPath() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

private:
  OriginalPathConstPointer         m_OriginalPath;
  OrthogonalCorrectionTablePointer m_OrthogonalCorrectionTable;
};
} // namespace itk

#endif
