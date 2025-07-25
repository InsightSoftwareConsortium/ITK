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
#ifndef itkDiffusionTensor3D_h
#define itkDiffusionTensor3D_h

// Undefine an eventual DiffusionTensor3D macro
#ifdef DiffusionTensor3D
#  undef DiffusionTensor3D
#endif

#include "itkSymmetricSecondRankTensor.h"

namespace itk
{
/** \class DiffusionTensor3D
 * \brief Represent a diffusion tensor as used in DTI images.
 *
 * This class implements a 3D symmetric tensor as it is used for representing
 * diffusion of water molecules in Diffusion Tensor Images.
 *
 * This class derives from the SymmetricSecondRankTensor, inheriting
 * most of the Tensor-related behavior. At this level we add the methods that
 * are specific to 3D and that are closely related to the concept of diffusion.
 *
 *
 * \author Jeffrey Duda from School of Engineering at University of Pennsylvania
 * \author Torsten Rohlfing from SRI International Neuroscience Program.
 *
 * This class was mostly based on files that Jeffrey Duda, Torsten Rohlfing and
 * Martin Styner contributed to the ITK users list during a discussion on
 * support for DiffusionTensorImages. A discussion on the design of this class
 * can be found in the WIKI pages of NAMIC:
 *
 * https://www.na-mic.org/Wiki/index.php/NAMIC_Wiki:DTI:ITK-DiffusionTensorPixelType
 *
 * \note This work is part of the National Alliance for Medical Image
 * Computing (NAMIC), funded by the National Institutes of Health
 * through the NIH Roadmap for Medical Research, Grant U54 EB005149.
 * Information on the National Centers for Biomedical Computing
 * can be obtained from http://commonfund.nih.gov/bioinformatics.
 *
 *
 * \note Contributions by Torsten Rohlfing were funded by the following NIH grants
 *
 * Alcohol, HIV and the Brain,
 * NIAAA AA12999, PI: A. Pfefferbaum
 *
 * Normal Aging of Brain Structure and Function
 * NIA AG 17919, PI: E.V. Sullivan.
 *
 *
 * For algorithmic details see \cite melhem2002.
 *
 * \sa SymmetricSecondRankTensor
 *
 * \ingroup ImageObjects   TensorObjects    Geometry
 * \ingroup ITKCommon
 */

template <typename TComponent>
class ITK_TEMPLATE_EXPORT DiffusionTensor3D : public SymmetricSecondRankTensor<TComponent, 3>
{
public:
  /** Standard class type aliases. */
  using Self = DiffusionTensor3D;
  using Superclass = SymmetricSecondRankTensor<TComponent, 3>;

  /** Propagating some type alias from the superclass */
  using typename Superclass::ValueType;
  using typename Superclass::ComponentType;
  using typename Superclass::ComponentArrayType;

  using typename Superclass::AccumulateValueType;
  using typename Superclass::RealValueType;

  using typename Superclass::EigenValuesArrayType;
  using typename Superclass::EigenVectorsMatrixType;

  /** Default Constructor. */
  DiffusionTensor3D() = default;

  /** Constructor with initialization. */
  /** @ITKStartGrouping */
  DiffusionTensor3D(const Superclass & r);
  DiffusionTensor3D(const ComponentType & r);
  DiffusionTensor3D(const ComponentArrayType r);
  /** @ITKEndGrouping */
  /** Constructor to enable casting...  */
  template <typename TCoordinateB>
  DiffusionTensor3D(const DiffusionTensor3D<TCoordinateB> & pa)
    : SymmetricSecondRankTensor<TComponent, 3>(pa)
  {}

  /** Pass-through assignment operator for the Array base class. */
  Self &
  operator=(const Superclass & r);

  Self &
  operator=(const ComponentType & r);

  Self &
  operator=(const ComponentArrayType r);

  /** Templated Pass-through assignment for the Array base class. */
  template <typename TCoordinateB>
  Self &
  operator=(const DiffusionTensor3D<TCoordinateB> & pa)
  {
    // NOTE (this != &pa ) because they are different pointer types
    // if this templated function is called
    // ComponentType 'itk::DiffusionTensor3D<double> *'
    // TCoordinateB   'const DiffusionTensor3D<float> *')
    SymmetricSecondRankTensor<TComponent, 3>::operator=(pa);
    return *this;
  }

  /** Get the trace value.
   *
   * Note that the indices are related to the fact
   * that we store only the upper-right triangle of
   * the matrix. Like
   *
   *       | 0  1  2  |
   *       | X  3  4  |
   *       | X  X  5  |
   *
   * The trace is therefore the sum of the components
   * M[0], M[3] and M[5].
   *
   */
  [[nodiscard]] AccumulateValueType
  GetTrace() const;

  /** Get the value of Fractional Anisotropy from the Tensor. */
  [[nodiscard]] RealValueType
  GetFractionalAnisotropy() const;

  /** Get the value of Relative Anisotropy from the Tensor. */
  [[nodiscard]] RealValueType
  GetRelativeAnisotropy() const;

  /** Get the inner scalar product from the Tensor. */
  [[nodiscard]] RealValueType
  GetInnerScalarProduct() const;
};


template <typename T>
inline void
swap(DiffusionTensor3D<T> & a, DiffusionTensor3D<T> & b) noexcept
{
  a.swap(b);
}

} // end namespace itk
#include "itkNumericTraitsDiffusionTensor3DPixel.h"

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkDiffusionTensor3D.hxx"
#endif

#endif
