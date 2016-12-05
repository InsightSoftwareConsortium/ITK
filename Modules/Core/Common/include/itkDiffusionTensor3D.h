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
#ifndef itkDiffusionTensor3D_h
#define itkDiffusionTensor3D_h

// Undefine an eventual DiffusionTensor3D macro
#ifdef DiffusionTensor3D
#undef DiffusionTensor3D
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
 * http://www.na-mic.org/Wiki/index.php/NAMIC_Wiki:DTI:ITK-DiffusionTensorPixelType
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
 * \par References
 * E. R. Melhem, S. Mori, G. Mukundan, M. A. Kraut, M. G. Pomper, and
 * P. C. M. van Zijl, "Diffusion tensor MR imaging of the brain and white
 * matter tractography," Am. J. Roentgenol., vol. 178, pp. 3-16, 2002.
*
 * \sa SymmetricSecondRankTensor
 *
 * \ingroup ImageObjects   TensorObjects    Geometry
 * \ingroup ITKCommon
 */

template< typename TComponent >
class ITK_TEMPLATE_EXPORT DiffusionTensor3D:public SymmetricSecondRankTensor< TComponent, 3 >
{
public:
  /** Standard class typedefs. */
  typedef DiffusionTensor3D                          Self;
  typedef SymmetricSecondRankTensor< TComponent, 3 > Superclass;

  /** Propagating some typedef from the superclass */
  typedef typename Superclass::ValueType     ValueType;
  typedef typename Superclass::ComponentType ComponentType;
#if defined( __GNUC__ ) && !defined( __INTEL_COMPILER ) && ( __GNUC__ == 3 )
  typedef ComponentType ComponentArrayType[6];
#else
  typedef typename Superclass::ComponentArrayType ComponentArrayType;
#endif
  typedef typename Superclass::AccumulateValueType AccumulateValueType;
  typedef typename Superclass::RealValueType       RealValueType;

  typedef typename Superclass::EigenValuesArrayType   EigenValuesArrayType;
  typedef typename Superclass::EigenVectorsMatrixType EigenVectorsMatrixType;

  /** Default Constructor. */
  DiffusionTensor3D();

  /** Constructor with initialization. */
  DiffusionTensor3D(const Superclass & r);
  DiffusionTensor3D(const ComponentType & r);
  DiffusionTensor3D(const ComponentArrayType r);

  /** Constructor to enable casting...  */
  template< typename TCoordRepB >
  DiffusionTensor3D(const DiffusionTensor3D< TCoordRepB > & pa):
    SymmetricSecondRankTensor< TComponent, 3 >(pa) {}

  /** Pass-through assignment operator for the Array base class. */
  Self & operator=(const Superclass & r);

  Self & operator=(const ComponentType & r);

  Self & operator=(const ComponentArrayType r);

  /** Templated Pass-through assignment for the Array base class. */
  template< typename TCoordRepB >
  Self & operator=(const DiffusionTensor3D< TCoordRepB > & pa)
  {
    //NOTE (this != &pa ) because they are different pointer types
    //if this templated function is called
    // ComponentType 'itk::DiffusionTensor3D<double> *'
    // TCoordRepB   'const DiffusionTensor3D<float> *')
    SymmetricSecondRankTensor< TComponent, 3 >::operator=(pa);
    return *this;
  }

  /** Get Trace value */
  AccumulateValueType GetTrace() const;

  /** Get the value of Fractional Anisotropy from the Tensor. */
  RealValueType GetFractionalAnisotropy() const;

  /** Get the value of Relative Anisotropy from the Tensor. */
  RealValueType GetRelativeAnisotropy() const;

  /** Get the Inner Scalar Product from the Tensor. */
  RealValueType GetInnerScalarProduct() const;
};
} // end namespace itk
#include "itkNumericTraitsDiffusionTensor3DPixel.h"

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkDiffusionTensor3D.hxx"
#endif

#endif
