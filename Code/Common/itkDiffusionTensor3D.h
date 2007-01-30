/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkDiffusionTensor3D.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkDiffusionTensor3D_h
#define __itkDiffusionTensor3D_h

// Undefine an eventual DiffusionTensor3D macro
#ifdef DiffusionTensor3D
#undef DiffusionTensor3D
#endif

#include <itkSymmetricSecondRankTensor.h>


namespace itk
{

/** \class DiffusionTensor3D
 * \brief Represent a diffusion tensor as used in DTI images.
 *
 * This class implements a 3D symmetric tensor as it is used for representing
 * diffusion of water molecules in Diffusion Tensor Images.
 *
 * This class derive from the SymmetricSecondRankTensor and from it inherit
 * most of the Tensor related behavior. At this level we add the methods that
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
 * can be obtained from http://nihroadmap.nih.gov/bioinformatics. 
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
 */

template < typename TComponent >
class DiffusionTensor3D: public SymmetricSecondRankTensor<TComponent,3>
{
public:
  /** Standard class typedefs. */
  typedef DiffusionTensor3D  Self;
  typedef SymmetricSecondRankTensor<TComponent, 3> Superclass;
  
  /** Propagating some typedef from the superclass */
  typedef typename Superclass::ValueType             ValueType;
  typedef typename Superclass::ComponentType         ComponentType;
#if defined(__GNUC__) && !defined(__INTEL_COMPILER) && (__GNUC__ == 3 && __GNUC_MINOR__ == 2 && __GNUC_PATCHLEVEL__ == 3)
  typedef ComponentType ComponentArrayType[6];
#else
  typedef typename Superclass::ComponentArrayType    ComponentArrayType;
#endif
  typedef typename Superclass::AccumulateValueType   AccumulateValueType;
  typedef typename Superclass::RealValueType         RealValueType;

  typedef typename Superclass::EigenValuesArrayType    EigenValuesArrayType;
  typedef typename Superclass::EigenVectorsMatrixType  EigenVectorsMatrixType;

  /** Default Constructor. */
  DiffusionTensor3D();

  /** Constructor with initialization. */
  DiffusionTensor3D(const Self& r);
  DiffusionTensor3D(const Superclass& r);
  DiffusionTensor3D(const ComponentType& r);
  DiffusionTensor3D(const ComponentArrayType r);

  /** Pass-through assignment operator for the Array base class. */
  Self& operator= (const Self& r);
  Self& operator= (const Superclass & r);
  Self& operator= (const ComponentType& r);
  Self& operator= (const ComponentArrayType r);

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

// Define instantiation macro for this template.
#define ITK_TEMPLATE_DiffusionTensor3D(_, EXPORT, x, y) namespace itk { \
  _(1(class EXPORT DiffusionTensor3D< ITK_TEMPLATE_1 x >)) \
  namespace Templates { typedef DiffusionTensor3D< ITK_TEMPLATE_1 x > \
                                            DiffusionTensor3D##y; } \
  }

#if ITK_TEMPLATE_EXPLICIT
# include "Templates/itkDiffusionTensor3D+-.h"
#endif

#if ITK_TEMPLATE_TXX
# include "itkDiffusionTensor3D.txx"
#endif

#endif
