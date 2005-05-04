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
 * \sa SymmetricSecondRankTensor
 *
 * \ingroup ImageObjects
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
  typedef typename Superclass::ComponentArrayType    ComponentArrayType;
  typedef typename Superclass::AccumulateValueType   AccumulateValueType;

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

};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkDiffusionTensor3D.txx"
#endif

#endif
