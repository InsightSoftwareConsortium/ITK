/*=========================================================================
*
* Copyright Insight Software Consortium
*
* Licensed under the Apache License, Version 2.0 (the "License");
* you may not use this file except in compliance with the License.
* You may obtain a copy of the License at
*
* http://www.apache.org/licenses/LICENSE-2.0.txt
*
* Unless required by applicable law or agreed to in writing, software
* distributed under the License is distributed on an "AS IS" BASIS,
* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
* See the License for the specific language governing permissions and
* limitations under the License.
*
*=========================================================================*/

#ifndef itkFEMElement3DC0LinearTriangularLaplaceBeltrami_h
#define itkFEMElement3DC0LinearTriangularLaplaceBeltrami_h

#include "itkFEMElement3DC0LinearTriangular.h"
#include "itkFEMElement3DMembrane.h"
#include "itkFEMElement3DMembrane1DOF.h"
#include "ITKFEMExport.h"

namespace itk
{
namespace fem
{
/**
 * \class Element3DC0LinearTriangularLaplaceBeltrami
 * \brief 3-noded finite element class in 3D space for surface LaplaceBeltrami problem.
 *
 * * This class combines the geometry of the FE problem defined in
 * \link Element3DC0LinearTriangular\endlink
 * and the physics of the problem defined in
 * \link Element3DMembrane1DOF\endlink
 *
 * \ingroup ITKFEM
 */
class ITKFEM_EXPORT Element3DC0LinearTriangularLaplaceBeltrami : public Element3DMembrane1DOF<Element3DC0LinearTriangular>
{
public:
  /** Standard class typedefs. */
  typedef Element3DC0LinearTriangularLaplaceBeltrami         Self;
  typedef Element3DMembrane1DOF<Element3DC0LinearTriangular> Superclass;
  typedef SmartPointer<Self>                                 Pointer;
  typedef SmartPointer<const Self>                           ConstPointer;

  /** Method for creation through the object factory. */
  itkSimpleNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(Element3DC0LinearTriangularLaplaceBeltrami, Element3DMembrane1DOF<Element3DC0LinearTriangular> );

  /** CreateAnother method will clone the existing instance of this type,
   * including its internal member variables. */
  virtual::itk::LightObject::Pointer CreateAnother(void) const ITK_OVERRIDE;

  /**
   * Default constructor only clears the internal storage
   */
  Element3DC0LinearTriangularLaplaceBeltrami();

  /**
   * Construct an element by specifying pointers to
   * 3 points and a material.
   */
  Element3DC0LinearTriangularLaplaceBeltrami(NodeIDType n1_, NodeIDType n2_, NodeIDType n3_, Material::ConstPointer p_);

   /** Get the degress of freesom for each node */
  virtual unsigned int GetNumberOfDegreesOfFreedomPerNode(void) const ITK_OVERRIDE
  {
    return 1;
  }

   /** Get the Stiffness matrix */
  virtual void GetStiffnessMatrix(MatrixType & Ke) const ITK_OVERRIDE;

protected:
  virtual void PrintSelf(std::ostream& os, Indent indent) const ITK_OVERRIDE;

};  // class Element3DC0LinearTriangularLaplaceBeltrami

}
}  // end namespace itk::fem

#endif  // #ifndef itkFEMElement3DC0LinearTriangularLaplaceBeltrami_h
