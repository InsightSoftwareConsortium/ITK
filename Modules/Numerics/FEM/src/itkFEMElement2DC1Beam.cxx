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

#include "itkFEMElement2DC1Beam.h"

namespace itk
{
namespace fem
{
// Overload the CreateAnother() method.
::itk::LightObject::Pointer Element2DC1Beam::CreateAnother(void) const
{
  ::itk::LightObject::Pointer smartPtr;
  Pointer copyPtr = Self::New();

  copyPtr->SetNode(0, this->GetNode(0) );
  copyPtr->SetNode(1, this->GetNode(1) );
  copyPtr->SetMaterial( this->GetMaterial() );
  copyPtr->SetGlobalNumber( this->GetGlobalNumber() );

  smartPtr = static_cast<Pointer>(copyPtr);

  return smartPtr;
}

Element2DC1Beam
::Element2DC1Beam() : Superclass(), m_mat(ITK_NULLPTR)
{
}

Element2DC1Beam
::Element2DC1Beam(NodeIDType n1_, NodeIDType n2_, Material::ConstPointer m_)
{
  // Set the geometrical points
  this->SetNode(0, n1_);
  this->SetNode(1, n2_);

  /*
   * Initialize the pointer to material object and check that
   * we were given the pointer to the right class.
   * If the material class was incorrect an exception is thrown.
   */
  m_mat = dynamic_cast<const MaterialLinearElasticity *>( m_.GetPointer() );
  if( !m_mat )
    {
    throw FEMExceptionWrongClass(__FILE__, __LINE__, "Element2DC0LinearLineStress::Element2DC0LinearLineStress()");
    }
}

void
Element2DC1Beam
::GetIntegrationPointAndWeight(unsigned int i, VectorType & pt, Float & w, unsigned int order) const
{
  // default integration order
  if( ( order == 0 ) || ( order >= 9 ) )
    {
    order = DefaultIntegrationOrder;
    }

  pt.set_size(1);

  pt[0] = gaussPoint[order][i];
  w = gaussWeight[order][i];
}

unsigned int
Element2DC1Beam
::GetNumberOfIntegrationPoints(unsigned int order) const
{
  // default integration order
  if( ( order == 0 ) || ( order >= 9 ) )
    {
    order = DefaultIntegrationOrder;
    }

  return order;
}

Element2DC1Beam::VectorType
Element2DC1Beam
::ShapeFunctions(const VectorType & pt) const
{
  // 2D Beam element has four shape functions, but we only
  // define two of them, since we're only interested in
  // in interpolating the displacements, not the rotation
  VectorType shapeF(2);

  shapeF[0] = 0.25 * ( 1 - pt[0] ) * ( 1 - pt[0] ) * ( 2 + pt[0] );
  shapeF[1] = 0.25 * ( 1 + pt[0] ) * ( 1 + pt[0] ) * ( 2 - pt[0] );

  return shapeF;
}

void
Element2DC1Beam
::ShapeFunctionDerivatives(const VectorType &, MatrixType & shapeD) const
{
  // FIXME: write proper implementation, since we need the 2nd
  //        order derivatives
  shapeD.set_size(1, 2);
  shapeD.fill(0.0);
}

Element2DC1Beam::Float
Element2DC1Beam
::JacobianDeterminant(const VectorType &, const MatrixType *) const
{
  // FIXME: this is only temporary implementation, so that GenericBodyLoads
  //        implementation works. Write the proper geometric definition
  //        of an element.

  // Get the length of the element
  // Note: This simple implementation is only valid for linear line elements.
  //       For higher order elements we must integrate to obtain the exact
  //       element length
  Float l = ( this->m_node[1]->GetCoordinates() - this->m_node[0]->GetCoordinates() ).magnitude();

  return l / 2;
}

void
Element2DC1Beam
::GetStiffnessMatrix(MatrixType & Ke) const
{
  const unsigned int NDOF = this->GetNumberOfDegreesOfFreedom();

  MatrixType k(NDOF, NDOF);
  MatrixType kb(NDOF, NDOF);

  Float x = m_node[1]->GetCoordinates()[0] - m_node[0]->GetCoordinates()[0];
  Float y = m_node[1]->GetCoordinates()[1] - m_node[0]->GetCoordinates()[1];
  Float l = std::sqrt(x * x + y * y);

  k[0][0] = 1; k[0][1] = 0; k[0][2] = 0; k[0][3] = -1; k[0][4] = 0; k[0][5] = 0;
  k[1][0] = 0; k[1][1] = 0; k[1][2] = 0; k[1][3] = 0; k[1][4] = 0; k[1][5] = 0;
  k[2][0] = 0; k[2][1] = 0; k[2][2] = 0; k[2][3] = 0; k[2][4] = 0; k[2][5] = 0;
  k[3][0] = -1; k[3][1] = 0; k[3][2] = 0; k[3][3] = 1; k[3][4] = 0; k[3][5] = 0;
  k[4][0] = 0; k[4][1] = 0; k[4][2] = 0; k[4][3] = 0; k[4][4] = 0; k[4][5] = 0;
  k[5][0] = 0; k[5][1] = 0; k[5][2] = 0; k[5][3] = 0; k[5][4] = 0; k[5][5] = 0;

  kb = ( m_mat->GetYoungsModulus() * m_mat->GetCrossSectionalArea() / l ) * k;

  k[0][0] = 0; k[0][1] = 0;   k[0][2] = 0;     k[0][3] = 0; k[0][4] = 0;   k[0][5] = 0;
  k[1][0] = 0; k[1][1] = 6;   k[1][2] = 3 * l;   k[1][3] = 0; k[1][4] = -6;   k[1][5] = 3 * l;
  k[2][0] = 0; k[2][1] = 3 * l; k[2][2] = 2 * l * l; k[2][3] = 0; k[2][4] = -3 * l; k[2][5] = l * l;
  k[3][0] = 0; k[3][1] = 0;   k[3][2] = 0;     k[3][3] = 0; k[3][4] = 0;   k[3][5] = 0;
  k[4][0] = 0; k[4][1] = -6;  k[4][2] = -3 * l;  k[4][3] = 0; k[4][4] = 6;   k[4][5] = -3 * l;
  k[5][0] = 0; k[5][1] = 3 * l; k[5][2] = l * l;   k[5][3] = 0; k[5][4] = -3 * l; k[5][5] = 2 * l * l;

  kb += ( 2 * m_mat->GetYoungsModulus() * m_mat->GetMomentOfInertia() / ( l * l * l ) ) * k;

  Float c = x / l;
  Float s = y / l;

  k[0][0] =  c; k[0][1] = s; k[0][2] = 0; k[0][3] = 0; k[0][4] = 0; k[0][5] = 0;
  k[1][0] = -s; k[1][1] = c; k[1][2] = 0; k[1][3] = 0; k[1][4] = 0; k[1][5] = 0;
  k[2][0] =  0; k[2][1] = 0; k[2][2] = 1; k[2][3] = 0; k[2][4] = 0; k[2][5] = 0;
  k[3][0] =  0; k[3][1] = 0; k[3][2] = 0; k[3][3] = c; k[3][4] = s; k[3][5] = 0;
  k[4][0] =  0; k[4][1] = 0; k[4][2] = 0; k[4][3] = -s; k[4][4] = c; k[4][5] = 0;
  k[5][0] =  0; k[5][1] = 0; k[5][2] = 0; k[5][3] = 0; k[5][4] = 0; k[5][5] = 1;

  Ke = k.transpose() * kb * k;
}

void
Element2DC1Beam
::GetMassMatrix(MatrixType & Me) const
{
  const unsigned int NDOF = this->GetNumberOfDegreesOfFreedom();
  MatrixType         m(NDOF, NDOF, 0.0);
  MatrixType         mb(NDOF, NDOF, 0.0);
  MatrixType         k(NDOF, NDOF, 0.0);

  Float x = m_node[1]->GetCoordinates()[0] - m_node[0]->GetCoordinates()[0];
  Float y = m_node[1]->GetCoordinates()[1] - m_node[0]->GetCoordinates()[1];
  Float l = std::sqrt(x * x + y * y);

  m[0][0] = 2.0; m[0][3] = 1.0;
  m[3][0] = 1.0; m[3][3] = 2.0;
  m = ( this->m_mat->GetDensityHeatProduct() * this->m_mat->GetCrossSectionalArea() * l / 6.0 ) * m;

  mb[1][1] = 156.0; mb[1][2] = 22.0 * l; mb[1][4] = 54.0; mb[1][5] = -13.0 * l;
  mb[2][1] = 22.0 * l; mb[2][2] = 4.0 * l * l; mb[2][4] = 13.0 * l; mb[2][5] = -3.0 * l * l;
  mb[4][1] = 54.0; mb[4][2] = 13.0 * l; mb[4][4] = 156.0; mb[4][5] = -22.0 * l;
  mb[5][1] = -13.0 * l; mb[5][2] = -3.0 * l * l; mb[5][4] = -22.0 * l; mb[5][5] = 4.0 * l * l;
  mb = ( this->m_mat->GetDensityHeatProduct() * this->m_mat->GetCrossSectionalArea() * l / 420.0 ) * mb;

  m = m + mb;

  Float c = x / l;
  Float s = y / l;

  k[0][0] =  c; k[0][1] = s; k[0][2] = 0; k[0][3] = 0; k[0][4] = 0; k[0][5] = 0;
  k[1][0] = -s; k[1][1] = c; k[1][2] = 0; k[1][3] = 0; k[1][4] = 0; k[1][5] = 0;
  k[2][0] =  0; k[2][1] = 0; k[2][2] = 1; k[2][3] = 0; k[2][4] = 0; k[2][5] = 0;
  k[3][0] =  0; k[3][1] = 0; k[3][2] = 0; k[3][3] = c; k[3][4] = s; k[3][5] = 0;
  k[4][0] =  0; k[4][1] = 0; k[4][2] = 0; k[4][3] = -s; k[4][4] = c; k[4][5] = 0;
  k[5][0] =  0; k[5][1] = 0; k[5][2] = 0; k[5][3] = 0; k[5][4] = 0; k[5][5] = 1;

  Me = k.transpose() * m * k;
}

void Element2DC1Beam::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "Materials: " << this->m_mat << std::endl;
}

}
}  // end namespace itk::fem
