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

#include "itkFEMElement3DC0LinearTriangularMembrane.h"

namespace itk
{
namespace fem
{

// Overload the CreateAnother() method
::itk::LightObject::Pointer Element3DC0LinearTriangularMembrane::CreateAnother(void) const
{
  ::itk::LightObject::Pointer smartPtr;
  Pointer copyPtr = Self::New();

  copyPtr->SetNode(0, this->GetNode(0) );
  copyPtr->SetNode(1, this->GetNode(1) );
  copyPtr->SetNode(2, this->GetNode(2) );
  copyPtr->SetNode(3, this->GetNode(3) );
  copyPtr->SetMaterial( this->GetMaterial() );
  copyPtr->SetGlobalNumber( this->GetGlobalNumber() );

  smartPtr = static_cast<Pointer>(copyPtr);

  return smartPtr;
}

Element3DC0LinearTriangularMembrane
::Element3DC0LinearTriangularMembrane() : Superclass()
{
}

Element3DC0LinearTriangularMembrane
::Element3DC0LinearTriangularMembrane(NodeIDType n1_, NodeIDType n2_, NodeIDType n3_,
                                      Material::ConstPointer m_) : Superclass()
{
  // Set the geometrical points
  this->SetNode(0, n1_);
  this->SetNode(1, n2_);
  this->SetNode(2, n3_);

  /*
   * Initialize the pointer to material object and check that
   * we were given the pointer to the right class.
   * If the material class was incorrect an exception is thrown.
   */
  m_mat = dynamic_cast<const MaterialLinearElasticity *>( m_.GetPointer() );
  if( !m_mat )
    {
    throw FEMExceptionWrongClass(__FILE__,
                                 __LINE__,
                                 "Element3DC0LinearTriangularMembrane::Element3DC0LinearTriangularMembrane()");
    }
}

/*
void Element3DC0LinearTriangularMembrane::GetStiffnessMatrix(MatrixType& Ke) const
{
  MatrixType D;
  unsigned int Nip=this->GetNumberOfIntegrationPoints(0);
  VectorType ip;
  Float w;
  this->GetIntegrationPointAndWeight(0,ip,w,0);
  //
  //std::cout<< " Nip " << Nip << " w " << w << std::endl;
  this->GetMaterialMatrix(D);

  Ke.set_size(3,3);


    int na=0;
    int nb=1;
    int nc=2;
      {
    VectorType A=this->GetNode(na)->GetCoordinates();
    VectorType B=this->GetNode(nb)->GetCoordinates();
    VectorType C=this->GetNode(nc)->GetCoordinates();
    VectorType BA =B-A;
    VectorType AC =A-C;
    VectorType CB =C-B;
    float bamag=BA.magnitude();
    float cbmag=CB.magnitude();
    float acmag=AC.magnitude();

    if (bamag > cbmag && bamag > acmag) { na=0; nb=1; nc=2; }
    if (cbmag > bamag && cbmag > acmag) { na=1; nb=2; nc=0; }
    if (acmag > bamag && acmag > cbmag) { na=2; nb=0; nc=1; }
      }

    VectorType A=this->GetNode(na)->GetCoordinates();
    VectorType B=this->GetNode(nb)->GetCoordinates();
    VectorType C=this->GetNode(nc)->GetCoordinates();
    VectorType BA =B-A;
    VectorType CA =C-A;
    VectorType CB =C-B;
    float bamag=BA.magnitude();
    float cbmag=CB.magnitude();
    float acmag=CA.magnitude();

    float t=(CA[0]*BA[0]+CA[1]*BA[1]+CA[2]*BA[2])/bamag*bamag;

    VectorType E = A+BA*t;
    VectorType CE =C-E;
    VectorType BE =B-E;
    VectorType AE =A-E;

    float cemag=CE.magnitude();
    float bemag=CE.magnitude();
    float aemag=AE.magnitude();

    float h1;
    if (acmag > aemag) h1=acmag; else h1=aemag;

    float theta1=asin(cemag/h1);

    float h2;
    if (cbmag > bemag) h2=cbmag; else h2=bemag;

    float theta2=asin(cemag/h2);


    float theta3=acos(-1.0)-theta1-theta2;

  float cottheta1=atan(theta1);
  float cottheta2=atan(theta2);
  float cottheta3=atan(theta3);

  Ke[0][0]=(cottheta3+cottheta2)*D[0][0];
  Ke[1][1]=(cottheta3+cottheta1)*D[0][0];
  Ke[2][2]=(cottheta1+cottheta2)*D[0][0];

  Ke[0][1]=-cottheta3*D[0][0];
  Ke[0][2]=-cottheta2*D[0][0];
  Ke[1][2]=-cottheta1*D[0][0];

  Ke[2][1]=Ke[1][2]*D[0][0];
  Ke[2][0]=Ke[0][2]*D[0][0];
  Ke[1][0]=Ke[0][1]*D[0][0];

//  std::cout << " lapl belt " << std::endl;
//  std::cout << Ke << std::endl;

}
*/

void
Element3DC0LinearTriangularMembrane::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
}

}
}  // end namespace itk::fem
