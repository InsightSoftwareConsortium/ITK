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

#include "itkFEMElement3DC0LinearTriangularLaplaceBeltrami.h"

namespace itk
{
namespace fem
{
// Overload the CreateAnother() method
::itk::LightObject::Pointer Element3DC0LinearTriangularLaplaceBeltrami::CreateAnother(void) const
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

Element3DC0LinearTriangularLaplaceBeltrami
::Element3DC0LinearTriangularLaplaceBeltrami() : Superclass()
{
}

Element3DC0LinearTriangularLaplaceBeltrami
::Element3DC0LinearTriangularLaplaceBeltrami(NodeIDType n1_, NodeIDType n2_, NodeIDType n3_,
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
  m_Mat = dynamic_cast<const MaterialLinearElasticity *>( m_.GetPointer() );

  if( !m_Mat )
    {
    throw FEMExceptionWrongClass(
            __FILE__,
            __LINE__,
            "Element3DC0LinearTriangularLaplaceBeltrami::Element3DC0LinearTriangularLaplaceBeltrami()");
    }
}

void Element3DC0LinearTriangularLaplaceBeltrami::GetStiffnessMatrix(MatrixType & Ke) const
{
  MatrixType cot, D, BB;

  this->GetMaterialMatrix(D);
  //
  // std::cout<< " Nip " << Nip << " w " << w << std::endl;
  this->GetMaterialMatrix(D);

  cot.set_size(3, 3);
  cot.fill(0.);

  int na = 0;
  int nb = 1;
  int nc = 2;

  VectorType A = this->GetNode(na)->GetCoordinates();
  VectorType B = this->GetNode(nb)->GetCoordinates();
  VectorType C = this->GetNode(nc)->GetCoordinates();
  VectorType BA = B - A;
  VectorType CA = C - A;
  VectorType CB = C - B;
  float      L1 = CB.magnitude();
  float      L2 = CA.magnitude();
  float      L3 = BA.magnitude();

  float s = ( L1 + L2 + L3 ) * .5;
  Float Area = sqrt( s * ( s - L1 ) * ( s - L2 ) * ( s - L3 ) );

  cot[0][0] = ( 2.0 * L1 * L1 ) * D[0][0];
  cot[1][1] = ( 2.0 * L2 * L2 ) * D[0][0];
  cot[2][2] = ( 2.0 * L3 * L3 ) * D[0][0];

  cot[0][1] = ( L3 * L3 - L1 * L1 - L2 * L2 ) * D[0][0];
  cot[0][2] = ( L2 * L2 - L1 * L1 - L3 * L3 ) * D[0][0];
  cot[1][2] = ( L1 * L1 - L3 * L3 - L2 * L2 ) * D[0][0];

  cot[1][0] = ( L3 * L3 - L1 * L1 - L2 * L2 ) * D[0][0];
  cot[2][0] = ( L2 * L2 - L1 * L1 - L3 * L3 ) * D[0][0];
  cot[2][1] = ( L1 * L1 - L3 * L3 - L2 * L2 ) * D[0][0];

  cot = cot * 1.0 / ( 8.0 * Area );

/*  if ( this->GetNode(0)->GetDegreeOfFreedom(0)==53 ||
       this->GetNode(1)->GetDegreeOfFreedom(0)==53 ||
       this->GetNode(2)->GetDegreeOfFreedom(0)==53 )
  {
  std::cout << " cot " << this->GetNode(0)->GetDegreeOfFreedom(0) << "  " <<this->GetNode(1)->GetDegreeOfFreedom(0) << "  " <<this->GetNode(2)->GetDegreeOfFreedom(0) <<std::endl;
  std::cout <<  cot <<std::endl;
  }

 */
  if( GetNumberOfDegreesOfFreedomPerNode() == 3 )
    {
    Ke.set_size(9, 9);
    Ke.fill(0.0);
    for( int dd = 0; dd < 3; dd++ )
      {
      Ke[0][dd * 3] = cot[0][dd];
      }
    for( int dd = 0; dd < 3; dd++ )
      {
      Ke[1][dd * 3 + 1] = cot[0][dd];
      }
    for( int dd = 0; dd < 3; dd++ )
      {
      Ke[2][dd * 3 + 2] = cot[0][dd];
      }
    for( int dd = 0; dd < 3; dd++ )
      {
      Ke[3][dd * 3] = cot[1][dd];
      }
    for( int dd = 0; dd < 3; dd++ )
      {
      Ke[4][dd * 3 + 1] = cot[1][dd];
      }
    for( int dd = 0; dd < 3; dd++ )
      {
      Ke[5][dd * 3 + 2] = cot[1][dd];
      }
    for( int dd = 0; dd < 3; dd++ )
      {
      Ke[6][dd * 3] = cot[2][dd];
      }
    for( int dd = 0; dd < 3; dd++ )
      {
      Ke[7][dd * 3 + 1] = cot[2][dd];
      }
    for( int dd = 0; dd < 3; dd++ )
      {
      Ke[8][dd * 3 + 2] = cot[2][dd];
      }
    }
  else
    {
    Ke = cot;
    }

//  std::cout << " Ke in elt " <<std::endl;
//  std::cout <<  Ke <<std::endl;
}

/*
void Element3DC0LinearTriangularLaplaceBeltrami::GetStiffnessMatrix(MatrixType& Ke) const
{
MatrixType cot,D,BB;
this->GetMaterialMatrix( D );

VectorType ip;
Float w;
MatrixType J;
MatrixType shapeDgl;
MatrixType shapeD;

//
//std::cout<< " Nip " << Nip << " w " << w << std::endl;
this->GetMaterialMatrix(D);

cot.set_size(3,3);
cot.fill(0.);


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

float cottheta1=cemag/aemag;
//  if (aemag == 0) cottheta1=1.0/tan(3.14159/2.0);
float cottheta2=cemag/bemag;
float cottheta3=1.0/tan(theta3);

//  if (fabs(cottheta1-1) < 1.e-6 && fabs(cottheta2-1) < 1.e-6) cottheta3=1.0;
//  std::cout <<" ct0 " << cottheta1 <<" ct1 " << cottheta2 <<" ct2 " << cottheta3  << std::endl;

cot[na][na]=(cottheta3+cottheta2)*D[0][0];
cot[nb][nb]=(cottheta3+cottheta1)*D[0][0];
cot[nc][nc]=(cottheta1+cottheta2)*D[0][0];

cot[na][na]=-cottheta3*D[0][0];
cot[na][nc]=-cottheta2*D[0][0];
cot[nb][nc]=-cottheta1*D[0][0];

cot[nc][nb]=cot[nb][nc]*D[0][0];
cot[nc][na]=cot[na][nc]*D[0][0];
cot[nb][na]=cot[na][nb]*D[0][0];

cot=cot*0.5;
Ke=cot;
if ( this->GetNode(0)->GetDegreeOfFreedom(0)==909 ||
     this->GetNode(1)->GetDegreeOfFreedom(0)==909 ||
     this->GetNode(2)->GetDegreeOfFreedom(0)==909 )

{
std::cout << " cot " << std::endl;
std::cout <<  cot <<std::endl;
}

Ke.set_size(9,9);
Ke.fill(0.0);

for(int dd=0; dd<3; dd++) Ke[0][dd*3]=cot[0][dd];
for(int dd=0; dd<3; dd++) Ke[1][dd*3+1]=cot[0][dd];
for(int dd=0; dd<3; dd++) Ke[2][dd*3+2]=cot[0][dd];

for(int dd=0; dd<3; dd++) Ke[3][dd*3]=cot[1][dd];
for(int dd=0; dd<3; dd++) Ke[4][dd*3+1]=cot[1][dd];
for(int dd=0; dd<3; dd++) Ke[5][dd*3+2]=cot[1][dd];

for(int dd=0; dd<3; dd++) Ke[6][dd*3]=cot[2][dd];
for(int dd=0; dd<3; dd++) Ke[7][dd*3+1]=cot[2][dd];
for(int dd=0; dd<3; dd++) Ke[8][dd*3+2]=cot[2][dd];

//  std::cout << " Ke in elt " <<std::endl;
//  std::cout <<  Ke <<std::endl;

}
*/

void
Element3DC0LinearTriangularLaplaceBeltrami::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
}

}
}  // end namespace itk::fem
