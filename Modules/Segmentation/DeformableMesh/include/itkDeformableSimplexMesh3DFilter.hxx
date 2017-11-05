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
/*=========================================================================
 *
 *  Portions of this file are subject to the VTK Toolkit Version 3 copyright.
 *
 *  Copyright (c) Ken Martin, Will Schroeder, Bill Lorensen
 *
 *  For complete copyright, license and disclaimer of warranty information
 *  please refer to the NOTICE file at the top of the ITK source tree.
 *
 *=========================================================================*/
#ifndef itkDeformableSimplexMesh3DFilter_hxx
#define itkDeformableSimplexMesh3DFilter_hxx

#include "itkDeformableSimplexMesh3DFilter.h"
#include "itkNumericTraits.h"
#include "itkMath.h"

#include <set>

#include "vxl_version.h"
#if VXL_VERSION_DATE_FULL > 20040406
#include "vnl/vnl_cross.h"
#define itk_cross_3d vnl_cross_3d
#else
#define itk_cross_3d cross_3d
#endif

namespace itk
{
/* Constructor. */
template< typename TInputMesh, typename TOutputMesh >
DeformableSimplexMesh3DFilter< TInputMesh, TOutputMesh >
::DeformableSimplexMesh3DFilter()
{
  m_Step = 0;
  m_Iterations = 20;
  m_Alpha = 0.2;
  m_Beta  = 0.01;
  m_Gamma = 0.05;
  m_Damping = 0.65;
  m_Rigidity = 1;

  m_ImageDepth = 0;
  m_ImageHeight = 0;
  m_ImageWidth = 0;

  this->ProcessObject::SetNumberOfRequiredInputs(1);

  OutputMeshPointer output = OutputMeshType::New();
  this->ProcessObject::SetNumberOfRequiredOutputs(1);
  this->ProcessObject::SetNthOutput( 0, output.GetPointer() );

  this->m_Data = ITK_NULLPTR;
}

template< typename TInputMesh, typename TOutputMesh >
DeformableSimplexMesh3DFilter< TInputMesh, TOutputMesh >
::~DeformableSimplexMesh3DFilter()
{}

/* PrintSelf. */
template< typename TInputMesh, typename TOutputMesh >
void
DeformableSimplexMesh3DFilter< TInputMesh, TOutputMesh >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "Alpha = " << this->GetAlpha() << std::endl;
  os << indent << "Beta = " << this->GetBeta() << std::endl;
  os << indent << "Gamma = " << this->GetGamma() << std::endl;
  os << indent << "Rigidity = " << this->GetRigidity() << std::endl;
  os << indent << "Iterations = " << this->GetIterations() << std::endl;
  os << indent << "Step = " << this->GetStep() << std::endl;
  os << indent << "ImageDepth = " << this->GetImageDepth() << std::endl;

  const GradientImageType * gradientImage = this->GetGradient();

  if ( gradientImage )
    {
    os << indent << "Gradient = " << gradientImage << std::endl;
    }
  else
    {
    os << indent << "Gradient = " << "(None)" << std::endl;
    }
  os << indent << "ImageHeight = " << this->GetImageHeight() << std::endl;
  os << indent << "ImageWidth = " << this->GetImageWidth() << std::endl;
  os << indent << "Damping = " << this->GetDamping() << std::endl;
  if ( this->m_Data.IsNotNull() )
    {
    os << indent << "Data = " << this->GetData() << std::endl;
    }
  else
    {
    os << indent << "Data = " << "(None)" << std::endl;
    }
} /* End PrintSelf. */

/* Generate Data */
template< typename TInputMesh, typename TOutputMesh >
void
DeformableSimplexMesh3DFilter< TInputMesh, TOutputMesh >
::GenerateData()
{
  this->Initialize();

  m_Step = 0;

  while ( m_Step < m_Iterations )
    {
    const float progress = static_cast< float >( m_Step )
                           / static_cast< float >( m_Iterations );

    this->UpdateProgress(progress);

    this->ComputeGeometry();

    if ( m_Step % 10 == 0 && m_Step > 0 )
      {
      this->UpdateReferenceMetrics();
      }

    this->ComputeDisplacement();
    m_Step++;
    }

  const InputMeshType *             inputMesh = this->GetInput(0);
  const InputPointsContainer *      points = inputMesh->GetPoints();
  InputPointsContainerConstIterator pointItr = points->Begin();

  while ( pointItr != points->End() )
    {
    SimplexMeshGeometry *data;
    IdentifierType       idx = pointItr.Index();
    data = this->m_Data->GetElement(idx);
    delete data->neighborSet;
    data->neighborSet = ITK_NULLPTR;
    pointItr++;
    }

  this->ComputeOutput();
}

/* Set default value of parameters and initialize local data container
 *  such as forces, displacements and displacement derivatives. */
template< typename TInputMesh, typename TOutputMesh >
void
DeformableSimplexMesh3DFilter< TInputMesh, TOutputMesh >
::Initialize()
{
  const InputMeshType *             inputMesh = this->GetInput(0);
  const InputPointsContainer *      points = inputMesh->GetPoints();
  InputPointsContainerConstIterator pointItr = points->Begin();

  const GradientImageType * gradientImage = this->GetGradient();
  if (gradientImage->GetSpacing()[0] != 1.0
      || gradientImage->GetSpacing()[1] != 1.0
      || gradientImage->GetSpacing()[2] != 1.0
      || gradientImage->GetOrigin()[0] != 0.0
      || gradientImage->GetOrigin()[1] != 0.0
      || gradientImage->GetOrigin()[2] != 0.0)
    {
    itkExceptionMacro("This filter assumes [0,0,0] origin and spacing of [1,1,1]."
        << " Gradient image has origin: " << gradientImage->GetOrigin()
        << " and spacing: " << gradientImage->GetSpacing());
    }

  if ( gradientImage )
    {
    GradientImageSizeType imageSize = gradientImage->GetBufferedRegion().GetSize();

    m_ImageWidth  = imageSize[0];
    m_ImageHeight = imageSize[1];
    m_ImageDepth  = imageSize[2];
    }
  else
    {
    m_ImageWidth  = 0;
    m_ImageHeight = 0;
    m_ImageDepth  = 0;
    }

  if ( this->m_Data.IsNull() )
    {
    this->m_Data = inputMesh->GetGeometryData();
    }

  while ( pointItr != points->End() )
    {
    SimplexMeshGeometry *data;
    IdentifierType       idx = pointItr.Index();

    data = this->m_Data->GetElement(idx);
    data->pos = pointItr.Value();

    //        InputMeshType::ArrayType neighbors =
    // this->GetInput(0)->GetNeighbors( pointItr.Index() );

    data->neighbors[0] = points->GetElement(data->neighborIndices[0]);
    data->neighbors[1] = points->GetElement(data->neighborIndices[1]);
    data->neighbors[2] = points->GetElement(data->neighborIndices[2]);

    // store neighborset with a specific radius
    InputNeighbors *       neighborsList = inputMesh->GetNeighbors(pointItr.Index(), m_Rigidity);
    InputNeighborsIterator neighborIt = neighborsList->begin();

    NeighborSetType *neighborSet = new NeighborSetType();
    while ( neighborIt != neighborsList->end() )
      {
      neighborSet->insert(*neighborIt++);
      }
    // garbage collection (from itkSimplexMesh)
    delete neighborsList;
    data->neighborSet =  neighborSet;

    pointItr++;
    }

}

/* Set the gradient image as an input */
template< typename TInputMesh, typename TOutputMesh >
void
DeformableSimplexMesh3DFilter< TInputMesh, TOutputMesh >
::SetGradient( const GradientImageType * gradientImage )
{
  this->SetNthInput( 1, const_cast< GradientImageType * >( gradientImage ) );
}

/* Get the gradient image as an input */
template< typename TInputMesh, typename TOutputMesh >
const typename
DeformableSimplexMesh3DFilter< TInputMesh, TOutputMesh >::GradientImageType *
DeformableSimplexMesh3DFilter< TInputMesh, TOutputMesh >
::GetGradient() const
{
  const GradientImageType *gradientImage =
    dynamic_cast< const GradientImageType * >( this->ProcessObject::GetInput(1) );

  return gradientImage;
}

/* Compute normals. */
template< typename TInputMesh, typename TOutputMesh >
void
DeformableSimplexMesh3DFilter< TInputMesh, TOutputMesh >
::ComputeGeometry()
{
  PointType           Foot;
  CovariantVectorType normal;
  CovariantVectorType z;
  VectorType          tmp;
  //   IdentifierType idx = 0;

  const InputMeshType *       inputMesh = this->GetInput(0);
  const InputPointsContainer *points  = inputMesh->GetPoints();

  typename GeometryMapType::Iterator dataIt = this->m_Data->Begin();

  SimplexMeshGeometry *data;

  while ( dataIt != this->m_Data->End() )
    {
    //      idx = dataIt.Index();
    data = dataIt.Value();

    data->neighbors[0] = points->GetElement(data->neighborIndices[0]);
    data->neighbors[1] = points->GetElement(data->neighborIndices[1]);
    data->neighbors[2] = points->GetElement(data->neighborIndices[2]);

    // compute normal
    normal.Fill(0.0);

    z.SetVnlVector( itk_cross_3d( ( data->neighbors[1] - data->neighbors[0] ).GetVnlVector(),
                                    ( data->neighbors[2] - data->neighbors[0] ).GetVnlVector() ) );
    z.Normalize();
    normal += z;

    // copy normal
    data->normal = normal;

    // compute the simplex angle
    data->ComputeGeometry();

    tmp = data->neighbors[0] - data->pos;

    double D = 1.0 / ( 2 * data->sphereRadius ); /* */

    double tmpNormalProd = dot_product( tmp.GetVnlVector(), data->normal.GetVnlVector() );

    double sinphi =  2 *data->circleRadius *D *itk::Math::sgn(tmpNormalProd);
    double phi = std::asin(sinphi);

    data->phi = phi;
    data->meanCurvature = std::abs(sinphi / data->circleRadius);
    tmp = data->pos - data->neighbors[0];

    //compute the foot of p projection of p onto the triangle spanned by its
    // neighbors
    double distance = -tmpNormalProd;
    tmp.SetVnlVector( ( data->pos ).GetVnlVector() - distance * normal.GetVnlVector() );
    Foot.Fill(0.0);
    Foot += tmp;

    data->distance = ( ( data->circleCenter ) - Foot ).GetNorm();

    data->eps = ComputeBarycentricCoordinates(Foot, data);
    dataIt.Value() = data;
    dataIt++;
    }
}

template< typename TInputMesh, typename TOutputMesh >
void
DeformableSimplexMesh3DFilter< TInputMesh, TOutputMesh >
::ComputeDisplacement()
{
  const InputMeshType *inputMesh = this->GetInput(0);

  const GradientImageType *gradientImage = this->GetGradient();

  // Filters should not modify their input...
  // There is a design flaw here.
  InputPointsContainer *nonConstPoints =
    const_cast< InputPointsContainer * >( inputMesh->GetPoints() );

  typename GeometryMapType::Iterator dataIt = this->m_Data->Begin();
  SimplexMeshGeometry *data;
  VectorType           displacement;

  while ( dataIt != this->m_Data->End() )
    {
    data = dataIt.Value();

    this->ComputeInternalForce(data);

    this->ComputeExternalForce(data,gradientImage);

    displacement.SetVnlVector( m_Alpha * ( data->internalForce ).GetVnlVector()
                                 + ( data->externalForce ).GetVnlVector() );

    data->pos += displacement;
    nonConstPoints->InsertElement(dataIt.Index(), data->pos);

    dataIt++;
    }
}

/* */
template< typename TInputMesh, typename TOutputMesh >
void
DeformableSimplexMesh3DFilter< TInputMesh, TOutputMesh >
::ComputeInternalForce(SimplexMeshGeometry *data)
{
  const double r = data->circleRadius;
  const double d = data->distance;
  const double phi = data->phi;

  const NeighborSetType *const neighborSet = data->neighborSet;

  NeighborSetType::const_iterator neighborIt = neighborSet->begin();

  double phiRef = 0.0;
  while ( neighborIt != neighborSet->end() )
    {
    phiRef += this->m_Data->GetElement(*neighborIt++)->phi;
    }
  phiRef /= static_cast<double>(neighborSet->size());

  double L=0.0;
  double L_Ref=0.0;
  if ( L_Func(r,d,phi,L) && L_Func(r,d,phi,L_Ref) )
    {
    const PointType eps = data->eps;
    const PointType epsRef = data->referenceMetrics;

    const double eps1Diff = epsRef[0] - eps[0];
    const double eps2Diff = epsRef[1] - eps[1];
    const double eps3Diff = epsRef[2] - eps[2];
    //    diffAbsSum = std::abs(eps1Diff)+std::abs(eps2Diff)+std::abs(eps3Diff);

    VectorType tangentForce;
    tangentForce.SetVnlVector(eps1Diff * ( data->neighbors[0] ).GetVnlVector()
      + eps2Diff * ( data->neighbors[1] ).GetVnlVector()
      + eps3Diff * ( data->neighbors[2] ).GetVnlVector()
    );


    VectorType normalForce;
    normalForce.SetVnlVector( -1.0 * ( L_Ref - L ) * ( data->normal ).GetVnlVector() );

    data->internalForce.Fill(0.0);

    data->internalForce += tangentForce + normalForce;
    }
}

/** Compute model Displacement according to image gradient forces */
template< typename TInputMesh, typename TOutputMesh >
void
DeformableSimplexMesh3DFilter< TInputMesh, TOutputMesh >
::ComputeExternalForce(SimplexMeshGeometry *data,const GradientImageType *gradientImage)
{
  PointType         vec_for, tmp_vec_1, tmp_vec_2, tmp_vec_3;
  GradientIndexType coord, coord2, tmp_co_1, tmp_co_2, tmp_co_3;

  coord[0] = static_cast< GradientIndexValueType >( data->pos[0] );
  coord[1] = static_cast< GradientIndexValueType >( data->pos[1] );
  coord[2] = static_cast< GradientIndexValueType >( data->pos[2] );

  coord2[0] = static_cast< GradientIndexValueType >( std::ceil(data->pos[0]) );
  coord2[1] = static_cast< GradientIndexValueType >( std::ceil(data->pos[1]) );
  coord2[2] = static_cast< GradientIndexValueType >( std::ceil(data->pos[2]) );

  tmp_co_1[0] = coord2[0];
  tmp_co_1[1] = coord[1];
  tmp_co_1[2] = coord[2];

  tmp_co_2[0] = coord[0];
  tmp_co_2[1] = coord2[1];
  tmp_co_2[2] = coord[2];

  tmp_co_3[0] = coord[0];
  tmp_co_3[1] = coord[1];
  tmp_co_3[2] = coord2[2];

  if ( ( coord[0] >= 0 ) && ( coord[1] >= 0 ) && ( coord[2] >= 0 )
       && ( coord2[0] < m_ImageWidth ) && ( coord2[1] < m_ImageHeight ) && ( coord2[2] < m_ImageDepth ) )
    {
    const GradientType & gradient0 = gradientImage->GetPixel(coord);
    const GradientType & gradient1 = gradientImage->GetPixel(tmp_co_1);
    const GradientType & gradient2 = gradientImage->GetPixel(tmp_co_2);
    const GradientType & gradient3 = gradientImage->GetPixel(tmp_co_3);

    vec_for[0] = gradient0[0];
    vec_for[1] = gradient0[1];
    vec_for[2] = gradient0[2];

    tmp_vec_1[0] = gradient1[0] - gradient0[0];
    tmp_vec_1[1] = gradient1[1] - gradient0[1];
    tmp_vec_1[2] = gradient1[2] - gradient0[2];
    tmp_vec_2[0] = gradient2[0] - gradient0[0];
    tmp_vec_2[1] = gradient2[1] - gradient0[1];
    tmp_vec_2[2] = gradient2[2] - gradient0[2];
    tmp_vec_3[0] = gradient3[0] - gradient0[0];
    tmp_vec_3[1] = gradient3[1] - gradient0[1];
    tmp_vec_3[2] = gradient3[2] - gradient0[2];

    vec_for[0] = vec_for[0] + ( ( data->pos )[0] - coord[0] ) * tmp_vec_1[0]
                 + ( ( data->pos )[1] - coord[1] ) * tmp_vec_2[0] + ( ( data->pos )[2] - coord[2] ) * tmp_vec_3[0];
    vec_for[1] = vec_for[1] + ( ( data->pos )[1] - coord[1] ) * tmp_vec_2[1]
                 + ( ( data->pos )[0] - coord[0] ) * tmp_vec_1[1] + ( ( data->pos )[2] - coord[2] ) * tmp_vec_3[1];
    vec_for[2] = vec_for[2] + ( ( data->pos )[2] - coord[2] ) * tmp_vec_3[2]
                 + ( ( data->pos )[1] - coord[1] ) * tmp_vec_2[2] + ( ( data->pos )[0] - coord[0] ) * tmp_vec_1[2];
    }
  else
    {
    vec_for.Fill(0);
    }

  double mag = dot_product( data->normal.GetVnlVector(), vec_for.GetVnlVector() );
  //      double mag = vec_for[0]*(data->normal)[0] +
  // vec_for[1]*(data->normal)[1]+ vec_for[2]*(data->normal)[2];

  vec_for[0] = mag * ( data->normal )[0]; /*num_for*/
  vec_for[1] = mag * ( data->normal )[1]; /*num_for*/
  vec_for[2] = mag * ( data->normal )[2]; /*num_for*/

  mag = vec_for.GetVectorFromOrigin().GetNorm();

  if ( mag > 0.5 )
    {
    for ( int i = 0; i < 3; i++ )
      {
      vec_for[i] = ( 0.5 * vec_for[i] ) / mag;
      }
    }

  data->externalForce[0] = m_Beta * vec_for[0];
  data->externalForce[1] = m_Beta * vec_for[1];
  data->externalForce[2] = m_Beta * vec_for[2];
}

/* Copy the content of m_Location into the Output. */
template< typename TInputMesh, typename TOutputMesh >
void
DeformableSimplexMesh3DFilter< TInputMesh, TOutputMesh >
::ComputeOutput()
{
  OutputMeshType *output = this->GetOutput();

  this->CopyInputMeshToOutputMeshPoints();
  this->CopyInputMeshToOutputMeshPointData();
  this->CopyInputMeshToOutputMeshCells();
  this->CopyInputMeshToOutputMeshCellData();

  output->SetGeometryData(this->m_Data);
  output->SetLastCellId( this->GetInput(0)->GetLastCellId() );
}

/*  */
template< typename TInputMesh, typename TOutputMesh >
void
DeformableSimplexMesh3DFilter< TInputMesh, TOutputMesh >
::UpdateReferenceMetrics()
{
  const InputMeshType *inputMesh = this->GetInput(0);

  // Filters should not change their input.
  // There is a design flaw here.
  InputMeshType *nonConstInputMesh = const_cast< InputMeshType * >( inputMesh );

  double H;
  double H_N1;
  double H_N2;
  double H_N3;
  double H_Mean;

  GeometryMapIterator dataIt = this->m_Data->Begin();

  SimplexMeshGeometry *data;

  while ( dataIt != this->m_Data->End() )
    {
    data = dataIt->Value();
    H_N1 = ( (SimplexMeshGeometry *)( this->m_Data->GetElement(data->neighborIndices[0]) ) )->meanCurvature;
    H_N2 = ( (SimplexMeshGeometry *)( this->m_Data->GetElement(data->neighborIndices[1]) ) )->meanCurvature;
    H_N3 = ( (SimplexMeshGeometry *)( this->m_Data->GetElement(data->neighborIndices[2]) ) )->meanCurvature;
    H = data->meanCurvature;

    H_Mean = ( H_N1 + H_N2 + H_N3 ) / 3.0;

    PointType deltaH;

    deltaH[0] = ( H_N1 - H_Mean ) / H_Mean;
    deltaH[1] = ( H_N2 - H_Mean ) / H_Mean;
    deltaH[2] = ( H_N3 - H_Mean ) / H_Mean;

    //deltaH[0] = (H_N1 - H_Mean)/H;
    //deltaH[1] = (H_N2 - H_Mean)/H;
    //deltaH[2] = (H_N3 - H_Mean)/H;

    PointType eps, eps_opt;
    // compute optimal reference metrics
    eps_opt[0] = ( 1.0 / 3.0 ) + m_Gamma * deltaH[0];
    eps_opt[1] = ( 1.0 / 3.0 ) + m_Gamma * deltaH[1];
    eps_opt[2] = ( 1.0 / 3.0 ) + m_Gamma * deltaH[2];

    eps = data->referenceMetrics;

    eps[0] = eps[0] + 0.5 * ( eps_opt[0] - eps[0] );
    eps[1] = eps[1] + 0.5 * ( eps_opt[1] - eps[1] );
    eps[2] = eps[2] + 0.5 * ( eps_opt[2] - eps[2] );

    // set current reference metrics
    data->referenceMetrics = eps;
    nonConstInputMesh->SetPointData(dataIt->Index(), H);
    dataIt.Value() = data;
    //      m_Data->InsertElement(dataIt->Index(),data);
    dataIt++;
    }
}

template< typename TInputMesh, typename TOutputMesh >
bool DeformableSimplexMesh3DFilter< TInputMesh, TOutputMesh >
::L_Func(const double r, const double d, const double phi, double & output)
{
  const double r2 = r * r;
  const double d2 = d * d;
  const double r2Minusd2 = r2 - d2;
  const double tanPhi = std::tan(phi);

  double eps = 1.0;

  if ( phi * itk::Math::sgn(phi) > itk::Math::pi_over_2 )
    {
    eps = -1.0;
    }
  double tmpSqr = r2 + r2Minusd2 * tanPhi * tanPhi;
  if ( tmpSqr > 0 )
    {
    const double denom = eps * ( std::sqrt(tmpSqr) + r );
    if ( Math::NotAlmostEquals( denom, 0.0 ) )
      {
      output = ( r2Minusd2 * tanPhi ) / denom;
      return true;
      }
    }
  return false;
}

template< typename TInputMesh, typename TOutputMesh >
typename DeformableSimplexMesh3DFilter< TInputMesh, TOutputMesh >::PointType
DeformableSimplexMesh3DFilter< TInputMesh, TOutputMesh >
::ComputeBarycentricCoordinates(PointType p, SimplexMeshGeometry *data)
{
  const PointType a = data->neighbors[0];
  const PointType b = data->neighbors[1];
  const PointType c = data->neighbors[2];

  VectorType n, na, nb, nc;
  n.SetVnlVector( itk_cross_3d( ( b - a ).GetVnlVector(), ( c - a ).GetVnlVector() ) );
  na.SetVnlVector( itk_cross_3d( ( c - b ).GetVnlVector(), ( p - b ).GetVnlVector() ) );
  nb.SetVnlVector( itk_cross_3d( ( a - c ).GetVnlVector(), ( p - c ).GetVnlVector() ) );
  nc.SetVnlVector( itk_cross_3d( ( b - a ).GetVnlVector(), ( p - a ).GetVnlVector() ) );

  PointType eps;
  eps[0] = dot_product( n.GetVnlVector(), na.GetVnlVector() ) / n.GetSquaredNorm();
  eps[1] = dot_product( n.GetVnlVector(), nb.GetVnlVector() ) / n.GetSquaredNorm();
  eps[2] = dot_product( n.GetVnlVector(), nc.GetVnlVector() ) / n.GetSquaredNorm();

  return eps;
}
} /* end namespace itk. */

#endif // itkDeformableSimplexMesh3DFilter_hxx
