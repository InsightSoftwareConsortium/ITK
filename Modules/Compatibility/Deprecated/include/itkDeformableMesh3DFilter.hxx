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
#ifndef itkDeformableMesh3DFilter_hxx
#define itkDeformableMesh3DFilter_hxx
#if !defined( ITK_LEGACY_REMOVE )

#include "itkDeformableMesh3DFilter.h"
#include "itkNumericTraits.h"

namespace itk
{
/* Constructor. */
template< typename TInputMesh, typename TOutputMesh >
DeformableMesh3DFilter< TInputMesh, TOutputMesh >
::DeformableMesh3DFilter()
{
  m_Step = 0;
  m_StepThreshold = 0;
  m_PotentialOn = 0;
  m_K = ITK_NULLPTR;
  m_ObjectLabel = 0;
  m_Scale.Fill(1.0);
  m_Stiffness.Fill(0.1);
  m_TimeStep = 0.01;
  m_PotentialMagnitude = NumericTraits< PixelType >::OneValue();
  m_GradientMagnitude = NumericTraits< PixelType >::OneValue();

  m_ImageDepth = 0;
  m_ImageHeight = 0;
  m_ImageWidth = 0;

  typename TOutputMesh::Pointer output = TOutputMesh::New();
  this->ProcessObject::SetNumberOfRequiredOutputs(1);
  this->ProcessObject::SetNthOutput( 0, output.GetPointer() );
}

template< typename TInputMesh, typename TOutputMesh >
DeformableMesh3DFilter< TInputMesh, TOutputMesh >
::~DeformableMesh3DFilter()
{
  delete[] m_K;
  m_K = ITK_NULLPTR;
}

/* PrintSelf. */
template< typename TInputMesh, typename TOutputMesh >
void
DeformableMesh3DFilter< TInputMesh, TOutputMesh >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "Stiffness = " << m_Stiffness;
  os << indent << "PotentialMagnitude = "
     << static_cast< typename NumericTraits< PixelType >::PrintType >( m_PotentialMagnitude )
     << std::endl;
  os << indent << "GradientMagnitude = "
     << static_cast< typename NumericTraits< PixelType >::PrintType >( m_GradientMagnitude )
     << std::endl;
  os << indent << "Scale = " << m_Scale;
  os << indent << "TimeStep = " << m_TimeStep << std::endl;
  os << indent << "PotentialOn = " << m_PotentialOn << std::endl;
  os << indent << "ObjectLabel = "
     << static_cast< typename NumericTraits< unsigned char >::PrintType >( m_ObjectLabel )
     << std::endl;
  os << indent << "StepThreshold = " << m_StepThreshold << std::endl;
  if ( m_Normals )
    {
    os << indent << "Normals = " << m_Normals << std::endl;
    }
  else
    {
    os << indent << "Normals = " << "(None)" << std::endl;
    }
  if ( m_Gradient )
    {
    os << indent << "Gradient = " << m_Gradient << std::endl;
    }
  else
    {
    os << indent << "Gradient = " << "(None)" << std::endl;
    }
  os << indent << "Step = " << m_Step << std::endl;
  os << indent << "ImageDepth = " << m_ImageDepth << std::endl;
  os << indent << "ImageHeight = " << m_ImageHeight << std::endl;
  os << indent << "ImageWidth = " << m_ImageWidth << std::endl;
} /* End PrintSelf. */

/* Set default value of parameters and initialize local data container
 *  such as forces, displacements and displacement derivatives. */
template< typename TInputMesh, typename TOutputMesh >
void
DeformableMesh3DFilter< TInputMesh, TOutputMesh >
::Initialize()
{
  m_NumberOfNodes = this->GetInput(0)->GetNumberOfPoints();
  m_NumberOfCells = this->GetInput(0)->GetNumberOfCells();

  m_Forces          = InputMeshType::New();
  m_Displacements   = InputMeshType::New();
  m_Derives         = InputMeshType::New();
  m_Normals         = InputMeshType::New();
  m_Locations       = InputMeshType::New();

  InputMeshConstPointer             inputMesh = this->GetInput(0);
  InputPointsContainerConstPointer  myPoints = inputMesh->GetPoints();
  InputPointsContainerConstIterator points = myPoints->Begin();

  InputPointsContainerPointer myForces = m_Forces->GetPoints();
  myForces->Reserve(m_NumberOfNodes);
  InputPointsContainerIterator forces = myForces->Begin();

  InputPointsContainerPointer myDerives = m_Derives->GetPoints();
  myDerives->Reserve(m_NumberOfNodes);
  InputPointsContainerIterator derives = myDerives->Begin();

  InputPointsContainerPointer myDisplacements = m_Displacements->GetPoints();
  myDisplacements->Reserve(m_NumberOfNodes);
  InputPointsContainerIterator displacements = myDisplacements->Begin();

  InputPointsContainerPointer myNormals = m_Normals->GetPoints();
  myNormals->Reserve(m_NumberOfNodes);
  InputPointsContainerIterator normals = myNormals->Begin();

  InputPointsContainerPointer myLocations = m_Locations->GetPoints();
  myLocations->Reserve(m_NumberOfNodes);
  InputPointsContainerIterator locations = myLocations->Begin();

  InputCellsContainerConstPointer  myCells = inputMesh->GetCells();
  InputCellsContainerConstIterator cells = myCells->Begin();

  ImageSizeType ImageSize = m_Gradient->GetBufferedRegion().GetSize();

  //---------------------------------------------------------------------
  //Get the image width/height and depth
  //---------------------------------------------------------------------
  m_ImageWidth  = ImageSize[0];
  m_ImageHeight = ImageSize[1];
  m_ImageDepth  = ImageSize[2];

  m_ModelXUpLimit = 0;
  m_ModelXDownLimit = m_ImageWidth;
  m_ModelYUpLimit = 0;
  m_ModelYDownLimit = m_ImageHeight;
  m_ModelZUpLimit = 0;
  m_ModelZDownLimit = m_ImageDepth;

  InputPointType d; d.Fill(0.0);

  int j = 0;
  while ( points != myPoints->End() )
    {
    for ( int i = 0; i < 3; i++ )
      {
      locations.Value()[i] = m_Scale[i] * points.Value()[i];
      }
    ++points;
    ++locations;
    forces.Value() = d;
    ++forces;
    normals.Value() = d;
    ++normals;
    derives.Value() = d;
    ++derives;
    displacements.Value() = d;
    ++displacements;
    m_Forces->SetPointData(j, 0.0);
    j++;
    }

  const typename InputMeshType::PointIdentifier *tp;
  PixelType            x = 0.0;
  PixelType *          x_pt;
  x_pt = &x;
  while ( cells != myCells->End() )
    {
    typename InputMeshType::CellType * cellPtr = cells.Value();
    tp = cellPtr->GetPointIds();
    for ( int i = 0; i < 3; i++ )
      {
      m_Forces->GetPointData( (int)( tp[i] ), x_pt );
      x = x + 1.0;
      m_Forces->SetPointData( (int)( tp[i] ), x );
      }
    ++cells;
    }

  this->SetDefaultStiffnessMatrix();

  // This prevents unnecessary re-executions of the pipeline.
  OutputMeshPointer outputMesh = this->GetOutput();
  outputMesh->SetBufferedRegion( outputMesh->GetRequestedRegion() );
}

/* Set the stiffness matrix. */
template< typename TInputMesh, typename TOutputMesh >
void
DeformableMesh3DFilter< TInputMesh, TOutputMesh >
::SetDefaultStiffnessMatrix()
{
  double us = 0.5;
  double vs = 0.5;
  double a = us * us, b = vs * vs;
  double area = us * vs / 2, k00, k01, k02, k11, k12, k22;

  k00 = area * ( m_Stiffness[1] / a + m_Stiffness[1] / b + m_Stiffness[0] );
  k01 = area * ( -m_Stiffness[1] / a + m_Stiffness[0] * 0.5 );
  k11 = area * ( m_Stiffness[1] / a + m_Stiffness[0] );
  k02 = area * ( -m_Stiffness[1] / b + m_Stiffness[0] * 0.5 );
  k12 = area * m_Stiffness[0] * 0.5;
  k22 = area * ( m_Stiffness[1] / b + m_Stiffness[0] );

  m_StiffnessMatrix[0][0][0] = k00;
  m_StiffnessMatrix[0][0][1] = k01;
  m_StiffnessMatrix[0][0][2] = k02;
  m_StiffnessMatrix[0][0][3] = 0.0;
  m_StiffnessMatrix[0][1][0] = k01;
  m_StiffnessMatrix[0][1][1] = k11;
  m_StiffnessMatrix[0][1][2] = k12;
  m_StiffnessMatrix[0][1][3] = 0.0;
  m_StiffnessMatrix[0][2][0] = k02;
  m_StiffnessMatrix[0][2][1] = k12;
  m_StiffnessMatrix[0][2][2] = k22;
  m_StiffnessMatrix[0][2][3] = 0.0;
  m_StiffnessMatrix[0][3][0] = 0.0;
  m_StiffnessMatrix[0][3][1] = 0.0;
  m_StiffnessMatrix[0][3][2] = 0.0;
  m_StiffnessMatrix[0][3][3] = 1.0;
}

/** Set the stiffness matrix. */
template< typename TInputMesh, typename TOutputMesh >
void
DeformableMesh3DFilter< TInputMesh, TOutputMesh >
::SetStiffnessMatrix(StiffnessMatrixType *stiff, int i)
{
  m_StiffnessMatrix[i][0][0] = stiff[0][0];
  m_StiffnessMatrix[i][0][1] = stiff[0][1];
  m_StiffnessMatrix[i][0][2] = stiff[0][2];
  m_StiffnessMatrix[i][0][3] = stiff[0][3];
  m_StiffnessMatrix[i][1][0] = stiff[1][0];
  m_StiffnessMatrix[i][1][1] = stiff[1][1];
  m_StiffnessMatrix[i][1][2] = stiff[1][2];
  m_StiffnessMatrix[i][1][3] = stiff[1][3];
  m_StiffnessMatrix[i][2][0] = stiff[2][0];
  m_StiffnessMatrix[i][2][1] = stiff[2][1];
  m_StiffnessMatrix[i][2][2] = stiff[2][2];
  m_StiffnessMatrix[i][2][3] = stiff[2][3];
  m_StiffnessMatrix[i][3][0] = stiff[3][0];
  m_StiffnessMatrix[i][3][1] = stiff[3][1];
  m_StiffnessMatrix[i][3][2] = stiff[3][2];
  m_StiffnessMatrix[i][3][3] = stiff[3][3];
}

/** Set the stiffness matrix. */
template< typename TInputMesh, typename TOutputMesh >
void
DeformableMesh3DFilter< TInputMesh, TOutputMesh >
::SetMeshStiffness()
{
  InputMeshConstPointer               inputMesh = this->GetInput(0);
  InputCellDataContainerConstPointer  myCellData = inputMesh->GetCellData();
  InputCellDataContainerConstIterator celldata = myCellData->Begin();

  m_K = new StiffnessMatrixRawPointer[m_NumberOfCells];

  unsigned int j = 0;
  while ( celldata != myCellData->End() )
    {
    const double x = celldata.Value();
    m_K[j] = m_StiffnessMatrix + ( (int)x );
    ++celldata;
    j++;
    }
}

/* Compute the derivatives using d' + Kd = f. */
template< typename TInputMesh, typename TOutputMesh >
void
DeformableMesh3DFilter< TInputMesh, TOutputMesh >
::ComputeDt()
{
  const typename InputMeshType::PointIdentifier *tp;

  InputMeshConstPointer            inputMesh = this->GetInput(0);
  InputCellsContainerConstPointer  myCells = inputMesh->GetCells();
  InputCellsContainerConstIterator cells = myCells->Begin();

  InputPointsContainerPointer  myForces = m_Forces->GetPoints();
  InputPointsContainerIterator forces = myForces->Begin();

  InputPointsContainerPointer  myDerives = m_Derives->GetPoints();
  InputPointsContainerIterator derives = myDerives->Begin();

  double p = 1.0;

  int i = 0;
  InputPointType v1, v2, v3;
  v1.Fill(0.);
  v2.Fill(0.);
  v3.Fill(0.);

  while ( cells != myCells->End() )
    {
    tp = cells.Value()->GetPointIds();
    ++cells;
    m_Displacements->GetPoint (tp[0], &v1);
    m_Displacements->GetPoint (tp[1], &v2);
    m_Displacements->GetPoint (tp[2], &v3);
    v1[0] *= m_K[i]->get(0, 0) * p;
    v1[1] *= m_K[i]->get(0, 0) * p;
    v1[2] *= m_K[i]->get(0, 0) * p;
    v2[0] *= m_K[i]->get(0, 1) * p;
    v2[1] *= m_K[i]->get(0, 1) * p;
    v2[2] *= m_K[i]->get(0, 1) * p;
    v3[0] *= m_K[i]->get(0, 2) * p;
    v3[1] *= m_K[i]->get(0, 2) * p;
    v3[2] *= m_K[i]->get(0, 2) * p;
    v1[0] += v2[0] + v3[0];
    v1[1] += v2[1] + v3[1];
    v1[2] += v2[2] + v3[2];
    m_Forces->GetPoint (tp[0], &v2);

    v2[0] -= v1[0];
    v2[1] -= v1[1];
    v2[2] -= v1[2];

    m_Forces->SetPoint (tp[0], v2);

    m_Displacements->GetPoint (tp[0], &v1);
    m_Displacements->GetPoint (tp[1], &v2);
    m_Displacements->GetPoint (tp[2], &v3);
    v1[0] *= m_K[i]->get(1, 0) * p;
    v1[1] *= m_K[i]->get(1, 0) * p;
    v1[2] *= m_K[i]->get(1, 0) * p;
    v2[0] *= m_K[i]->get(1, 1) * p;
    v2[1] *= m_K[i]->get(1, 1) * p;
    v2[2] *= m_K[i]->get(1, 1) * p;
    v3[0] *= m_K[i]->get(1, 2) * p;
    v3[1] *= m_K[i]->get(1, 2) * p;
    v3[2] *= m_K[i]->get(1, 2) * p;
    v1[0] += v2[0] + v3[0];
    v1[1] += v2[1] + v3[1];
    v1[2] += v2[2] + v3[2];
    m_Forces->GetPoint (tp[1], &v2);

    v2[0] -= v1[0];
    v2[1] -= v1[1];
    v2[2] -= v1[2];

    m_Forces->SetPoint (tp[1], v2);

    m_Displacements->GetPoint (tp[0], &v1);
    m_Displacements->GetPoint (tp[1], &v2);
    m_Displacements->GetPoint (tp[2], &v3);
    v1[0] *= m_K[i]->get(2, 0) * p;
    v1[1] *= m_K[i]->get(2, 0) * p;
    v1[2] *= m_K[i]->get(2, 0) * p;
    v2[0] *= m_K[i]->get(2, 1) * p;
    v2[1] *= m_K[i]->get(2, 1) * p;
    v2[2] *= m_K[i]->get(2, 1) * p;
    v3[0] *= m_K[i]->get(2, 2) * p;
    v3[1] *= m_K[i]->get(2, 2) * p;
    v3[2] *= m_K[i]->get(2, 2) * p;
    v1[0] += v2[0] + v3[0];
    v1[1] += v2[1] + v3[1];
    v1[2] += v2[2] + v3[2];
    m_Forces->GetPoint (tp[2], &v2);

    v2[0] -= v1[0];
    v2[1] -= v1[1];
    v2[2] -= v1[2];

    m_Forces->SetPoint (tp[2], v2);
    ++i;
    }

  while ( derives != myDerives->End() )
    {
    derives.Value() = forces.Value();
    ++derives;
    ++forces;
    }
}

/** Update the displacements using d_{new} = d_{old} + timestep*d'. */
template< typename TInputMesh, typename TOutputMesh >
void
DeformableMesh3DFilter< TInputMesh, TOutputMesh >
::Advance()
{
  typename TInputMesh::PointType s, d, ds;

  InputPointsContainerPointer  myDerives = m_Derives->GetPoints();
  InputPointsContainerIterator derives = myDerives->Begin();
  InputPointsContainerPointer  myDisplacements = m_Displacements->GetPoints();
  InputPointsContainerIterator displacements = myDisplacements->Begin();
  InputPointsContainerPointer  myPoints = m_Locations->GetPoints();
  InputPointsContainerIterator points = myPoints->Begin();

  while ( derives != myDerives->End() )
    {
    ds = derives.Value();
    s = points.Value();
    d = displacements.Value();
    s[0] += m_TimeStep * ds[0];
    s[1] += m_TimeStep * ds[1];
    s[2] += m_TimeStep * ds[2];
    d[0] += m_TimeStep * ds[0];
    d[1] += m_TimeStep * ds[1];
    d[2] += m_TimeStep * ds[2];

    /** do not update the displacements if the nodes is moving out of the image
      region. */
    if ( ( s[0] > 0 ) && ( s[1] > 0 ) && ( s[2] > 0 )
         && ( s[2] < m_ImageDepth ) && ( s[0] < m_ImageWidth ) && ( s[1] < m_ImageHeight ) )
      {
      points.Value() = s;
      displacements.Value() = d;
      }

    ++derives;
    ++points;
    ++displacements;
    }
}

/* Copy the content of m_Location into the Output. */
template< typename TInputMesh, typename TOutputMesh >
void
DeformableMesh3DFilter< TInputMesh, TOutputMesh >
::ComputeOutput()
{
  int i;

  typename TriCell::CellAutoPointer insertCell;
  typename OutputMeshType::PointIdentifier      tripoints[3];
  const typename InputMeshType::PointIdentifier *tp;
  double               x;

  OutputMeshType *output = this->GetOutput();

  typedef typename OutputMeshType::CellsContainer CellsContainer;

  output->SetCells( CellsContainer::New() );

  output->GetCells()->Reserve(m_NumberOfCells);

  output->SetCellsAllocationMethod(OutputMeshType::CellsAllocatedDynamicallyCellByCell);

  OutputPointsContainerPointer myPoints = output->GetPoints();
  myPoints->Reserve(m_NumberOfNodes);
  OutputPointsContainerIterator points = myPoints->Begin();

  InputPointsContainerPointer  myLocations = m_Locations->GetPoints();
  InputPointsContainerIterator locations = myLocations->Begin();

  InputMeshConstPointer            inputMesh = this->GetInput(0);
  InputCellsContainerConstPointer  myCells = inputMesh->GetCells();
  InputCellsContainerConstIterator cells = myCells->Begin();

  InputCellDataContainerConstPointer  myCellData = inputMesh->GetCellData();
  InputCellDataContainerConstIterator celldata = myCellData->Begin();

  i = 0;
  for (; i < m_NumberOfNodes; i++ )
    {
    points.Value() = locations.Value();
    ++locations;
    ++points;
    }

  for ( i = 0; i < m_NumberOfCells; i++ )
    {
    tp = cells.Value()->GetPointIds();
    tripoints[0] = tp[0];
    tripoints[1] = tp[1];
    tripoints[2] = tp[2];
    insertCell.TakeOwnership(new TriCell);
    insertCell->SetPointIds(tripoints);
    output->SetCell(i, insertCell);
    x = celldata.Value();
    output->SetCellData(i, (PixelType)x);
    ++cells;
    ++celldata;
    }
}

/* Generate Data */
template< typename TInputMesh, typename TOutputMesh >
void
DeformableMesh3DFilter< TInputMesh, TOutputMesh >
::GenerateData()
{
  this->Initialize();
  this->SetMeshStiffness();

  while ( m_Step < m_StepThreshold )
    {
    const float progress =
      static_cast< float >( m_Step )
      / static_cast< float >( m_StepThreshold );

    this->UpdateProgress(progress);
    this->ComputeNormals();
    this->GradientFit();

    if ( m_PotentialOn )
      {
      this->PotentialFit();
      }

    this->ComputeDt();
    this->Advance();
    m_Step++;
    }

  this->ComputeOutput();
}

/** compute the force given out by the binary mask. */
template< typename TInputMesh, typename TOutputMesh >
void
DeformableMesh3DFilter< TInputMesh, TOutputMesh >
::PotentialFit()
{
  PixelType max, extends[3], t, xs, ys, zs;

  typename TInputMesh::PointType vec_for, vec_nor, vec_p, vec_1, vec_2;
  int            i, label;
  ImageIndexType coord = { { 0, 0, 0 } };
  ImageIndexType extend = { { 0, 0, 0 } };
  int            flag = 0;

  InputPointsContainerPointer  Points = m_Locations->GetPoints();
  InputPointsContainerIterator points = Points->Begin();

  InputPointsContainerPointer  myForces = m_Forces->GetPoints();
  InputPointsContainerIterator forces = myForces->Begin();

  InputPointsContainerPointer  myNormals = m_Normals->GetPoints();
  InputPointsContainerIterator normals = myNormals->Begin();

  i = 0;

  while ( i < m_NumberOfNodes )
    {
    xs = ys = zs = 1.0;
    vec_p = points.Value();

    coord[0] = (int)vec_p[0];
    coord[1] = (int)vec_p[1];
    coord[2] = (int)vec_p[2];

    if ( m_Potential->GetPixel(coord) != m_ObjectLabel )
      {
      xs = ys = zs = -1.0;
      flag = 1;
      }
    extends[0] = vec_p[0];
    extends[1] = vec_p[1];
    extends[2] = vec_p[2];
    extend[0] = (int)vec_p[0];
    extend[1] = (int)vec_p[1];
    extend[2] = (int)vec_p[2];

    vec_nor = normals.Value();

    max = std::abs(vec_nor[0]);

    //---------------------------------------------------------------------
    // all the movement in z direction is now disabled for further test
    //---------------------------------------------------------------------
    if ( std::abs(vec_nor[1]) > max ) { max = std::abs(vec_nor[1]); }
    if ( std::abs(vec_nor[2]) > max ) { max = std::abs(vec_nor[2]); }
    if ( flag )
      {
      vec_1[0] = -1 * vec_nor[0] / max;
      vec_1[1] = -1 * vec_nor[1] / max;
      vec_1[2] = -1 * vec_nor[2] / max;
      }
    else
      {
      vec_1[0] = vec_nor[0] / max;
      vec_1[1] = vec_nor[1] / max;
      vec_1[2] = vec_nor[2] / max;
      }

    t = 0.0;

    while ( t < 5.0 )
      {
      extends[0] += vec_1[0];
      extends[1] += vec_1[1];
      extends[2] += vec_1[2];
      extend[0] = (int)( extends[0] + 1 );
      extend[1] = (int)( extends[1] + 1 );
      extend[2] = (int)( extends[2] + 1 );
      if ( ( extend[0] <= 0 ) || ( extend[1] <= 0 ) || ( extend[2] <= 0 ) ) { break; }

      extend[0] = (int)( extends[0] );
      extend[1] = (int)( extends[1] );
      extend[2] = (int)( extends[2] );
      if ( ( extend[0] >= m_ImageWidth ) || ( extend[1] >= m_ImageHeight )
           || ( extend[2] >= m_ImageDepth ) ) { break; }

      label = m_Potential->GetPixel(extend);
      if ( !flag )
        {
        if ( label != m_ObjectLabel ) { break; }
        }
      else if ( label == m_ObjectLabel )
        {
        break;
        }

      t += 1.0;
      }

    vec_2[0] = t * m_PotentialMagnitude * vec_nor[0] * xs;
    vec_2[1] = t * m_PotentialMagnitude * vec_nor[1] * ys;
    vec_2[2] = t * m_PotentialMagnitude * vec_nor[2] * zs;

    vec_for = forces.Value();
    vec_for[0] += vec_2[0];
    vec_for[1] += vec_2[1];
    vec_for[2] += vec_2[2];
    forces.Value() = vec_for;

    ++forces;
    ++points;
    ++normals;
    ++i;
    }
}

/** Fit the model using the gradient information. */
template< typename TInputMesh, typename TOutputMesh >
void
DeformableMesh3DFilter< TInputMesh, TOutputMesh >
::GradientFit()
{
  ImageIndexType coord, coord2, tmp_co_1, tmp_co_2, tmp_co_3;
  InputPointType v1, v2;
  PixelType      mag;

  typename TInputMesh::PointType vec_nor, vec_loc, vec_for, tmp_vec_1, tmp_vec_2, tmp_vec_3;

  InputPointsContainerPointer  myLocations = m_Locations->GetPoints();
  InputPointsContainerIterator locations = myLocations->Begin();

  InputPointsContainerPointer  myForces = m_Forces->GetPoints();
  InputPointsContainerIterator forces = myForces->Begin();

  InputPointDataContainerPointer  myForceData = m_Forces->GetPointData();
  InputPointDataContainerIterator forcedata = myForceData->Begin();

  InputPointsContainerPointer  myNormals = m_Normals->GetPoints();
  InputPointsContainerIterator normals = myNormals->Begin();

  /* New gradient fit method testing. */
  while ( forces != myForces->End() )
    {
    vec_loc = locations.Value();
    vec_nor = normals.Value();

    coord[0] = static_cast< IndexValueType >( vec_loc[0] );
    coord[1] = static_cast< IndexValueType >( vec_loc[1] );
    coord[2] = static_cast< IndexValueType >( vec_loc[2] );

    coord2[0] = static_cast< IndexValueType >( std::ceil(vec_loc[0]) );
    coord2[1] = static_cast< IndexValueType >( std::ceil(vec_loc[1]) );
    coord2[2] = static_cast< IndexValueType >( std::ceil(vec_loc[2]) );

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
      vec_for[0] = m_Gradient->GetPixel(coord)[0];
      vec_for[1] = m_Gradient->GetPixel(coord)[1];
      vec_for[2] = m_Gradient->GetPixel(coord)[2];

      tmp_vec_1[0] = m_Gradient->GetPixel(tmp_co_1)[0] - m_Gradient->GetPixel(coord)[0];
      tmp_vec_1[1] = m_Gradient->GetPixel(tmp_co_1)[1] - m_Gradient->GetPixel(coord)[1];
      tmp_vec_1[2] = m_Gradient->GetPixel(tmp_co_1)[2] - m_Gradient->GetPixel(coord)[2];
      tmp_vec_2[0] = m_Gradient->GetPixel(tmp_co_2)[0] - m_Gradient->GetPixel(coord)[0];
      tmp_vec_2[1] = m_Gradient->GetPixel(tmp_co_2)[1] - m_Gradient->GetPixel(coord)[1];
      tmp_vec_2[2] = m_Gradient->GetPixel(tmp_co_2)[2] - m_Gradient->GetPixel(coord)[2];
      tmp_vec_3[0] = m_Gradient->GetPixel(tmp_co_3)[0] - m_Gradient->GetPixel(coord)[0];
      tmp_vec_3[1] = m_Gradient->GetPixel(tmp_co_3)[1] - m_Gradient->GetPixel(coord)[1];
      tmp_vec_3[2] = m_Gradient->GetPixel(tmp_co_3)[2] - m_Gradient->GetPixel(coord)[2];

      vec_for[0] = vec_for[0] + ( vec_loc[0] - coord[0] ) * tmp_vec_1[0]
                   + ( vec_loc[1] - coord[1] ) * tmp_vec_2[0] + ( vec_loc[2] - coord[2] ) * tmp_vec_3[0];
      vec_for[1] = vec_for[1] + ( vec_loc[1] - coord[1] ) * tmp_vec_2[1]
                   + ( vec_loc[0] - coord[0] ) * tmp_vec_1[1] + ( vec_loc[2] - coord[2] ) * tmp_vec_3[1];
      vec_for[2] = vec_for[2] + ( vec_loc[2] - coord[2] ) * tmp_vec_3[2]
                   + ( vec_loc[1] - coord[1] ) * tmp_vec_2[2] + ( vec_loc[0] - coord[0] ) * tmp_vec_1[2];
      }
    else
      {
      vec_for[0] = 0;
      vec_for[1] = 0;
      vec_for[2] = 0;
      }

    mag = vec_for[0] * vec_nor[0] + vec_for[1] * vec_nor[1] + vec_for[2] * vec_nor[2];

    vec_for[0] = m_GradientMagnitude * mag * vec_nor[0]; /*num_for*/
    vec_for[1] = m_GradientMagnitude * mag * vec_nor[1]; /*num_for*/
    vec_for[2] = m_GradientMagnitude * mag * vec_nor[2]; /*num_for*/

    mag = std::sqrt(vec_for[0] * vec_for[0] + vec_for[1] * vec_for[1] + vec_for[2] * vec_for[2]);
    if ( mag > 0.5 )
      {
      for ( int i = 0; i < 3; i++ )
        {
        vec_for[i] = ( 0.5 * vec_for[i] ) / mag;
        }
      }
    forces.Value() = vec_for;

    ++forces;
    ++forcedata;
    ++locations;
    ++normals;
    }
}

/* Compute normals. */
template< typename TInputMesh, typename TOutputMesh >
void
DeformableMesh3DFilter< TInputMesh, TOutputMesh >
::ComputeNormals()
{
  const typename InputMeshType::PointIdentifier *tp;
  InputPointType       v1, v2, v3, v4, d;

  v1.Fill(0.);
  v2.Fill(0.);
  v3.Fill(0.);
  d.Fill(0.);

  double coa, cob, coc;
  double absvec;

  InputMeshConstPointer            inputMesh = this->GetInput(0);
  InputCellsContainerConstPointer  myCells = inputMesh->GetCells();
  InputCellsContainerConstIterator cells = myCells->Begin();

  InputPointsContainerPointer  myNormals = m_Normals->GetPoints();
  InputPointsContainerIterator normals = myNormals->Begin();

  while ( normals != myNormals->End() )
    {
    normals.Value() = d;
    ++normals;
    }

  while ( cells != myCells->End() )
    {
    tp = cells.Value()->GetPointIds();
    ++cells;

    m_Locations->GetPoint (tp[0], &v1);
    m_Locations->GetPoint (tp[1], &v2);
    m_Locations->GetPoint (tp[2], &v3);

    coa = -( v1[1] * ( v2[2] - v3[2] )
             + v2[1] * ( v3[2] - v1[2] )
             + v3[1] * ( v1[2] - v2[2] ) );
    cob = -( v1[2] * ( v2[0] - v3[0] )
             + v2[2] * ( v3[0] - v1[0] )
             + v3[2] * ( v1[0] - v2[0] ) );
    coc = -( v1[0] * ( v2[1] - v3[1] )
             + v2[0] * ( v3[1] - v1[1] )
             + v3[0] * ( v1[1] - v2[1] ) );

    absvec = -std::sqrt ( (double)( ( coa * coa ) + ( cob * cob ) + ( coc * coc ) ) );

    itkAssertInDebugAndIgnoreInReleaseMacro (absvec != 0);

    v4[0] = coa / absvec;
    v4[1] = cob / absvec;
    v4[2] = coc / absvec;
    m_Normals->GetPoint (tp[0], &v1);
    m_Normals->GetPoint (tp[1], &v2);
    m_Normals->GetPoint (tp[2], &v3);

    v1[0] += v4[0];
    v1[1] += v4[1];
    v1[2] += v4[2];

    v2[0] += v4[0];
    v2[1] += v4[1];
    v2[2] += v4[2];

    v3[0] += v4[0];
    v3[1] += v4[1];
    v3[2] += v4[2];

    m_Normals->SetPoint (tp[0], v1);
    m_Normals->SetPoint (tp[1], v2);
    m_Normals->SetPoint (tp[2], v3);
    }

  normals = myNormals->Begin();
  while ( normals != myNormals->End() )
    {
    v1 = normals.Value();

    absvec = std::sqrt( (double)( ( v1[0] * v1[0] ) + ( v1[1] * v1[1] )
                                 + ( v1[2] * v1[2] ) ) );
    v1[0] = v1[0] / absvec;
    v1[1] = v1[1] / absvec;
    v1[2] = v1[2] / absvec;

    normals.Value() = v1;
    ++normals;
    }
}
} /* end namespace itk. */

#endif //#if !defined( ITK_LEGACY_REMOVE )
#endif
