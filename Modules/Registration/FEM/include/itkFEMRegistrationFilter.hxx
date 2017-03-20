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
#ifndef itkFEMRegistrationFilter_hxx
#define itkFEMRegistrationFilter_hxx

#include "itkFEMRegistrationFilter.h"

#include "itkFEMElements.h"
#include "itkFEMLoadBC.h"

#include "itkMath.h"
#include "itkGroupSpatialObject.h"
#include "itkLinearInterpolateImageFunction.h"
#include "itkSpatialObject.h"
#include "itkFEMObjectSpatialObject.h"
#include "itkDisplacementFieldJacobianDeterminantFilter.h"
#include "itkStatisticsImageFilter.h"
#include "itkRecursiveGaussianImageFilter.h"

#include "vnl/algo/vnl_determinant.h"
#include "itkMath.h"

namespace itk
{
namespace fem
{

template <typename TMovingImage, typename TFixedImage, typename TFemObject>
FEMRegistrationFilter<TMovingImage, TFixedImage, TFemObject>::FEMRegistrationFilter() :
  m_DoLineSearchOnImageEnergy( 1 ),
  m_LineSearchMaximumIterations( 100 ),
  m_TotalIterations( 0 ),
  m_MaxLevel( 1 ),
  m_FileCount( 0 ),
  m_CurrentLevel( 0 ),
  m_WhichMetric( 0 ),
  m_TimeStep( 1 ),
  m_Energy( 0.0 ),
  m_MinE( vnl_huge_val( 0 ) ),
  m_MinJacobian( 1.0 ),
  m_Alpha( 1.0 ),
  m_UseLandmarks( false ),
  m_UseMassMatrix( true ),
  m_UseNormalizedGradient( false ),
  m_CreateMeshFromImage( true ),
  m_EmployRegridding( 1 ),
  m_DescentDirection( positive ),
  m_EnergyReductionFactor( 0.0 ),
  m_MaximumError( 0.1 ),
  m_MaximumKernelWidth( 30 )
{
  this->SetNumberOfRequiredInputs(2);

  m_NumberOfIntegrationPoints.set_size(1);
  m_NumberOfIntegrationPoints[m_CurrentLevel] = 4;

  m_MetricWidth.set_size(1);
  m_MetricWidth[m_CurrentLevel] = 3;

  m_Maxiters.set_size(1);
  m_Maxiters[m_CurrentLevel] = 1;

  m_E.set_size(1);
  m_E[m_CurrentLevel] = 1.;

  m_Rho.set_size(1);
  m_Rho[m_CurrentLevel] = 1.;

  m_Gamma.set_size(1);
  m_Gamma[m_CurrentLevel] = 1;

  m_CurrentLevelImageSize.Fill( 0 );

  m_MeshPixelsPerElementAtEachResolution.set_size(1);
  m_MeshPixelsPerElementAtEachResolution[m_CurrentLevel] = 1;

  m_ImageScaling.Fill( 1 );
  m_CurrentImageScaling.Fill( 1 );
  m_FullImageSize.Fill( 0 );
  m_ImageOrigin.Fill( 0 );
  m_StandardDeviations.Fill( 1.0 );

  // Set up the default interpolator
  typename DefaultInterpolatorType::Pointer interp = DefaultInterpolatorType::New();
  m_Interpolator = static_cast<InterpolatorType *>( interp.GetPointer() );
  m_Interpolator->SetInputImage(m_Field);
}

template <typename TMovingImage, typename TFixedImage, typename TFemObject>
FEMRegistrationFilter<TMovingImage, TFixedImage, TFemObject>::~FEMRegistrationFilter()
{
}

template <typename TMovingImage, typename TFixedImage, typename TFemObject>
void FEMRegistrationFilter<TMovingImage, TFixedImage, TFemObject>::SetMaxLevel(unsigned int level)
{
  m_MaxLevel = level;

  m_E.set_size(level);
  m_Gamma.set_size(level);
  m_Rho.set_size(level);
  m_Maxiters.set_size(level);
  m_MeshPixelsPerElementAtEachResolution.set_size(level);
  m_NumberOfIntegrationPoints.set_size(level);
  m_MetricWidth.set_size(level);

  for(unsigned int i = 0; i < level; i++ )
    {
    m_Gamma[i] = 1;
    m_E[i] = 1.0;
    m_Rho[i] = 1.0;
    m_Maxiters[i] = 1;
    m_MeshPixelsPerElementAtEachResolution[i] = 1;
    m_NumberOfIntegrationPoints[i] = 4;
    m_MetricWidth[i] = 3;
    }
}

template <typename TMovingImage, typename TFixedImage, typename TFemObject>
void FEMRegistrationFilter<TMovingImage, TFixedImage, TFemObject>::SetStandardDeviations(double value)
{
  unsigned int j;

  for( j = 0; j < ImageDimension; j++ )
  {
    if( Math::NotExactlyEquals(value, m_StandardDeviations[j]) )
    {
      break;
    }
  }
  if( j < ImageDimension )
  {
    this->Modified();
    for( j = 0; j < ImageDimension; j++ )
    {
      m_StandardDeviations[j] = value;
    }
  }
}


template <typename TMovingImage, typename TFixedImage, typename TFemObject>
void FEMRegistrationFilter<TMovingImage, TFixedImage, TFemObject>::RunRegistration(void)
{

  MultiResSolve();

  if( m_Field )
    {
    if( m_TotalField )
      {
      m_Field = m_TotalField;
      }
    this->ComputeJacobian( );
    WarpImage(m_OriginalMovingImage);
    }
}

template <typename TMovingImage, typename TFixedImage, typename TFemObject>
void FEMRegistrationFilter<TMovingImage, TFixedImage, TFemObject>::SetMovingImage(MovingImageType* R)
{
  m_MovingImage = R;
  if( m_TotalIterations == 0 )
    {
    m_OriginalMovingImage = R;
    this->ProcessObject::SetNthInput( 0, const_cast<MovingImageType *>( R ) );
    }
}

template <typename TMovingImage, typename TFixedImage, typename TFemObject>
void FEMRegistrationFilter<TMovingImage, TFixedImage, TFemObject>::SetFixedImage(FixedImageType* T)
{
  m_FixedImage = T;
  m_FullImageSize = m_FixedImage->GetLargestPossibleRegion().GetSize();

  if( m_TotalIterations == 0 )
    {
    this->ProcessObject::SetNthInput( 1, const_cast<FixedImageType *>( T ) );
    }
  VectorType disp;
  for( unsigned int i = 0; i < ImageDimension; i++ )
    {
    disp[i] = 0.0;
    m_ImageOrigin[i] = 0;
    }

  m_CurrentLevelImageSize = m_FullImageSize;
}

template <typename TMovingImage, typename TFixedImage, typename TFemObject>
void FEMRegistrationFilter<TMovingImage, TFixedImage, TFemObject>::SetInputFEMObject(FEMObjectType* F,
                                                                                     unsigned int level)
{
  this->ProcessObject::SetNthInput( 2 + level, const_cast<FEMObjectType *>( F ) );
}

template <typename TMovingImage, typename TFixedImage, typename TFemObject>
typename FEMRegistrationFilter<TMovingImage, TFixedImage, TFemObject>::FEMObjectType *
FEMRegistrationFilter<TMovingImage, TFixedImage, TFemObject>::GetInputFEMObject(unsigned int level)
{
  return static_cast<FEMObjectType *>(this->ProcessObject::GetInput(2 + level) );
}

template <typename TMovingImage, typename TFixedImage, typename TFemObject>
void FEMRegistrationFilter<TMovingImage, TFixedImage, TFemObject>::ChooseMetric(unsigned int which)
{
  // Choose the similarity Function

  typedef itk::MeanSquareRegistrationFunction<FixedImageType, MovingImageType, FieldType> MetricType0;
  typedef itk::NCCRegistrationFunction<FixedImageType, MovingImageType, FieldType>        MetricType1;
  typedef itk::MIRegistrationFunction<FixedImageType, MovingImageType, FieldType>         MetricType2;
  typedef itk::DemonsRegistrationFunction<FixedImageType, MovingImageType, FieldType>     MetricType3;

  m_WhichMetric = (unsigned int)which;

  switch( which )
    {
    case 0:
      m_Metric = MetricType0::New();
      SetDescentDirectionMinimize();
      break;
    case 1:
      m_Metric = MetricType1::New();
      SetDescentDirectionMinimize();
      break;
    case 2:
      m_Metric = MetricType2::New();
      SetDescentDirectionMaximize();
      break;
    case 3:
      m_Metric = MetricType3::New();
      SetDescentDirectionMinimize();
      break;
    default:
      m_Metric = MetricType0::New();
      SetDescentDirectionMinimize();
    }

  m_Metric->SetGradientStep( m_Gamma[m_CurrentLevel] );
  m_Metric->SetNormalizeGradient( m_UseNormalizedGradient );
}

template <typename TMovingImage, typename TFixedImage, typename TFemObject>
void FEMRegistrationFilter<TMovingImage, TFixedImage, TFemObject>::WarpImage( const MovingImageType * ImageToWarp)
{
  typename WarperType::Pointer warper = WarperType::New();
  typedef typename WarperType::CoordRepType WarperCoordRepType;
  typedef itk::LinearInterpolateImageFunction<MovingImageType, WarperCoordRepType>
  InterpolatorType1;
  typename InterpolatorType1::Pointer interpolator = InterpolatorType1::New();

  warper = WarperType::New();
  warper->SetInput( ImageToWarp );
  warper->SetDisplacementField( m_Field );
  warper->SetInterpolator( interpolator );
  warper->SetOutputOrigin( m_FixedImage->GetOrigin() );
  warper->SetOutputSpacing( m_FixedImage->GetSpacing() );
  warper->SetOutputDirection( m_FixedImage->GetDirection() );
  typename FixedImageType::PixelType padValue = 0;
  warper->SetEdgePaddingValue( padValue );
  warper->Update();

  m_WarpedImage = warper->GetOutput();
}

template <typename TMovingImage, typename TFixedImage, typename TFemObject>
void FEMRegistrationFilter<TMovingImage, TFixedImage, TFemObject>::CreateMesh(unsigned int PixelsPerElement,
                                                                              SolverType *solver)
{
  vnl_vector<unsigned int> pixPerElement;
  pixPerElement.set_size( ImageDimension );
  pixPerElement.fill( PixelsPerElement );

  if( ImageDimension == 2 && dynamic_cast<Element2DC0LinearQuadrilateral *>(&*m_Element) != ITK_NULLPTR )
    {
    m_Material->SetYoungsModulus(this->GetElasticity(m_CurrentLevel) );

    itkDebugMacro( << " Generating regular Quad mesh " << std::endl );
    typename ImageToMeshType::Pointer meshFilter = ImageToMeshType::New();
    meshFilter->SetInput( m_MovingImage );
    meshFilter->SetPixelsPerElement( pixPerElement );
    meshFilter->SetElement( &*m_Element );
    meshFilter->SetMaterial( m_Material );
    meshFilter->Update();
    m_FEMObject = meshFilter->GetOutput();
    m_FEMObject->FinalizeMesh();
    itkDebugMacro( << " Generating regular mesh done " << std::endl );
    }
  else if( ImageDimension == 3 && dynamic_cast<Element3DC0LinearHexahedron *>(&*m_Element) != ITK_NULLPTR )
    {
    m_Material->SetYoungsModulus( this->GetElasticity(m_CurrentLevel) );

    itkDebugMacro( << " Generating regular Hex mesh " << std::endl );
    typename ImageToMeshType::Pointer meshFilter = ImageToMeshType::New();
    meshFilter->SetInput( m_MovingImage );
    meshFilter->SetPixelsPerElement( pixPerElement );
    meshFilter->SetElement( &*m_Element );
    meshFilter->SetMaterial( m_Material );
    meshFilter->Update();
    m_FEMObject = meshFilter->GetOutput();
    m_FEMObject->FinalizeMesh();
    itkDebugMacro( << " Generating regular mesh done " << std::endl );
    }
  else
    {
    FEMException e(__FILE__, __LINE__);
    e.SetDescription("CreateMesh - wrong image or element type");
    e.SetLocation(ITK_LOCATION);
    throw e;
    }

  if( m_UseLandmarks )
    {
    for( unsigned int i = 0; i < m_LandmarkArray.size(); i++ )
      {
      m_FEMObject->AddNextLoad( (m_LandmarkArray[i]) );
      }
    }

  solver->SetInput(m_FEMObject);
  solver->InitializeInterpolationGrid(m_FixedImage->GetBufferedRegion(),
                                        m_FixedImage->GetOrigin(),
                                        m_FixedImage->GetSpacing(),
                                        m_FixedImage->GetDirection());
}

template <typename TMovingImage, typename TFixedImage, typename TFemObject>
void FEMRegistrationFilter<TMovingImage, TFixedImage, TFemObject>
::ApplyImageLoads(TMovingImage*  movingimg, TFixedImage* fixedimg )
{
  m_Load = FEMRegistrationFilter<TMovingImage, TFixedImage, TFemObject>::ImageMetricLoadType::New();
  m_Load->SetMovingImage(movingimg);
  m_Load->SetFixedImage(fixedimg);
  if( !m_Field )
    {
    this->InitializeField();
    }
  m_Load->SetDisplacementField( this->GetDisplacementField() );
  m_Load->SetMetric(m_Metric);
  m_Load->InitializeMetric();
  m_Load->SetGamma(m_Gamma[m_CurrentLevel]);
  ImageSizeType r;
  for( unsigned int dd = 0; dd < ImageDimension; dd++ )
    {
    r[dd] = m_MetricWidth[m_CurrentLevel];
    }
  m_Load->SetMetricRadius(r);
  m_Load->SetNumberOfIntegrationPoints(m_NumberOfIntegrationPoints[m_CurrentLevel]);
  m_Load->SetGlobalNumber(m_FEMObject->GetNumberOfLoads() + 1);
  if( m_DescentDirection == positive )
    {
    m_Load->SetDescentDirectionMinimize( );
    }
  else
    {
    m_Load->SetDescentDirectionMaximize( );
    }
  m_FEMObject->AddNextLoad(m_Load.GetPointer());
  m_Load = dynamic_cast<typename FEMRegistrationFilter<TMovingImage, TFixedImage, TFemObject>::ImageMetricLoadType *>
    (&*m_FEMObject->GetLoadWithGlobalNumber(m_FEMObject->GetNumberOfLoads() ) );
}

template <typename TMovingImage, typename TFixedImage, typename TFemObject>
void FEMRegistrationFilter<TMovingImage, TFixedImage, TFemObject>::ApplyLoads(
  ImageSizeType ImgSz, double* scaling)
{
  // Apply the boundary conditions. We pin the image corners.
  // First compute which elements these will be.
  //
  itkDebugMacro( << " Applying loads " );

  vnl_vector<Float> pd; pd.set_size(ImageDimension);
  vnl_vector<Float> pu; pu.set_size(ImageDimension);

  // Now scale the landmarks

  itkDebugMacro( " Number of LM loads: " << m_LandmarkArray.size() );

  // Step over all the loads again to scale them by the global landmark weight.
  if( !m_LandmarkArray.empty() )
    {
    for( unsigned int lmind = 0; lmind < m_LandmarkArray.size(); lmind++ )
      {
      m_LandmarkArray[lmind]->GetElementArray()[0] = ITK_NULLPTR;

      itkDebugMacro( << " Prescale Pt: " << m_LandmarkArray[lmind]->GetTarget() );
      if( scaling )
        {
        m_LandmarkArray[lmind]->ScalePointAndForce(scaling, m_EnergyReductionFactor);
        itkDebugMacro( << " Postscale Pt: " << m_LandmarkArray[lmind]->GetTarget() << "; scale: " << scaling[0] );
        }

      pu = m_LandmarkArray[lmind]->GetSource();
      pd = m_LandmarkArray[lmind]->GetPoint();

      int numElements = m_FEMObject->GetNumberOfElements();
      for( int i = 0; i < numElements; i++ )
        {
        if( m_FEMObject->GetElement(i)->GetLocalFromGlobalCoordinates(pu, pd ) )
          {
          m_LandmarkArray[lmind]->SetPoint(pd);
          m_LandmarkArray[lmind]->GetElementArray()[0] =  m_FEMObject->GetElement(i);
          }
        }

      m_LandmarkArray[lmind]->SetGlobalNumber(lmind);
      LoadLandmark::Pointer l5 = dynamic_cast<LoadLandmark *>( &*m_LandmarkArray[lmind]->CreateAnother() );
      m_FEMObject->AddNextLoad(l5);
      }
    itkDebugMacro( << " Landmarks done" );
    }

  // Now apply the BC loads
  LoadBC::Pointer l1;

  // Pin one corner of image
  unsigned int CornerCounter, ii, EdgeCounter = 0;

  int numNodes = m_FEMObject->GetNumberOfNodes();

  Element::VectorType coord;

  bool EdgeFound;
  unsigned int nodect = 0;
  for( int i = 0; i < numNodes; i++ )
    {
    if( EdgeCounter >= ImageDimension )
      {
      return;
      }

    coord = m_FEMObject->GetNode(i)->GetCoordinates();
    CornerCounter = 0;
    for( ii = 0; ii < ImageDimension; ii++ )
      {
      if( Math::AlmostEquals( coord[ii], m_ImageOrigin[ii] )
       || Math::AlmostEquals( coord[ii], ImgSz[ii] - 1 ) )
        {
        CornerCounter++;
        }
      }

    if( CornerCounter == ImageDimension ) // the node is located at a true corner
      {
      unsigned int ndofpernode = (*(m_FEMObject->GetNode(i)->m_elements.begin() ) )->GetNumberOfDegreesOfFreedomPerNode();
      unsigned int numnodesperelt = (*(m_FEMObject->GetNode(i)->m_elements.begin() ) )->GetNumberOfNodes();
      unsigned int whichnode;

      unsigned int maxnode = numnodesperelt - 1;

      typedef typename Element::Node::SetOfElements NodeEltSetType;
      for( NodeEltSetType::iterator elt = m_FEMObject->GetNode(i)->m_elements.begin();
           elt != m_FEMObject->GetNode(i)->m_elements.end(); elt++ )
        {
        for( whichnode = 0; whichnode <= maxnode; whichnode++ )
          {
          coord = (*elt)->GetNode(whichnode)->GetCoordinates();
          CornerCounter = 0;
          for( ii = 0; ii < ImageDimension; ii++ )
            {
            if( Math::AlmostEquals( coord[ii], m_ImageOrigin[ii] )
             || Math::AlmostEquals( coord[ii], ImgSz[ii] - 1 ) )
              {
              CornerCounter++;
              }
            }
          if( CornerCounter == ImageDimension - 1 )
            {
            EdgeFound = true;
            }
          else
            {
            EdgeFound = false;
            }
          if( EdgeFound )
            {
            for( unsigned int jj = 0; jj < ndofpernode; jj++ )
              {
              itkDebugMacro( " Which node " << whichnode );
              itkDebugMacro( " Edge coord " << coord );

              l1 = LoadBC::New();
              // now we get the element from the node -- we assume we need fix the dof only once
              // even if more than one element shares it.

              l1->SetElement(Element::ConstPointer(*elt));
              unsigned int localdof = whichnode * ndofpernode + jj;
              l1->SetDegreeOfFreedom(localdof);
              l1->SetValue(vnl_vector<double>(1, 0.0) );

              m_FEMObject->AddNextLoad(l1);
              }
            EdgeCounter++;
            }
          }
        } // end elt loop
      }
    nodect++;
    itkDebugMacro( << " Node: " << nodect );
    }
}

template <typename TMovingImage, typename TFixedImage, typename TFemObject>
void FEMRegistrationFilter<TMovingImage, TFixedImage, TFemObject>::IterativeSolve(SolverType *solver)
{
  if( !m_Load )
    {
    itkExceptionMacro( <<"No Load set" );
    }

  bool Done = false;
  unsigned int iters = 0;
  m_MinE = 10.e99;
  Float deltE = 0;
  while( !Done && iters < m_Maxiters[m_CurrentLevel] )
    {
    const Float lastdeltE = deltE;
    const unsigned int DLS = m_DoLineSearchOnImageEnergy;
    //  Reset the variational similarity term to zero.

    Float LastE = m_Load->GetCurrentEnergy();
    m_Load->SetCurrentEnergy(0.0);
    m_Load->InitializeMetric();

    if( !m_Field )
      {
      itkExceptionMacro( <<"No Field set" );
      }
    solver->SetUseMassMatrix( m_UseMassMatrix );

    // Solve the system of equations for displacements (u=K^-1*F)
    solver->Modified();
    solver->Update();
    m_Load->PrintCurrentEnergy();

    if( m_DescentDirection == 1 )
      {
      deltE = (LastE - m_Load->GetCurrentEnergy() );
      }
    else
      {
      deltE = (m_Load->GetCurrentEnergy() - LastE );
      }

    if(  DLS == 2 && deltE < 0.0 )
      {
      itkDebugMacro( << " Line search " );
      const float tol = 1.0; // ((0.01  < LastE) ? 0.01 : LastE/10.);
      LastE = this->GoldenSection(solver, tol, m_LineSearchMaximumIterations);
      deltE = (m_MinE - LastE);
      itkDebugMacro( << " Line search done " << std::endl );
      }

    iters++;

    if( deltE == 0.0 )
      {
      itkDebugMacro( << " No change in energy " << std::endl);
      Done = true;
      }
    if( (DLS == 0)  && (  iters >= m_Maxiters[m_CurrentLevel] ) )
      {
      Done = true;
      }
    else if( (DLS > 0) &&
             ( iters >= m_Maxiters[m_CurrentLevel] || (deltE < 0.0 && iters > 5 && lastdeltE < 0.0) ) )
      {
      Done = true;
      }
    float curmaxsol = solver->GetCurrentMaxSolution();
    if( Math::AlmostEquals( curmaxsol, 0.0f ) )
      {
      curmaxsol = 1.0;
      }
    Float mint = m_Gamma[m_CurrentLevel] / curmaxsol;
    if( mint > 1 )
      {
      mint = 1.0;
      }

    if( solver->GetCurrentMaxSolution() < 0.01 && iters > 2 )
      {
      Done = true;
      }
    solver->AddToDisplacements(mint);
    m_MinE = LastE;

    InterpolateVectorField(solver);

    if( m_EmployRegridding != 0 )
      {
      if( iters % m_EmployRegridding == 0  )
        {
        this->EnforceDiffeomorphism(1.0, solver, true);
        }
      }
    itkDebugMacro( << " min E: " << m_MinE << "; delt E: " << deltE << "; iters: " << iters << std::endl );
    m_TotalIterations++;
    }
}

template <typename TMovingImage, typename TFixedImage, typename TFemObject>
void FEMRegistrationFilter<TMovingImage, TFixedImage, TFemObject>
::InitializeField()
{
  m_Field = FieldType::New();

  m_FieldRegion.SetSize(m_CurrentLevelImageSize );
  m_Field->SetOrigin( m_FixedImage->GetOrigin() );
  m_Field->SetSpacing( m_FixedImage->GetSpacing() );
  m_Field->SetDirection( m_FixedImage->GetDirection() );
  m_Field->SetLargestPossibleRegion( m_FieldRegion );
  m_Field->SetBufferedRegion( m_FieldRegion );
  m_Field->SetLargestPossibleRegion( m_FieldRegion );
  m_Field->Allocate();

  VectorType disp;
  for( unsigned int t = 0; t < ImageDimension; t++ )
    {
    disp[t] = 0.0;
    }
  FieldIterator fieldIter( m_Field, m_FieldRegion );
  fieldIter.GoToBegin();
  for(; !fieldIter.IsAtEnd(); ++fieldIter )
    {
    fieldIter.Set(disp);
    }
}

template <typename TMovingImage, typename TFixedImage, typename TFemObject>
void
FEMRegistrationFilter<TMovingImage, TFixedImage, TFemObject>::InterpolateVectorField(SolverType *solver)
{
  typename FieldType::Pointer field = m_Field;

  if( !field )
    {
    this->InitializeField();
    }
  m_FieldSize = field->GetLargestPossibleRegion().GetSize();

  itkDebugMacro( << " Interpolating vector field of size " << m_FieldSize);

  Float rstep, sstep, tstep;

  vnl_vector<double> Pos;  // solution at the point
  vnl_vector<double> Sol;  // solution at the local point
  vnl_vector<double> Gpt;  // global position given by local point

  VectorType disp;
  for( unsigned int t = 0; t < ImageDimension; t++ )
    {
    disp[t] = 0.0;
    }
  FieldIterator fieldIter( field, field->GetLargestPossibleRegion() );

  fieldIter.GoToBegin();
  typename FixedImageType::IndexType rindex = fieldIter.GetIndex();

  Sol.set_size(ImageDimension);
  Gpt.set_size(ImageDimension);

  if( ImageDimension == 2 )
    {
    Element::ConstPointer eltp;
    for(; !fieldIter.IsAtEnd(); ++fieldIter )
      {
      // Get element pointer from the solver elt pointer image
      typename FieldType::PointType physicalPoint;
      rindex = fieldIter.GetIndex();
      field->TransformIndexToPhysicalPoint(rindex, physicalPoint);
      for( unsigned int d = 0; d < ImageDimension; d++ )
        {
        Gpt[d] = (double) (physicalPoint[d]);
        }

      eltp = solver->GetElementAtPoint(Gpt);
      if( eltp )
        {
        eltp->GetLocalFromGlobalCoordinates(Gpt, Pos);

        unsigned int Nnodes = eltp->GetNumberOfNodes();
        typename Element::VectorType shapef(Nnodes);
        shapef = eltp->ShapeFunctions(Pos);
        Float solval;
        for( unsigned int f = 0; f < ImageDimension; f++ )
          {
          solval = 0.0;
          for( unsigned int n = 0; n < Nnodes; n++ )
            {
            solval += shapef[n] * solver->GetLinearSystem()->GetSolutionValue(
                eltp->GetNode(n)->GetDegreeOfFreedom(f), solver->GetTotalSolutionIndex() );
            }
          Sol[f] = solval;
          disp[f] = (Float) 1.0 * Sol[f];
          }
        field->SetPixel(rindex, disp );
        }
      }
    }

  if( ImageDimension == 3 )
    {
    // FIXME SHOULD BE 2.0 over meshpixperelt
    rstep = 1.25 / ( (double)m_MeshPixelsPerElementAtEachResolution[m_CurrentLevel]);
    sstep = 1.25 / ( (double)m_MeshPixelsPerElementAtEachResolution[m_CurrentLevel]);
    tstep = 1.25 / ( (double)m_MeshPixelsPerElementAtEachResolution[m_CurrentLevel]);

    Pos.set_size(ImageDimension);
    int numElements = solver->GetInput()->GetNumberOfElements();
    for( int i = 0; i < numElements; i++ )
      {
      Element::Pointer eltp = solver->GetInput()->GetElement(i);
      for( double r = -1.0; r <= 1.0; r = r + rstep )
        {
        for( double s = -1.0; s <= 1.0; s = s + sstep )
          {
          for( double t = -1.0; t <= 1.0; t = t + tstep )
            {
            Pos[0] = r;
            Pos[1] = s;
            Pos[2] = t;

            unsigned int numNodes = eltp->GetNumberOfNodes();
            typename Element::VectorType shapef(numNodes);

#define FASTHEX
#ifdef FASTHEX
// FIXME temporarily using hexahedron shape f for speed
            shapef[0] = (1 - r) * (1 - s) * (1 - t) * 0.125;
            shapef[1] = (1 + r) * (1 - s) * (1 - t) * 0.125;
            shapef[2] = (1 + r) * (1 + s) * (1 - t) * 0.125;
            shapef[3] = (1 - r) * (1 + s) * (1 - t) * 0.125;
            shapef[4] = (1 - r) * (1 - s) * (1 + t) * 0.125;
            shapef[5] = (1 + r) * (1 - s) * (1 + t) * 0.125;
            shapef[6] = (1 + r) * (1 + s) * (1 + t) * 0.125;
            shapef[7] = (1 - r) * (1 + s) * (1 + t) * 0.125;
#else
            shapef = (*eltp)->ShapeFunctions(Pos);
#endif

            Float solval, posval;
            bool inimage = true;
            typename FixedImageType::PointType physicalPoint;

            for( unsigned int f = 0; f < ImageDimension; f++ )
              {
              solval = 0.0;
              posval = 0.0;
              for( unsigned int n = 0; n < numNodes; n++ )
                {
                posval += shapef[n] * ( ( (eltp)->GetNodeCoordinates(n) )[f]);
                solval += shapef[n] * solver->GetLinearSystem()->GetSolutionValue(
                    (eltp)->GetNode(n)->GetDegreeOfFreedom(f), solver->GetTotalSolutionIndex() );
                }
              Sol[f] = solval;
              Gpt[f] = posval;
              disp[f] = (Float) 1.0 * Sol[f];
              physicalPoint[f] = Gpt[f];
              }

            inimage = m_FixedImage->TransformPhysicalPointToIndex(physicalPoint, rindex);
            if( inimage )
              {
              field->SetPixel(rindex, disp);
              }
            }
          }
        } // end of for loops
      }   // end of elt array loop

    }

  // Ensure that the values are exact at the nodes. They won't necessarily be unless we use this code.
  itkDebugMacro( << " Interpolation done " << std::endl);
}

template <typename TMovingImage, typename TFixedImage, typename TFemObject>
void FEMRegistrationFilter<TMovingImage, TFixedImage, TFemObject>::ComputeJacobian( )
{
  m_MinJacobian = 1.0;

  typedef typename itk::DisplacementFieldJacobianDeterminantFilter< FieldType, float, FloatImageType > JacobianFilterType;
  typename JacobianFilterType::Pointer jacobianFilter = JacobianFilterType::New();
  jacobianFilter->SetInput( m_Field );
  jacobianFilter->Update( );
  m_FloatImage = jacobianFilter->GetOutput();

  typedef typename itk::StatisticsImageFilter< FloatImageType > StatisticsFilterType;
  typename StatisticsFilterType::Pointer statisticsFilter = StatisticsFilterType::New();
  statisticsFilter->SetInput( m_FloatImage );
  statisticsFilter->Update( );

  m_MinJacobian = statisticsFilter->GetMinimum();

  itkDebugMacro( << " Min Jacobian: " << m_MinJacobian << std::endl);
}

template <typename TMovingImage, typename TFixedImage, typename TFemObject>
void FEMRegistrationFilter<TMovingImage, TFixedImage, TFemObject>::EnforceDiffeomorphism(float thresh,
                                                                                         SolverType *solver,
                                                                                         bool onlywriteimages )
{
  itkDebugMacro( << " Checking Jacobian using threshold " << thresh );

  this->ComputeJacobian();

  if( m_MinJacobian < thresh )
  {
    // Smooth deformation field
    this->SmoothDisplacementField();
  }

  typename WarperType::Pointer warper = WarperType::New();
  typedef typename WarperType::CoordRepType WarperCoordRepType;
  typedef itk::LinearInterpolateImageFunction<MovingImageType, WarperCoordRepType>
  InterpolatorType1;
  typename InterpolatorType1::Pointer interpolator = InterpolatorType1::New();

  // If using landmarks, warp them
  if( m_UseLandmarks )
    {
    itkDebugMacro( << " Warping landmarks: " << m_LandmarkArray.size() );

    if( !m_LandmarkArray.empty() )
      {
      for( unsigned int lmind = 0; lmind < m_LandmarkArray.size(); lmind++ )
        {
        itkDebugMacro( << " Old source: " << m_LandmarkArray[lmind]->GetSource() );
        itkDebugMacro( << " Target: " << m_LandmarkArray[lmind]->GetTarget() );

        // Convert the source to warped coords.
        m_LandmarkArray[lmind]->GetSource() = m_LandmarkArray[lmind]->GetSource()
            + (dynamic_cast<LoadLandmark *>( &*solver->GetOutput()->GetLoadWithGlobalNumber(lmind) )->GetForce() );
        itkDebugMacro( << " New source: " << m_LandmarkArray[lmind]->GetSource() );
        itkDebugMacro( << " Target: " << m_LandmarkArray[lmind]->GetTarget() );
        LoadLandmark::Pointer l5 = dynamic_cast<LoadLandmark *>( &*m_LandmarkArray[lmind]->CreateAnother() );
        solver->GetOutput()->AddNextLoad(l5);
        }
      itkDebugMacro( << " Warping landmarks done " );
      }
    else
      {
      itkDebugMacro( << " Landmark array empty " );
      }
    }

  // Store the total deformation by composing with the full field
  if( !m_TotalField && !onlywriteimages )
    {
    itkDebugMacro( << " Allocating total deformation field " );

    m_TotalField = FieldType::New();

    m_FieldRegion.SetSize(m_Field->GetLargestPossibleRegion().GetSize() );
    m_TotalField->SetLargestPossibleRegion( m_FieldRegion );
    m_TotalField->SetBufferedRegion( m_FieldRegion );
    m_TotalField->SetLargestPossibleRegion( m_FieldRegion );
    m_TotalField->Allocate();

    VectorType disp;
    disp.Fill(0.0);

    FieldIterator fieldIter( m_TotalField, m_FieldRegion );

    for( fieldIter.GoToBegin(); !fieldIter.IsAtEnd(); ++fieldIter )
      {
      fieldIter.Set(disp);
      }
    }

  if( onlywriteimages )
    {
    warper = WarperType::New();
    warper->SetInput( m_OriginalMovingImage );
    warper->SetDisplacementField( m_Field );
    warper->SetInterpolator( interpolator );
    warper->SetOutputOrigin( m_FixedImage->GetOrigin() );
    warper->SetOutputSpacing( m_FixedImage->GetSpacing() );
    warper->SetOutputDirection( m_FixedImage->GetDirection() );
    typename MovingImageType::PixelType padValue = 0;
    warper->SetEdgePaddingValue( padValue );
    warper->Update();
    m_WarpedImage = warper->GetOutput();
    }
  else if( m_TotalField )
    {

    typename InterpolatorType::ContinuousIndexType inputIndex;

    typedef typename InterpolatorType::OutputType InterpolatedType;

    InterpolatedType interpolatedValue;

    m_Interpolator->SetInputImage(m_Field);

    typename FixedImageType::IndexType index;
    FieldIterator totalFieldIter( m_TotalField, m_TotalField->GetLargestPossibleRegion() );
    totalFieldIter.GoToBegin();
    unsigned int jj;
    float pathsteplength = 0;
    while( !totalFieldIter.IsAtEnd()  )
      {
      index = totalFieldIter.GetIndex();
      for( jj = 0; jj < ImageDimension; jj++ )
        {
        inputIndex[jj] = (WarperCoordRepType) index[jj];
        interpolatedValue[jj] = 0.0;
        }

      if ( m_Interpolator->IsInsideBuffer( inputIndex ) )
        {
        interpolatedValue =
            m_Interpolator->EvaluateAtContinuousIndex( inputIndex );
        }
      VectorType interped;
      float temp = 0.0;
      for( jj = 0; jj < ImageDimension; jj++ )
        {
        interped[jj] = interpolatedValue[jj];
        temp += interped[jj] * interped[jj];
        }
      pathsteplength += std::sqrt(temp);
      m_TotalField->SetPixel(index, m_TotalField->GetPixel(index) + interped);
      ++totalFieldIter;
      }

    itkDebugMacro( << " Incremental path length: " << pathsteplength );

    // Set the field to zero
    FieldIterator fieldIter( m_Field, m_Field->GetLargestPossibleRegion() );
    fieldIter.GoToBegin();
    while( !fieldIter.IsAtEnd()  )
      {
      VectorType disp;
      disp.Fill(0.0);
      fieldIter.Set(disp);
      ++fieldIter;
      }

    // Now do the same for the solver
    int numNodes = solver->GetOutput()->GetNumberOfNodes();
    for( int i = 0; i < numNodes; i++ )
      {
      // Now put it into the solution!
      for( unsigned int ii = 0; ii < ImageDimension; ii++ )
        {
        solver->GetLinearSystemWrapper()->
        SetSolutionValue( (solver->GetOutput()->GetNode(i) )->GetDegreeOfFreedom(
                              ii), 0.0, solver->GetTotalSolutionIndex() );
        solver->GetLinearSystemWrapper()->
        SetSolutionValue( (solver->GetOutput()->GetNode(i) )->GetDegreeOfFreedom(
                              ii), 0.0, solver->GetSolutionTMinus1Index() );
        }
      }

    warper = WarperType::New();
    warper->SetInput( m_OriginalMovingImage );
    warper->SetDisplacementField( m_TotalField );
    warper->SetInterpolator( interpolator );
    warper->SetOutputOrigin( m_FixedImage->GetOrigin() );
    warper->SetOutputSpacing( m_FixedImage->GetSpacing() );
    warper->SetOutputDirection( m_FixedImage->GetDirection() );
    typename FixedImageType::PixelType padValue = 0;
    warper->SetEdgePaddingValue( padValue );
    warper->Update();

    // Set it as the new moving image
    this->SetMovingImage( warper->GetOutput() );

    m_WarpedImage = m_MovingImage;

    m_Load->SetMovingImage( this->GetMovingImage() );
    }
  itkDebugMacro( << " Enforcing diffeomorphism done " );
}

template <typename TMovingImage, typename TFixedImage, typename TFemObject>
void
FEMRegistrationFilter<TMovingImage, TFixedImage, TFemObject>::SmoothDisplacementField()
{

  typedef RecursiveGaussianImageFilter< FieldType, FieldType > GaussianFilterType;
  typename GaussianFilterType::Pointer smoother = GaussianFilterType::New();

  for( unsigned int dim = 0; dim < ImageDimension; ++dim )
    {
    // Sigma accounts for the subsampling of the pyramid
    double sigma = m_StandardDeviations[dim];

    smoother->SetInput( m_Field );
    smoother->SetSigma(sigma);
    smoother->SetDirection(dim);
    smoother->Update();

    m_Field = smoother->GetOutput();
    m_Field->DisconnectPipeline();
    }
}

template <typename TMovingImage, typename TFixedImage, typename TFemObject>
typename FEMRegistrationFilter<TMovingImage, TFixedImage, TFemObject>::FieldPointer
FEMRegistrationFilter<TMovingImage, TFixedImage, TFemObject>::ExpandVectorField( ExpandFactorsType* expandFactors,
                                                                                 FieldType* field)
{
  // Re-size the vector field
  if( !field )
    {
    field = m_Field;
    }

  itkDebugMacro( << " Input field size: " << m_Field->GetLargestPossibleRegion().GetSize() );
  itkDebugMacro( << " Expand factors: " );
  VectorType pad;
  for( unsigned int i = 0; i < ImageDimension; i++ )
    {
    pad[i] = 0.0;
    itkDebugMacro( << expandFactors[i] << " " );
    }
  itkDebugMacro( << std::endl );
  typename ExpanderType::Pointer m_FieldExpander = ExpanderType::New();
  m_FieldExpander->SetInput(field);
  m_FieldExpander->SetExpandFactors( expandFactors );
  // use default
// TEST_RMV20100728   m_FieldExpander->SetEdgePaddingValue( pad );
  m_FieldExpander->UpdateLargestPossibleRegion();

  m_FieldSize = m_FieldExpander->GetOutput()->GetLargestPossibleRegion().GetSize();

  return m_FieldExpander->GetOutput();
}

template <typename TMovingImage, typename TFixedImage, typename TFemObject>
void FEMRegistrationFilter<TMovingImage, TFixedImage, TFemObject>::SampleVectorFieldAtNodes(SolverType *solver)
{
  // Here, we need to iterate through the nodes, get the nodal coordinates,
  // sample the VF at the node and place the values in the SolutionVector.
  int numNodes = solver->GetOutput()->GetNumberOfNodes();
  Element::VectorType coord;
  VectorType SolutionAtNode;

  m_Interpolator->SetInputImage(m_Field);
  for( int i = 0; i < numNodes; i++ )
    {
    coord = solver->GetOutput()->GetNode(i)->GetCoordinates();
    typename InterpolatorType::ContinuousIndexType inputIndex;
    typedef typename InterpolatorType::OutputType InterpolatedType;
    InterpolatedType interpolatedValue;
    for( unsigned int jj = 0; jj < ImageDimension; jj++ )
      {
      inputIndex[jj] = (CoordRepType) coord[jj];
      interpolatedValue[jj] = 0.0;
      }
    if( m_Interpolator->IsInsideBuffer( inputIndex ) )
      {
      interpolatedValue =
        m_Interpolator->EvaluateAtContinuousIndex( inputIndex );
      }
    for( unsigned int jj = 0; jj < ImageDimension; jj++ )
      {
      SolutionAtNode[jj] = interpolatedValue[jj];
      }
    // Now put it into the solution!
    for( unsigned int ii = 0; ii < ImageDimension; ii++ )
      {
      Float Sol = SolutionAtNode[ii];
      solver->GetLinearSystemWrapper()->
      SetSolutionValue(solver->GetOutput()->GetNode(i)->GetDegreeOfFreedom(ii), Sol, solver->GetTotalSolutionIndex() );
      solver->GetLinearSystemWrapper()->
      SetSolutionValue(solver->GetOutput()->GetNode(i)->GetDegreeOfFreedom(
                         ii), Sol, solver->GetSolutionTMinus1Index() );
      }
    }
}

template <typename TMovingImage, typename TFixedImage, typename TFemObject>
void FEMRegistrationFilter<TMovingImage, TFixedImage, TFemObject>::PrintVectorField(unsigned int modnum)
{
  FieldIterator fieldIter( m_Field, m_Field->GetLargestPossibleRegion() );

  fieldIter.GoToBegin();
  unsigned int ct = 0;

  float max = 0;
  while( !fieldIter.IsAtEnd()  )
    {
    VectorType disp = fieldIter.Get();
    if( (ct % modnum) == 0 )
      {
      itkDebugMacro( << " Field pix: " << fieldIter.Get() << std::endl);
      }
    for( unsigned int i = 0; i < ImageDimension; i++ )
      {
      if( std::fabs(disp[i]) > max )
        {
        max = std::fabs(disp[i]);
        }
      }
    ++fieldIter;
    ct++;

    }

  itkDebugMacro( << " Max vec: " << max << std::endl );
}

template <typename TMovingImage, typename TFixedImage, typename TFemObject>
void FEMRegistrationFilter<TMovingImage, TFixedImage, TFemObject>::MultiResSolve()
{
  for( m_CurrentLevel = 0; m_CurrentLevel < m_MaxLevel; m_CurrentLevel++ )
    {
    itkDebugMacro( << " Beginning level " << m_CurrentLevel << std::endl );

    typename SolverType::Pointer solver = SolverType::New();

    if( m_Maxiters[m_CurrentLevel] > 0 )
      {
      unsigned int meshResolution = this->m_MeshPixelsPerElementAtEachResolution(m_CurrentLevel);

      solver->SetTimeStep(m_TimeStep);
      solver->SetRho(m_Rho[m_CurrentLevel]);
      solver->SetAlpha(m_Alpha);

      if( m_CreateMeshFromImage )
        {
        this->CreateMesh(meshResolution, solver);
        }
      else
        {
        m_FEMObject = this->GetInputFEMObject( m_CurrentLevel );
        }

      this->ApplyLoads(m_FullImageSize, ITK_NULLPTR);
      this->ApplyImageLoads(m_MovingImage, m_FixedImage );


      unsigned int ndofpernode = m_Element->GetNumberOfDegreesOfFreedomPerNode();
      unsigned int numnodesperelt = m_Element->GetNumberOfNodes() + 1;
      unsigned int ndof = solver->GetInput()->GetNumberOfDegreesOfFreedom();
      unsigned int nzelts;
      nzelts = numnodesperelt * ndofpernode * ndof;

      // Used when reading a mesh from file
      // nzelts=((2*numnodesperelt*ndofpernode*ndof > 25*ndof) ? 2*numnodesperelt*ndofpernode*ndof : 25*ndof);

      LinearSystemWrapperItpack itpackWrapper;
      unsigned int maxits = 2 * solver->GetInput()->GetNumberOfDegreesOfFreedom();
      itpackWrapper.SetMaximumNumberIterations(maxits);
      itpackWrapper.SetTolerance(1.e-1);
      itpackWrapper.JacobianConjugateGradient();
      itpackWrapper.SetMaximumNonZeroValuesInMatrix(nzelts);
      solver->SetLinearSystemWrapper(&itpackWrapper);
      solver->SetUseMassMatrix( m_UseMassMatrix );
      if( m_CurrentLevel > 0 )
        {
        this->SampleVectorFieldAtNodes(solver);
        }
      this->IterativeSolve(solver);
      }

    // Now expand the field for the next level, if necessary.

    itkDebugMacro( << " End level: " << m_CurrentLevel );

    } // end image resolution loop

  if( m_TotalField )
    {
    itkDebugMacro( << " Copy field: " <<  m_TotalField->GetLargestPossibleRegion().GetSize() );
    itkDebugMacro( << " To: " << m_Field->GetLargestPossibleRegion().GetSize() << std::endl);
    FieldIterator fieldIter( m_TotalField, m_TotalField->GetLargestPossibleRegion() );
    fieldIter.GoToBegin();
    for(; !fieldIter.IsAtEnd(); ++fieldIter )
      {
      typename FixedImageType::IndexType index = fieldIter.GetIndex();
      m_TotalField->SetPixel(index, m_TotalField->GetPixel(index)
                             + m_Field->GetPixel(index) );
      }
    }
}

template <typename TMovingImage, typename TFixedImage, typename TFemObject>
Element::Float FEMRegistrationFilter<TMovingImage, TFixedImage, TFemObject>::EvaluateResidual(SolverType *solver,
                                                                                              Float t)
{
  Float SimE = m_Load->EvaluateMetricGivenSolution(solver->GetOutput()->GetModifiableElementContainer(), t);
  Float maxsim = 1.0;
  for( unsigned int i = 0; i < ImageDimension; i++ )
    {
    maxsim *= (Float)m_FullImageSize[i];
    }
  if( m_WhichMetric != 0 )
    {
    SimE = maxsim - SimE;
    }
  return std::fabs( static_cast<double>(SimE) ); // +defe;
}

template <typename TMovingImage, typename TFixedImage, typename TFemObject>
void FEMRegistrationFilter<TMovingImage, TFixedImage, TFemObject>::FindBracketingTriplet(SolverType *solver, Float* a,
                                                                                         Float* b,
                                                                                         Float* c)
{
  // See Numerical Recipes

  const Float Gold = 1.618034;
  const Float Glimit = 100.0;
  const Float Tiny = 1.e-20;
  Float ax = 0.0;
  Float bx = 1.0;
  Float fa = std::fabs( this->EvaluateResidual(solver, ax) );
  Float fb = std::fabs( this->EvaluateResidual(solver, bx) );

  Float dum;

  if( fb > fa )
    {
    dum = ax; ax = bx; bx = dum;
    dum = fb; fb = fa; fa = dum;
    }

  Float cx = bx + Gold * (bx - ax);  // first guess for c - the 3rd pt needed to bracket the min
  Float fc = std::fabs( this->EvaluateResidual(solver, cx) );

  Float ulim, u, r, q, fu;
  while( fb > fc )
  // && std::fabs(ax) < 3. && std::fabs(bx) < 3. && std::fabs(cx) < 3.)
    {
    r = (bx - ax) * (fb - fc);
    q = (bx - cx) * (fb - fa);
    Float denom = (2.0 * solver->GSSign(solver->GSMax(std::fabs(q - r), Tiny), q - r) );
    u = (bx) - ( (bx - cx) * q - (bx - ax) * r) / denom;
    ulim = bx + Glimit * (cx - bx);
    if( (bx - u) * (u - cx) > 0.0 )
      {
      fu = std::fabs( this->EvaluateResidual(solver, u) );
      if( fu < fc )
        {
        ax = bx;
        bx = u;
        *a = ax; *b = bx; *c = cx;
        return;
        }
      else if( fu > fb )
        {
        cx = u;
        *a = ax; *b = bx; *c = cx;
        return;
        }

      u = cx + Gold * (cx - bx);
      fu = std::fabs( this->EvaluateResidual(solver, u) );

      }
    else if( (cx - u) * (u - ulim) > 0.0 )
      {
      fu = std::fabs( this->EvaluateResidual(solver, u) );
      if( fu < fc )
        {
        bx = cx; cx = u; u = cx + Gold * (cx - bx);
        fb = fc; fc = fu; fu = std::fabs( this->EvaluateResidual(solver, u) );
        }

      }
    else if( (u - ulim) * (ulim - cx) >= 0.0 )
      {
      u = ulim;
      fu = std::fabs( this->EvaluateResidual(solver, u) );
      }
    else
      {
      u = cx + Gold * (cx - bx);
      fu = std::fabs( this->EvaluateResidual(solver, u) );
      }

    ax = bx; bx = cx; cx = u;
    fa = fb; fb = fc; fc = fu;

    }

  if( std::fabs(ax) > 1.e3 || std::fabs(bx) > 1.e3 || std::fabs(cx) > 1.e3 )
    {
    ax = -2.0; bx = 1.0; cx = 2.0;
    } // to avoid crazy numbers caused by bad bracket (u goes nuts)

  *a = ax; *b = bx; *c = cx;
}

template <typename TMovingImage, typename TFixedImage, typename TFemObject>
Element::Float FEMRegistrationFilter<TMovingImage, TFixedImage, TFemObject>::GoldenSection(
  SolverType *solver, Float tol, unsigned int MaxIters)
{
  // We should now have a, b and c, as well as f(a), f(b), f(c),
  // where b gives the minimum energy position;
  Float ax, bx, cx;

  this->FindBracketingTriplet(solver, &ax, &bx, &cx);

  const Float R = 0.6180339;
  const Float C = (1.0 - R);

  Float x0 = ax;
  Float x1;
  Float x2;
  Float x3 = cx;
  if( std::fabs(cx - bx) > std::fabs(bx - ax) )
    {
    x1 = bx;
    x2 = bx + C * (cx - bx);
    }
  else
    {
    x2 = bx;
    x1 = bx - C * (bx - ax);
    }

  Float f1 = std::fabs( this->EvaluateResidual(solver, x1) );
  Float f2 = std::fabs( this->EvaluateResidual(solver, x2) );
  unsigned int iters = 0;
  while( std::fabs(x3 - x0) > tol * (std::fabs(x1) + std::fabs(x2) ) && iters < MaxIters )
    {
    iters++;
    if( f2 < f1 )
      {
      x0 = x1; x1 = x2; x2 = R * x1 + C * x3;
      f1 = f2; f2 = std::fabs( this->EvaluateResidual(solver, x2) );
      }
    else
      {
      x3 = x2; x2 = x1; x1 = R * x2 + C * x0;
      f2 = f1; f1 = std::fabs( this->EvaluateResidual(solver, x1) );
      }
    }

  Float xmin, fmin;
  if( f1 < f2 )
    {
    xmin = x1;
    fmin = f1;
    }
  else
    {
    xmin = x2;
    fmin = f2;
    }

  solver->SetEnergyToMin(xmin);
  return std::fabs( static_cast<double>(fmin) );
}

template <typename TMovingImage, typename TFixedImage, typename TFemObject>
void FEMRegistrationFilter<TMovingImage, TFixedImage, TFemObject>::AddLandmark(PointType source, PointType target)
{
  typename LoadLandmark::Pointer newLandmark = LoadLandmark::New();

  vnl_vector<Float> localSource;
  vnl_vector<Float> localTarget;
  localSource.set_size(ImageDimension);
  localTarget.set_size(ImageDimension);
  for( unsigned int i = 0; i < ImageDimension; i++ )
    {
    localSource[i] = source[i];
    localTarget[i] = target[i];
    }

  newLandmark->SetSource( localSource );
  newLandmark->SetTarget( localTarget );
  newLandmark->SetPoint( localSource );

  m_LandmarkArray.push_back( newLandmark );
}

template <typename TMovingImage, typename TFixedImage, typename TFemObject>
void FEMRegistrationFilter<TMovingImage, TFixedImage, TFemObject>::InsertLandmark(unsigned int index, PointType source,
                                                                                  PointType target)
{
  typename LoadLandmark::Pointer newLandmark = LoadLandmark::New();

  vnl_vector<Float> localSource;
  vnl_vector<Float> localTarget;
  localSource.set_size(ImageDimension);
  localTarget.set_size(ImageDimension);
  for( unsigned int i = 0; i < ImageDimension; i++ )
    {
    localSource[i] = source[i];
    localTarget[i] = target[i];
    }

  newLandmark->SetSource( localSource );
  newLandmark->SetTarget( localTarget );
  newLandmark->SetPoint( localSource );

  m_LandmarkArray.insert( m_LandmarkArray.begin() + index, newLandmark );
}

template <typename TMovingImage, typename TFixedImage, typename TFemObject>
void FEMRegistrationFilter<TMovingImage, TFixedImage, TFemObject>::DeleteLandmark(unsigned int index)
{
  m_LandmarkArray.erase( m_LandmarkArray.begin() + index );
}

template <typename TMovingImage, typename TFixedImage, typename TFemObject>
void FEMRegistrationFilter<TMovingImage, TFixedImage, TFemObject>::ClearLandmarks()
{
  m_LandmarkArray.clear();
}

template <typename TMovingImage, typename TFixedImage, typename TFemObject>
void FEMRegistrationFilter<TMovingImage, TFixedImage, TFemObject>::GetLandmark(unsigned int index, PointType& source,
                                                                               PointType& target)
{
  Element::VectorType localSource;
  Element::VectorType localTarget;

  localTarget = m_LandmarkArray[index]->GetTarget();
  localSource = m_LandmarkArray[index]->GetSource();
  for( unsigned int i = 0; i < ImageDimension; i++ )
    {
    source[i] = localSource[i];
    target[i] = localTarget[i];
    }
}

template <typename TMovingImage, typename TFixedImage, typename TFemObject>
void FEMRegistrationFilter<TMovingImage, TFixedImage, TFemObject>::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf( os, indent );

  os << indent << "Min E: " << m_MinE << std::endl;
  os << indent << "Current Level: " << m_CurrentLevel << std::endl;
  os << indent << "Max Iterations: " << m_Maxiters << std::endl;
  os << indent << "Max Level: " << m_MaxLevel << std::endl;
  os << indent << "Total Iterations: " << m_TotalIterations << std::endl;

  os << indent << "Descent Direction: " << m_DescentDirection << std::endl;
  os << indent << "E: " << m_E << std::endl;
  os << indent << "Gamma: " << m_Gamma << std::endl;
  os << indent << "Rho: " << m_Rho << std::endl;
  os << indent << "Time Step: " << m_TimeStep << std::endl;
  os << indent << "Alpha: " << m_Alpha << std::endl;

  os << indent << "Smoothing Standard Deviation: " << m_StandardDeviations << std::endl;
  os << indent << "Maximum Error: " << m_MaximumError << std::endl;
  os << indent << "Maximum Kernel Width: " << m_MaximumKernelWidth << std::endl;

  os << indent << "Pixels Per Element: " << m_MeshPixelsPerElementAtEachResolution << std::endl;
  os << indent << "Number of Integration Points: " << m_NumberOfIntegrationPoints << std::endl;
  os << indent << "Metric Width: " << m_MetricWidth << std::endl;
  os << indent << "Line Search Energy = " << m_DoLineSearchOnImageEnergy << std::endl;
  os << indent << "Line Search Maximum Iterations: " << m_LineSearchMaximumIterations << std::endl;
  os << indent << "Use Mass Matrix: " << m_UseMassMatrix << std::endl;
  os << indent << "Employ Regridding: " << m_EmployRegridding << std::endl;

  os << indent << "Use Landmarks: " << m_UseLandmarks << std::endl;
  os << indent << "Use Normalized Gradient: " << m_UseNormalizedGradient << std::endl;
  os << indent << "Min Jacobian = " << m_MinJacobian << std::endl;

  os << indent << "Image Scaling: " << m_ImageScaling << std::endl;
  os << indent << "Current Image Scaling: " << m_CurrentImageScaling << std::endl;
  os << indent << "Full Image Size: " << m_FullImageSize << std::endl;
  os << indent << "Image Origin: " << m_ImageOrigin << std::endl;
  os << indent << "Create Mesh: " << m_CreateMeshFromImage << std::endl;

  itkPrintSelfObjectMacro( Field );
  itkPrintSelfObjectMacro( TotalField );
  itkPrintSelfObjectMacro( Load );
  itkPrintSelfObjectMacro( WarpedImage );
  itkPrintSelfObjectMacro( FEMObject );
  itkPrintSelfObjectMacro( Interpolator );
}

}
}  // end namespace itk::fem

#endif
