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
#ifndef itkWindowConvergenceMonitoringFunction_hxx
#define itkWindowConvergenceMonitoringFunction_hxx

#include "itkWindowConvergenceMonitoringFunction.h"

#include "itkBSplineControlPointImageFunction.h"
#include "itkBSplineScatteredDataPointSetToImageFilter.h"
#include "itkImage.h"
#include "itkPointSet.h"
#include "itkVector.h"

namespace itk
{
namespace Function
{

template<typename TScalar>
WindowConvergenceMonitoringFunction<TScalar>
::WindowConvergenceMonitoringFunction() :
  m_WindowSize( 10 ),
  m_TotalEnergy( 0 )
{}

template<typename TScalar>
WindowConvergenceMonitoringFunction<TScalar>
::~WindowConvergenceMonitoringFunction()
{}

template<typename TScalar>
void
WindowConvergenceMonitoringFunction<TScalar>
::AddEnergyValue( const EnergyValueType value )
{
  itkDebugMacro( "Adding energy value " << value );

  this->m_EnergyValues.push_back( value );
  if( this->GetNumberOfEnergyValues() > this->m_WindowSize )
    {
    this->m_EnergyValues.pop_front();
    }
  this->m_TotalEnergy += itk::Math::abs( value );

  this->Modified();
}

template<typename TScalar>
void
WindowConvergenceMonitoringFunction<TScalar>
::ClearEnergyValues()
{
  Superclass::ClearEnergyValues();
  this->m_TotalEnergy = NumericTraits<RealType>::ZeroValue();
}

template<typename TScalar>
typename WindowConvergenceMonitoringFunction<TScalar>::RealType
WindowConvergenceMonitoringFunction<TScalar>
::GetConvergenceValue() const
{
  if( this->GetNumberOfEnergyValues() < this->m_WindowSize )
    {
    return NumericTraits<RealType>::max();
    }

  typedef Vector<RealType, 1>                     ProfilePointDataType;
  typedef Image<ProfilePointDataType, 1>          CurveType;
  typedef PointSet<ProfilePointDataType, 1>       EnergyProfileType;
  typedef typename EnergyProfileType::PointType   ProfilePointType;

  typename CurveType::PointType    origin;
  typename CurveType::SizeType     size;
  typename CurveType::SpacingType  spacing;

  origin[0] = 0.0;
  size[0] = 11;
  spacing[0] = 0.1;

  typedef BSplineScatteredDataPointSetToImageFilter<EnergyProfileType, CurveType> BSplinerType;
  typename BSplinerType::Pointer bspliner = BSplinerType::New();
  bspliner->SetOrigin( origin );
  bspliner->SetSpacing( spacing );
  bspliner->SetSize( size );
  bspliner->SetNumberOfLevels( 1 );
  bspliner->SetSplineOrder( 1 );
  typename BSplinerType::ArrayType ncps;
  ncps.Fill( bspliner->GetSplineOrder()[0] + 1 );
  bspliner->SetNumberOfControlPoints( ncps );

  typename EnergyProfileType::Pointer energyProfileWindow = EnergyProfileType::New();
  energyProfileWindow->Initialize();

  for( unsigned int n = 0; n < this->m_WindowSize; n++ )
    {
    ProfilePointType windowPoint;
    windowPoint[0] = static_cast<typename ProfilePointType::CoordRepType>( n ) /
      static_cast<typename ProfilePointType::CoordRepType>( this->m_WindowSize - 1 );
    energyProfileWindow->SetPoint( n, windowPoint );
    energyProfileWindow->SetPointData( n, ProfilePointDataType( this->m_EnergyValues[n] / this->m_TotalEnergy ) );
    }

  bspliner->SetInput( energyProfileWindow );
  bspliner->Update();

  typedef BSplineControlPointImageFunction<CurveType> BSplinerFunctionType;
  typename BSplinerFunctionType::Pointer bsplinerFunction = BSplinerFunctionType::New();
  bsplinerFunction->SetOrigin( origin );
  bsplinerFunction->SetSpacing( spacing );
  bsplinerFunction->SetSize( size );
  bsplinerFunction->SetSplineOrder( bspliner->GetSplineOrder() );
  bsplinerFunction->SetInputImage( bspliner->GetPhiLattice() );

  ProfilePointType endPoint;
  endPoint[0] = NumericTraits<RealType>::OneValue();
  typename BSplinerFunctionType::GradientType gradient =
    bsplinerFunction->EvaluateGradientAtParametricPoint( endPoint );

  RealType convergenceValue = -gradient[0][0];

  return convergenceValue;
}

/**
 * Standard "PrintSelf" method
 */
template<typename TScalar>
void
WindowConvergenceMonitoringFunction<TScalar>
::PrintSelf( std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf( os, indent );

  os << "Window size: " << this->m_WindowSize << std::endl;
}

} // end namespace function
} // end namespace itk

#endif
