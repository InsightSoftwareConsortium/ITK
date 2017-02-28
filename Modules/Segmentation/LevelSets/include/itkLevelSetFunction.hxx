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
#ifndef itkLevelSetFunction_hxx
#define itkLevelSetFunction_hxx

#include "itkLevelSetFunction.h"
#include "vnl/algo/vnl_symmetric_eigensystem.h"

namespace itk
{
template< typename TImageType >
typename LevelSetFunction< TImageType >::ScalarValueType
LevelSetFunction< TImageType >::ComputeCurvatureTerm(const NeighborhoodType & neighborhood,
                                                     const FloatOffsetType & offset, GlobalDataStruct *gd)
{
  if ( m_UseMinimalCurvature == false )
    {
    return this->ComputeMeanCurvature(neighborhood, offset, gd);
    }
  else
    {
    if ( ImageDimension == 3 )
      {
      return this->ComputeMinimalCurvature(neighborhood, offset, gd);
      }
    else if ( ImageDimension == 2 )
      {
      return this->ComputeMeanCurvature(neighborhood, offset, gd);
      }
    else
      {
      return this->ComputeMinimalCurvature(neighborhood, offset, gd);
      }
    }
}

template< typename TImageType >
typename LevelSetFunction< TImageType >::ScalarValueType
LevelSetFunction< TImageType >
::ComputeMinimalCurvature(
  const NeighborhoodType & itkNotUsed(neighborhood),
  const FloatOffsetType & itkNotUsed(offset), GlobalDataStruct *gd)
{
  unsigned int          i, j, n;
  ScalarValueType       gradMag = std::sqrt(gd->m_GradMagSqr);
  ScalarValueType       Pgrad[ImageDimension][ImageDimension];
  ScalarValueType       tmp_matrix[ImageDimension][ImageDimension];
  const ScalarValueType ZERO = NumericTraits< ScalarValueType >::ZeroValue();

  vnl_matrix_fixed< ScalarValueType, ImageDimension, ImageDimension > Curve;
  const ScalarValueType                                               MIN_EIG = NumericTraits< ScalarValueType >::min();

  ScalarValueType mincurve;

  for ( i = 0; i < ImageDimension; i++ )
    {
    Pgrad[i][i] = 1.0 - gd->m_dx[i] * gd->m_dx[i] / gradMag;
    for ( j = i + 1; j < ImageDimension; j++ )
      {
      Pgrad[i][j] = gd->m_dx[i] * gd->m_dx[j] / gradMag;
      Pgrad[j][i] = Pgrad[i][j];
      }
    }

  //Compute Pgrad * Hessian * Pgrad
  for ( i = 0; i < ImageDimension; i++ )
    {
    for ( j = i; j < ImageDimension; j++ )
      {
      tmp_matrix[i][j] = ZERO;
      for ( n = 0; n < ImageDimension; n++ )
        {
        tmp_matrix[i][j] += Pgrad[i][n] * gd->m_dxy[n][j];
        }
      tmp_matrix[j][i] = tmp_matrix[i][j];
      }
    }

  for ( i = 0; i < ImageDimension; i++ )
    {
    for ( j = i; j < ImageDimension; j++ )
      {
      Curve(i, j) = ZERO;
      for ( n = 0; n < ImageDimension; n++ )
        {
        Curve(i, j) += tmp_matrix[i][n] * Pgrad[n][j];
        }
      Curve(j, i) = Curve(i, j);
      }
    }

  //Eigensystem
  vnl_symmetric_eigensystem< ScalarValueType > eig(Curve);

  mincurve = itk::Math::abs( eig.get_eigenvalue(ImageDimension - 1) );
  for ( i = 0; i < ImageDimension; i++ )
    {
    if ( itk::Math::abs( eig.get_eigenvalue(i) ) < mincurve
         && itk::Math::abs( eig.get_eigenvalue(i) ) > MIN_EIG )
      {
      mincurve = itk::Math::abs( eig.get_eigenvalue(i) );
      }
    }

  return ( mincurve / gradMag );
}

template< typename TImageType >
typename LevelSetFunction< TImageType >::ScalarValueType
LevelSetFunction< TImageType >
::Compute3DMinimalCurvature(const NeighborhoodType & neighborhood,
                            const FloatOffsetType & offset, GlobalDataStruct *gd)
{
  ScalarValueType mean_curve = this->ComputeMeanCurvature(neighborhood, offset, gd);

  int             i0 = 0, i1 = 1, i2 = 2;
  ScalarValueType gauss_curve =
    ( 2 * ( gd->m_dx[i0] * gd->m_dx[i1] * ( gd->m_dxy[i2][i0]
                                            * gd->m_dxy[i1][i2] - gd->m_dxy[i0][i1] * gd->m_dxy[i2][i2] )
            + gd->m_dx[i1] * gd->m_dx[i2] * ( gd->m_dxy[i2][i0]
                                              * gd->m_dxy[i0][i1] - gd->m_dxy[i1][i2] * gd->m_dxy[i0][i0] )
            + gd->m_dx[i0] * gd->m_dx[i2] * ( gd->m_dxy[i1][i2]
                                              * gd->m_dxy[i0][i1] - gd->m_dxy[i2][i0] * gd->m_dxy[i1][i1] ) )
      + gd->m_dx[i0] * gd->m_dx[i0] * ( gd->m_dxy[i1][i1]
                                        * gd->m_dxy[i2][i2] - gd->m_dxy[i1][i2] * gd->m_dxy[i1][i2] )
      + gd->m_dx[i1] * gd->m_dx[i1] * ( gd->m_dxy[i0][i0]
                                        * gd->m_dxy[i2][i2] - gd->m_dxy[i2][i0] * gd->m_dxy[i2][i0] )
      + gd->m_dx[i2] * gd->m_dx[i2] * ( gd->m_dxy[i1][i1]
                                        * gd->m_dxy[i0][i0] - gd->m_dxy[i0][i1] * gd->m_dxy[i0][i1] ) )
    / ( gd->m_dx[i0] * gd->m_dx[i0] + gd->m_dx[i1] * gd->m_dx[i1] + gd->m_dx[i2] * gd->m_dx[i2] );

  ScalarValueType discriminant = mean_curve * mean_curve - gauss_curve;

  if ( discriminant < 0.0 )
    {
    discriminant = 0.0;
    }
  discriminant = std::sqrt(discriminant);
  return  ( mean_curve - discriminant );
}

template< typename TImageType >
typename LevelSetFunction< TImageType >::ScalarValueType
LevelSetFunction< TImageType >::ComputeMeanCurvature(
  const NeighborhoodType & itkNotUsed(neighborhood),
  const FloatOffsetType & itkNotUsed(offset), GlobalDataStruct *gd)
{
  // Calculate the mean curvature
  ScalarValueType curvature_term = NumericTraits< ScalarValueType >::ZeroValue();
  unsigned int    i, j;

  for ( i = 0; i < ImageDimension; i++ )
    {
    for ( j = 0; j < ImageDimension; j++ )
      {
      if ( j != i )
        {
        curvature_term -= gd->m_dx[i] * gd->m_dx[j] * gd->m_dxy[i][j];
        curvature_term += gd->m_dxy[j][j] * gd->m_dx[i] * gd->m_dx[i];
        }
      }
    }

  return ( curvature_term / gd->m_GradMagSqr );
}

template< typename TImageType >
typename LevelSetFunction< TImageType >::VectorType
LevelSetFunction< TImageType >::InitializeZeroVectorConstant()
{
  VectorType ans;

  for ( unsigned int i = 0; i < ImageDimension; ++i )
    {
    ans[i] = NumericTraits< ScalarValueType >::ZeroValue();
    }

  return ans;
}

template< typename TImageType >
typename LevelSetFunction< TImageType >::VectorType
LevelSetFunction< TImageType >::m_ZeroVectorConstant =
  LevelSetFunction< TImageType >::InitializeZeroVectorConstant();

template< typename TImageType >
void
LevelSetFunction< TImageType >::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "WaveDT: " << m_WaveDT << std::endl;
  os << indent << "DT: " << m_DT << std::endl;
  os << indent << "UseMinimalCurvature " << m_UseMinimalCurvature << std::endl;
  os << indent << "EpsilonMagnitude: " << m_EpsilonMagnitude << std::endl;
  os << indent << "AdvectionWeight: " << m_AdvectionWeight << std::endl;
  os << indent << "PropagationWeight: " << m_PropagationWeight << std::endl;
  os << indent << "CurvatureWeight: " << m_CurvatureWeight << std::endl;
  os << indent << "LaplacianSmoothingWeight: " << m_LaplacianSmoothingWeight << std::endl;
}

template< typename TImageType >
double LevelSetFunction< TImageType >::m_WaveDT = 1.0 / ( 2.0 * ImageDimension );

template< typename TImageType >
double LevelSetFunction< TImageType >::m_DT     = 1.0 / ( 2.0 * ImageDimension );

template< typename TImageType >
typename LevelSetFunction< TImageType >::TimeStepType
LevelSetFunction< TImageType >
::ComputeGlobalTimeStep(void *GlobalData) const
{
  TimeStepType dt;

  GlobalDataStruct *d = (GlobalDataStruct *)GlobalData;

  d->m_MaxAdvectionChange += d->m_MaxPropagationChange;

  if ( itk::Math::abs(d->m_MaxCurvatureChange) > 0.0 )
    {
    if ( d->m_MaxAdvectionChange > 0.0 )
      {
      dt = std::min( ( m_WaveDT / d->m_MaxAdvectionChange ),
                         (    m_DT / d->m_MaxCurvatureChange ) );
      }
    else
      {
      dt = m_DT / d->m_MaxCurvatureChange;
      }
    }
  else
    {
    if ( d->m_MaxAdvectionChange > 0.0 )
      {
      dt = m_WaveDT / d->m_MaxAdvectionChange;
      }
    else
      {
      dt = 0.0;
      }
    }

  double maxScaleCoefficient = 0.0;
  for ( unsigned int i = 0; i < ImageDimension; i++ )
    {
    maxScaleCoefficient = std::max(this->m_ScaleCoefficients[i], maxScaleCoefficient);
    }
  dt /= maxScaleCoefficient;

  // reset the values
  d->m_MaxAdvectionChange   = NumericTraits< ScalarValueType >::ZeroValue();
  d->m_MaxPropagationChange = NumericTraits< ScalarValueType >::ZeroValue();
  d->m_MaxCurvatureChange   = NumericTraits< ScalarValueType >::ZeroValue();

  return dt;
}

template< typename TImageType >
void
LevelSetFunction< TImageType >
::Initialize(const RadiusType & r)
{
  this->SetRadius(r);

  // Dummy neighborhood.
  NeighborhoodType it;
  it.SetRadius(r);

  // Find the center index of the neighborhood.
  m_Center =  it.Size() / 2;

  // Get the stride length for each axis.
  for ( unsigned int i = 0; i < ImageDimension; i++ )
    {
    m_xStride[i] = it.GetStride(i);
    }
}

template< typename TImageType >
typename LevelSetFunction< TImageType >::PixelType
LevelSetFunction< TImageType >
::ComputeUpdate(const NeighborhoodType & it, void *globalData,
                const FloatOffsetType & offset)
{
  unsigned int          i, j;
  const ScalarValueType ZERO = NumericTraits< ScalarValueType >::ZeroValue();
  const ScalarValueType center_value  = it.GetCenterPixel();

  const NeighborhoodScalesType neighborhoodScales = this->ComputeNeighborhoodScales();

  ScalarValueType laplacian, x_energy, laplacian_term, propagation_term,
                  curvature_term, advection_term, propagation_gradient;
  VectorType advection_field;

  // Global data structure
  GlobalDataStruct *gd = (GlobalDataStruct *)globalData;

  // Compute the Hessian matrix and various other derivatives.  Some of these
  // derivatives may be used by overloaded virtual functions.
  gd->m_GradMagSqr = 1.0e-6;
  for ( i = 0; i < ImageDimension; i++ )
    {
    const unsigned int positionA =
      static_cast< unsigned int >( m_Center + m_xStride[i] );
    const unsigned int positionB =
      static_cast< unsigned int >( m_Center - m_xStride[i] );

    gd->m_dx[i] = 0.5 * ( it.GetPixel(positionA)
                          - it.GetPixel(positionB) ) * neighborhoodScales[i];
    gd->m_dxy[i][i] = ( it.GetPixel(positionA)
                        + it.GetPixel(positionB) - 2.0 * center_value )
                      * itk::Math::sqr(neighborhoodScales[i]);

    gd->m_dx_forward[i]  = ( it.GetPixel(positionA) - center_value ) * neighborhoodScales[i];

    gd->m_dx_backward[i] = ( center_value - it.GetPixel(positionB) ) * neighborhoodScales[i];

    gd->m_GradMagSqr += gd->m_dx[i] * gd->m_dx[i];

    for ( j = i + 1; j < ImageDimension; j++ )
      {
      const unsigned int positionAa = static_cast< unsigned int >(
        m_Center - m_xStride[i] - m_xStride[j] );
      const unsigned int positionBa = static_cast< unsigned int >(
        m_Center - m_xStride[i] + m_xStride[j] );
      const unsigned int positionCa = static_cast< unsigned int >(
        m_Center + m_xStride[i] - m_xStride[j] );
      const unsigned int positionDa = static_cast< unsigned int >(
        m_Center + m_xStride[i] + m_xStride[j] );

      gd->m_dxy[i][j] = gd->m_dxy[j][i] = 0.25 * ( it.GetPixel(positionAa)
                                                   - it.GetPixel(positionBa)
                                                   - it.GetPixel(positionCa)
                                                   + it.GetPixel(positionDa) )
                                          * neighborhoodScales[i] * neighborhoodScales[j];
      }
    }

  if ( Math::NotAlmostEquals(m_CurvatureWeight, ZERO) )
    {
    curvature_term = this->ComputeCurvatureTerm(it, offset, gd) * m_CurvatureWeight
                     * this->CurvatureSpeed(it, offset);

    gd->m_MaxCurvatureChange = std::max( gd->m_MaxCurvatureChange,
                                             itk::Math::abs(curvature_term) );
    }
  else
    {
    curvature_term = ZERO;
    }

  // Calculate the advection term.
  //  $\alpha \stackrel{\rightharpoonup}{F}(\mathbf{x})\cdot\nabla\phi $
  //
  // Here we can use a simple upwinding scheme since we know the
  // sign of each directional component of the advective force.
  //
  if ( Math::NotAlmostEquals(m_AdvectionWeight, ZERO) )
    {
    advection_field = this->AdvectionField(it, offset, gd);
    advection_term = ZERO;

    for ( i = 0; i < ImageDimension; i++ )
      {
      x_energy = m_AdvectionWeight * advection_field[i];

      if ( x_energy > ZERO )
        {
        advection_term += advection_field[i] * gd->m_dx_backward[i];
        }
      else
        {
        advection_term += advection_field[i] * gd->m_dx_forward[i];
        }

      gd->m_MaxAdvectionChange =
        std::max( gd->m_MaxAdvectionChange, itk::Math::abs(x_energy) );
      }
    advection_term *= m_AdvectionWeight;
    }
  else
    {
    advection_term = ZERO;
    }

  if ( Math::NotAlmostEquals(m_PropagationWeight, ZERO) )
    {
    // Get the propagation speed
    propagation_term = m_PropagationWeight * this->PropagationSpeed(it, offset, gd);

    //
    // Construct upwind gradient values for use in the propagation speed term:
    //  $\beta G(\mathbf{x})\mid\nabla\phi\mid$
    //
    // The following scheme for "upwinding" in the normal direction is taken
    // from Sethian, Ch. 6 as referenced above.
    //
    propagation_gradient = ZERO;

    if ( propagation_term > ZERO )
      {
      for ( i = 0; i < ImageDimension; i++ )
        {
        propagation_gradient += itk::Math::sqr( std::max(gd->m_dx_backward[i], ZERO) )
                                + itk::Math::sqr( std::min(gd->m_dx_forward[i],  ZERO) );
        }
      }
    else
      {
      for ( i = 0; i < ImageDimension; i++ )
        {
        propagation_gradient += itk::Math::sqr( std::min(gd->m_dx_backward[i], ZERO) )
                                + itk::Math::sqr( std::max(gd->m_dx_forward[i],  ZERO) );
        }
      }

    // Collect energy change from propagation term.  This will be used in
    // calculating the maximum time step that can be taken for this iteration.
    gd->m_MaxPropagationChange =
      std::max( gd->m_MaxPropagationChange,
                    itk::Math::abs(propagation_term) );

    propagation_term *= std::sqrt(propagation_gradient);
    }
  else { propagation_term = ZERO; }

  if ( Math::NotAlmostEquals(m_LaplacianSmoothingWeight, ZERO) )
    {
    laplacian = ZERO;

    // Compute the laplacian using the existing second derivative values
    for ( i = 0; i < ImageDimension; i++ )
      {
      laplacian += gd->m_dxy[i][i];
      }

    // Scale the laplacian by its speed and weight
    laplacian_term =
      laplacian * m_LaplacianSmoothingWeight * LaplacianSmoothingSpeed(it, offset, gd);
    }
  else
    {
    laplacian_term = ZERO;
    }
  // Return the combination of all the terms.
  return (PixelType)( curvature_term - propagation_term
                      - advection_term - laplacian_term );
}
} // end namespace itk

#endif
