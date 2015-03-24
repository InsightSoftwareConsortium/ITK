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
#ifndef itkNonUniformBSpline_hxx
#define itkNonUniformBSpline_hxx
#if !defined( ITK_LEGACY_REMOVE )

#include "itkNonUniformBSpline.h"
#include "itkIntTypes.h"

#include "vnl/vnl_vector.h"
#include "vnl/vnl_matrix.h"
#include "vnl/algo/vnl_lsqr.h"
#include "vnl/vnl_linear_system.h"

// #define DEBUG_SPLINE

namespace itk
{
/** Constructor */
template< unsigned int TDimension >
NonUniformBSpline< TDimension >
::NonUniformBSpline()
{
  // Cubic bspline => 4th order
  m_SplineOrder = 3;
  m_SpatialDimension = TDimension;
}

/** Destructor */
template< unsigned int TDimension >
NonUniformBSpline< TDimension >
::~NonUniformBSpline()
{}

/** Print the object */
template< unsigned int TDimension >
void
NonUniformBSpline< TDimension >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "NonUniformBSpline(" << this << ")" << std::endl;

  os << indent << "Chord lengths : " << std::endl;
  for ( ChordLengthListType::const_iterator iter = m_CumulativeChordLength.begin();
        iter != m_CumulativeChordLength.end();
        iter++ )
    {
    os << indent << indent << *iter << std::endl;
    }
  os << indent << "Knots : " << std::endl;
  for ( KnotListType::const_iterator kiter = m_Knots.begin();
        kiter != m_Knots.end();
        kiter++ )
    {
    os << indent << indent << *kiter << std::endl;
    }
  os << indent << "Control Points : " << std::endl;
  for ( typename ControlPointListType::const_iterator cpiter = m_ControlPoints.begin();
        cpiter != m_ControlPoints.end();
        cpiter++ )
    {
    os << indent << indent << *cpiter << std::endl;
    }
}

/** Set the list of points composing the tube */
template< unsigned int TDimension >
void
NonUniformBSpline< TDimension >
::SetPoints(PointListType & points)
{
  m_Points.clear();

  typename PointListType::iterator it, end;
  it = points.begin();
  end = points.end();
  while ( it != end )
    {
    m_Points.push_back(*it);
    it++;
    }

  this->Modified();
}

/** Set the list of points composing the tube */
template< unsigned int TDimension >
void
NonUniformBSpline< TDimension >
::SetKnots(KnotListType & knots)
{
  m_Knots.clear();

  int    len = knots.size();
  double max_knot = knots[len - 1];

  typename KnotListType::iterator it;
  typename KnotListType::iterator end;

  it = knots.begin();
  end = knots.end();

  while ( it != end )
    {
    m_Knots.push_back(*it / max_knot);
    it++;
    }

  this->Modified();
}

template< unsigned int TDimension >
double
NonUniformBSpline< TDimension >
::NonUniformBSplineFunctionRecursive(unsigned int order, unsigned int i, double t) const
{
  if ( order == 1 )
    {
    if ( m_Knots[i] <= t && t < m_Knots[i + 1] )
      {
      return 1;
      }
    else
      {
      return 0;
      }
    }

  //
  // Be careful, we must use the passed in parameter for the order since this
  // function is recursive.
  //
  double numer1 = ( t - m_Knots[i] ) * NonUniformBSplineFunctionRecursive(order - 1, i, t);
  double denom1 = ( m_Knots[i + order - 1] - m_Knots[i] );
  double val1 = numer1 / denom1;
  if ( denom1 == 0 && numer1 == 0 )
    {
    val1 = 0;
    }
  else if ( denom1 == 0 )
    {
    std::cout << "Error : " << denom1 << ", " << numer1 << std::endl;
    }

  double numer2 = ( m_Knots[i + order] - t ) * NonUniformBSplineFunctionRecursive(order - 1, i + 1, t);
  double denom2 = ( m_Knots[i + order] - m_Knots[i + 1] );
  double val2 = numer2 / denom2;
  if ( denom2 == 0 && numer2 == 0 )
    {
    val2 = 0;
    }
  else if ( denom2 == 0 )
    {
    std::cout << "Error : " << denom2 << ", " << numer2 << std::endl;
    }

  return val1 + val2;
}

template< unsigned int TDimension >
void
NonUniformBSpline< TDimension >
::ComputeChordLengths()
{
  m_ChordLength.clear();
  m_CumulativeChordLength.clear();

  m_ChordLength.push_back(0);
  m_CumulativeChordLength.push_back(0);

  double              total_chord_length = 0.0;
  ChordLengthListType temp;

  const SizeValueType numberOfPoints = static_cast< SizeValueType >( m_Points.size() );

  for ( SizeValueType i = 0; i <  numberOfPoints - 1; i++ )
    {
    PointType pt = m_Points[i];
    PointType pt2 = m_Points[i + 1];

    double chord = pt.EuclideanDistanceTo(pt2);
    m_ChordLength.push_back(chord);
    total_chord_length = total_chord_length + chord;
    temp.push_back(total_chord_length);
    }

  for ( ChordLengthListType::iterator aiter = temp.begin();
        aiter != temp.end();
        aiter++ )
    {
    m_CumulativeChordLength.push_back(*aiter / total_chord_length);
    }

  //
  // Debug printouts
  //
#ifdef DEBUG_SPLINE
  std::cout << "Total chord length : " << total_chord_length << std::endl;

  std::cout << "Chord length : " << std::endl;
  for ( ChordLengthListType::iterator aiter2 = m_ChordLength.begin();
        aiter2 != m_ChordLength.end();
        aiter2++ )
    {
    std::cout << *aiter2 << std::endl;
    }

  std::cout << "Cumulative chord length : " << std::endl;
  for ( ChordLengthListType::iterator aiter3 = m_CumulativeChordLength.begin();
        aiter3 != m_CumulativeChordLength.end();
        aiter3++ )
    {
    std::cout << *aiter3 << std::endl;
    }
  std::cout << std::endl;
#endif
}

template< unsigned int TDimension >
void
NonUniformBSpline< TDimension >
::SetControlPoints(ControlPointListType & ctrlpts)
{
  m_ControlPoints.clear();
  for ( typename ControlPointListType::iterator iter = ctrlpts.begin();
        iter != ctrlpts.end();
        iter++ )
    {
    m_ControlPoints.push_back(*iter);
    }
  this->Modified();
}

template< unsigned int TDimension >
const typename
NonUniformBSpline< TDimension >::ControlPointListType &
NonUniformBSpline< TDimension >::GetControlPoints() const
{
  return this->m_ControlPoints;
}

template< unsigned int TDimension >
const typename
NonUniformBSpline< TDimension >::KnotListType &
NonUniformBSpline< TDimension >::GetKnots() const
{
  return this->m_Knots;
}

template< unsigned int TDimension >
const typename
NonUniformBSpline< TDimension >::PointListType &
NonUniformBSpline< TDimension >::GetPoints() const
{
  return this->m_Points;
}

template< unsigned int TDimension >
void
NonUniformBSpline< TDimension >::ComputeControlPoints()
{
  unsigned int dim = m_Points[0].GetPointDimension();

#ifdef DEBUG_SPLINE
  std::cout << "Points have dimension : " << dim  << std::endl;
#endif

  //
  // +1 in cols for radius
  //
  vnl_matrix< double > data_matrix(m_Points.size(), dim);

  //
  // Form data point matrix
  //
  int rr = 0;
  for ( typename PointListType::iterator iter = m_Points.begin();
        iter != m_Points.end();
        iter++ )
    {
    PointType pt = ( *iter );
    for ( unsigned int i = 0; i < dim; i++ )
      {
      data_matrix(rr, i) = pt.GetVnlVector()[i];
      }
    rr++;
    }

#ifdef DEBUG_SPLINE
  std::cout << std::endl << "Data matrix" << std::endl;
  std::cout << data_matrix << std::endl;
#endif

  //
  // Form basis function matrix
  //
  //int num_basis_functions = 2 * m_SplineOrder - 1;
  //int num_basis_functions = m_Points.size();
  int num_rows = m_Points.size();

  //
  // Assumes multiplicity k (m_SplineOrder at the ends).
  //
  int num_cols = m_Knots.size() - m_SplineOrder;

  vnl_matrix< double > N_matrix(num_rows, num_cols);

  //N_matrix(0, 0) = 1.0;

  for ( int r = 0; r < num_rows; r++ )
    {
    for ( int c = 0; c < num_cols; c++ )
      {
      double t = m_CumulativeChordLength[r];
      N_matrix(r, c) = NonUniformBSplineFunctionRecursive(m_SplineOrder, c, t);
      }
    }

  N_matrix(num_rows - 1, num_cols - 1) = 1.0;

#ifdef DEBUG_SPLINE
  std::cout << "Basis function matrix : " << std::endl;
  std::cout << N_matrix << std::endl;
#endif

//FIXME: Use the LSQR linear solver here:
  vnl_matrix< double > B;

// = vnl_matrix_inverse<double>(N_matrix.transpose() * N_matrix) *
// N_matrix.transpose() * data_matrix;

//  vnl_linear_system ls( N_matrix.rows(), N_matrix.cols() );

//  vnl_lsqr solver( ls );

//#ifdef DEBUG_SPLINE
  std::cout << "Control point matrix : " << std::endl;
  std::cout << B << std::endl;
//#endif

  m_ControlPoints.clear();

  for ( unsigned int j = 0; j < B.rows(); j++ )
    {
    vnl_vector< double >  v = B.get_row(j);
    itk::Vector< double > iv;
    iv.SetVnlVector(v);
    itk::Point< double, TDimension > pt;
    for ( unsigned int d = 0; d < dim; d++ )
      {
      pt[d] = v(d);
      }
    m_ControlPoints.push_back(pt);
    }
}

template< unsigned int TDimension >
typename NonUniformBSpline< TDimension >::PointType
NonUniformBSpline< TDimension >
::EvaluateSpline(const itk::Array< double > & p) const
{
  double t = p[0];

  return EvaluateSpline(t);
}

template< unsigned int TDimension >
typename NonUniformBSpline< TDimension >::PointType
NonUniformBSpline< TDimension >
::EvaluateSpline(double t) const
{
  int i = 0;

  vnl_vector< double > result(TDimension);
  result.fill(0);

  for ( typename ControlPointListType::const_iterator cpiter = m_ControlPoints.begin();
        cpiter != m_ControlPoints.end();
        cpiter++ )
    {
    ControlPointType     pt = *cpiter;
    vnl_vector< double > v = pt.GetVnlVector();

    const double N =  this->NonUniformBSplineFunctionRecursive(m_SplineOrder, i, t);

    for ( unsigned j = 0; j < TDimension; j++ )
      {
      result[j] += N * v[j];
      }

    i++;
    }

  double array[TDimension];
  for ( unsigned int d = 0; d < TDimension; d++ )
    {
    array[d] = result[d];
    }

  ControlPointType sum(array);
#ifdef DEBUG_SPLINE
  std::cout << "Result : " << result << std::endl;
  std::cout << "Sum : " << sum << std::endl;
#endif

  return sum;
}
} // end namespace itk

#endif //#if !defined( ITK_LEGACY_REMOVE )
#endif
