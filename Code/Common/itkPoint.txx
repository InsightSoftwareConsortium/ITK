/*==========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkPoint.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$
Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

=========================================================================*/
#ifndef _itkPoint_txx
#define _itkPoint_txx
#include "itkPoint.h" 
#include <vnl/vnl_math.h>



namespace itk
{


/*
 * Assignment Operator
 */
template<class T, unsigned int TPointDimension>
Point<T, TPointDimension>&
Point<T, TPointDimension>
::operator= (const Self& r)
{
  BaseArray::operator=(r);
  return *this;
}


/*
 * Assignment from a Base Array
 */
template<class T, unsigned int TPointDimension>
Point<T, TPointDimension>&
Point<T, TPointDimension>
::operator= (const typename BaseArray::Reference& r)
{
  BaseArray::operator=(r);
  return *this;
}


/*
 * Assignment from a Const Base Array
 */
template<class T, unsigned int TPointDimension>
Point<T, TPointDimension>&
Point<T, TPointDimension>
::operator= (const typename BaseArray::ConstReference& r)
{
  BaseArray::operator=(r);
  return *this;
}


/*
 * Assignemt from a plain array
 */
template<class T, unsigned int TPointDimension>
Point<T, TPointDimension>&
Point<T, TPointDimension>
::operator= (const ValueType r[PointDimension])
{
  BaseArray::operator=(r);
  return *this;
}


/*
 * Assignment from a const scalar value
 */
template<class T, unsigned int TPointDimension>
typename Point<T, TPointDimension>::ArrayCommaListCopier
Point<T, TPointDimension>
::operator= (const ValueType& r)
{
  return BaseArray::operator=(r);
}


/*
 * In place increment by a vector
 */
template<class T, unsigned int TPointDimension>
const Point<T, TPointDimension> &
Point<T, TPointDimension>
::operator+=( const Vector<T, TPointDimension> & vec )
{
  for( unsigned int i=0; i<TPointDimension; i++) 
  {
    (*this)[i] += vec[i];
  }
  return *this;
}

 
/*
 * In place subtract a vector
 */
template<class T, unsigned int TPointDimension>
const Point<T, TPointDimension> &
Point<T, TPointDimension>
::operator-=( const Vector<T, TPointDimension> & vec )
{
  for( unsigned int i=0; i<TPointDimension; i++) 
  {
    (*this)[i] -= vec[i];
  }
  return *this;
}

 

/*
 * Add operator with a vector
 */
template<class T, unsigned int TPointDimension>
Point<T, TPointDimension> 
Point<T, TPointDimension>
::operator+( const Vector<T, TPointDimension> & vec ) const
{
  Self result;
  for( unsigned int i=0; i<TPointDimension; i++) 
  {
    result[i] = (*this)[i] + vec[i];
  }
  return result;
}



/*
 * Subtract a vector, return a point
 */
template<class T, unsigned int TPointDimension>
Point<T, TPointDimension> 
Point<T, TPointDimension>
::operator-( const Vector<T, TPointDimension> & vec )  const
{
  Self result;
  for( unsigned int i=0; i<TPointDimension; i++) 
  {
    result[i] = (*this)[i] - vec[i];
  }
  return result;
}



/*
 * Difference between two points
 */
template<class T, unsigned int TPointDimension>
Vector<T, TPointDimension> 
Point<T, TPointDimension>
::operator-( const Point<T, TPointDimension> & pnt )  const
{
  VectorType result;
  for( unsigned int i=0; i<TPointDimension; i++) 
  {
    result[i] = (*this)[i] - pnt[i];
  }
  return result;
}


/*
 * Return a vnl_vector_ref
 */
template<class T, unsigned int TPointDimension >
vnl_vector_ref< T >
Point<T, TPointDimension>
::Get_vnl_vector( void ) 
{
  vnl_vector_ref< T > vector_ref( TPointDimension, this->GetDataPointer());
  return vector_ref;
}
 

/*
 * Returns Squared Euclidean distance between two points
 */
template<class T, unsigned int TPointDimension>
typename Point<T, TPointDimension>::ValueType
Point<T, TPointDimension>
::SquaredEuclideanDistanceTo( const Point<T, TPointDimension> & pnt )  const
{
  ValueType sum = NumericTraits<ValueType>::Zero;
  for( unsigned int i=0; i<TPointDimension; i++) 
  {
    const ValueType difference = (*this)[i] - pnt[i];
    sum += difference * difference;
  }
  return sum;
}



/*
 * Returns Euclidean distance between two points
 */
template<class T, unsigned int TPointDimension>
typename Point<T, TPointDimension>::ValueType
Point<T, TPointDimension>
::EuclideanDistanceTo( const Point<T, TPointDimension> & pnt )  const
{
  const double distance = sqrt( 
                static_cast<double>( SquaredEuclideanDistanceTo( pnt ) ) ) ;
  return static_cast<ValueType>( distance );
}
 


/*
 * Set the point to the median point of the two arguments
 */
template<class T, unsigned int TPointDimension>
void
Point<T, TPointDimension>
::SetToMidPoint( const Point<T, TPointDimension> & A,
               const Point<T, TPointDimension> & B )  
{
  for( unsigned int i=0; i<TPointDimension; i++) 
  {
    (*this)[i] = ( A[i] + B[i] ) /2.0;
  }
}





/*
 * Set the point to the barycentric combination of two points
 */
template<class T, unsigned int TPointDimension>
void
Point<T, TPointDimension>
::SetToBarycentricCombination( const Point<T, TPointDimension> & A,
                               const Point<T, TPointDimension> & B,
                               double alpha )  
{
  const double wa = alpha;
  const double wb = 1.0 - alpha;
  for( unsigned int i=0; i<TPointDimension; i++) 
  {
    (*this)[i] =  wa * A[i] + wb * B[i];
  }
}



/*
 * Set the point to the barycentric combination of three points
 */
template<class T, unsigned int TPointDimension>
void
Point<T, TPointDimension>
::SetToBarycentricCombination( const Point<T, TPointDimension> & A,
                               const Point<T, TPointDimension> & B,
                               const Point<T, TPointDimension> & C,
                               double weightForA, 
                               double weightForB  )
{
  const double weightForC = 1.0 - weightForA - weightForB;
  for( unsigned int i=0; i<TPointDimension; i++) 
  {
    (*this)[i] =  weightForA * A[i] + weightForB * B[i] + weightForC * C[i];
  }
}



/*
 * Set the point to the barycentric combination of N points
 */
template<class T, unsigned int TPointDimension>
void
Point<T, TPointDimension>
::SetToBarycentricCombination( const Point<T, TPointDimension> * P,
                               const double * weights, unsigned int N )
{
  Fill( NumericTraits<T>::Zero ); // put this point to null
  double weightSum = 0.0;
  for( unsigned int j=0; j<N-1; j++) 
  {
    const double weight = weights[j];
    weightSum += weight;
    for( unsigned int i=0; i<TPointDimension; i++) 
    {
      (*this)[i] +=  weight * P[j][i];
    }
  }
  const double weight = ( 1.0 - weightSum );
  for( unsigned int i=0; i<TPointDimension; i++) 
  {
    (*this)[i] +=  weight * P[N-1][i];
  }

}




/*
 * Set the point to the barycentric combination of N points in a Container
 */
template< class TPointContainer, class TWeightContainer >
typename TPointContainer::Element 
BarycentricCombination<TPointContainer,TWeightContainer>
::Evaluate( const typename TPointContainer::Pointer & points, const TWeightContainer & weights ) 
{
  
  typedef typename TPointContainer::Element   PointType;
  typedef typename PointType::ValueType       ValueType;
  PointType barycentre;
  barycentre.Fill( NumericTraits< ValueType >::Zero ); // set to null

  typename TPointContainer::Iterator point = points->Begin();
  typename TPointContainer::Iterator final = points->End();
  final--; // move to the Nth point
  
  double weightSum = 0.0;

  const unsigned int PointDimension = PointType::PointDimension;
  unsigned int j = 0;

  while( point != final )
  {
    const double weight = weights[j++];
    weightSum += weight;
    for( unsigned int i=0; i<PointDimension; i++) 
    {
      barycentre[i] +=  weight * (point->Value())[i];
    }
    point++;
  }
  const double weight = ( 1.0 - weightSum );
  for( unsigned int i=0; i<PointDimension; i++) 
  {
    barycentre[i] +=  weight * (point->Value())[i];
  }

  return barycentre;

}





/*
 * Print content to an ostream
 */
template<class T, unsigned int TPointDimension>
std::ostream &
operator<<(std::ostream& os,const Point<T,TPointDimension> & vct ) 
{
  for( unsigned int i=0; i<TPointDimension; i++)
  {
    os <<  vct[i] << "  ";
  }
  return os;
}



/*
 * Read content from an istream
 */
template<class T, unsigned int TPointDimension>
std::istream &
operator>>(std::istream& is, Point<T,TPointDimension> & vct ) 
{
  for( unsigned int i=0; i<TPointDimension; i++)
  {
    is >>  vct[i];
  }
  return is;
}



} // end namespace itk


#endif
