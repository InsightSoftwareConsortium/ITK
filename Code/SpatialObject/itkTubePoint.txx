/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkTubePoint.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#include "itkTubePoint.h"

namespace itk 
{
  void 
  TubePoint
  ::CommonConstruction() 
  {
    m_ID = 0;
    m_R = 0;
    m_Medialness = 0;
    m_Ridgeness = 0;
    m_Branchness = 0;
    m_Mark = false;
    m_Alpha1 = 0;
    m_Alpha2 = 0;
    m_Alpha3 = 0;
  }

  TubePoint
  ::TubePoint(const unsigned short int numDimensions) 
  {
    CommonConstruction();
    m_NumDimensions = numDimensions;
    m_T = new VectorType(m_NumDimensions);
    m_V1 = new VectorType(m_NumDimensions);
    m_V2 = new VectorType(m_NumDimensions);
  }

  TubePoint
  ::~TubePoint() 
  {
    delete m_T;
    delete m_V1;
    delete m_V2;
  }

  TubePoint::Self & 
  TubePoint
  ::GetReference( void )
  {
    return *this;
  }

  TubePoint::Pointer 
  TubePoint
  ::GetPointer( void )
  {
    return this;
  }
    
  TubePoint::ConstPointer 
  TubePoint
  ::GetConstPointer( void )
  {
    return this;
  }

  unsigned int 
  TubePoint
  ::GetId( void ) 
  {
    return m_ID;
  }

  void 
  TubePoint
  ::SetId( const unsigned int newID ) 
  {
    m_ID = newID;
  }

  float 
  TubePoint
  ::GetRadius( void ) const 
  {
    return m_R;
  }

  void 
  TubePoint
  ::SetRadius( const float newR ) 
  {
    m_R = newR;
  }

  float 
  TubePoint
  ::GetMedialness( void ) const 
  {
    return m_Medialness;
  }

  void 
  TubePoint
  ::SetMedialness( const float newMedialness ) 
  {
    m_Medialness = newMedialness;
  }

  float 
  TubePoint
  ::GetRidgeness( void ) const
  {
    return m_Ridgeness;
  }

  void 
  TubePoint
  ::SetRidgeness( const float newRidgeness ) 
  {
    m_Ridgeness = newRidgeness;
  }

  float 
  TubePoint
  ::GetBranchness( void ) const
  {
    return m_Branchness;
  }

  void 
  TubePoint
  ::SetBranchness( const float newBranchness ) 
  {
    m_Branchness = newBranchness;
  }

  bool 
  TubePoint::
  GetMark( void ) const
  {
    return m_Mark;
  }

  void 
  TubePoint
  ::SetMark( const bool newMark ) 
  {
    m_Mark = newMark;
  }

  unsigned short int 
  TubePoint
  ::GetNumDimensions( void ) const
  {
    return m_NumDimensions;
  }

  TubePoint::PointType 
  TubePoint
  ::GetCenterLinePoint( void ) const
  {
    return m_X;
  }

  // n-D case
  void 
  TubePoint
  ::SetCenterLinePoint( const PointType & newX ) 
  {
    m_X = newX;
  }

  // 3-D case
  void 
  TubePoint
  ::SetCenterLinePoint( const double x0, const double x1, const double x2 ) 
  {
    m_X[0] = x0;
    m_X[1] = x1;
    m_X[2] = x2;
  }

  // 2-D case
  void 
  TubePoint
  ::SetCenterLinePoint( const double x0, const double x1 ) 
  {
    m_X[0] = x0;
    m_X[1] = x1;
  }

  TubePoint::VectorType * 
  TubePoint
  ::GetTangent( void ) 
  {
    return m_T;
  }

  // n-D case
  void 
  TubePoint
  ::SetTangent( const VectorType & newT ) 
  {
    *m_T = newT;
  }

  // 3-D case
  void 
  TubePoint
  ::SetTangent( const double t0, const double t1, const double t2 ) 
  {
    (* m_T) (0) = t0;
    (* m_T) (1) = t1;
    (* m_T) (2) = t2;
  }

  // 2-D case
  void 
  TubePoint
  ::SetTangent( const double t0, const double t1 ) 
  {
    (* m_T) (0) = t0;
    (* m_T) (1) = t1;
  }

  TubePoint::VectorType * 
  TubePoint::GetV1() 
  {
    return m_V1;
  }

  // n-D case
  void 
  TubePoint
  //::SetV1( const TubePoint::VectorType & newV1 ) 
  ::SetV1( const VectorType & newV1 ) 
  {
    *m_V1 = newV1;
  }

  // 3-D case
  void 
  TubePoint
  ::SetV1( const double v10, const double v11, const double v12 ) 
  {
    (* m_V1) (0) = v10;
    (* m_V1) (1) = v11;
    (* m_V1) (2) = v12;
  }

  // 2-D case
  void 
  TubePoint
  ::SetV1( const double v10, const double v11 ) 
  {
    (* m_V1) (0) = v10;
    (* m_V1) (1) = v11;
  }

  TubePoint::VectorType * 
  TubePoint
  ::GetV2() 
  {
    return m_V2;
  }

  // n-D case
  void 
  TubePoint
  ::SetV2( const VectorType & newV2 ) 
  {
    *m_V2 = newV2;
  }

  // 3-D case
  void 
  TubePoint
  ::SetV2( const double v20, const double v21, const double v22 ) 
  {
    (* m_V2) (0) = v20;
    (* m_V2) (1) = v21;
    (* m_V2) (2) = v22;
  }

  // 2-D case
  void 
  TubePoint
  ::SetV2( const double v20, const double v21 ) 
  {
    (* m_V2) (0) = v20;
    (* m_V2) (1) = v21;
  }

  float 
  TubePoint
  ::GetAlpha1( void ) const
  {
    return m_Alpha1;
  }

  void 
  TubePoint
  ::SetAlpha1( const float newAlpha ) 
  {
    m_Alpha1 = newAlpha;
  }

  float 
  TubePoint
  ::GetAlpha2( void ) const
  {
    return m_Alpha2;
  }

  void 
  TubePoint
  ::SetAlpha2( const float newAlpha ) 
  {
    m_Alpha2 = newAlpha;
  }

  float 
  TubePoint
  ::GetAlpha3( void ) const
  {
    return m_Alpha3;
  }

  void 
  TubePoint
  ::SetAlpha3( const float newAlpha ) 
  {
    m_Alpha3 = newAlpha;
  }

  /*
  std::ostream & 
  operator <<( std::ostream & os, TubePoint & tubePoint ) 
  {
    os << "ID: " << tubePoint.m_ID << " ";
    os << "#Dims: " << tubePoint.m_NumDimensions << " ";
    os << "R: " << tubePoint.m_R << " ";
    os << "Medialness: " << tubePoint.m_Medialness << " ";
    os << "Ridgeness: " << tubePoint.m_Ridgeness << " ";
    os << "Mark: " << tubePoint.m_Mark << std::endl;
    os << "X: " << * (tubePoint.m_X) << "  ";
    os << "T: " << * (tubePoint.m_T);

    return os;
  }
  */

  TubePoint::Self & 
  TubePoint
  ::operator=(const TubePoint & rhs) 
  {
    m_ID = rhs.m_ID;
    m_R = rhs.m_R;
    m_Medialness = rhs.m_Medialness;
    m_Ridgeness = rhs.m_Ridgeness;
    m_Branchness = rhs.m_Branchness;
    m_Mark = rhs.m_Mark;
    m_NumDimensions = rhs.m_NumDimensions;
    m_X = rhs.m_X;
    * m_T = * (rhs.m_T);

    return * this;
  }

} // end namespace itk
