/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkTubePoint.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkTubePoint_h
#define __itkTubePoint_h

#include "itkPoint.h"
#include "vnl/vnl_vector_fixed.h"

namespace itk 
{

/** \class TubePoint
* \brief Point used for a tube definition
*
* This class contains all the functions necessary to define a point
* that can be used to build tubes.
*
* \also TubeSpatialObject TubeNetworkSpatialObject
*/ 

class ITK_EXPORT TubePoint 
{

public:

  /** Constructor. This one defines the # of dimensions in the TubePoint */
  TubePoint(const unsigned short int numDimensions = 3);

  /** Default destructor. */
  ~TubePoint( void );

  typedef TubePoint Self;
  typedef Self* Pointer;
  typedef const Self* ConstPointer;
  typedef itk::Point< double, 3 > PointType;
  typedef vnl_vector < double > VectorType;

  /** 
  * Get the tubePoint Id. 
  */
  unsigned int GetId( void ) const;

  /** 
  * Set the tubePoint Id.
  */
  void SetId(const unsigned int newID);

  /**
  * Returns a reference to self.
  */
  Self & GetReference( void );

  /**
  * Returns a pointer to self.
  */
  Pointer GetPointer( void );
  
  /**
  * Returns a const pointer to self.
  */
  ConstPointer GetConstPointer( void );

  /** 
  * Return a pointer to the point object.
  */
  PointType * GetPointerToCenterLinePoint( void );

  /** 
  * Return a reference to the point object.
  */
  PointType & GetReferenceToCenterLinePoint( void );

  /** 
  * Set the point object. Couldn't use macros for these methods.
  */
  void SetCenterLinePoint(const PointType & newX);
  void SetCenterLinePoint(const double x0, const double x1);
  void SetCenterLinePoint(const double x0, const double x1, const double x2);

  /** 
  * Get T 
  */
  VectorType * GetTangent( void );

  /** Set T. Couldn't use macros for these methods */
  void SetTangent(const VectorType & newT);
  void SetTangent(const double t0, const double t1);
  void SetTangent(const double t0, const double t1, const double t2);

  /** Get V1 */
  VectorType * GetV1( void );

  /** Set V1 */
  void SetV1(const VectorType & newV1);
  void SetV1(const double v10, const double v11);
  void SetV1(const double v10, const double v11, const double v12);

  /** Get V2 */
  VectorType * GetV2( void );

  /** Set V2 */
  void SetV2(const VectorType & newV2);
  void SetV2(const double v20, const double v21);
  void SetV2(const double v20, const double v21, const double v22);

  /** Get R */
  float GetRadius( void ) const;

  /** Set R */
  void SetRadius(const float newR);

  /** Get Medialness */
  float GetMedialness( void ) const;

  /** Set Medialness */
  void SetMedialness(const float newMedialness);

  /** Get Ridgeness */
  float GetRidgeness( void ) const;

  /** Set Ridgeness */
  void SetRidgeness(const float newRidgeness);

  /** Get Branchness */
  float GetBranchness( void ) const;

  /** Set Branchness */
  void SetBranchness(const float newBranchness);

  /** Get Mark */
  bool GetMark( void ) const;

  /** Set Mark */
  void SetMark(const bool newMark);

  /** Get Alpha1 */
  float GetAlpha1( void ) const;

  /** Set Alpha1 */
  void SetAlpha1(const float newAlpha);

  /** Get Alpha2 */
  float GetAlpha2( void ) const;

  /** Set Alpha2 */
  void SetAlpha2(const float newAlpha);

  /** Get Alpha3 */
  float GetAlpha3( void ) const;

  /** Set Alpha3 */
  void SetAlpha3(const float newAlpha);

  /** Get # of dimensions */
  unsigned short int GetNumDimensions( void ) const;

  /** Print info about this tubepoint */
  //std::ostream & operator << (std::ostream & os, TubePoint & tubePoint);

  /** Copy one tubepoint to another */
  TubePoint & operator = (const TubePoint & rhs);

protected:
  /** A unique ID assigned to this tubepoint */
  unsigned int m_ID;

  PointType * m_X;
  VectorType * m_T;
  VectorType * m_V1;
  VectorType * m_V2;

  /** First of 3 alpha values */
  float m_Alpha1;

  /** Second of 3 alpha values */
  float m_Alpha2;

  /** Third of 3 alpha values */
  float m_Alpha3;

  /** The radius of the tube point */
  float m_R;

  /** The medialness of the tube point */
  float m_Medialness;

  /** The ridgeness of the tube point */
  float m_Ridgeness;

  /** The branchness of the tube point */
  float m_Branchness;

  /** Is the tube point marked (selected) ? */
  bool m_Mark;

  /** # of dimensions */
  unsigned short int m_NumDimensions;

  /**  Initialization of the common variables */
  void CommonConstruction( void );
};

} // end of namespace itk

#endif // __itkTubePoint_h
