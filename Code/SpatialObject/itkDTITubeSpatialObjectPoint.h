/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkDTITubeSpatialObjectPoint.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkDTITubeSpatialObjectPoint_h
#define __itkDTITubeSpatialObjectPoint_h

#include "itkTubeSpatialObjectPoint.h"
#include "itkCovariantVector.h"
#include "vnl/vnl_vector_fixed.h"

namespace itk 
{

/** \class DTITubeSpatialObjectPoint
* \brief Point used for a tube definition
*
* This class contains all the functions necessary to define a point
* that can be used to build tubes.
*
* \sa DTITubeSpatialObject 
*/ 
template < unsigned int TPointDimension = 3 >
class DTITubeSpatialObjectPoint 
  : public TubeSpatialObjectPoint<TPointDimension>
{

public:

  typedef DTITubeSpatialObjectPoint                 Self;
  typedef SpatialObjectPoint<TPointDimension>       Superclass;
  typedef Point< double, TPointDimension >          PointType;
  typedef Vector<double, TPointDimension >          VectorType;
  typedef CovariantVector<double, TPointDimension > CovariantVectorType;
  typedef std::pair<std::string,float>              FieldType;
  typedef std::vector<FieldType>                    FieldListType;

  // If you add a type here you need to modify the TranslateEnumToChar
  // to translate the enum to a string
  typedef enum {FA,ADC,GA} FieldEnumType;

  /** Constructor. This one defines the # of dimensions in the 
   * DTITubeSpatialObjectPoint */
  DTITubeSpatialObjectPoint( void );

  /** Default destructor. */
  virtual ~DTITubeSpatialObjectPoint( void );

  /** Set/Get the tensor matrix */
  void SetTensorMatrix(const float* matrix)
    {
    for(unsigned int i=0;i<6;i++)
      {
      m_TensorMatrix[i] = matrix[i];
      }
    }

  const float* GetTensorMatrix() const {return m_TensorMatrix;}

  /** Copy one DTITubeSpatialObjectPoint to another */
  Self & operator=(const DTITubeSpatialObjectPoint & rhs);

  /** Add a field to the point list*/
  void AddField(const char* name,float value);

  /** Add a field to the point list*/
  void AddField(FieldEnumType name,float value);

  /** Set a field value */
  void SetField(FieldEnumType name,float value);
  void SetField(const char* name,float value);

  /** Return the list of extra fields */
  const FieldListType & GetFields() const {return m_Fields;}

  /** Return the value of the specific fiedls */
  float GetField(const char* name) const;
  float GetField(FieldEnumType name) const;


protected:

  float m_TensorMatrix[6];
  FieldListType m_Fields;

  /** Print the object */
  void PrintSelf( std::ostream & os, Indent indent) const;

  /** Translate the enum to char */
  std::string TranslateEnumToChar(FieldEnumType name) const;
};

} // end of namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkDTITubeSpatialObjectPoint.txx"
#endif

#endif // __itkDTITubeSpatialObjectPoint_h
