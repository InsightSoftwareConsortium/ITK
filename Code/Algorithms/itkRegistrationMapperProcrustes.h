/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRegistrationMapperProcrustes.h
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
#ifndef __itkRegistrationMapperProcrustes_h
#define __itkRegistrationMapperProcrustes_h

#include "itkRegistrationMapper.h"

namespace itk
{
  
/** \class RegistrationMapperProcrustes
 * \brief N-D points from one coodinate system to another
 *
 *  This class is templated over the type of the transformation
 *  used to convert between the coordinate systems
 *
 * \ingroup Functions
 *
 */

template <class TTransformation, unsigned int NDimension> 
class ITK_EXPORT RegistrationMapperProcrustes : 
    public RegistrationMapper< 
        VectorContainer< unsigned long, Point<double,NDimension> >,
        TTransformation >
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef RegistrationMapperProcrustes  Self;



  /**
   *  Type of the Domain
   */
  typedef VectorContainer< unsigned long, 
                           Point<double,NDimension> >    DomainType;


  /**
   * Standard "Superclass" typedef.
   */
  typedef RegistrationMapper< DomainType, TTransformation > Superclass;



  /** 
   * Smart pointer typedef support 
   */
  typedef SmartPointer<Self>   Pointer;
  typedef SmartPointer<const Self>  ConstPointer;



  /**
   *  Type of the Transformation
   */
  typedef TTransformation       TransformationType;
  

  /**
   *  Pointer type for the Reference 
   */
  typedef typename DomainType::Pointer DomainPointer;


  /**
   *  Pointer type for the Transformation
   */
  typedef typename TransformationType::Pointer TransformationPointer;


  /** 
   * Run-time type information (and related methods).
   */
  itkTypeMacro(RegistrationMapperProcrustes, RegistrationMapper);


  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);
  


  /**
   * Connect the Domain
   */
   void SetDomain( DomainType * );


  /**
   * Connect the Transformation
   */
   void SetTransformation( TransformationType * );


  /**
   * Transform a point from one coordinate system
   */
   Point<double,NDimension> Transform( const Point<double,NDimension> & );


  
protected:

  DomainPointer            m_Domain;
  TransformationPointer    m_Transformation;

  RegistrationMapperProcrustes();
  virtual ~RegistrationMapperProcrustes() {};
  RegistrationMapperProcrustes(const Self&) {}
  void operator=(const Self&) {}


};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkRegistrationMapperProcrustes.txx"
#endif

#endif



