/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit (ITK)
  Module:    itkDemonsRegistrationFilter.h
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
#ifndef _itkDemonsRegistrationFilter_h_
#define _itkDemonsRegistrationFilter_h_

#include "itkPDEDeformableRegistrationFilter.h"
#include "itkDemonsRegistrationFunction.h"

namespace itk {

/**
 * \class DemonsRegistrationFilter
 * \brief Deformably register two images using the demons algorithm.
 *
 * DemonsRegistrationFilter implements the demons deformable algorithm that 
 * register two images by computing the deformation field which will map a 
 * reference image onto a target image.
 *
 * A deformation field is represented as a image whose pixel type is some
 * vector type with at least N elements, where N is the dimension of
 * the target image. The vector type must support element access via operator
 * []. It is assumed that the vector elements behave like floating point
 * scalars.
 *
 * This class is templated over the Reference image type, Target image type
 * and the deformation field type.
 *
 * The input reference and target images are set via methods SetReference
 * and SetTarget respectively. An initial deformation field maybe set via
 * SetInitialDeformationField or SetInput. If no initial field is set,
 * a zero field is used as the initial condition.
 *
 * The algorithm has one parameters: the number of iteration to be performed.
 *
 * The output deformation field can be obtained via methods GetOutput
 * or GetDeformationField.
 *
 * This class make use of the finite difference solver hierarchy. Update
 * for each iteration is computed in DemonsRegistrationFunction.
 *
 * \warning This filter assumes that the reference type, target type
 * and deformation field type all have the same number of dimensions.
 * 
 * \sa DemonsRegistrationFunction
 *
 */
template<class TReference, class TTarget, class TDeformationField>
class ITK_EXPORT DemonsRegistrationFilter : 
  public PDEDeformableRegistrationFilter< TReference, TTarget,
    TDeformationField>
{
public:

  /**
   * Standard "Self" typedef.
   */
  typedef DemonsRegistrationFilter    Self;

  /**
   * Standard "Superclass" typedef.
   */
  typedef PDEDeformableRegistrationFilter<
    TReference, TTarget,TDeformationField>    Superclass;

  /**
   * Smart pointer support for this class.
   */
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self> ConstPointer;

  /**
   * Run-time type information (and related methods)
   */
  itkTypeMacro( DemonsRegistrationFilter, 
    PDEDeformableRegistrationFilter );

  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);

  /**
   * Reference image type.
   */
  typedef typename Superclass::ReferenceType   ReferenceType;
  typedef typename Superclass::ReferencePointer  ReferencePointer;

  /**
   * Target image type.
   */
  typedef typename Superclass::TargetType    TargetType;
  typedef typename Superclass::TargetPointer  TargetPointer;
  
  /**
   * Deformation field type.
   */
  typedef typename Superclass::DeformationFieldType 
    DeformationFieldType;
  typedef typename Superclass::DeformationFieldPointer  
    DeformationFieldPointer;


  /**
   * FiniteDifferenceEquation type
   */
  typedef typename Superclass::FiniteDifferenceEquationType
    FiniteDifferenceEquationType;

  /**
   * DemonsRegistrationFilterFunction type
   */
  typedef DemonsRegistrationFunction<ReferenceType,TargetType,
    DeformationFieldType>  DemonsRegistrationFunctionType;


protected:
  DemonsRegistrationFilter();
  ~DemonsRegistrationFilter() {}
  void PrintSelf(std::ostream& os, Indent indent) const
  {
    Superclass::PrintSelf( os, indent ); 
  }

  /**
   * Initialize the state of filter and equation before each iteration.
   */
  virtual void InitializeIteration();


private:
  Self(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
  
     

};


} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkDemonsRegistrationFilter.txx"
#endif

#endif
