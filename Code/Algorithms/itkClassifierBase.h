/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkClassifierBase.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkClassifierBase_h
#define _itkClassifierBase_h

#include <vector>

#include "itkLightProcessObject.h"
#include "itkMembershipFunctionBase.h"
#include "itkDecisionRuleBase.h"

namespace itk
{

/** \class ClassifierBase
 * \brief Base class for classifier object
 * 
 * ClassifierBase is the base class for the classifier objects. It provides
 * the basic function definitions that are inherent to a classifier objects.
 *
 * This is the SuperClass for the classifier framework. This is an
 * abstract class defining an interface for all the classification objects 
 * available through the classifier framework in the ITK toolkit.
 *
 * The basic functionality of the classifier framework base class is to    
 * classify each data point in given a data set to one of the N classes where 
 * N is either specified by the user or automatically determined using methods 
 * such as k-means clustering techniques. 
 * 
 * The classifier framework supports two types of data where
 * (1) The input is a series of data points that needs to be classified. 
 * These points may be in the form of images where the pixels can be viewed  
 * as a list and the spatial location of the pixels in not of a concern. This 
 * classification can be carried out via the use of 
 * SampleClassifier. 
 * 
 * (2)The input is specifically an image and the pixels cannot be viewed as 
 * as a list of pixels since the spatial location of the pixel in needed
 * for various algorithmic implementation such as Markov Random Field based
 * approached. This type of data can be classified via the use of
 * GeneralImageClassifierBase.
 *
 * User will call The Update() function to run the classification proces
 *
 * The user must generate the membership function before asking for data to 
 * be classified. One can automatically generate the membership function from 
 * the training data via the use of itkImageModelEstimator class. The 
 * membershio functions have to be populated by using the AddMembershipFunction
 * If membership functions are not set prior to calling for classification, an 
 * exception in thrown.
 *
 * As the method name indicates, you can have more than one membership 
 * function. One for each classes. The order you put the membership 
 * calculator becomes the class label for the class that is represented
 * by the membership calculator. 
 *
 * The fourth argument is the type of decision rule. The main role of a 
 * decision rule is comparing the return values of the membership
 * calculators. However,
 * decision rule can include some prior knowledge that can improve the
 * result. To plug in the decision rule, use SetDecisionRule method.  
 *
 * Before you call the Update method to start the classification process, 
 * you should plug in all necessary parts ( one or more membership 
 * functions, a decision rule, the unclassified data). 
 *
 * This class is templated over the container type. 
 *
 * \ingroup ClassificationFilters 
 */
template <class TDataContainer>
class ITK_EXPORT ClassifierBase : public LightProcessObject
{
public:
  /** Standard class typedefs. */
  typedef ClassifierBase   Self;
  typedef LightProcessObject Superclass;

  /** Run-time type information (and related methods). */
  itkTypeMacro(ClassifierBase,LightProcessObject);

  /** Sets the number of classes. */
  itkSetMacro(NumberOfClasses, unsigned int);

  /** Gets the number of classes. */
  itkGetConstMacro(NumberOfClasses, unsigned int);

  /**Sets the decision rule */
  typedef typename TDataContainer::ValueType MeasurementVectorType ;
  
  /** Typedefs for membership funciton */
  typedef Statistics::MembershipFunctionBase< MeasurementVectorType > 
  MembershipFunctionType;
  typedef typename MembershipFunctionType::Pointer MembershipFunctionPointer ;

  typedef std::vector< MembershipFunctionPointer > 
  MembershipFunctionPointerVector;

  /** Type alias for decision rule */
  typedef DecisionRuleBase DecisionRuleType;

  /** Sets the pointer to the decision rule.
   * Stores the decision rule that makes the real decision using 
   * informations from MembershipFunctions and other prior knowledge */
  void SetDecisionRule( DecisionRuleType* ptrToDecisionRule )
  {
    m_DecisionRulePointer = ptrToDecisionRule;
  }

  /** Gets the pointer to the decision rule being used. */
  DecisionRuleType* GetDecisionRule(void)
  {
    return m_DecisionRulePointer.GetPointer() ;
  }
  
  /** Gets the MembershipFunction that are plugged in by
   * the AddMembershipFunction method. The index is assigned according
   * to the order each membership function has been added using the
   * AddMemberShipFunction method */
  const MembershipFunctionType* GetMembershipFunction(unsigned int index)const
  {
    return m_MembershipFunctions[index].GetPointer() ;
  }

  /** Gets the number of membership functions */
  unsigned int GetNumberOfMembershipFunctions() 
  {
    return static_cast<unsigned int>( m_MembershipFunctions.size() );
  }

  /** Stores a membership function of a class in its internal vector */
  unsigned int AddMembershipFunction(MembershipFunctionType* function);

  /** Starts the classification process */
  void Update() ;
  
protected:
  ClassifierBase() ;
  ~ClassifierBase() ;
  void PrintSelf(std::ostream& os, Indent indent) const;

  /** The real classification logic implementaion. All the subclasses
   * of this class should implement this method. */
  virtual void GenerateData() = 0 ;

private:
  ClassifierBase(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  /** Number of classes */
  unsigned int                       m_NumberOfClasses;

  /** Pointer to the decision rule to be used for classification. */
  typename DecisionRuleType::Pointer m_DecisionRulePointer;

  /** Container to hold the membership functions */
  MembershipFunctionPointerVector    m_MembershipFunctions ;

}; // class Classifier


} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkClassifierBase.txx"
#endif



#endif
















