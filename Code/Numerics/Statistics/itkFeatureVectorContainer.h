/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFeatureVectorContainer.h
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
#ifndef __itkFeatureVectorContainer_h
#define __itkFeatureVectorContainer_h

#include "itkMacro.h"
#include "itkPoint.h"
#include "itkSize.h"
#include "itkLightObject.h"

namespace itk{ 
  namespace Statistics{

/** \class FeatureVectorContainer 
 *  \brief FeatureVectorContainer defines common iterfaces for each subclasses
 *
 * A feature vector is a vector of different types of values. For example,
 * users might have an image with each pixel have a gray level intensity value
 * and a gradient magnitude. In this example, user think each pixel as a 
 * feature vector that has two types of values, intensity and gradient
 * magnitude. In this sense, the meaning of "feature" can be interchaneable 
 * with "measurement".
 * 
 * The primary purpose of FeatureVectorContainer clases are storing such 
 * feature vectors and providing different types of element access methods.
 *
 * The "TFeatureElement" template argument defines the type of each values
 * in a feature vector. The "VFeatureDimension" template argument defines 
 * the number of elements in a feature vector. The type of feature vector 
 * is fixed as ITK Point. The resulting feature vector type is 
 * Point< TFeatureElement, VFeatureDimension >.
 *
 * There are two common ways of accessing data. First, users can use 
 * an "InstanceIdentifier", say 'id' which is sequential index of 
 * feature vectors to access a feature vector in a container. 
 * Second, users can access a feature value using dimension and 
 * the index of a feature value, say 'n' in that dimension. The 'id' and 'n'
 * is equal in the FeatureVectorList classes. However, in Histogram class, its
 * different. See Histogram. For algorithms using these indexes, there are
 * methods to get the length of each dimension, GetSize() and 
 * GetSize(dimension). See each method's comment.
 *
 * Users may need to know two other groups of methods to fully utilize
 * FeatureVectorContainer. One group of methods is about frequency of feature
 * vectors. In Histogram classes, you can use such methods to get/set the
 * frequency of each feature vector. The other group of methods is about
 * characteristics of each subclasses of this class. If user want to 
 * create or know the characteristics of a subclass, use them. The set of
 * characteristics tell that if the data is sorted (true for Histogram),
 * if the each feature vector is unique (true for Histogram), and if the
 * container supports "meaningful" frequency interfaces. In FeatureVectorList
 * classes, users cannot set frequency, and GetFrequency() methods return 
 * one if the element exists, else zero. So, currently FeatureVectorList's
 * supporting frequency is set to false. 
 */

template < class TFeatureElement = float, unsigned int VFeatureDimension = 1 > 
class ITK_EXPORT FeatureVectorContainer : public LightObject
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef FeatureVectorContainer  Self;
  
  /** 
   * Run-time type information (and related methods).
   */
  itkTypeMacro(FeatureVectorContainer, LightObject);

  /**
   * Feature typedef support
   */ 
  typedef Point<TFeatureElement, VFeatureDimension> FeatureVectorType ;

  /**
   * typedef for each element in a feature vector
   */ 
  typedef TFeatureElement FeatureElementType ;

  /**
   * frequency value typedef
   */
  typedef float FrequencyType ;

  /**
   * Instance Identifier typedef support
   * this identifier will be unique sequential id for each feature vector
   * in a FeatureVectorContainer subclass.
   */ 
  typedef unsigned long InstanceIdentifier;

  /**
   * size value type 
   */
  typedef Size< VFeatureDimension > SizeType ;
  typedef SizeType::SizeValueType SizeValueType ;

  /**
   * returns SizeType object whose each element is the number of
   * elements in each dimension
   */
  virtual SizeType GetSize() = 0 ;
  
  /**
   * returns SizeValueType value that is the number of elements in the
   * 'dimension' dimension.
   */
  virtual SizeValueType GetSize(unsigned int dimension) = 0 ;


  /**
   * retunrs the feature of the instance which is identified by the 'id'
   */
  virtual FeatureVectorType GetFeature(const InstanceIdentifier id)  = 0 ;

  /**
   * returns the frequency of the instance which is identified by the 'id'
   */
  virtual FrequencyType GetFrequency(const InstanceIdentifier id)  = 0 ;

  /**
   * returns the feature element which is the 'n'-th element 
   * in the 'd' dimension of the feature vector
   */
  virtual FeatureElementType GetFeatureElement(const unsigned int d, 
                                               const unsigned long n) = 0 ;
  
  /**
   * returns the frequency of the 'n'-th element in the 'd' dimension 
   * of the feature vector
   */
  virtual FrequencyType GetFrequency(const unsigned int d,
                                     const unsigned long n)  = 0 ;

  /**
   * returns the total frequency for the 'd' dimension
   */
  virtual FrequencyType GetTotalFrequency(const unsigned int d)  = 0 ;

  /**
   * if a subclass of this class has its data sorted, return true
   * else, false.
   */
  bool IsSorted() { return m_Sorted ; }

  /**
   * sets the Sorted flag
   */
  void SetSortedFlag(bool flag) { m_Sorted = flag ; }

  /**
   * if a subclass of this class has frequency values associated with
   * each feature and support GetFrequency mehtod, returns true,
   * else, false.
   */
  bool IsSupportingFrequency() { return m_SupportingFrequency ; }

  /**
   * sets the SupportingFrequency flag
   */
  void SetSupportingFrequencyFlag(bool flag) { m_SupportingFrequency = flag ; }

  /**
   * if a subclass of this class allowes duplicate feature vectors, returns 
   * true, else, false.
   */
  bool IsAllowingDuplicates() { return m_AllowingDuplicates ; }

  /**
   * sets the AllowingDuplicates flag
   */
  void SetAllowingDuplicatesFlag(bool flag) { m_AllowingDuplicates = flag ; }
  
protected:
  FeatureVectorContainer() {}
  virtual ~FeatureVectorContainer() {}
  
private:
  FeatureVectorContainer(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
  
  bool m_Sorted ;
  bool m_SupportingFrequency ;
  bool m_AllowingDuplicates ;
} ; // end of class


  } // end of namespace Statistics 
} // end of namespace itk

#endif
