/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImportFeatureVectorListToHistogram.h
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
#ifndef __itkImportFeatureVectorListToHistogram_h
#define __itkImportFeatureVectorListToHistogram_h

#include "itkObject.h"
#include "itkFeatureVectorContainer.h"
#include "itkHistogram.h"

namespace itk{
  namespace Statistics{

/** \class ImportFeatureVectorListToHistogram
 *  \brief Imports data from FeatureVectorList object to Histogram object
 *
 * Before beginning import process, users should prepare the Histogram object
 * by calling histogram object's Initialize(Size), SetBinMin(dimension, n), 
 * and SetBinMax(dimension, n) methods.
 *
 * To do: selective importing for subset of feature vector dimensions
 */
template< class TFeatureVectorList, class THistogram >
class ITK_EXPORT ImportFeatureVectorListToHistogram :
      public Object
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef ImportFeatureVectorListToHistogram Self;
  
  /**
   * Standard "Superclass" typedef
   */
  typedef Object Superclass;
  
  /** 
   * Smart pointer typedef support 
   */
  typedef SmartPointer<Self>   Pointer;
  
  /**
   * Run-time type information (and related methods).
   */
  itkTypeMacro(ImageFeatureVectorListAdaptor, FeatureVectorList) ;
  
  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self) ;

  typedef TFeatureVectorList FeatureVectorListType ;
  typedef typename TFeatureVectorList::Pointer FeatureVectorListPointer ;
  typedef THistogram HistogramType ;
  typedef typename THistogram::Pointer HistogramPointer ;

  /**
   * plug in the FeatureVectorList object
   */
  void SetFeatureVectorList(FeatureVectorListPointer list)
  {
    m_List = list ;
  }

  /**
   * plug in the Histogram object
   */
  void SetHistogram(HistogramPointer histogram)
  {
    m_Histogram = histogram ;
  }

  /**
   * starts import procedure
   */
  void Run() ;

protected:
  ImportFeatureVectorListToHistogram() ;
  virtual ~ImportFeatureVectorListToHistogram() {}
private:
  FeatureVectorListPointer m_List ;
  HistogramPointer m_Histogram ;
} ; // end of class

  } // end of namespace Statistics 
} // end of namespace itk 

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkImportFeatureVectorListToHistogram.txx"
#endif

#endif
