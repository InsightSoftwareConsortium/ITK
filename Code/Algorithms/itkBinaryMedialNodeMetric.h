/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBinaryMedialNodeMetric.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkBinaryMedialNodeMetric_h
#define __itkBinaryMedialNodeMetric_h

#include "itkLightObject.h"

#include "vnl/vnl_vector_fixed.h"
#include "itkBloxCoreAtomPixel.h"

namespace itk
{
  
/** 
 * Metric to compare two medial nodes using the Signature Technique as described in:
 * 
 * Tamburo, Cois, Shelton, Stetten.  "Medial Node Correspondences Towards Automated
 * Registration", Lecture Notes in Computer Science (in press), 2003.
 *
 */

template <int VDimensions>
class BinaryMedialNodeMetric : public LightObject
{
public:

  /** Standard class typedefs. */
  typedef BinaryMedialNodeMetric          Self;
  typedef LightObject                     Superclass; 
  typedef SmartPointer<Self>              Pointer;
  typedef SmartPointer<const Self>        ConstPointer;

  /** Method for creation through the object factory */
  itkNewMacro(Self);

  /** We use the value 2.0 to represent an empty space in an eigen value list, 
   *  since eigen values can never be greater than 1.0.  This is an int because
   *  itkStaticConstMacro had issues with the use of a double */
  itkStaticConstMacro(EMPTY, int, 2);

  /** Run-time type information (and related methods) */
  itkTypeMacro(BinaryMedialNodeMetric, LightObject);

  /** typedef for core atom pixel */
  typedef BloxCoreAtomPixel<VDimensions> MedialNode;

  /** The type used to store the position of the BloxPixel. */
  typedef Point<double, VDimensions> PositionType;

  /** Initialize the Metric. */
  void Initialize(void);

  /** Sets the input medial nodes. */
  void SetMedialNodes(MedialNode * NodeA1, MedialNode * NodeA2, MedialNode * NodeB1, MedialNode * NodeB2);

  /** Function to print the list of combined eigen values. */
  void PrintCombinedEigens();

  /** Function to set a boolean to show the calculation process of the metric. */
  void ShowCalculation(){m_ShowCalc = true;}

  /** Function to set a boolean to not show the calculation process of the metric. */
  void DontShowCalculation(){m_ShowCalc = false;}

  /** Function to return the result of the metric calculations */
  double GetResult(){return m_Result;}
  //itkSetMacro(Result, double);
  //itkGetMacro(Result, double);
  

protected:
  /** Default Constructor */
  BinaryMedialNodeMetric();

  /** Default Destructor */
  ~BinaryMedialNodeMetric() {};

  void PrintSelf(std::ostream& os, Indent indent) const;

private:

  /** Function used internally to arrange the eigen value list. */
  void OrderValues();

  /** Arrays to hold combined eigen and distance values. */
  double m_CombinedEigenValues[VDimensions*2];
  double m_CombinedDistanceValues[VDimensions*2];
  
  /** A key array which tells us which node the eigen values were from originally */
  int m_CombinedEigensKey[VDimensions*2];

  /** Base node pair */
  MedialNode * m_NodeA1;
  MedialNode * m_NodeA2;

  /** Second node pair */
  MedialNode * m_NodeB1;
  MedialNode * m_NodeB2;
  
  /** List of Eigen values for each pair */
  typename MedialNode::EigenvalueType m_EigenA;
  typename MedialNode::EigenvalueType m_EigenB;

  /** Distance vectors for each pair */
  PositionType m_DistanceVectorA;
  PositionType m_DistanceVectorB;

  /** Double between 0-1.0 indicating the result of the metric comparison. */
  double m_Result;

  /** Boolean controlling output of teh calculation process (default is false)*/
  bool m_ShowCalc;
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkBinaryMedialNodeMetric.txx"
#endif

#endif



