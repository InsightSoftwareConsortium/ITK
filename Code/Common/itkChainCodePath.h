/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkChainCodePath.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef _itkChainCodePath_h
#define _itkChainCodePath_h

#include "itkPath.h"
#include "itkIndex.h"
#include "itkOffset.h"
#include <vector>

namespace itk
{


/** \class ChainCodePath
 * \brief  Represent a path as a sequence of connected image index offsets
 *
 * This class is intended to represent sequences of connected indices in an
 * image.  It does so by storing the offset of each index from its immediately
 * preceeding, connected, index.  The only image index stored directly is that
 * of the first index.  ChainCodePath maps a 1D integer input (step number) to
 * an ND interger output (either an offset or an image index, depending on
 * whether Evaluate or EvaluateToIndex is called).
 *
 * \sa ChainCodePath2D
 * \sa ParametricPath
 * \sa Path
 * \sa Index
 * \sa Offset
 *
 * \ingroup PathObjects
 */
template <unsigned int VDimension>
class ITK_EXPORT ChainCodePath : public
Path< unsigned int, Offset< VDimension >, VDimension >
{
public:
  /** Dimension underlying input image. */
  itkStaticConstMacro(Dimension, unsigned int, VDimension);

  /** Standard class typedefs. */
  typedef ChainCodePath<VDimension>                               Self;
  typedef Path< unsigned int, Offset< VDimension >, VDimension >  Superclass;

  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
  
  /** Run-time type information (and related methods). */
  itkTypeMacro(ChainCodePath, Path);

  
  /** OutputType typedef support. */
  typedef typename Superclass::OutputType   OutputType;
  typedef typename Superclass::InputType    InputType;

  /** The output type of this function is an Index */
  typedef OutputType               OffsetType;
  typedef Index<VDimension>        IndexType;

  typedef std::vector<OffsetType>  ChainCodeType;



  // Functions inherited from Path
  
  /** Evaluate the chaincode for the offset at the specified path-position. */
  virtual OutputType Evaluate( const InputType & input ) const
    {
    return m_Chain[input];
    }
  
  /** Like Evaluate(), but returns the index at the specified path-position. */
  virtual IndexType EvaluateToIndex( const InputType & input ) const;
  
  /** Increment the input variable passed by reference and then return the
   * offset stored at the new path-position.  If the chaincode is unable to be
   * incremented, input is not changed and an offset of zero is returned, which
   * may be used to check for the end of the chain code. */
  virtual OffsetType IncrementInput(InputType & input) const;

  /** Where does the path end (what is the last valid input value)? */
  virtual inline InputType EndOfInput() const
    {
    return NumberOfSteps();  // 0 is before the first step, 1 is after it
    }
  


  // Functions specific to ChainCodePath

  /** New() method for dynamic construction */
  itkNewMacro( Self );
  
  /** Set/Get the index associated with the initial position of the path */
  itkSetMacro( Start, IndexType );
  itkGetConstReferenceMacro( Start, IndexType );
  
  /** Insert a new step into the chaincode at a specified position */
  virtual inline void InsertStep( InputType position, OffsetType step )
    {
    m_Chain.insert( m_Chain.begin()+position, step );
    this->Modified();
    }
  
  /** Change the direction of a step in the chaincode */
  virtual inline void ChangeStep( InputType position, OffsetType step )
    {
    m_Chain[position]=step;
    this->Modified();
    }
  
  /** Remove all steps from the chain code */
  virtual inline void Clear()
    {
    m_Chain.clear();
    this->Modified();
    }
  
  /** How many steps in the chaincode? */
  virtual inline unsigned int NumberOfSteps() const
    {
    return m_Chain.size();
    }

  /** Needed for Pipelining */
  virtual void Initialize(void)
    {
    m_Start = m_ZeroIndex;
    this->Clear();
    }



protected:
  ChainCodePath();
  ~ChainCodePath() {}
  void PrintSelf(std::ostream& os, Indent indent) const;


private:
  ChainCodePath(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
  
  IndexType     m_Start;            // origin image index for the path
  ChainCodeType m_Chain;            // the chain code (vector of offsets)
};

} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkChainCodePath.txx"
#endif

#endif
