/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/

#ifndef itkNodePair_h
#define itkNodePair_h

#include <utility>

namespace itk
{
/**
 * \class NodePair
 * \brief Represents a Node and its associated value (front value)
 *
 * \ingroup ITKFastMarching
 */
template< typename TNode, typename TOutputPixel >
class NodePair : private std::pair< TNode, TOutputPixel >
{
public:
  typedef NodePair                         Self;
  typedef std::pair< TNode, TOutputPixel > Superclass;

  typedef TNode         NodeType;
  typedef TOutputPixel  OutputPixelType;

  NodePair() : Superclass() {}
  NodePair( const TNode& iNode, const TOutputPixel& iValue ) :
    Superclass( iNode, iValue ) {}
  NodePair( const Self& iPair ) : Superclass( iPair ) {}

  void operator = ( const Self& iPair )
    {
    this->first = iPair.first;
    this->second = iPair.second;
    }

  void SetValue( const TOutputPixel& iValue )
    {
    this->second = iValue;
    }
  const TOutputPixel & GetValue() const
    {
    return this->second;
    }
  TOutputPixel & GetValue()
    {
    return this->second;
    }
  void SetNode( const TNode& iNode )
    {
    this->first = iNode;
    }
  const TNode & GetNode() const
    {
    return this->first;
    }
  TNode & GetNode()
    {
    return this->first;
    }

  bool operator < ( const Self& iRight ) const
    {
    return this->second < iRight.second;
    }

  bool operator > ( const Self& iRight ) const
    {
    return this->second > iRight.second;
    }

  bool operator <= ( const Self& iRight ) const
    {
    return this->second <= iRight.second;
    }

  bool operator >= ( const Self& iRight ) const
    {
    return this->second >= iRight.second;
    }
};

} // end namespace itk
#endif // itkNodePair_h
