/*
   For more information, please see: http://software.sci.utah.edu

   The MIT License

   Copyright (c) 2021 Scientific Computing and Imaging Institute,
   University of Utah.


   Permission is hereby granted, free of charge, to any person obtaining a
   copy of this software and associated documentation files (the "Software"),
   to deal in the Software without restriction, including without limitation
   the rights to use, copy, modify, merge, publish, distribute, sublicense,
   and/or sell copies of the Software, and to permit persons to whom the
   Software is furnished to do so, subject to the following conditions:

   The above copyright notice and this permission notice shall be included
   in all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
   OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
   THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
   LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
   FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
   DEALINGS IN THE SOFTWARE.
 */
 // Adapted from: https://github.com/ljzhu/FastGrowCut

#ifndef FastGrowCut_h
#define FastGrowCut_h

#include <math.h>
#include <queue>
#include <set>
#include <vector>
#include <stdlib.h>
#include <fstream>
#include <iterator>

#include "fibheap.h"
#include "HeapNode.h"
namespace FGC {

const float DIST_INF = std::numeric_limits<float>::max();
const float DIST_EPSION = 1e-3f;
const unsigned char NNGBH = 26;
typedef float FPixelType;

template<typename SrcPixelType, typename LabPixelType>
class FastGrowCut {
public:
  void SetSourceImage( const std::vector<SrcPixelType>& imSrc );
  void SetSeedlImage( std::vector<LabPixelType>& imSeed );
  void SetWorkMode( bool bSegInitialized = false );
  void SetImageSize( const std::vector<long>& imSize );
  void DoSegmentation();
  void GetLabelImage( std::vector<LabPixelType>& imLab );
  void GetForegroundmage( std::vector<LabPixelType>& imFgrd );

private:
  void InitializationAHP();
  void DijkstraBasedClassificationAHP();

  std::vector<SrcPixelType> m_imSrc;
  std::vector<LabPixelType> m_imSeed;
  std::vector<LabPixelType> m_imLabPre;
  std::vector<FPixelType> m_imDistPre;
  std::vector<LabPixelType> m_imLab;
  std::vector<FPixelType> m_imDist;

  std::vector<long> m_imSize;
  long m_DIMX{0}, m_DIMY{0}, m_DIMZ{0}, m_DIMXY{0}, m_DIMXYZ{0};
  std::vector<int> m_indOff;
  std::vector<unsigned char>  m_NBSIZE;

  FibHeap m_heap;
  std::vector<HeapNode> m_hpNodes;
  bool m_bSegInitialized {false};
};
} // end namespace FGC

#include "FastGrowCut.hxx"

#endif // FastGrowCut_h
