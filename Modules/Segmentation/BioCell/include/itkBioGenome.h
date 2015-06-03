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
#ifndef itkBioGenome_h
#define itkBioGenome_h


#if defined( _WIN32 )
#include "itkWindows.h"
#endif
#include "itkMacro.h"
#include "ITKBioCellExport.h"

#include <map>
#include <string>
#include <cmath>

namespace itk
{
namespace bio
{
/** \class Genome
 * \brief This class implements the abstraction of a biological genome.
 *
 * The genome is considered to be a sequence of genes, each one having
 * a name and a level of expression. This class is intended to be used
 * by artificial cells implementing cellular algorithms for image processing.
 *
 * \ingroup ITKBioCell
 */
class ITKBioCell_EXPORT Genome
{
public:
  typedef   std::string                    GeneIdType;
  typedef   std::map< GeneIdType, double > MapType;

public:
  Genome();
  virtual ~Genome();

  void Copy(const Genome & genome);

  void InsertGene(const GeneIdType & geneId);

  void KnockOutGene(const GeneIdType & geneId);

  double GetExpressionLevel(const GeneIdType & geneId);

  void   SetExpressionLevel(const GeneIdType & geneId, double level);

  /** This method computes a normalized Sigmoide function that can
   *  be used for gene network computations.  */
  static double Sigmoide(double threshold, double slant, double value)
  {
    return std::atan( ( value - threshold ) / slant ) / 3.1416 + 0.5001;
  }

private:
  MapType m_Map;
};
} // end namespace bio
} // end namespace itk

#endif
