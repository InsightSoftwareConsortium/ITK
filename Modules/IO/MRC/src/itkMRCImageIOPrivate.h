/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef itkMRCImageIOPrivate_h
#define itkMRCImageIOPrivate_h

/*
 *  This code was contributed in the Insight Journal paper:
 *  "A Streaming IO Base Class and Support for Streaming the MRC and VTK File Format"
 *  by Lowekamp B., Chen D.
 *  https://doi.org/10.54294/ufs19w
 *
 */

namespace itk
{

namespace
{

const char * const magicMAP = "MAP ";
} // end anonymous namespace

} // namespace itk

#endif // itkMRCImageIOPrivate_h
