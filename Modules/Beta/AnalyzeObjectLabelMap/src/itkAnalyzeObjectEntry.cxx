/*=========================================================================
 *
 *  Copyright NumFOCUS
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
#include "itkAnalyzeObjectEntry.h"
#include <cstring>
namespace itk
{

AnalyzeObjectEntry::~AnalyzeObjectEntry() = default;

AnalyzeObjectEntry::AnalyzeObjectEntry()

{
  std::memset(this->m_Name, 0, sizeof(this->m_Name));
}

// AnalyzeObjectEntry & AnalyzeObjectEntry
// ::operator=( const AnalyzeObjectEntry & rhs )

// Copy everything but the name.  Each ObjectEntry must have a unique name.
void
AnalyzeObjectEntry::Copy(AnalyzeObjectEntry::Pointer rhs)
{
  this->SetBlendFactor(rhs->GetBlendFactor());
  this->SetCopyFlag(rhs->GetCopyFlag());
  this->SetDisplayFlag(rhs->GetDisplayFlag());
  this->SetEndBlue(rhs->GetEndBlue());
  this->SetEndGreen(rhs->GetEndGreen());
  this->SetEndRed(rhs->GetEndRed());
  this->SetMaximumXValue(rhs->GetMaximumXValue());
  this->SetMaximumYValue(rhs->GetMaximumYValue());
  this->SetMaximumZValue(rhs->GetMaximumZValue());
  this->SetMinimumXValue(rhs->GetMinimumXValue());
  this->SetMinimumYValue(rhs->GetMinimumYValue());
  this->SetMinimumZValue(rhs->GetMinimumZValue());
  this->SetMirrorFlag(rhs->GetMirrorFlag());
  this->SetNeighborsUsedFlag(rhs->GetNeighborsUsedFlag());
  this->SetOpacity(rhs->GetOpacity());
  this->SetOpacityThickness(rhs->GetOpacityThickness());
  this->SetXRotation(rhs->GetXRotation());
  this->SetYRotation(rhs->GetYRotation());
  this->SetZRotation(rhs->GetZRotation());
  this->SetXRotationIncrement(rhs->GetXRotationIncrement());
  this->SetYRotationIncrement(rhs->GetYRotationIncrement());
  this->SetZRotationIncrement(rhs->GetZRotationIncrement());
  this->SetShades(rhs->GetShades());
  this->SetStartBlue(rhs->GetStartBlue());
  this->SetStartGreen(rhs->GetStartGreen());
  this->SetStartRed(rhs->GetStartRed());
  this->SetStatusFlag(rhs->GetStatusFlag());
  this->SetXTranslation(rhs->GetXTranslation());
  this->SetYTranslation(rhs->GetYTranslation());
  this->SetZTranslation(rhs->GetZTranslation());
  this->SetXTranslationIncrement(rhs->GetXTranslationIncrement());
  this->SetYTranslationIncrement(rhs->GetYTranslationIncrement());
  this->SetZTranslationIncrement(rhs->GetZTranslationIncrement());
  this->SetXCenter(rhs->GetXCenter());
  this->SetYCenter(rhs->GetYCenter());
  this->SetZCenter(rhs->GetZCenter());
}

void
AnalyzeObjectEntry ::Print(std::ostream & myfile)
{
  myfile << this->m_Name << std::endl;
  myfile << m_DisplayFlag << std::endl;
  myfile << (int)m_CopyFlag << std::endl;
  myfile << (int)m_MirrorFlag << std::endl;
  myfile << (int)m_StatusFlag << std::endl;
  myfile << (int)m_NeighborsUsedFlag << std::endl;
  myfile << m_Shades << std::endl;
  myfile << m_StartRed << std::endl;
  myfile << m_StartGreen << std::endl;
  myfile << m_StartBlue << std::endl;
  myfile << m_EndRed << std::endl;
  myfile << m_EndGreen << std::endl;
  myfile << m_EndBlue << std::endl;
  myfile << m_XRotation << std::endl;
  myfile << this->m_YRotation << std::endl;
  myfile << this->m_ZRotation << std::endl;
  myfile << this->m_XTranslation << std::endl;
  myfile << this->m_YTranslation << std::endl;
  myfile << this->m_ZTranslation << std::endl;
  myfile << this->m_XCenter << std::endl;
  myfile << this->m_YCenter << std::endl;
  myfile << this->m_ZCenter << std::endl;
  myfile << this->m_XRotationIncrement << std::endl;
  myfile << this->m_YRotationIncrement << std::endl;
  myfile << this->m_ZRotationIncrement << std::endl;
  myfile << this->m_XTranslationIncrement << std::endl;
  myfile << this->m_YTranslationIncrement << std::endl;
  myfile << this->m_ZTranslationIncrement << std::endl;
  myfile << this->m_MinimumXValue << std::endl;
  myfile << this->m_MinimumYValue << std::endl;
  myfile << this->m_MinimumZValue << std::endl;
  myfile << this->m_MaximumXValue << std::endl;
  myfile << this->m_MaximumYValue << std::endl;
  myfile << this->m_MaximumZValue << std::endl;
  myfile << this->m_Opacity << std::endl;
  myfile << this->m_OpacityThickness << std::endl;
  myfile << m_BlendFactor << std::endl;
  myfile << "= \n";
}

template <typename TValue>
void
AnalyzeObjectEntry ::ReadBytes(std::ifstream & inputFileStream,
                               TValue *        dest,
                               const int       Replications,
                               const bool      NeedByteSwap)
{
  if (inputFileStream.read(reinterpret_cast<char *>(dest), sizeof(TValue) * Replications).fail())
  {
    itkExceptionMacro("6: Unable to read in object #1 description.");
  }
  if (NeedByteSwap)
  {
    itk::ByteSwapper<TValue>::SwapFromSystemToBigEndian(dest);
  }
}

void
AnalyzeObjectEntry ::ReadFromFilePointer(std::ifstream & inputFileStream,
                                         const bool      NeedByteSwap,
                                         const bool /* NeedBlendFactor */)
{
  ReadBytes<char>(inputFileStream, this->m_Name, 32, NeedByteSwap);
  ReadBytes<int>(inputFileStream, &(this->m_DisplayFlag), 1, NeedByteSwap);
  ReadBytes<unsigned char>(inputFileStream, &m_CopyFlag, 1, NeedByteSwap);
  ReadBytes<unsigned char>(inputFileStream, &m_MirrorFlag, 1, NeedByteSwap);
  ReadBytes<unsigned char>(inputFileStream, &m_StatusFlag, 1, NeedByteSwap);
  ReadBytes<unsigned char>(inputFileStream, &m_NeighborsUsedFlag, 1, NeedByteSwap);
  ReadBytes<int>(inputFileStream, &m_Shades, 1, NeedByteSwap);
  ReadBytes<int>(inputFileStream, &m_StartRed, 1, NeedByteSwap);
  ReadBytes<int>(inputFileStream, &m_StartGreen, 1, NeedByteSwap);
  ReadBytes<int>(inputFileStream, &m_StartBlue, 1, NeedByteSwap);
  ReadBytes<int>(inputFileStream, &m_EndRed, 1, NeedByteSwap);
  ReadBytes<int>(inputFileStream, &m_EndGreen, 1, NeedByteSwap);
  ReadBytes<int>(inputFileStream, &m_EndBlue, 1, NeedByteSwap);
  ReadBytes<int>(inputFileStream, &m_XRotation, 1, NeedByteSwap);
  ReadBytes<int>(inputFileStream, &m_YRotation, 1, NeedByteSwap);
  ReadBytes<int>(inputFileStream, &m_ZRotation, 1, NeedByteSwap);
  ReadBytes<int>(inputFileStream, &m_XTranslation, 1, NeedByteSwap);
  ReadBytes<int>(inputFileStream, &m_YTranslation, 1, NeedByteSwap);
  ReadBytes<int>(inputFileStream, &m_ZTranslation, 1, NeedByteSwap);
  ReadBytes<int>(inputFileStream, &m_XCenter, 1, NeedByteSwap);
  ReadBytes<int>(inputFileStream, &m_YCenter, 1, NeedByteSwap);
  ReadBytes<int>(inputFileStream, &m_ZCenter, 1, NeedByteSwap);
  ReadBytes<int>(inputFileStream, &m_XRotationIncrement, 1, NeedByteSwap);
  ReadBytes<int>(inputFileStream, &m_YRotationIncrement, 1, NeedByteSwap);
  ReadBytes<int>(inputFileStream, &m_ZRotationIncrement, 1, NeedByteSwap);
  ReadBytes<int>(inputFileStream, &m_XTranslationIncrement, 1, NeedByteSwap);
  ReadBytes<int>(inputFileStream, &m_YTranslationIncrement, 1, NeedByteSwap);
  ReadBytes<int>(inputFileStream, &m_ZTranslationIncrement, 1, NeedByteSwap);
  ReadBytes<short int>(inputFileStream, &m_MinimumXValue, 1, NeedByteSwap);
  ReadBytes<short int>(inputFileStream, &m_MinimumYValue, 1, NeedByteSwap);
  ReadBytes<short int>(inputFileStream, &m_MinimumZValue, 1, NeedByteSwap);
  ReadBytes<short int>(inputFileStream, &m_MaximumXValue, 1, NeedByteSwap);
  ReadBytes<short int>(inputFileStream, &m_MaximumYValue, 1, NeedByteSwap);
  ReadBytes<short int>(inputFileStream, &m_MaximumZValue, 1, NeedByteSwap);
  ReadBytes<float>(inputFileStream, &m_Opacity, 1, NeedByteSwap);
  ReadBytes<int>(inputFileStream, &m_OpacityThickness, 1, NeedByteSwap);
  // I am going to comment this if-statment out for right now, so the the program reads in
  // the Blend Factor for any version.  The documentation that I got said that the Blend
  // Factor should not be in the files for version 6 or earlier but I guess it is.
  // I tried opening up some version six object maps and they were erroring out because they were 4 bits off.
  // if(NeedBlendFactor)
  //  {
  ReadBytes<float>(inputFileStream, &m_BlendFactor, 1, NeedByteSwap);
  // }
}

void
AnalyzeObjectEntry ::SwapObjectEndedness()
{
  itk::ByteSwapper<int>::SwapFromSystemToBigEndian(&(this->m_DisplayFlag));
  itk::ByteSwapper<int>::SwapFromSystemToBigEndian(&(this->m_Shades));
  itk::ByteSwapper<int>::SwapFromSystemToBigEndian(&(this->m_StartRed));
  itk::ByteSwapper<int>::SwapFromSystemToBigEndian(&(this->m_StartGreen));
  itk::ByteSwapper<int>::SwapFromSystemToBigEndian(&(this->m_StartBlue));
  itk::ByteSwapper<int>::SwapFromSystemToBigEndian(&(this->m_EndRed));
  itk::ByteSwapper<int>::SwapFromSystemToBigEndian(&(this->m_EndGreen));
  itk::ByteSwapper<int>::SwapFromSystemToBigEndian(&(this->m_EndBlue));
  itk::ByteSwapper<int>::SwapFromSystemToBigEndian(&(this->m_XRotation));
  itk::ByteSwapper<int>::SwapFromSystemToBigEndian(&(this->m_YRotation));
  itk::ByteSwapper<int>::SwapFromSystemToBigEndian(&(this->m_ZRotation));
  itk::ByteSwapper<int>::SwapFromSystemToBigEndian(&(this->m_XTranslation));
  itk::ByteSwapper<int>::SwapFromSystemToBigEndian(&(this->m_YTranslation));
  itk::ByteSwapper<int>::SwapFromSystemToBigEndian(&(this->m_ZTranslation));
  itk::ByteSwapper<int>::SwapFromSystemToBigEndian(&(this->m_XCenter));
  itk::ByteSwapper<int>::SwapFromSystemToBigEndian(&(this->m_YCenter));
  itk::ByteSwapper<int>::SwapFromSystemToBigEndian(&(this->m_ZCenter));
  itk::ByteSwapper<int>::SwapFromSystemToBigEndian(&(this->m_XRotationIncrement));
  itk::ByteSwapper<int>::SwapFromSystemToBigEndian(&(this->m_YRotationIncrement));
  itk::ByteSwapper<int>::SwapFromSystemToBigEndian(&(this->m_ZRotationIncrement));
  itk::ByteSwapper<int>::SwapFromSystemToBigEndian(&(this->m_XTranslationIncrement));
  itk::ByteSwapper<int>::SwapFromSystemToBigEndian(&(this->m_YTranslationIncrement));
  itk::ByteSwapper<int>::SwapFromSystemToBigEndian(&(this->m_ZTranslationIncrement));
  itk::ByteSwapper<short int>::SwapFromSystemToBigEndian(&(this->m_MinimumXValue));
  itk::ByteSwapper<short int>::SwapFromSystemToBigEndian(&(this->m_MinimumYValue));
  itk::ByteSwapper<short int>::SwapFromSystemToBigEndian(&(this->m_MinimumZValue));
  itk::ByteSwapper<short int>::SwapFromSystemToBigEndian(&(this->m_MaximumXValue));
  itk::ByteSwapper<short int>::SwapFromSystemToBigEndian(&(this->m_MaximumYValue));
  itk::ByteSwapper<short int>::SwapFromSystemToBigEndian(&(this->m_MaximumZValue));
  itk::ByteSwapper<float>::SwapFromSystemToBigEndian(&(this->m_Opacity));
  itk::ByteSwapper<int>::SwapFromSystemToBigEndian(&(this->m_OpacityThickness));
  itk::ByteSwapper<float>::SwapFromSystemToBigEndian(&(this->m_BlendFactor));
}

void
AnalyzeObjectEntry ::Write(std::ofstream & outputFileStream)
{
  outputFileStream.write(reinterpret_cast<char *>(m_Name), sizeof(char) * 32);
  outputFileStream.write(reinterpret_cast<char *>(&m_DisplayFlag), sizeof(m_DisplayFlag));
  outputFileStream.write(reinterpret_cast<char *>(&m_CopyFlag), sizeof(m_CopyFlag));
  outputFileStream.write(reinterpret_cast<char *>(&m_MirrorFlag), sizeof(m_MirrorFlag));
  outputFileStream.write(reinterpret_cast<char *>(&m_StatusFlag), sizeof(m_StatusFlag));
  outputFileStream.write(reinterpret_cast<char *>(&m_NeighborsUsedFlag), sizeof(m_NeighborsUsedFlag));
  outputFileStream.write(reinterpret_cast<char *>(&m_Shades), sizeof(m_Shades));
  outputFileStream.write(reinterpret_cast<char *>(&m_StartRed), sizeof(m_StartRed));
  outputFileStream.write(reinterpret_cast<char *>(&m_StartGreen), sizeof(m_StartGreen));
  outputFileStream.write(reinterpret_cast<char *>(&m_StartBlue), sizeof(m_StartBlue));
  outputFileStream.write(reinterpret_cast<char *>(&m_EndRed), sizeof(m_EndRed));
  outputFileStream.write(reinterpret_cast<char *>(&m_EndGreen), sizeof(m_EndGreen));
  outputFileStream.write(reinterpret_cast<char *>(&m_EndBlue), sizeof(m_EndBlue));
  outputFileStream.write(reinterpret_cast<char *>(&m_XRotation), sizeof(m_XRotation));
  outputFileStream.write(reinterpret_cast<char *>(&m_YRotation), sizeof(m_YRotation));
  outputFileStream.write(reinterpret_cast<char *>(&m_ZRotation), sizeof(m_ZRotation));
  outputFileStream.write(reinterpret_cast<char *>(&m_XTranslation), sizeof(m_XTranslation));
  outputFileStream.write(reinterpret_cast<char *>(&m_YTranslation), sizeof(m_YTranslation));
  outputFileStream.write(reinterpret_cast<char *>(&m_ZTranslation), sizeof(m_ZTranslation));
  outputFileStream.write(reinterpret_cast<char *>(&m_XCenter), sizeof(m_XCenter));
  outputFileStream.write(reinterpret_cast<char *>(&m_YCenter), sizeof(m_YCenter));
  outputFileStream.write(reinterpret_cast<char *>(&m_ZCenter), sizeof(m_ZCenter));
  outputFileStream.write(reinterpret_cast<char *>(&m_XRotationIncrement), sizeof(m_XRotationIncrement));
  outputFileStream.write(reinterpret_cast<char *>(&m_YRotationIncrement), sizeof(m_YRotationIncrement));
  outputFileStream.write(reinterpret_cast<char *>(&m_ZRotationIncrement), sizeof(m_ZRotationIncrement));
  outputFileStream.write(reinterpret_cast<char *>(&m_XTranslationIncrement), sizeof(m_XTranslationIncrement));
  outputFileStream.write(reinterpret_cast<char *>(&m_YTranslationIncrement), sizeof(m_YTranslationIncrement));
  outputFileStream.write(reinterpret_cast<char *>(&m_ZTranslationIncrement), sizeof(m_ZTranslationIncrement));
  outputFileStream.write(reinterpret_cast<char *>(&m_MinimumXValue), sizeof(m_MinimumXValue));
  outputFileStream.write(reinterpret_cast<char *>(&m_MinimumYValue), sizeof(m_MinimumYValue));
  outputFileStream.write(reinterpret_cast<char *>(&m_MinimumZValue), sizeof(m_MinimumZValue));
  outputFileStream.write(reinterpret_cast<char *>(&m_MaximumXValue), sizeof(m_MaximumXValue));
  outputFileStream.write(reinterpret_cast<char *>(&m_MaximumYValue), sizeof(m_MaximumYValue));
  outputFileStream.write(reinterpret_cast<char *>(&m_MaximumZValue), sizeof(m_MaximumZValue));
  outputFileStream.write(reinterpret_cast<char *>(&m_Opacity), sizeof(m_Opacity));
  outputFileStream.write(reinterpret_cast<char *>(&m_OpacityThickness), sizeof(m_OpacityThickness));
  outputFileStream.write(reinterpret_cast<char *>(&m_BlendFactor), sizeof(m_BlendFactor));
}

void
AnalyzeObjectEntry ::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
}

} // namespace itk
