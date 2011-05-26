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

/**
 * Example on the use of DicomImageIO for reading a single DICOM slice, rescale
 * the intensities and save it in a different file format.
 *
 */

import InsightToolkit.*;

public class DicomSliceRead
{
  public static void main( String argv[] )
  {
    System.out.println("DicomSliceRead Example");

    itkImageFileReaderUS2_Pointer reader = itkImageFileReaderUS2.itkImageFileReaderUS2_New();
    itkImageFileWriterUC2_Pointer writer = itkImageFileWriterUC2.itkImageFileWriterUC2_New();

    itkRescaleIntensityImageFilterUS2UC2_Pointer filter = itkRescaleIntensityImageFilterUS2UC2.itkRescaleIntensityImageFilterUS2UC2_New();

    filter.SetInput( reader.GetOutput() );
    writer.SetInput( filter.GetOutput() );

    itkDicomImageIO_Pointer dicomIO = itkDicomImageIO.itkDicomImageIO_New();

    reader.SetImageIO( dicomIO.GetPointer() );

    filter.SetOutputMinimum( (short)0 );
    filter.SetOutputMaximum( (short) 255);

    reader.SetFileName( argv[0] );
    writer.SetFileName( argv[1] );

    writer.Update();
  }

}
