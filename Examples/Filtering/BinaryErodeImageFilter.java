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
 *  Example on the use of the BinaryErodeImageFilter
 *
 */

import InsightToolkit.*;

public class BinaryErodeImageFilter
{
  public static void main( String argv[] )
  {
    System.out.println("BinaryErodeImageFilter Example");

    itkImageFileReaderUC2_Pointer reader = itkImageFileReaderUC2.itkImageFileReaderUC2_New();
    itkImageFileWriterUC2_Pointer writer = itkImageFileWriterUC2.itkImageFileWriterUC2_New();

    itkBinaryErodeImageFilterUC2UC2_Pointer filter = itkBinaryErodeImageFilterUC2UC2.itkBinaryErodeImageFilterUC2UC2_New();

    filter.SetInput( reader.GetOutput() );
    writer.SetInput( filter.GetOutput() );

    reader.SetFileName( argv[0] );
    writer.SetFileName( argv[1] );


    itkBinaryBallStructuringElementUC2 element = new itkBinaryBallStructuringElementUC2();

    element.SetRadius( 1 );
    element.CreateStructuringElement();

    filter.SetKernel( element );

    short value = 255;

    filter.SetErodeValue( value );

    writer.Update();
  }

}


