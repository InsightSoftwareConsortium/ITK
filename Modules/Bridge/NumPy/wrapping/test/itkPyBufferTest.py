#==========================================================================
#
#   Copyright NumFOCUS
#
#   Licensed under the Apache License, Version 2.0 (the "License");
#   you may not use this file except in compliance with the License.
#   You may obtain a copy of the License at
#
#          http://www.apache.org/licenses/LICENSE-2.0.txt
#
#   Unless required by applicable law or agreed to in writing, software
#   distributed under the License is distributed on an "AS IS" BASIS,
#   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#   See the License for the specific language governing permissions and
#   limitations under the License.
#
#==========================================================================*/

import sys
import unittest
import datetime as dt

import itk
import numpy as np

class TestNumpyITKMemoryviewInterface(unittest.TestCase):
    """ This tests numpy array <-> ITK Scalar Image conversion. """

    def setUp(self):
        pass

    def test_NumPyBridge_itkScalarImage(self):
        "Try to convert all pixel types to NumPy array view"

        Dimension             = 3
        ScalarImageType       = itk.Image[itk.UC, Dimension]
        RegionType            = itk.ImageRegion[Dimension]

        region                = RegionType()
        region.SetSize(0, 30);
        region.SetSize(1, 20);
        region.SetSize(2, 10);

        scalarImage           = ScalarImageType.New()
        scalarImage.SetRegions(region);
        scalarImage.Allocate();

        scalarndarr           = itk.PyBuffer[ScalarImageType].GetArrayViewFromImage(scalarImage)
        self.assertTrue(hasattr(scalarndarr, 'itk_base'))
        self.assertTrue(scalarndarr.itk_base, scalarImage)
        self.assertTrue(isinstance(scalarndarr, np.ndarray))
        convertedscalarImage  = itk.PyBuffer[ScalarImageType].GetImageViewFromArray(scalarndarr)

        ScalarDiffFilterType  = itk.ComparisonImageFilter[ScalarImageType, ScalarImageType]
        ScalarDiffFilter      = ScalarDiffFilterType.New()
        ScalarDiffFilter.SetValidInput(scalarImage)
        ScalarDiffFilter.SetTestInput(convertedscalarImage)

        ScalarDiffFilter.Update()
        diff = ScalarDiffFilter.GetTotalDifference()

        self.assertEqual(0, diff)
        assert(hasattr(convertedscalarImage, 'base'))
        self.assertEqual(scalarndarr.ndim, convertedscalarImage.ndim)
        self.assertEqual(scalarndarr.dtype, convertedscalarImage.dtype)
        self.assertTupleEqual(scalarndarr.shape, convertedscalarImage.shape)

    def test_NumPyBridge_itkScalarImageDeepCopy(self):
        "Try to convert all pixel types to NumPy array view with a deep copy"

        Dimension             = 3
        ScalarImageType       = itk.Image[itk.UC, Dimension]
        RegionType            = itk.ImageRegion[Dimension]

        region                = RegionType()
        region.SetSize(0, 30);
        region.SetSize(1, 20);
        region.SetSize(2, 10);

        scalarImage           = ScalarImageType.New()
        scalarImage.SetRegions(region);
        scalarImage.Allocate();
        scalarImage.FillBuffer(0)

        # Check that scalarndarr is not a view, but a deep copy
        scalarndarr           = itk.PyBuffer[ScalarImageType].GetArrayFromImage(scalarImage)
        scalarImage.SetPixel([0,0,0],1)
        self.assertNotEqual(scalarImage.GetPixel([0,0,0]), scalarndarr[0,0,0])

        convertedscalarImage  = itk.PyBuffer[ScalarImageType].GetImageFromArray(scalarndarr)
        scalarndarr[0,0,0] = 2
        self.assertNotEqual(convertedscalarImage.GetPixel([0,0,0]), scalarndarr[0,0,0])

    def test_NumPyBridge_itkVectorImage(self):
        "Try to convert all pixel types to NumPy array view"

        Dimension             = 3
        VectorImageType       = itk.VectorImage[itk.UC, Dimension]
        RegionType            = itk.ImageRegion[Dimension]

        region                = RegionType()
        region.SetSize(0, 30);
        region.SetSize(1, 20);
        region.SetSize(2, 10);

        vectorImage           = VectorImageType.New()
        vectorImage.SetRegions(region);
        vectorImage.SetNumberOfComponentsPerPixel(3);
        vectorImage.Allocate();
        vectorndarr           = itk.PyBuffer[VectorImageType].GetArrayViewFromImage(vectorImage)

        convertedvectorImage  = itk.PyBuffer[VectorImageType].GetImageViewFromArray(vectorndarr, is_vector=True)
        self.assertEqual(vectorndarr.ndim, convertedvectorImage.ndim)
        self.assertEqual(vectorndarr.dtype, convertedvectorImage.dtype)
        self.assertTupleEqual(vectorndarr.shape, convertedvectorImage.shape)

    def test_NumPyBridge_itkRGBImage(self):
        "Try to convert an RGB ITK image to NumPy array view"

        Dimension             = 3
        PixelType             = itk.RGBPixel[itk.UC]
        RGBImageType          = itk.Image[PixelType, Dimension]
        RegionType            = itk.ImageRegion[Dimension]

        region                = RegionType()
        region.SetSize(0, 30);
        region.SetSize(1, 20);
        region.SetSize(2, 10);

        rgbImage              = RGBImageType.New()
        rgbImage.SetRegions(region);
        rgbImage.Allocate();
        rgbndarr              = itk.PyBuffer[RGBImageType].GetArrayViewFromImage(rgbImage)

        convertedRGBImage     = itk.PyBuffer[RGBImageType].GetImageViewFromArray(rgbndarr, is_vector=True)
        self.assertEqual(rgbndarr.ndim, convertedRGBImage.ndim)
        self.assertEqual(rgbndarr.dtype, convertedRGBImage.dtype)
        self.assertTupleEqual(rgbndarr.shape, convertedRGBImage.shape)

    def test_NumPyBridge_itkRGBAImage(self):
        "Try to convert an RGBA ITK image to NumPy array view"

        Dimension             = 3
        PixelType             = itk.RGBAPixel[itk.UC]
        RGBAImageType         = itk.Image[PixelType, Dimension]
        RegionType            = itk.ImageRegion[Dimension]

        region                = RegionType()
        region.SetSize(0, 30);
        region.SetSize(1, 20);
        region.SetSize(2, 10);

        rgbaImage             = RGBAImageType.New()
        rgbaImage.SetRegions(region);
        rgbaImage.Allocate();
        rgbandarr             = itk.PyBuffer[RGBAImageType].GetArrayViewFromImage(rgbaImage)

        convertedRGBAImage    = itk.PyBuffer[RGBAImageType].GetImageViewFromArray(rgbandarr, is_vector=True)
        self.assertEqual(rgbandarr.ndim, convertedRGBAImage.ndim)
        self.assertEqual(rgbandarr.dtype, convertedRGBAImage.dtype)
        self.assertTupleEqual(rgbandarr.shape, convertedRGBAImage.shape)

    def test_NumPyBridge_itkVectorPixelImage(self):
        "Try to convert an ITK image with vector pixels to NumPy array view"

        Dimension             = 3
        PixelType             = itk.Vector[itk.F,Dimension]
        VectorImageType       = itk.Image[PixelType, Dimension]
        RegionType            = itk.ImageRegion[Dimension]

        region                = RegionType()
        region.SetSize(0, 30);
        region.SetSize(1, 20);
        region.SetSize(2, 10);

        vectorImage           = VectorImageType.New()
        vectorImage.SetRegions(region);
        vectorImage.Allocate();
        vectorndarr           = itk.PyBuffer[VectorImageType].GetArrayViewFromImage(vectorImage)

        convertedVectorImage  = itk.PyBuffer[VectorImageType].GetImageViewFromArray(vectorndarr, is_vector=True)
        self.assertEqual(vectorndarr.ndim, convertedVectorImage.ndim)
        self.assertEqual(vectorndarr.dtype, convertedVectorImage.dtype)
        self.assertTupleEqual(vectorndarr.shape, convertedVectorImage.shape)

    def test_NumPyBridge_FortranOrder(self):
        "Try to convert an ITK image to / from a NumPy array with Fortran order"

        Dimension = 2
        PixelType = itk.ctype('signed short')
        ImageType = itk.Image[PixelType, Dimension]
        dtype = np.int16

        arr = np.arange(6, dtype=dtype)
        arrC = np.reshape(arr, (2, 3), order='C')
        assert(arrC.flags.c_contiguous)
        image = itk.PyBuffer[ImageType].GetImageViewFromArray(arrC)

        index = itk.Index[Dimension]()
        index[0] = 0
        index[1] = 0
        assert(image.GetPixel(index) == 0)
        index[0] = 1
        index[1] = 0
        assert(image.GetPixel(index) == 1)
        index[0] = 0
        index[1] = 1
        assert(image.GetPixel(index) == 3)

        arrFortran = np.reshape(arr, (3, 2), order='F')
        assert(arrFortran.flags.f_contiguous)
        image = itk.PyBuffer[ImageType].GetImageViewFromArray(arrFortran)
        index[0] = 0
        index[1] = 0
        assert(image.GetPixel(index) == 0)
        index[0] = 1
        index[1] = 0
        assert(image.GetPixel(index) == 1)
        index[0] = 0
        index[1] = 1
        assert(image.GetPixel(index) == 3)

    def test_NumPyBridge_ImageFromBuffer(self):
        "Create an image with image_from_array with a non-writeable input"

        data = b'hello world '
        array = np.frombuffer(data, dtype=np.uint8)
        array = array.reshape((2, 6))
        image = itk.image_from_array(array)

    def test_non_contiguous_array(self):
        "Check that a non-contiguous array can be converted to an itk.Image without issue"

        data = np.random.random((10, 10, 10))
        data = data[..., 0]   # slicing the array makes it non-contiguous
        assert not data.flags['C_CONTIGUOUS']
        assert not data.flags['F_CONTIGUOUS']
        image = itk.image_from_array(data)

if __name__ == '__main__':
    unittest.main(verbosity=2)
