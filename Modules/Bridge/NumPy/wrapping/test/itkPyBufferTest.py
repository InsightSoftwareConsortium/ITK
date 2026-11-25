# ==========================================================================
#
#   Copyright NumFOCUS
#
#   Licensed under the Apache License, Version 2.0 (the "License");
#   you may not use this file except in compliance with the License.
#   You may obtain a copy of the License at
#
#          https://www.apache.org/licenses/LICENSE-2.0.txt
#
#   Unless required by applicable law or agreed to in writing, software
#   distributed under the License is distributed on an "AS IS" BASIS,
#   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#   See the License for the specific language governing permissions and
#   limitations under the License.
#
# ==========================================================================

import sys
import unittest
import warnings
import datetime as dt

import itk
import numpy as np


class TestNumpyITKMemoryviewInterface(unittest.TestCase):
    """This tests numpy array <-> ITK Scalar Image conversion."""

    def setUp(self):
        pass

    def test_NDArrayITKBase_pickle(self):
        """
        Test the serialization of itk.NDArrayITKBase
        """
        Dimension = 3
        ScalarImageType = itk.Image[itk.UC, Dimension]
        RegionType = itk.ImageRegion[Dimension]

        region = RegionType()
        region.SetSize(0, 6)
        region.SetSize(1, 6)
        region.SetSize(2, 6)

        scalarImage = ScalarImageType.New()
        scalarImage.SetRegions(region)
        scalarImage.Allocate(True)
        scalarImage.SetPixel([0, 0, 0], 5)
        scalarImage.SetPixel([0, 0, 1], 3)
        scalarImage.SetPixel([5, 5, 5], 8)
        ndarray_itk_base = itk.array_view_from_image(scalarImage)

        import pickle

        ## test serialization of itk ndarray itk base
        pickled = pickle.dumps(ndarray_itk_base)
        reloaded = pickle.loads(pickled)
        equal = (reloaded == ndarray_itk_base).all()
        assert equal, "Different results before and after pickle"

        try:
            import dask
            from distributed.protocol.serialize import dask_dumps, dask_loads
        except (ImportError, RuntimeError):
            pass
        else:
            header, frames = dask_dumps(ndarray_itk_base)
            recon_obj = dask_loads(header, frames)
            equal = (recon_obj == ndarray_itk_base).all()
            assert equal, "Different results before and after pickle"

    def test_EmptyImage_pickle(self):
        """
        Test the serialization of an empty itk.Image
        """
        Dimension = 3
        ImageType = itk.Image[itk.UC, Dimension]
        RegionType = itk.ImageRegion[Dimension]

        region = RegionType()
        region.SetSize(0, 6)
        region.SetSize(1, 6)
        region.SetSize(2, 6)

        image = ImageType.New()
        image.SetRegions(region)
        spacing = [3.0, 4.0, 5.0]
        image.SetSpacing(spacing)
        # Before allocation
        # scalarImage.Allocate(True)

        import pickle

        pickled = pickle.dumps(image)
        reloaded = pickle.loads(pickled)
        reloaded_spacing = reloaded.GetSpacing()
        spacing = reloaded.GetSpacing()
        assert spacing[0] == reloaded_spacing[0]
        assert spacing[1] == reloaded_spacing[1]
        assert spacing[2] == reloaded_spacing[2]

        ndarray_itk_base = itk.array_view_from_image(image)
        assert ndarray_itk_base == None

    def test_NumPyBridge_itkScalarImage(self):
        "Try to convert all pixel types to NumPy array view"

        Dimension = 3
        ScalarImageType = itk.Image[itk.UC, Dimension]
        RegionType = itk.ImageRegion[Dimension]

        region = RegionType()
        region.SetSize(0, 30)
        region.SetSize(1, 20)
        region.SetSize(2, 10)

        scalarImage = ScalarImageType.New()
        scalarImage.SetRegions(region)
        scalarImage.Allocate()

        scalarndarr = itk.PyBuffer[ScalarImageType].GetArrayViewFromImage(scalarImage)
        self.assertTrue(hasattr(scalarndarr, "itk_base"))
        self.assertTrue(scalarndarr.itk_base, scalarImage)
        self.assertTrue(isinstance(scalarndarr, np.ndarray))
        convertedscalarImage = itk.PyBuffer[ScalarImageType].GetImageViewFromArray(
            scalarndarr
        )

        ScalarDiffFilterType = itk.ComparisonImageFilter[
            ScalarImageType, ScalarImageType
        ]
        ScalarDiffFilter = ScalarDiffFilterType.New()
        ScalarDiffFilter.SetValidInput(scalarImage)
        ScalarDiffFilter.SetTestInput(convertedscalarImage)

        ScalarDiffFilter.Update()
        diff = ScalarDiffFilter.GetTotalDifference()

        self.assertEqual(0, diff)
        assert hasattr(convertedscalarImage, "base")
        self.assertEqual(scalarndarr.ndim, convertedscalarImage.ndim)
        self.assertEqual(scalarndarr.dtype, convertedscalarImage.dtype)
        self.assertTupleEqual(scalarndarr.shape, convertedscalarImage.shape)

    def test_NumPyBridge_itkScalarImageDeepCopy(self):
        "Try to convert all pixel types to NumPy array view with a deep copy"

        Dimension = 3
        ScalarImageType = itk.Image[itk.UC, Dimension]
        RegionType = itk.ImageRegion[Dimension]

        region = RegionType()
        region.SetSize(0, 30)
        region.SetSize(1, 20)
        region.SetSize(2, 10)

        scalarImage = ScalarImageType.New()
        scalarImage.SetRegions(region)
        scalarImage.AllocateInitialized()

        # Check that scalarndarr is not a view, but a deep copy
        scalarndarr = itk.PyBuffer[ScalarImageType].GetArrayFromImage(scalarImage)
        scalarImage.SetPixel([0, 0, 0], 1)
        self.assertNotEqual(scalarImage.GetPixel([0, 0, 0]), scalarndarr[0, 0, 0])

        convertedscalarImage = itk.PyBuffer[ScalarImageType].GetImageFromArray(
            scalarndarr
        )
        scalarndarr[0, 0, 0] = 2
        self.assertNotEqual(
            convertedscalarImage.GetPixel([0, 0, 0]), scalarndarr[0, 0, 0]
        )

    def test_NumPyBridge_itkVectorImage(self):
        "Try to convert all pixel types to NumPy array view"

        Dimension = 3
        VectorImageType = itk.VectorImage[itk.UC, Dimension]
        RegionType = itk.ImageRegion[Dimension]

        region = RegionType()
        region.SetSize(0, 30)
        region.SetSize(1, 20)
        region.SetSize(2, 10)

        vectorImage = VectorImageType.New()
        vectorImage.SetRegions(region)
        vectorImage.SetNumberOfComponentsPerPixel(3)
        vectorImage.Allocate()
        vectorndarr = itk.PyBuffer[VectorImageType].GetArrayViewFromImage(vectorImage)

        convertedvectorImage = itk.PyBuffer[VectorImageType].GetImageViewFromArray(
            vectorndarr, is_vector=True
        )
        self.assertEqual(vectorndarr.ndim, convertedvectorImage.ndim)
        self.assertEqual(vectorndarr.dtype, convertedvectorImage.dtype)
        self.assertTupleEqual(vectorndarr.shape, convertedvectorImage.shape)

        vectorImage = VectorImageType.New()
        vectorImage.SetRegions(region)
        vectorImage.SetNumberOfComponentsPerPixel(1)
        vectorImage.Allocate()
        vectorndarr = itk.PyBuffer[VectorImageType].GetArrayViewFromImage(vectorImage)

        convertedvectorImage = itk.PyBuffer[VectorImageType].GetImageViewFromArray(
            vectorndarr, is_vector=True
        )
        self.assertEqual(vectorndarr.ndim, convertedvectorImage.ndim)
        self.assertEqual(vectorndarr.dtype, convertedvectorImage.dtype)
        self.assertTupleEqual(vectorndarr.shape, convertedvectorImage.shape)

        # F-contiguous vector ndarray: after the simplification of the
        # GetImageViewFromArray vector-shape handling, F-order inputs are
        # internally deep-copied to C order; the resulting image must still
        # report the F-input shape.  Exercise both the components=1 case
        # (where the vector dim collapses) and the multi-component case
        # (which is the more common F-contig regression site).
        for componentCount in (1, 3):
            with self.subTest(componentCount=componentCount):
                vectorImage = VectorImageType.New()
                vectorImage.SetRegions(region)
                vectorImage.SetNumberOfComponentsPerPixel(componentCount)
                vectorImage.Allocate()
                cVectorndarr = itk.PyBuffer[VectorImageType].GetArrayViewFromImage(
                    vectorImage
                )
                # Seed with a known pattern so we can pixel-compare both layouts.
                cVectorndarr[...] = np.arange(
                    cVectorndarr.size, dtype=cVectorndarr.dtype
                ).reshape(cVectorndarr.shape)
                fortranVectorndarr = np.asfortranarray(cVectorndarr)
                self.assertTrue(fortranVectorndarr.flags.f_contiguous)
                with warnings.catch_warnings():
                    warnings.simplefilter("ignore")
                    convertedFortranVectorImage = itk.PyBuffer[
                        VectorImageType
                    ].GetImageViewFromArray(fortranVectorndarr, is_vector=True)
                self.assertEqual(
                    fortranVectorndarr.ndim, convertedFortranVectorImage.ndim
                )
                self.assertEqual(
                    fortranVectorndarr.dtype, convertedFortranVectorImage.dtype
                )
                self.assertTupleEqual(
                    fortranVectorndarr.shape, convertedFortranVectorImage.shape
                )
                # Pixel-data round-trip: F input -> deep-copied C buffer ->
                # array view should equal np.ascontiguousarray of the input.
                roundTripView = itk.PyBuffer[VectorImageType].GetArrayViewFromImage(
                    convertedFortranVectorImage
                )
                self.assertTrue(
                    np.array_equal(
                        roundTripView, np.ascontiguousarray(fortranVectorndarr)
                    )
                )

    def test_NumPyBridge_itkRGBImage(self):
        "Try to convert an RGB ITK image to NumPy array view"

        Dimension = 3
        PixelType = itk.RGBPixel[itk.UC]
        RGBImageType = itk.Image[PixelType, Dimension]
        RegionType = itk.ImageRegion[Dimension]

        region = RegionType()
        region.SetSize(0, 30)
        region.SetSize(1, 20)
        region.SetSize(2, 10)

        rgbImage = RGBImageType.New()
        rgbImage.SetRegions(region)
        rgbImage.Allocate()
        rgbndarr = itk.PyBuffer[RGBImageType].GetArrayViewFromImage(rgbImage)

        convertedRGBImage = itk.PyBuffer[RGBImageType].GetImageViewFromArray(
            rgbndarr, is_vector=True
        )
        self.assertEqual(rgbndarr.ndim, convertedRGBImage.ndim)
        self.assertEqual(rgbndarr.dtype, convertedRGBImage.dtype)
        self.assertTupleEqual(rgbndarr.shape, convertedRGBImage.shape)

    def test_NumPyBridge_itkRGBAImage(self):
        "Try to convert an RGBA ITK image to NumPy array view"

        Dimension = 3
        PixelType = itk.RGBAPixel[itk.UC]
        RGBAImageType = itk.Image[PixelType, Dimension]
        RegionType = itk.ImageRegion[Dimension]

        region = RegionType()
        region.SetSize(0, 30)
        region.SetSize(1, 20)
        region.SetSize(2, 10)

        rgbaImage = RGBAImageType.New()
        rgbaImage.SetRegions(region)
        rgbaImage.Allocate()
        rgbandarr = itk.PyBuffer[RGBAImageType].GetArrayViewFromImage(rgbaImage)

        convertedRGBAImage = itk.PyBuffer[RGBAImageType].GetImageViewFromArray(
            rgbandarr, is_vector=True
        )
        self.assertEqual(rgbandarr.ndim, convertedRGBAImage.ndim)
        self.assertEqual(rgbandarr.dtype, convertedRGBAImage.dtype)
        self.assertTupleEqual(rgbandarr.shape, convertedRGBAImage.shape)

    def test_NumPyBridge_itkVectorPixelImage(self):
        "Try to convert an ITK image with vector pixels to NumPy array view"

        Dimension = 3
        PixelType = itk.Vector[itk.F, Dimension]
        VectorImageType = itk.Image[PixelType, Dimension]
        RegionType = itk.ImageRegion[Dimension]

        region = RegionType()
        region.SetSize(0, 30)
        region.SetSize(1, 20)
        region.SetSize(2, 10)

        vectorImage = VectorImageType.New()
        vectorImage.SetRegions(region)
        vectorImage.Allocate()
        vectorndarr = itk.PyBuffer[VectorImageType].GetArrayViewFromImage(vectorImage)

        convertedVectorImage = itk.PyBuffer[VectorImageType].GetImageViewFromArray(
            vectorndarr, is_vector=True
        )
        self.assertEqual(vectorndarr.ndim, convertedVectorImage.ndim)
        self.assertEqual(vectorndarr.dtype, convertedVectorImage.dtype)
        self.assertTupleEqual(vectorndarr.shape, convertedVectorImage.shape)

    def test_NumPyBridge_FortranOrder(self):
        "Try to convert an ITK image to / from a NumPy array with Fortran order"

        Dimension = 2
        PixelType = itk.ctype("signed short")
        ImageType = itk.Image[PixelType, Dimension]
        dtype = np.int16

        arr = np.arange(6, dtype=dtype)
        arrC = np.reshape(arr, (2, 3), order="C")
        assert arrC.flags.c_contiguous
        image = itk.PyBuffer[ImageType].GetImageViewFromArray(arrC)

        index = itk.Index[Dimension]()
        index[0] = 0
        index[1] = 0
        assert image.GetPixel(index) == 0
        index[0] = 1
        index[1] = 0
        assert image.GetPixel(index) == 1
        index[0] = 0
        index[1] = 1
        assert image.GetPixel(index) == 3

        # F-contiguous arrays are deep-copied to C-contiguous internally so
        # that ITK[i,j] == NumPy[j,i] holds for both layouts. arrFortran has
        # shape (3,2) with values arrFortran[i,j] == arr[i + 3*j]; after the
        # internal C-copy, ITK image dims are (2,3) and pixel (i,j) maps to
        # arrFortran[j,i].
        arrFortran = np.reshape(arr, (3, 2), order="F")
        assert arrFortran.flags.f_contiguous
        with warnings.catch_warnings():
            warnings.simplefilter("ignore")
            image = itk.PyBuffer[ImageType].GetImageViewFromArray(arrFortran)
        index[0] = 0
        index[1] = 0
        assert image.GetPixel(index) == arrFortran[0, 0]
        index[0] = 1
        index[1] = 0
        assert image.GetPixel(index) == arrFortran[0, 1]
        index[0] = 0
        index[1] = 1
        assert image.GetPixel(index) == arrFortran[1, 0]

    def test_NumPyBridge_ImageFromBuffer(self):
        "Create an image with image_from_array with a non-writeable input"

        data = b"hello world "
        array = np.frombuffer(data, dtype=np.uint8)
        array = array.reshape((2, 6))
        image = itk.image_from_array(array)

    def test_non_contiguous_array(self):
        "Check that a non-contiguous array can be converted to an itk.Image without issue"

        data = np.random.random((10, 10, 10))
        data = data[..., 0]  # slicing the array makes it non-contiguous
        assert not data.flags["C_CONTIGUOUS"]
        assert not data.flags["F_CONTIGUOUS"]
        image = itk.image_from_array(data)


if __name__ == "__main__":
    unittest.main(verbosity=2)
