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

"""Order-explicit spatial key protocol: the NumPy (z,y,x) frame is a full
isomorphic image of the ITK (x,y,z) frame under phi = flip (P.M.P for
matrices), verified against the C++ geometry oracles in both domains."""

import warnings

import numpy as np

import itk

rng = np.random.default_rng(20260726)


def rot(axis, deg):
    c, s = np.cos(np.radians(deg)), np.sin(np.radians(deg))
    if axis == "z":
        return np.array([[c, -s, 0], [s, c, 0], [0, 0, 1]])
    if axis == "y":
        return np.array([[c, 0, s], [0, 1, 0], [-s, 0, c]])
    return np.array([[1, 0, 0], [0, c, -s], [0, s, c]])


def phi_v(v):
    return np.flip(np.asarray(v, dtype=float))


def phi_m(m):
    return np.flip(np.asarray(m, dtype=float), axis=None)


D_XYZ = rot("z", 30) @ rot("x", 20)
E_XYZ = rot("y", -40) @ rot("z", 10)
TOL = 1e-12


def buffer_address(image):
    return np.asarray(itk.array_view_from_image(image)).__array_interface__["data"][0]


def make_image(start_index):
    image = itk.Image[itk.F, 3].New()
    region = itk.ImageRegion[3]()
    region.SetIndex(start_index)
    region.SetSize([7, 9, 11])
    image.SetRegions(region)
    image.Allocate()
    image.SetSpacing([1.0, 2.0, 3.0])
    image.SetOrigin([5.0, 10.0, 15.0])
    image.SetDirection(itk.matrix_from_array(D_XYZ))
    return image


def continuous_index_oracle(image, ci_xyz):
    ci = itk.ContinuousIndex[itk.D, 3]()
    for k in range(3):
        ci.SetElement(k, float(ci_xyz[k]))
    return np.asarray(image.TransformContinuousIndexToPhysicalPoint(ci))


for start_index in ([0, 0, 0], [4, -3, 10]):
    image = make_image(start_index)

    o_xyz = image["origin_xyz"]
    s_xyz = image["spacing_xyz"]
    d_xyz = image["direction_xyz"]
    i_xyz = image["index_xyz"]
    n_xyz = image["size_xyz"]
    o_zyx = image["origin_zyx"]
    s_zyx = image["spacing_zyx"]
    d_zyx = image["direction_zyx"]
    i_zyx = image["index_zyx"]
    n_zyx = image["size_zyx"]

    # xyz keys match member functions; zyx keys are phi of xyz
    assert np.allclose(o_xyz, np.asarray(image.GetOrigin()))
    assert np.allclose(s_xyz, np.asarray(image.GetSpacing()))
    assert np.allclose(d_xyz, itk.array_from_matrix(image.GetDirection()))
    assert np.array_equal(
        i_xyz, np.asarray(image.GetLargestPossibleRegion().GetIndex())
    )
    assert np.array_equal(n_xyz, np.asarray(image.GetLargestPossibleRegion().GetSize()))
    for a, b in ((o_zyx, o_xyz), (s_zyx, s_xyz), (i_zyx, i_xyz), (n_zyx, n_xyz)):
        assert np.allclose(a, phi_v(b))
    assert np.allclose(d_zyx, phi_m(d_xyz))
    assert np.asarray(itk.array_view_from_image(image)).shape == tuple(
        int(v) for v in n_zyx
    )

    # phi is a homomorphism: composition and inversion close in either domain
    assert np.allclose(phi_m(D_XYZ @ E_XYZ), phi_m(D_XYZ) @ phi_m(E_XYZ))
    assert np.allclose(phi_m(np.linalg.inv(D_XYZ)), np.linalg.inv(phi_m(D_XYZ)))

    for _ in range(100):
        ci = rng.uniform(-5, 15, 3)
        p_oracle = continuous_index_oracle(image, ci)
        # forward computed in each domain vs oracle
        p_itk = o_xyz + d_xyz @ (s_xyz * ci)
        p_np = o_zyx + d_zyx @ (s_zyx * phi_v(ci))
        assert np.abs(p_itk - p_oracle).max() < TOL
        assert np.abs(p_np - phi_v(p_oracle)).max() < TOL

        p = rng.uniform(-50, 80, 3)
        ci_oracle = np.asarray(
            image.TransformPhysicalPointToContinuousIndex([float(v) for v in p])
        )
        # inverse computed in each domain vs oracle
        ci_itk = np.linalg.inv(d_xyz) @ (p - o_xyz) / s_xyz
        ci_np = np.linalg.inv(d_zyx) @ (phi_v(p) - o_zyx) / s_zyx
        assert np.abs(ci_itk - ci_oracle).max() < TOL
        assert np.abs(ci_np - phi_v(ci_oracle)).max() < TOL

    # region completeness: physical location of array voxel [0, 0, 0]
    p0_oracle = np.asarray(image.TransformIndexToPhysicalPoint(start_index))
    assert np.allclose(o_xyz + d_xyz @ (s_xyz * i_xyz), p0_oracle)
    assert np.allclose(phi_v(o_zyx + d_zyx @ (s_zyx * i_zyx)), p0_oracle)

    # setters round-trip in both conventions
    other = make_image(start_index)
    other["origin_zyx"] = o_zyx
    other["spacing_zyx"] = s_zyx
    other["direction_zyx"] = d_zyx
    assert np.allclose(np.asarray(other.GetOrigin()), o_xyz)
    assert np.allclose(np.asarray(other.GetSpacing()), s_xyz)
    assert np.allclose(itk.array_from_matrix(other.GetDirection()), d_xyz)
    other["origin_xyz"] = o_xyz + 1.0
    other["direction_xyz"] = E_XYZ
    assert np.allclose(np.asarray(other.GetOrigin()), o_xyz + 1.0)
    assert np.allclose(itk.array_from_matrix(other.GetDirection()), E_XYZ)

    # index/size writes go through SetRegions + Allocate
    resized = make_image(start_index)
    resized["index_xyz"] = [1, 2, 3]
    assert np.array_equal(
        np.asarray(resized.GetLargestPossibleRegion().GetIndex()), [1, 2, 3]
    )
    resized["index_zyx"] = phi_v([1, 2, 3]).astype(int)
    assert np.array_equal(resized["index_xyz"], [1, 2, 3])
    before = buffer_address(resized)
    resized["size_zyx"] = [13, 9, 7]
    assert np.array_equal(
        np.asarray(resized.GetLargestPossibleRegion().GetSize()), [7, 9, 13]
    )
    view = np.asarray(itk.array_view_from_image(resized))
    assert view.shape == (13, 9, 7)
    assert buffer_address(resized) != before, "size write must reallocate storage"
    view[-1, -1, -1] = 42.0
    assert np.asarray(itk.array_view_from_image(resized))[-1, -1, -1] == 42.0

    # bare keys warn but keep their historical (z,y,x) behavior
    with warnings.catch_warnings(record=True) as caught:
        warnings.simplefilter("always")
        assert np.allclose(image["origin"], o_zyx)
        assert np.allclose(image["spacing"], s_zyx)
        assert np.allclose(image["direction"], d_zyx)
        image["origin"] = o_zyx
    deprecations = [w for w in caught if issubclass(w.category, DeprecationWarning)]
    assert len(deprecations) == 4
    assert "6706" in str(deprecations[0].message)

    # ITK_FUTURE_LEGACY_REMOVE escalates bare-key access to FutureWarning
    import itkConfig

    saved = itkConfig.FutureLegacyRemove
    try:
        itkConfig.FutureLegacyRemove = True
        with warnings.catch_warnings(record=True) as caught:
            warnings.simplefilter("always")
            image["origin"]
        assert len(caught) == 1 and issubclass(caught[0].category, FutureWarning)
    finally:
        itkConfig.FutureLegacyRemove = saved

    # explicit keys never warn
    with warnings.catch_warnings(record=True) as caught:
        warnings.simplefilter("always")
        image["origin_zyx"]
        image["direction_xyz"]
        image["origin_zyx"] = o_zyx
    assert not [w for w in caught if issubclass(w.category, DeprecationWarning)]

    # all protocol keys are advertised
    advertised = set(image.keys())
    for suffix in ("xyz", "zyx"):
        for base in ("origin", "spacing", "direction", "index", "size"):
            assert f"{base}_{suffix}" in advertised

print("Test finished.")
