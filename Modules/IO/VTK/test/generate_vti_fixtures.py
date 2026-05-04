#!/usr/bin/env python3
"""Generate synthetic .vti test fixtures for VTIImageIO regression tests.

Writes small, deterministic VTK XML ImageData (.vti) files plus MetaIO
(.mhd + .raw) oracles under Modules/IO/VTK/test/Input/.  Fixtures are
kept below the 100 KB per-file hook cap so they can live directly in
the git history without relying on ExternalData infrastructure.

Usage:
    python3 Modules/IO/VTK/test/generate_vti_fixtures.py

Re-running is idempotent: outputs are byte-deterministic for a given
Python version.  The generator uses only the Python standard library
(xml, struct, base64, zlib) — no pvpython / vtk / paraview required.

Two caveats:
  1. Fixtures produced here are NOT from a reference writer; they
     only test "our reader agrees with this generator's encoding".
     Cross-validation against real ParaView output must happen via
     ExternalData once the content-link-upload.itk.org migration
     completes (see ITK issue #4340).
  2. Guard fixtures (VTI_guard_*.vti) intentionally carry invalid or
     placeholder payloads.  They exist only to trigger the
     F-NNN guard exceptions and MUST NOT be used in round-trip tests.
"""
from __future__ import annotations

import base64
import pathlib
import struct
import zlib


# ---------------------------------------------------------------------------
# MetaIO oracle writer
# ---------------------------------------------------------------------------
def write_mhd(
    path: pathlib.Path,
    dim_size: tuple[int, ...],
    spacing: tuple[float, ...],
    direction: tuple[float, ...],  # row-major 3x3 = 9 floats
    element_type: str,  # "MET_FLOAT", "MET_UCHAR", etc.
    n_components: int,
    raw_name: str,
    raw_bytes: bytes,
) -> None:
    """Write a MetaIO .mhd + companion .raw pair.

    Direction is row-major 3x3 regardless of image dimensionality;
    2D fixtures pad with the identity z-row / z-column.
    """
    assert len(dim_size) == 3
    assert len(spacing) == 3
    assert len(direction) == 9
    header = [
        "ObjectType = Image",
        "NDims = 3",
        "BinaryData = True",
        "BinaryDataByteOrderMSB = False",
        "CompressedData = False",
        f"TransformMatrix = {' '.join(repr(v) for v in direction)}",
        "Offset = 0 0 0",
        "CenterOfRotation = 0 0 0",
        f"ElementSpacing = {' '.join(repr(v) for v in spacing)}",
        f"DimSize = {' '.join(str(v) for v in dim_size)}",
    ]
    if n_components != 1:
        header.append(f"ElementNumberOfChannels = {n_components}")
    header.append(f"ElementType = {element_type}")
    header.append(f"ElementDataFile = {raw_name}")
    path.write_text("\n".join(header) + "\n")
    (path.parent / raw_name).write_bytes(raw_bytes)


# ---------------------------------------------------------------------------
# VTI writers
# ---------------------------------------------------------------------------
def _vti_open(
    whole_extent: tuple[int, int, int, int, int, int],
    origin: tuple[float, float, float],
    spacing: tuple[float, float, float],
    direction: tuple[float, ...] | None,
    header_type: str = "UInt64",
    byte_order: str = "LittleEndian",
    compressor: str | None = None,
    version: str = "1.0",
) -> str:
    attrs = [
        'type="ImageData"',
        f'version="{version}"',
        f'byte_order="{byte_order}"',
        f'header_type="{header_type}"',
    ]
    if compressor:
        attrs.append(f'compressor="{compressor}"')
    img_attrs = [
        f'WholeExtent="{" ".join(str(v) for v in whole_extent)}"',
        f'Origin="{" ".join(repr(v) for v in origin)}"',
        f'Spacing="{" ".join(repr(v) for v in spacing)}"',
    ]
    if direction is not None:
        img_attrs.append(f'Direction="{" ".join(repr(v) for v in direction)}"')
    lines = [
        '<?xml version="1.0"?>',
        f'<VTKFile {" ".join(attrs)}>',
        f'  <ImageData {" ".join(img_attrs)}>',
        f'    <Piece Extent="{" ".join(str(v) for v in whole_extent)}">',
    ]
    return "\n".join(lines) + "\n"


def _vti_close() -> str:
    return "    </Piece>\n  </ImageData>\n</VTKFile>\n"


def _header_word(value: int, header_type: str) -> bytes:
    return struct.pack("<Q" if header_type == "UInt64" else "<I", value)


def _pack_uncompressed_block(payload: bytes, header_type: str) -> bytes:
    """VTK block layout for uncompressed inline binary: single UInt header = nbytes."""
    return _header_word(len(payload), header_type) + payload


def _pack_zlib_single_block(payload: bytes, header_type: str) -> bytes:
    """VTK multi-block ZLib header with a single block covering all payload."""
    compressed = zlib.compress(payload, level=6)
    return b"".join(
        [
            _header_word(1, header_type),  # num_blocks
            _header_word(len(payload), header_type),  # uncompressed_standard_block_size
            _header_word(len(payload), header_type),  # uncompressed_last_block_size
            _header_word(len(compressed), header_type),  # compressed_size_of_block_0
            compressed,
        ]
    )


def write_vti_ascii_scalar(
    path: pathlib.Path,
    whole_extent,
    origin,
    spacing,
    direction,
    vtk_type: str,
    name: str,
    values,
    *,
    values_per_line: int = 8,
) -> None:
    body = [_vti_open(whole_extent, origin, spacing, direction)]
    body.append(f'      <PointData Scalars="{name}">\n')
    body.append(
        f'        <DataArray type="{vtk_type}" Name="{name}" '
        f'NumberOfComponents="1" format="ascii">\n'
    )
    for i in range(0, len(values), values_per_line):
        body.append(
            "          "
            + " ".join(repr(v) for v in values[i : i + values_per_line])
            + "\n"
        )
    body.append("        </DataArray>\n")
    body.append("      </PointData>\n      <CellData>\n      </CellData>\n")
    body.append(_vti_close())
    path.write_text("".join(body))


def write_vti_appended(
    path: pathlib.Path,
    whole_extent,
    origin,
    spacing,
    direction,
    *,
    vtk_type: str,
    name: str,
    n_components: int,
    payload: bytes,
    encoding: str,  # "raw" or "base64"
    header_type: str = "UInt64",
    compressor: str | None = None,
    active_role: str = "Scalars",
) -> None:
    """Write an appended-data .vti with a single DataArray at offset 0."""
    if compressor == "vtkZLibDataCompressor":
        block = _pack_zlib_single_block(payload, header_type)
    elif compressor is None:
        block = _pack_uncompressed_block(payload, header_type)
    else:
        raise ValueError(
            f"Generator does not know how to pack compressor={compressor!r}"
        )

    header_txt = _vti_open(
        whole_extent,
        origin,
        spacing,
        direction,
        header_type=header_type,
        compressor=compressor,
    )
    da = (
        f'      <PointData {active_role}="{name}">\n'
        f'        <DataArray type="{vtk_type}" Name="{name}" '
        f'NumberOfComponents="{n_components}" format="appended" offset="0"/>\n'
        f"      </PointData>\n      <CellData>\n      </CellData>\n"
    )

    with path.open("wb") as f:
        f.write(header_txt.encode("utf-8"))
        f.write(da.encode("utf-8"))
        f.write(b"    </Piece>\n  </ImageData>\n")
        f.write(f'  <AppendedData encoding="{encoding}">\n   _'.encode())
        if encoding == "raw":
            f.write(block)
        elif encoding == "base64":
            f.write(base64.b64encode(block))
        else:
            raise ValueError(f"unknown AppendedData encoding: {encoding}")
        f.write(b"\n  </AppendedData>\n</VTKFile>\n")


def write_vti_guard(
    path: pathlib.Path,
    *,
    compressor: str | None = None,
    multipiece: bool = False,
) -> None:
    """Emit a minimal .vti whose XML header triggers a guard-exception path.

    Payload bytes are placeholders; the guard fires during header parse
    before any decoding runs.
    """
    extent = (0, 1, 0, 1, 0, 0)
    origin = (0.0, 0.0, 0.0)
    spacing = (1.0, 1.0, 1.0)
    hdr = _vti_open(extent, origin, spacing, None, compressor=compressor)
    if multipiece:
        # Emit two <Piece> tags inside a single ImageData to trigger F-005.
        # Replace the single opening <Piece ...> line from _vti_open with two
        # self-closed pieces, then close ImageData + VTKFile manually (do NOT
        # call _vti_close(), which would leave an orphan </Piece>).
        hdr = hdr.replace(
            f'    <Piece Extent="{" ".join(str(v) for v in extent)}">\n',
            '    <Piece Extent="0 0 0 1 0 0">\n'
            "      <PointData/><CellData/>\n"
            "    </Piece>\n"
            '    <Piece Extent="1 1 0 1 0 0">\n'
            "      <PointData/><CellData/>\n"
            "    </Piece>\n",
        )
        body = hdr + "  </ImageData>\n</VTKFile>\n"
    else:
        body = (
            hdr
            + '      <PointData Scalars="scalars">\n'
            + '        <DataArray type="Float32" Name="scalars" '
            'NumberOfComponents="1" format="appended" offset="0"/>\n'
            + "      </PointData>\n      <CellData>\n      </CellData>\n"
            + "    </Piece>\n  </ImageData>\n"
            + '  <AppendedData encoding="raw">\n   _placeholder\n'
            + "  </AppendedData>\n</VTKFile>\n"
        )
    path.write_text(body)


# ---------------------------------------------------------------------------
# Fixture catalogue
# ---------------------------------------------------------------------------
def main() -> None:
    out = pathlib.Path(__file__).resolve().parent / "Input"
    out.mkdir(exist_ok=True)

    IDENTITY = (1.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 1.0)

    # --- Fixture A: UInt8 scalar, 4x4x2, uncompressed appended-raw, UInt64 header.
    extent = (0, 3, 0, 3, 0, 1)
    dim_size = (4, 4, 2)
    scalar_u8 = bytes(range(32))
    write_vti_appended(
        out / "VTI_scalar_u8_appended_raw.vti",
        extent,
        (0.0, 0.0, 0.0),
        (1.0, 1.0, 1.0),
        IDENTITY,
        vtk_type="UInt8",
        name="scalars",
        n_components=1,
        payload=scalar_u8,
        encoding="raw",
    )
    write_mhd(
        out / "VTI_scalar_u8_appended_raw.mhd",
        dim_size,
        (1.0, 1.0, 1.0),
        IDENTITY,
        "MET_UCHAR",
        1,
        "VTI_scalar_u8_appended_raw.raw",
        scalar_u8,
    )

    # --- Fixture B: Float32 scalar, ZLib-compressed appended-raw, UInt64 header.
    scalar_f32 = struct.pack("<32f", *[i * 0.25 for i in range(32)])
    write_vti_appended(
        out / "VTI_scalar_f32_zlib_appended.vti",
        extent,
        (0.0, 0.0, 0.0),
        (1.0, 1.0, 1.0),
        IDENTITY,
        vtk_type="Float32",
        name="scalars",
        n_components=1,
        payload=scalar_f32,
        encoding="raw",
        compressor="vtkZLibDataCompressor",
    )
    write_mhd(
        out / "VTI_scalar_f32_zlib_appended.mhd",
        dim_size,
        (1.0, 1.0, 1.0),
        IDENTITY,
        "MET_FLOAT",
        1,
        "VTI_scalar_f32_zlib_appended.raw",
        scalar_f32,
    )

    # --- Fixture C: RGBA<UInt8>, appended-raw, UInt64 header.
    rgba_payload = bytes(
        b
        for i in range(32)
        for b in (i * 8 % 256, (i * 8 + 2) % 256, (i * 8 + 4) % 256, 255)
    )
    write_vti_appended(
        out / "VTI_rgba_u8_appended_raw.vti",
        extent,
        (0.0, 0.0, 0.0),
        (1.0, 1.0, 1.0),
        IDENTITY,
        vtk_type="UInt8",
        name="rgba",
        n_components=4,
        payload=rgba_payload,
        encoding="raw",
    )
    write_mhd(
        out / "VTI_rgba_u8_appended_raw.mhd",
        dim_size,
        (1.0, 1.0, 1.0),
        IDENTITY,
        "MET_UCHAR",
        4,
        "VTI_rgba_u8_appended_raw.raw",
        rgba_payload,
    )

    # --- Fixture C2: Vector<Float32, 3>, ZLib appended-raw, UInt64 header.
    # Mirrors the shape of ParaView's VHFColorZLib.vti (3-component Float32,
    # vtkZLibDataCompressor, appended-raw, UInt64 header) so this test
    # covers the same code path as the upstream-broken
    # itkVTIImageIOReadWriteTestVHFColorZLib test without depending on an
    # ExternalData-hosted ParaView fixture (blocked on ITK #4340).  The
    # PointData Vectors="vectors" hint makes the reader dispatch to
    # IOPixelEnum::VECTOR rather than RGB.
    vec3_payload = struct.pack(
        f"<{32 * 3}f",
        *[(i % 7) * 0.125 + (c + 1) * 0.5 for i in range(32) for c in range(3)],
    )
    write_vti_appended(
        out / "VTI_vector3_f32_zlib_appended.vti",
        extent,
        (0.0, 0.0, 0.0),
        (1.0, 1.0, 1.0),
        IDENTITY,
        vtk_type="Float32",
        name="vectors",
        n_components=3,
        payload=vec3_payload,
        encoding="raw",
        compressor="vtkZLibDataCompressor",
        active_role="Vectors",
    )
    write_mhd(
        out / "VTI_vector3_f32_zlib_appended.mhd",
        dim_size,
        (1.0, 1.0, 1.0),
        IDENTITY,
        "MET_FLOAT",
        3,
        "VTI_vector3_f32_zlib_appended.raw",
        vec3_payload,
    )

    # --- Fixture D: Symmetric tensor Float32, ASCII, 2x2x1 = 4 pixels.
    # Per-pixel tensor in matrix form e[i][j] with i,j in [0..2]:
    #   e[0][0]=i*1.0  e[0][1]=i*2.0  e[0][2]=i*3.0
    #   e[1][1]=i*4.0  e[1][2]=i*5.0  e[2][2]=i*6.0
    # VTK canonical order: [XX, YY, ZZ, XY, YZ, XZ]
    # ITK internal order:  [e00, e01, e02, e11, e12, e22]
    tensor_extent = (0, 1, 0, 1, 0, 0)
    tensor_dim = (2, 2, 1)
    vtk_floats = []  # VTK canonical order for the .vti
    itk_floats = []  # ITK internal order for the MHD oracle
    for p in range(4):
        e00 = p * 1.0
        e01 = p * 2.0
        e02 = p * 3.0
        e11 = p * 4.0
        e12 = p * 5.0
        e22 = p * 6.0
        vtk_floats.extend([e00, e11, e22, e01, e12, e02])  # XX YY ZZ XY YZ XZ
        itk_floats.extend([e00, e01, e02, e11, e12, e22])  # upper-triangular row-major
    # Inline-write tensor ASCII VTI with Tensors="..." active hint.
    # (The generic write_vti_ascii_scalar helper is scalar-only.)
    lines = [_vti_open(tensor_extent, (0.0, 0.0, 0.0), (1.0, 1.0, 1.0), IDENTITY)]
    lines.append('      <PointData Tensors="tensors">\n')
    lines.append(
        '        <DataArray type="Float32" Name="tensors" '
        'NumberOfComponents="6" format="ascii">\n'
    )
    for i in range(0, len(vtk_floats), 6):
        lines.append(
            "          " + " ".join(repr(v) for v in vtk_floats[i : i + 6]) + "\n"
        )
    lines.append("        </DataArray>\n")
    lines.append("      </PointData>\n      <CellData>\n      </CellData>\n")
    lines.append(_vti_close())
    (out / "VTI_tensor_f32_ascii.vti").write_text("".join(lines))
    write_mhd(
        out / "VTI_tensor_f32_ascii.mhd",
        tensor_dim,
        (1.0, 1.0, 1.0),
        IDENTITY,
        "MET_FLOAT",
        6,
        "VTI_tensor_f32_ascii.raw",
        struct.pack(f"<{len(itk_floats)}f", *itk_floats),
    )

    # --- Guard fixtures (no MHD oracle; tests only assert exception).
    write_vti_guard(out / "VTI_guard_lz4.vti", compressor="vtkLZ4DataCompressor")
    write_vti_guard(out / "VTI_guard_lzma.vti", compressor="vtkLZMADataCompressor")
    write_vti_guard(
        out / "VTI_guard_unknown_compressor.vti", compressor="vtkBogusDataCompressor"
    )
    write_vti_guard(out / "VTI_guard_multipiece.vti", multipiece=True)

    # Summary
    print("Generated VTI fixtures under", out)
    for f in sorted(out.glob("VTI_*.vti")):
        print(f"  {f.stat().st_size:>6}  {f.name}")
    for f in sorted(out.glob("VTI_*.mhd")):
        print(f"  {f.stat().st_size:>6}  {f.name}")


if __name__ == "__main__":
    main()
