# ITKIOStimulate

Reads and writes Stimulate (CMRR, University of Minnesota) SDT/SPR images.
The format description below is from the Stimulate User's Guide, appendix
"Sdt File Format", which is no longer online.

## File pair

- `.sdt` (Signal DaTa): raw binary voxel data only.
- `.spr` (Signal PaRameters): ASCII, one `attribute: value` pair per line.

Pass the `.spr` file to the reader, not the `.sdt` file.

## Mandatory parameters

| Attribute | Meaning |
|---|---|
| `numDim` | number of dimensions |
| `dim` | number of voxels along each dimension |
| `dataType` | one of `BYTE`, `WORD`, `LWORD`, `REAL`, `COMPLEX` |

## Optional parameters

| Attribute | Meaning |
|---|---|
| `origin` | center of the first voxel, one value per dimension; if absent and `fov` is given, the image is centered |
| `fov` | distance between the outer edges of the first and last voxel, per dimension; default `interval * dim` |
| `interval` | center-to-center voxel spacing, per dimension; default `fov / dim` |
| `displayRange` | `low high`; values below `low` display black, above `high` white, linear between |
| `fidName` | name of the associated `.sdt` file |
| `sdtOrient` | slice orientation, e.g. `sag` |

## Example

```text
numDim: 4
dim: 128 128 20 150
origin: -10.000000 -10.000000 0.000000 0.000000
fov: 20.000000 20.000000 2.000000 15.000000
interval: 0.156250 0.156250 1.000000 1.000000
dataType: REAL
displayRange: 0.000000 347.904358
fidName: stimFid.sdt
sdtOrient: sag
```
