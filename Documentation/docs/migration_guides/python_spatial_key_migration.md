# Python spatial key migration

The bare `image['origin']`, `image['spacing']`, and `image['direction']`
string keys on `itk.Image` are deprecated in favor of order-explicit keys.
The bare keys are order-ambiguous across toolkits: ITK returns NumPy
`(z, y, x)` order while SimpleITK returns `(x, y, z)` order for the same key
names, so generic code consuming these keys silently corrupts geometry when
handed the other toolkit's image
([issue #6706](https://github.com/InsightSoftwareConsortium/ITK/issues/6706)).

## The order-explicit keys

| Key | Order | Value |
|---|---|---|
| `origin_xyz`, `spacing_xyz` | ITK `(x, y, z)` | `GetOrigin()` / `GetSpacing()` |
| `direction_xyz` | ITK | `GetDirection()` as a NumPy matrix |
| `index_xyz`, `size_xyz` | ITK | LargestPossibleRegion index / size |
| `origin_zyx`, `spacing_zyx`, `index_zyx`, `size_zyx` | NumPy `(z, y, x)` | `np.flip` of the `_xyz` value |
| `direction_zyx` | NumPy | `np.flip(direction_xyz, axis=None)` (= P·D·P) |

The `_zyx` frame is a complete coordinate frame matching `np.array(image)`
indexing, with physical coordinates also listed in `(z, y, x)` order:

```python
# forward, equals np.flip(TransformContinuousIndexToPhysicalPoint(i)):
p = image['origin_zyx'] + image['direction_zyx'] @ (image['spacing_zyx'] * i)
# inverse, equals np.flip(TransformPhysicalPointToContinuousIndex(p)):
i = np.linalg.inv(image['direction_zyx']) @ (p - image['origin_zyx']) / image['spacing_zyx']
# np.array(image)[0, 0, 0] sits at continuous index image['index_zyx']
```

Writing `index_*` or `size_*` calls `SetRegions()` followed by `Allocate()`, so
the pixel buffer always matches the advertised extent. A size change discards
the previous pixel data.

## Migrating

The bare keys returned `(z, y, x)` order, so the drop-in replacement is the
`_zyx` key:

```python
spacing = image['spacing']      # deprecated
spacing = image['spacing_zyx']  # identical values
spacing = image['spacing_xyz']  # GetSpacing() order, matches SimpleITK
```

## Warning behavior

- By default, bare-key access emits a `DeprecationWarning` (hidden outside
  `__main__` unless enabled with `-W` or `warnings.simplefilter`).
- When ITK is configured with `ITK_FUTURE_LEGACY_REMOVE=ON`, the warning is a
  `FutureWarning`, which Python displays everywhere by default.
- The `ITK_PYTHON_FUTURE_LEGACY_REMOVE` environment variable overrides the
  configured default at runtime (`itkConfig.FutureLegacyRemove`).
