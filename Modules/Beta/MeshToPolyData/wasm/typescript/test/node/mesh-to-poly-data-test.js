import test from 'ava'
import path from 'path'

import { IntTypes, FloatTypes, PixelTypes } from "itk-wasm"

import { readMeshNode } from "@itk-wasm/mesh-io"

import { meshToPolyDataNode, polyDataToMeshNode } from '../../dist/index-node.js'

const inputPathPrefix = path.resolve('..', 'test', 'data', 'input')

const testFilePath = path.join(inputPathPrefix, 'cow.vtk')
const testBYUFilePath = path.resolve(inputPathPrefix, 'cube.byu')

test('meshToPolyData converts a mesh to a polydata', async (t) => {
  const mesh = await readMeshNode(testFilePath)
  const { polyData } = await meshToPolyDataNode(mesh)
  t.is(polyData.numberOfPoints, 2903)
  t.is(polyData.polygonsBufferSize, 15593)
  const { mesh: meshRoundTrip } = await polyDataToMeshNode(polyData)
  t.is(meshRoundTrip.meshType.dimension, 3)
  t.is(meshRoundTrip.meshType.pointComponentType, FloatTypes.Float32)
  t.is(meshRoundTrip.meshType.cellComponentType, IntTypes.UInt32)
  t.is(meshRoundTrip.meshType.pointPixelType, PixelTypes.Scalar)
  t.is(meshRoundTrip.meshType.cellPixelType, PixelTypes.Scalar)
  t.is(meshRoundTrip.numberOfPoints, 2903)
  t.is(meshRoundTrip.numberOfCells, 3263)
})

test('meshToPolyData converts a BYU mesh to a polydata', async (t) => {
  const mesh = await readMeshNode(testBYUFilePath)
  const { polyData } = await meshToPolyDataNode(mesh)
  t.is(polyData.numberOfPoints, 8)
  t.is(polyData.polygonsBufferSize, 30)
  const { mesh: meshRoundTrip } = await polyDataToMeshNode(polyData)
  t.is(meshRoundTrip.meshType.dimension, 3)
  t.is(meshRoundTrip.meshType.pointComponentType, FloatTypes.Float32)
  t.is(meshRoundTrip.meshType.cellComponentType, IntTypes.UInt32)
  t.is(meshRoundTrip.meshType.pointPixelType, PixelTypes.Scalar)
  t.is(meshRoundTrip.meshType.cellPixelType, PixelTypes.Scalar)
  t.is(meshRoundTrip.numberOfPoints, 8)
  t.is(meshRoundTrip.numberOfCells, 6)
})
