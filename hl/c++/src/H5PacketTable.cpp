/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://www.hdfgroup.org/licenses.               *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/* Packet Table wrapper classes
 *
 * Wraps the H5PT Packet Table C functions in C++ objects
 *
 * Nat Furrer and James Laird
 * February 2004
 */

/* High-level library internal header file */
#include "H5HLprivate2.h"

#include "H5PacketTable.h"

/********************************/
/* PacketTable superclass       */
/********************************/

/* Null constructor
 * Sets table_id to "invalid"
 */
PacketTable::PacketTable() : table_id{H5I_INVALID_HID}
{
}

/* "Open" Constructor
 * Opens an existing packet table, which can contain either fixed-length or
 * variable-length packets.
 */
PacketTable::PacketTable(hid_t fileID, const char *name) : table_id{H5PTopen(fileID, name)}
{
}

/* "Open" Constructor - will be deprecated because of char* name */
PacketTable::PacketTable(hid_t fileID, char *name) : table_id{H5PTopen(fileID, name)}
{
}

/* Destructor
 * Cleans up the packet table
 */
PacketTable::~PacketTable()
{
    H5PTclose(table_id);
}

/* IsValid
 * Returns true if this packet table is valid, false otherwise.
 * Use this after the constructor to ensure HDF did not have
 * any trouble making or opening the packet table.
 */
bool
PacketTable::IsValid() const
{
    return H5PTis_valid(table_id) == 0;
}

/* IsVariableLength
 * Return 1 if this packet table uses variable-length datatype,
 * and 0, otherwise.  Returns -1 if the table is invalid (not open).
 */
int
PacketTable::IsVariableLength() const
{
    return H5PTis_varlen(table_id);
}

/* ResetIndex
 * Sets the index to point to the first packet in the packet table
 */
void
PacketTable::ResetIndex() const
{
    H5PTcreate_index(table_id);
}

/* SetIndex
 * Sets the index to point to the packet specified by index.
 * Returns 0 on success, negative on failure (if index is out of bounds)
 */
int
PacketTable::SetIndex(hsize_t index) const
{
    return H5PTset_index(table_id, index);
}

/* SetIndex
 * Sets the index to point to the packet specified by index.
 * Returns 0 on success, negative on failure (if index is out of bounds)
 */
hsize_t
PacketTable::GetIndex(int &error) const
{
    hsize_t index;

    error = H5PTget_index(table_id, &index);
    if (error < 0)
        return 0;
    else
        return index;
}

/* GetPacketCount
 * Returns the number of packets in the packet table.  Error
 * is set to 0 on success.  On failure, returns 0 and
 * error is set to negative.
 */
hsize_t
PacketTable::GetPacketCount(int &error) const
{
    hsize_t npackets;

    error = H5PTget_num_packets(table_id, &npackets);
    return npackets;
}

/* GetTableId
 * Returns the identifier of the packet table
 */
hid_t
PacketTable::GetTableId() const
{
    return table_id;
}

/* GetDatatype
 * Returns the datatype identifier used by the packet table, on success,
 * or H5I_INVALID_HID, on failure.
 * Note: it is best to avoid using this identifier in applications, unless
 * the desired functionality cannot be performed via the packet table ID.
 */
hid_t
PacketTable::GetDatatype() const
{
    return H5PTget_type(table_id);
}

/* GetDataset
 * Returns the dataset identifier associated with the packet table, on
 * success, or H5I_INVALID_HID, on failure.
 * Note: it is best to avoid using this identifier in applications, unless
 * the desired functionality cannot be performed via the packet table ID.
 */
hid_t
PacketTable::GetDataset() const
{
    return H5PTget_dataset(table_id);
}

/* FreeBuff
 * Frees the buffers created when variable-length packets are read.
 * Takes the number of hvl_t structs to be freed and a pointer to their
 * location in memory.
 * Returns 0 on success, negative on error.
 */
int
PacketTable::FreeBuff(size_t numStructs, hvl_t *buffer) const
{
    return H5PTfree_vlen_buff(table_id, numStructs, buffer);
}

/********************************/
/* Fixed-Length Packet Table    */
/********************************/

/* Constructor
 * Creates a packet table to store either fixed- or variable-length packets.
 * Takes the ID of the file the packet table will be created in, the ID of
 * the property list to specify compression, the name of the packet table,
 * the ID of the datatype, and the size of a memory chunk used in chunking.
 */
FL_PacketTable::FL_PacketTable(hid_t fileID, const char *name, hid_t dtypeID, hsize_t chunkSize,
                               hid_t plistID)
{
    table_id = H5PTcreate(fileID, name, dtypeID, chunkSize, plistID);
}

/* Constructor - deprecated
 * Creates a packet table to store either fixed- or variable-length packets.
 * Takes the ID of the file the packet table will be created in, the ID of
 * the property list to specify compression, the name of the packet table,
 * the ID of the datatype, and the size of a memory chunk used in chunking.
 * Note: The above constructor has a better prototype, which allows default
 * values to be used.  This constructor was only released in 1.10.0.
 */
FL_PacketTable::FL_PacketTable(hid_t fileID, hid_t plistID, const char *name, hid_t dtypeID,
                               hsize_t chunkSize)
{
    table_id = H5PTcreate(fileID, name, dtypeID, chunkSize, plistID);
}

/* Constructor
 * Creates a packet table to store either fixed- or variable-length packets.
 * Takes the ID of the file the packet table will be created in, the name of
 * the packet table, the ID of the datatype of the set, and the size
 * of a memory chunk used in chunking.
 * Note: this overload will be deprecated in favor of the constructor above.
 */
FL_PacketTable::FL_PacketTable(hid_t fileID, char *name, hid_t dtypeID, hsize_t chunkSize, int compression)
{
    table_id = H5PTcreate_fl(fileID, name, dtypeID, chunkSize, compression);
}

/* "Open" Constructor
 * Opens an existing fixed-length packet table.
 * Fails if the packet table specified is variable-length.
 */
FL_PacketTable::FL_PacketTable(hid_t fileID, const char *name) : PacketTable(fileID, name)
{
}

/* "Open" Constructor - will be deprecated because of char* name */
FL_PacketTable::FL_PacketTable(hid_t fileID, char *name) : PacketTable(fileID, name)
{
}

/* AppendPacket
 * Adds a single packet to the packet table.  Takes a pointer
 * to the location of the data in memory.
 * Returns 0 on success, negative on failure
 */
int
FL_PacketTable::AppendPacket(void *data)
{
    return H5PTappend(table_id, 1, data);
}

/* AppendPackets (multiple packets)
 * Adds multiple packets to the packet table.  Takes the number of packets
 * to be added and a pointer to their location in memory.
 * Returns 0 on success, -1 on failure.
 */
int
FL_PacketTable::AppendPackets(size_t numPackets, void *data)
{
    return H5PTappend(table_id, numPackets, data);
}

/* GetPacket (indexed)
 * Gets a single packet from the packet table.  Takes the index
 * of the packet (with 0 being the first packet) and a pointer
 * to memory where the data should be stored.
 * Returns 0 on success, negative on failure
 */
int
FL_PacketTable::GetPacket(hsize_t index, void *data)
{
    return H5PTread_packets(table_id, index, 1, data);
}

/* GetPackets (multiple packets)
 * Gets multiple packets at once, all packets between
 * startIndex and endIndex inclusive.  Also takes a pointer to
 * the memory where these packets should be stored.
 * Returns 0 on success, negative on failure.
 */
int
FL_PacketTable::GetPackets(hsize_t startIndex, hsize_t endIndex, void *data)
{
    // Make sure the range of indexes is valid
    if (startIndex > endIndex)
        return -1;

    return H5PTread_packets(table_id, startIndex, static_cast<size_t>(endIndex - startIndex + 1), data);
}

/* GetNextPacket (single packet)
 * Gets the next packet in the packet table.  Takes a pointer to
 * memory where the packet should be stored.
 * Returns 0 on success, negative on failure.  Index
 * is not advanced to the next packet on failure.
 */
int
FL_PacketTable::GetNextPacket(void *data)
{
    return H5PTget_next(table_id, 1, data);
}

/* GetNextPackets (multiple packets)
 * Gets the next numPackets packets in the packet table.  Takes a
 * pointer to memory where these packets should be stored.
 * Returns 0 on success, negative on failure.  Index
 * is not advanced on failure.
 */
int
FL_PacketTable::GetNextPackets(size_t numPackets, void *data)
{
    return H5PTget_next(table_id, numPackets, data);
}

/* Removed "ifdef VLPT_REMOVED" block. 03/08/2016, -BMR */
