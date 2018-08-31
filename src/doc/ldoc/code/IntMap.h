/* Machine generated. Edit at your own risk 
     Reproduce with the following
    --attribs_default true
    --base_include asdl_base.h
    --default_only true
    --line_width 74
    --mono_types true
    --output_directory .
    --view C
    */
#ifndef _IntMap_
#define _IntMap_
#include "asdl_base.h"

#include "bst.h"


    
/*
    Defined Types
*/
typedef struct IntMap_entry_s* IntMap_entry_ty;
typedef struct IntMap_entries_s* IntMap_entries_ty;
typedef struct IntMap_int_map_s* IntMap_int_map_ty;
typedef struct IntMap_entry_list_s* IntMap_entry_list_ty;

    
/*
    Defined Constructors and Support Functions
*/
IntMap_entry_ty IntMap_entry(int_ty key, int_ty value);
IntMap_entries_ty IntMap_entries(IntMap_entry_list_ty entries);
IntMap_int_map_ty IntMap_int_map(int_ty size, Bst_bst_ty map);
IntMap_entry_list_ty IntMap_entry_list(IntMap_entry_ty head,
                                      IntMap_entry_list_ty tail);
IntMap_entry_ty IntMap_read_entry(instream_ty s);
IntMap_entry_ty IntMap_read_tagged_entry(instream_ty s);
Bst_bst_ty IntMap_read_entries(instream_ty s);
Bst_bst_ty IntMap_read_tagged_entries(instream_ty s);
IntMap_int_map_ty IntMap_read_int_map(instream_ty s);
IntMap_int_map_ty IntMap_read_tagged_int_map(instream_ty s);
IntMap_entry_list_ty IntMap_read_entry_list(instream_ty s);
void IntMap_write_entry(IntMap_entry_ty x, outstream_ty s);
void IntMap_write_tagged_entry(IntMap_entry_ty x, outstream_ty s);
void IntMap_write_entries(Bst_bst_ty x, outstream_ty s);
void IntMap_write_tagged_entries(Bst_bst_ty x, outstream_ty s);
void IntMap_write_int_map(IntMap_int_map_ty x, outstream_ty s);
void IntMap_write_tagged_int_map(IntMap_int_map_ty x, outstream_ty s);
void IntMap_write_entry_list(IntMap_entry_list_ty x, outstream_ty s);

    
/*
    Type Representation
*/
struct IntMap_entry_s { int_ty key; int_ty value;};
struct IntMap_entries_s { IntMap_entry_list_ty entries;};
struct IntMap_int_map_s { int_ty size; Bst_bst_ty map;};
struct IntMap_entry_list_s {
    IntMap_entry_ty head;
    IntMap_entry_list_ty tail;
};


#endif /* _IntMap_ */
