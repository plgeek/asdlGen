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
#include "IntMap.h"
  /* proto_types for wrapper functions */
  static entries2bst(IntMap_entries_ty x) {
    /* code to convert int_map to a bst */ 
 }

  static IntMap_entries_ty bst2entries(Bst_bst_ty x) {
 /* code to convert bst to an int map */
 }

IntMap_entry_ty IntMap_entry(int_ty key, int_ty value)
{
     IntMap_entry_ty ret;
     ret = malloc(sizeof(*ret));
     if(ret == NULL)
         die();
     ret->key = key;
     ret->value = value;
     return ret;
     
}
IntMap_entries_ty IntMap_entries(IntMap_entry_list_ty entries)
{
     IntMap_entries_ty ret;
     ret = malloc(sizeof(*ret));
     if(ret == NULL)
         die();
     ret->entries = entries;
     return ret;
     
}
IntMap_int_map_ty IntMap_int_map(int_ty size, Bst_bst_ty map)
{
     IntMap_int_map_ty ret;
     ret = malloc(sizeof(*ret));
     if(ret == NULL)
         die();
     ret->size = size;
     ret->map = map;
     return ret;
     
}
IntMap_entry_list_ty IntMap_entry_list(IntMap_entry_ty head,
                                      IntMap_entry_list_ty tail)
{
     IntMap_entry_list_ty ret;
     ret = malloc(sizeof(*ret));
     if(ret == NULL)
         die();
     ret->head = head;
     ret->tail = tail;
     return ret;
     
}
IntMap_entry_ty IntMap_read_entry(instream_ty s)
{
     IntMap_entry_ty ret;
     {
          IntMap_entry_ty t10_;
          t10_ = malloc(sizeof(*t10_));
          if(t10_ == NULL)
              die();
          t10_->key = read_int(s);
          t10_->value = read_int(s);
          ret = t10_;
          
     }
     return ret;
     
}
IntMap_entry_ty IntMap_read_tagged_entry(instream_ty s)
{
     IntMap_entry_ty ret;
     if(read_tag(s) != 6)
         die();
     ret = IntMap_read_entry(s);
     return ret;
     
}
Bst_bst_ty IntMap_read_entries(instream_ty s)
{
     Bst_bst_ty ret;
     {
          IntMap_entries_ty t10_;
          t10_ = malloc(sizeof(*t10_));
          if(t10_ == NULL)
              die();
          t10_->entries = IntMap_read_entry_list(s);
          ret = bst2entries(t10_);
          
     }
     return ret;
     
}
Bst_bst_ty IntMap_read_tagged_entries(instream_ty s)
{
     Bst_bst_ty ret;
     if(read_tag(s) != 5)
         die();
     ret = IntMap_read_entries(s);
     return ret;
     
}
IntMap_int_map_ty IntMap_read_int_map(instream_ty s)
{
     IntMap_int_map_ty ret;
     {
          IntMap_int_map_ty t10_;
          t10_ = malloc(sizeof(*t10_));
          if(t10_ == NULL)
              die();
          t10_->size = read_int(s);
          t10_->map = IntMap_read_entries(s);
          ret = t10_;
          
     }
     return ret;
     
}
IntMap_int_map_ty IntMap_read_tagged_int_map(instream_ty s)
{
     IntMap_int_map_ty ret;
     if(read_tag(s) != 4)
         die();
     ret = IntMap_read_int_map(s);
     return ret;
     
}
IntMap_entry_list_ty IntMap_read_entry_list(instream_ty s)
{
     IntMap_entry_list_ty ret;
     {
          int_ty t1_;
          IntMap_entry_list_ty t2_;
          t1_ = read_tag(s);
          if(t1_ != 0)
              ret = IntMap_entry_list(IntMap_read_entry(s), NULL);
          else
              return NULL;
          t1_--;
          t2_ = ret;
          while(t1_ != 0)
          {
               t2_->tail = IntMap_entry_list(IntMap_read_entry(s), NULL);
               t2_ = t2_->tail;
               t1_--;
               
          }
          
     }
     return ret;
     
}
void IntMap_write_entry(IntMap_entry_ty x, outstream_ty s)
{
     {
          IntMap_entry_ty t10_;
          t10_ = x;
          write_int(t10_->key, s);
          write_int(t10_->value, s);
          
     }
     
}
void IntMap_write_tagged_entry(IntMap_entry_ty x, outstream_ty s)
{
     write_tag(6, s);
     IntMap_write_entry(x, s);
     
}
void IntMap_write_entries(Bst_bst_ty x, outstream_ty s)
{
     {
          IntMap_entries_ty t10_;
          t10_ = entries2bst(x);
          IntMap_write_entry_list(t10_->entries, s);
          
     }
     
}
void IntMap_write_tagged_entries(Bst_bst_ty x, outstream_ty s)
{
     write_tag(5, s);
     IntMap_write_entries(x, s);
     
}
void IntMap_write_int_map(IntMap_int_map_ty x, outstream_ty s)
{
     {
          IntMap_int_map_ty t10_;
          t10_ = x;
          write_int(t10_->size, s);
          IntMap_write_entries(t10_->map, s);
          
     }
     
}
void IntMap_write_tagged_int_map(IntMap_int_map_ty x, outstream_ty s)
{
     write_tag(4, s);
     IntMap_write_int_map(x, s);
     
}
void IntMap_write_entry_list(IntMap_entry_list_ty x, outstream_ty s)
{
     {
          int_ty t1_;
          IntMap_entry_list_ty t2_;
          t1_ = 0;
          t2_ = x;
          while(t2_ != NULL)
          {
               t2_ = t2_->tail;
               t1_++;
               
          }
          write_tag(t1_, s);
          t2_ = x;
          while(t1_ != 0)
          {
               IntMap_write_entry(t2_->head, s);
               t2_ = t2_->tail;
               t1_--;
               
          }
          
     }
     
}


