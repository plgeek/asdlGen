(*
  Machine generated. Edit at your own risk 
  Reproduce with the following
 --line_width=74
 --no_action=false
 --output_directory=frontend
 --sexp_pickler=true
 --view=SML
 --xml_pickler=false
 *)
structure AsdlUtil : AsdlUtil_SIG =
  struct
    
    fun attrbs_type_decl x_ = 
        (case (x_) of 
            (Asdl.SumType{name, attribs, c, cs}) => {name=name}
          | (Asdl.ProductType{name, f, fs}) => {name=name})
        (* end case *)
    
    and attrbs_decl x_ = 
        (case (x_) of 
            (Asdl.Module{name, imports, decls}) => {name=name}
          | (Asdl.PrimitiveModule{name, exports}) => {name=name}
          | (Asdl.View{name, decls}) => {name=name})
        (* end case *)
    
    and sexp_rd_import s_ = 
        
        ((SexpPkl.rd_lp s_);
         let
           val tmp_ = (case ((SexpPkl.get_sym s_)) of 
                 "Asdl_Imports" => let
                   val module = (StdPrimsUtil.sexp_rd_identifier s_)
                   val alias = (SexpPkl.rd_option StdPrimsUtil.sexp_rd_identifier s_)
                 in Asdl.Imports{module=module, alias=alias}
                 end
               | _ => (SexpPkl.die ()))
             (* end case *)
         in 
         ((SexpPkl.rd_rp s_);
          tmp_)
         end)
    
    and sexp_rd_path s_ = 
        
        ((SexpPkl.rd_lp s_);
         (SexpPkl.rd_sym "Asdl_path" s_);
         let
           val tmp_ = let
               val qualifier = (SexpPkl.rd_option StdPrimsUtil.sexp_rd_identifier s_)
               val base = (StdPrimsUtil.sexp_rd_identifier s_)
             in {qualifier=qualifier, base=base}
             end
         in 
         ((SexpPkl.rd_rp s_);
          tmp_)
         end)
    
    and sexp_rd_tycon s_ = 
        
        ((SexpPkl.rd_lp s_);
         let
           val tmp_ = (case ((SexpPkl.get_sym s_)) of 
                 "Asdl_Option" => Asdl.Option
               | "Asdl_Sequence" => Asdl.Sequence
               | "Asdl_Shared" => Asdl.Shared
               | _ => (SexpPkl.die ()))
             (* end case *)
         in 
         ((SexpPkl.rd_rp s_);
          tmp_)
         end)
    
    and sexp_rd_field s_ = 
        
        ((SexpPkl.rd_lp s_);
         (SexpPkl.rd_sym "Asdl_field" s_);
         let
           val tmp_ = let
               val typ = (sexp_rd_path s_)
               val label_opt = (SexpPkl.rd_option StdPrimsUtil.sexp_rd_identifier s_)
               val tycon_opt = (SexpPkl.rd_option sexp_rd_tycon s_)
             in {typ=typ, label_opt=label_opt, tycon_opt=tycon_opt}
             end
         in 
         ((SexpPkl.rd_rp s_);
          tmp_)
         end)
    
    and sexp_rd_constructor s_ = 
        
        ((SexpPkl.rd_lp s_);
         (SexpPkl.rd_sym "Asdl_constructor" s_);
         let
           val tmp_ = let
               val name = (StdPrimsUtil.sexp_rd_identifier s_)
               val fs = (SexpPkl.rd_list sexp_rd_field s_)
             in {name=name, fs=fs}
             end
         in 
         ((SexpPkl.rd_rp s_);
          tmp_)
         end)
    
    and sexp_rd_type_decl s_ = 
        
        ((SexpPkl.rd_lp s_);
         let
           val tmp_ = (case ((SexpPkl.get_sym s_)) of 
                 "Asdl_SumType" => let
                   val name = (StdPrimsUtil.sexp_rd_identifier s_)
                   val attribs = (SexpPkl.rd_list sexp_rd_field s_)
                   val c = (sexp_rd_constructor s_)
                   val cs = (SexpPkl.rd_list sexp_rd_constructor s_)
                 in Asdl.SumType{name=name, attribs=attribs, c=c, cs=cs}
                 end
               | "Asdl_ProductType" => let
                   val name = (StdPrimsUtil.sexp_rd_identifier s_)
                   val f = (sexp_rd_field s_)
                   val fs = (SexpPkl.rd_list sexp_rd_field s_)
                 in Asdl.ProductType{name=name, f=f, fs=fs}
                 end
               | _ => (SexpPkl.die ()))
             (* end case *)
         in 
         ((SexpPkl.rd_rp s_);
          tmp_)
         end)
    
    and sexp_rd_view_decl s_ = 
        
        ((SexpPkl.rd_lp s_);
         (SexpPkl.rd_sym "Asdl_view_decl" s_);
         let
           val tmp_ = let
               val entity = (SexpPkl.rd_list StdPrimsUtil.sexp_rd_identifier s_)
               val prop = (StdPrimsUtil.sexp_rd_string s_)
               val value = (StdPrimsUtil.sexp_rd_string s_)
             in {entity=entity, prop=prop, value=value}
             end
         in 
         ((SexpPkl.rd_rp s_);
          tmp_)
         end)
    
    and sexp_rd_decl s_ = 
        
        ((SexpPkl.rd_lp s_);
         let
           val tmp_ = (case ((SexpPkl.get_sym s_)) of 
                 "Asdl_Module" => let
                   val name = (StdPrimsUtil.sexp_rd_identifier s_)
                   val imports = (SexpPkl.rd_list sexp_rd_import s_)
                   val decls = (SexpPkl.rd_list sexp_rd_type_decl s_)
                 in Asdl.Module{name=name, imports=imports, decls=decls}
                 end
               | "Asdl_PrimitiveModule" => let
                   val name = (StdPrimsUtil.sexp_rd_identifier s_)
                   val exports = (SexpPkl.rd_list StdPrimsUtil.sexp_rd_identifier s_)
                 in Asdl.PrimitiveModule{name=name, exports=exports}
                 end
               | "Asdl_View" => let
                   val name = (StdPrimsUtil.sexp_rd_identifier s_)
                   val decls = (SexpPkl.rd_list sexp_rd_view_decl s_)
                 in Asdl.View{name=name, decls=decls}
                 end
               | _ => (SexpPkl.die ()))
             (* end case *)
         in 
         ((SexpPkl.rd_rp s_);
          tmp_)
         end)
    
    and sexp_rd_constructor_list s_ = 
        (SexpPkl.rd_list sexp_rd_constructor s_)
    
    and sexp_rd_field_list s_ = 
        (SexpPkl.rd_list sexp_rd_field s_)
    
    and sexp_rd_tycon_option s_ = 
        (SexpPkl.rd_option sexp_rd_tycon s_)
    
    and sexp_rd_view_decl_list s_ = 
        (SexpPkl.rd_list sexp_rd_view_decl s_)
    
    and sexp_rd_type_decl_list s_ = 
        (SexpPkl.rd_list sexp_rd_type_decl s_)
    
    and sexp_rd_import_list s_ = 
        (SexpPkl.rd_list sexp_rd_import s_)
    
    and sexp_wr_import x_ s_ = 
        (case (x_) of 
            (Asdl.Imports{module, alias}) => 
            ((SexpPkl.wr_lp s_);
             (SexpPkl.wr_sym "Asdl_Imports" s_);
             (StdPrimsUtil.sexp_wr_identifier module s_);
             (SexpPkl.wr_option StdPrimsUtil.sexp_wr_identifier alias s_);
             (SexpPkl.wr_rp s_)))
        (* end case *)
    
    and sexp_wr_path x_ s_ = 
        (case (x_) of 
            {qualifier, base} : Asdl.path => 
            ((SexpPkl.wr_lp s_);
             (SexpPkl.wr_sym "Asdl_path" s_);
             (SexpPkl.wr_option StdPrimsUtil.sexp_wr_identifier qualifier s_);
             (StdPrimsUtil.sexp_wr_identifier base s_);
             (SexpPkl.wr_rp s_)))
        (* end case *)
    
    and sexp_wr_tycon x_ s_ = 
        (case (x_) of 
            Asdl.Option => 
            ((SexpPkl.wr_lp s_);
             (SexpPkl.wr_sym "Asdl_Option" s_);
             (SexpPkl.wr_rp s_))
          | Asdl.Sequence => 
            ((SexpPkl.wr_lp s_);
             (SexpPkl.wr_sym "Asdl_Sequence" s_);
             (SexpPkl.wr_rp s_))
          | Asdl.Shared => 
            ((SexpPkl.wr_lp s_);
             (SexpPkl.wr_sym "Asdl_Shared" s_);
             (SexpPkl.wr_rp s_)))
        (* end case *)
    
    and sexp_wr_field x_ s_ = 
        (case (x_) of 
            {typ, label_opt, tycon_opt} : Asdl.field => 
            ((SexpPkl.wr_lp s_);
             (SexpPkl.wr_sym "Asdl_field" s_);
             (sexp_wr_path typ s_);
             (SexpPkl.wr_option StdPrimsUtil.sexp_wr_identifier label_opt s_);
             (SexpPkl.wr_option sexp_wr_tycon tycon_opt s_);
             (SexpPkl.wr_rp s_)))
        (* end case *)
    
    and sexp_wr_constructor x_ s_ = 
        (case (x_) of 
            {name, fs} : Asdl.constructor => 
            ((SexpPkl.wr_lp s_);
             (SexpPkl.wr_sym "Asdl_constructor" s_);
             (StdPrimsUtil.sexp_wr_identifier name s_);
             (SexpPkl.wr_list sexp_wr_field fs s_);
             (SexpPkl.wr_rp s_)))
        (* end case *)
    
    and sexp_wr_type_decl x_ s_ = 
        (case (x_) of 
            (Asdl.SumType{name, attribs, c, cs}) => 
            ((SexpPkl.wr_lp s_);
             (SexpPkl.wr_sym "Asdl_SumType" s_);
             (StdPrimsUtil.sexp_wr_identifier name s_);
             (SexpPkl.wr_list sexp_wr_field attribs s_);
             (sexp_wr_constructor c s_);
             (SexpPkl.wr_list sexp_wr_constructor cs s_);
             (SexpPkl.wr_rp s_))
          | (Asdl.ProductType{name, f, fs}) => 
            ((SexpPkl.wr_lp s_);
             (SexpPkl.wr_sym "Asdl_ProductType" s_);
             (StdPrimsUtil.sexp_wr_identifier name s_);
             (sexp_wr_field f s_);
             (SexpPkl.wr_list sexp_wr_field fs s_);
             (SexpPkl.wr_rp s_)))
        (* end case *)
    
    and sexp_wr_view_decl x_ s_ = 
        (case (x_) of 
            {entity, prop, value} : Asdl.view_decl => 
            ((SexpPkl.wr_lp s_);
             (SexpPkl.wr_sym "Asdl_view_decl" s_);
             (SexpPkl.wr_list StdPrimsUtil.sexp_wr_identifier entity s_);
             (StdPrimsUtil.sexp_wr_string prop s_);
             (StdPrimsUtil.sexp_wr_string value s_);
             (SexpPkl.wr_rp s_)))
        (* end case *)
    
    and sexp_wr_decl x_ s_ = 
        (case (x_) of 
            (Asdl.Module{name, imports, decls}) => 
            ((SexpPkl.wr_lp s_);
             (SexpPkl.wr_sym "Asdl_Module" s_);
             (StdPrimsUtil.sexp_wr_identifier name s_);
             (SexpPkl.wr_list sexp_wr_import imports s_);
             (SexpPkl.wr_list sexp_wr_type_decl decls s_);
             (SexpPkl.wr_rp s_))
          | (Asdl.PrimitiveModule{name, exports}) => 
            ((SexpPkl.wr_lp s_);
             (SexpPkl.wr_sym "Asdl_PrimitiveModule" s_);
             (StdPrimsUtil.sexp_wr_identifier name s_);
             (SexpPkl.wr_list StdPrimsUtil.sexp_wr_identifier exports s_);
             (SexpPkl.wr_rp s_))
          | (Asdl.View{name, decls}) => 
            ((SexpPkl.wr_lp s_);
             (SexpPkl.wr_sym "Asdl_View" s_);
             (StdPrimsUtil.sexp_wr_identifier name s_);
             (SexpPkl.wr_list sexp_wr_view_decl decls s_);
             (SexpPkl.wr_rp s_)))
        (* end case *)
    
    and sexp_wr_constructor_list x_ s_ = 
        (SexpPkl.wr_list sexp_wr_constructor x_ s_)
    
    and sexp_wr_field_list x_ s_ = 
        (SexpPkl.wr_list sexp_wr_field x_ s_)
    
    and sexp_wr_tycon_option x_ s_ = 
        (SexpPkl.wr_option sexp_wr_tycon x_ s_)
    
    and sexp_wr_view_decl_list x_ s_ = 
        (SexpPkl.wr_list sexp_wr_view_decl x_ s_)
    
    and sexp_wr_type_decl_list x_ s_ = 
        (SexpPkl.wr_list sexp_wr_type_decl x_ s_)
    
    and sexp_wr_import_list x_ s_ = 
        (SexpPkl.wr_list sexp_wr_import x_ s_)
    
    
  end (* struct *)

