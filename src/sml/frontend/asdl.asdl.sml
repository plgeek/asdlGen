structure Asdl : Asdl_SIG = 
    struct
    open Base
    datatype asdl_type = SumType of (identifier * field list * constructor
                                     * constructor list)
                       | ProductType of (identifier * field * field list)
    and field = Id of (identifier list * identifier option)
              | Option of (identifier list * identifier option)
              | Sequence of (identifier list * identifier option)
    and constructor = Con of (identifier * field list)
    withtype asdl_module = {name:identifier,
                            imports:identifier list,
                            defs:asdl_type list}
    
    fun write_asdl_type x s = 
            (case (x) of 
                  (SumType(identifier1, field_list1, constructor1,
                          constructor_list1)) =>
                    (write_tag 1 s;
                      write_identifier identifier1 s;
                      write_list write_field field_list1 s;
                      write_constructor constructor1 s;
                      write_list write_constructor constructor_list1 s)
                | (ProductType(identifier1, field1, field_list1)) =>
                    (write_tag 2 s;
                      write_identifier identifier1 s;
                      write_field field1 s;
                      write_list write_field field_list1 s))
    and read_asdl_type s = 
            (case (read_tag s) of 
                  1 =>
                    let 
                        val identifier1 =  read_identifier s
                        val field_list1 =  read_list read_field s
                        val constructor1 =  read_constructor s
                        val constructor_list1 = 
                          read_list read_constructor s
                    in
                        SumType(identifier1, field_list1, constructor1,
                                constructor_list1)
                    end
                | 2 =>
                    let 
                        val identifier1 =  read_identifier s
                        val field1 =  read_field s
                        val field_list1 =  read_list read_field s
                    in
                        ProductType(identifier1, field1, field_list1)
                    end
                | _ => die ())
    and write_field x s = 
            (case (x) of 
                  (Id(identifier_list1, identifier_opt1)) =>
                    (write_tag 1 s;
                      write_list write_identifier identifier_list1 s;
                      write_option write_identifier identifier_opt1 s)
                | (Option(identifier_list1, identifier_opt1)) =>
                    (write_tag 2 s;
                      write_list write_identifier identifier_list1 s;
                      write_option write_identifier identifier_opt1 s)
                | (Sequence(identifier_list1, identifier_opt1)) =>
                    (write_tag 3 s;
                      write_list write_identifier identifier_list1 s;
                      write_option write_identifier identifier_opt1 s))
    and read_field s = 
            (case (read_tag s) of 
                  1 =>
                    let 
                        val identifier_list1 = 
                          read_list read_identifier s
                        val identifier_opt1 = 
                          read_option read_identifier s
                    in
                        Id(identifier_list1, identifier_opt1)
                    end
                | 2 =>
                    let 
                        val identifier_list1 = 
                          read_list read_identifier s
                        val identifier_opt1 = 
                          read_option read_identifier s
                    in
                        Option(identifier_list1, identifier_opt1)
                    end
                | 3 =>
                    let 
                        val identifier_list1 = 
                          read_list read_identifier s
                        val identifier_opt1 = 
                          read_option read_identifier s
                    in
                        Sequence(identifier_list1, identifier_opt1)
                    end
                | _ => die ())
    and write_constructor x s = 
            (case (x) of 
                  (Con(identifier1, field_list1)) =>
                    (write_tag 1 s;
                      write_identifier identifier1 s;
                      write_list write_field field_list1 s))
    and read_constructor s = 
            (case (read_tag s) of 
                  1 =>
                    let 
                        val identifier1 =  read_identifier s
                        val field_list1 =  read_list read_field s
                    in
                        Con(identifier1, field_list1)
                    end
                | _ => die ())
    and write_asdl_module x s = 
            let 
                val {name, imports, defs} =  x
            in
                (write_identifier name s;
                  write_list write_identifier imports s;
                  write_list write_asdl_type defs s)
            end
    and read_asdl_module s = 
            let 
                val name =  read_identifier s
                val imports =  read_list read_identifier s
                val defs =  read_list read_asdl_type s
            in
                {name=name, imports=imports, defs=defs}
            end
    
end